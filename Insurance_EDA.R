##########################   Title: Business Analytics  ########################
#########################    Insurance Business Report
###################################    EDA
#############################   Jan Bohnenstengel  


# ---- Setup -------------------------------------------------------------------
library(tidyverse)
library(randomForest)
library(ggridges)
library(RColorBrewer)
library(sysfonts)
library(patchwork)
library(showtext)
library(rpart)
library(rpart.plot)
library(kableExtra)

rm(list = ls())

Sys.setlocale("LC_TIME", "C")

options(scipen = 100, digits = 5)

font_add_google(name = "Roboto Condensed",
                family = "Roboto Condensed")

showtext.auto()
showtext_opts(dpi = 320)


setwd("C:/Users/janni/Uni lokal/WU/Semester_2/insurance_claims_git")
raw_data <- read.csv("data/insurance_claims.csv")

raw_data$holdout_order <- 1:nrow(raw_data)
raw_data <- raw_data |> 
  relocate(holdout_order, .before = sex)


# ---- Transforming Variables --------------------------------------------------
summary(raw_data)

table(raw_data$fraud)
# --> Very high share of fraudulent cases!!!


table(raw_data$sex)

### Sex
raw_data <- raw_data |> 
  mutate(female = case_when(
    sex == "F" ~ 1,
    sex == "M" ~ 0,
    sex == "" ~ 2
  )) |> 
  relocate(female, .after = sex)

raw_data$sex <- NULL


## ---- Log and factorize Vars ------------------------------------------------- 

### Sum insured
raw_data <- raw_data |> 
  mutate(sum_insured_log = log(sum_insured+1)) |> 
  relocate(sum_insured_log, .after = sum_insured)
summary(raw_data$sum_insured)

### Claim amount
raw_data <- raw_data |> 
  mutate(claim_amt_log = log(claim_amt+1)) |> 
  relocate(claim_amt_log, .after = claim_amt)

### Settle amount
raw_data <- raw_data |> 
  mutate(settle_amt_log = log(settle_amt+1)) |> 
  relocate(settle_amt_log, .after = settle_amt)

### factorize product code & policy type 
raw_data <- raw_data |> 
  mutate(prod_code = as.factor(prod_code)) |> 
  mutate(policy_type = as.factor(policy_type))


### Factorize payment type, hospital type
raw_data$payment_type <- as.factor(raw_data$payment_type)
raw_data$hospital_networked <- as.factor(raw_data$hosp_type)
raw_data$hosp_type <- NULL


## ---- Date Vars --------------------------------------------------------------

date_vars <- c("dob", "policy_start_dt", "policy_end_dt", "claim_dt",
               "admit_dt", "discharge_dt", "payment_dt")

for (var in date_vars) {
  raw_data[[var]] <- as.Date(raw_data[[var]], format = "%d-%b-%Y")
}

# Age at time of claim
raw_data$age_at_claim <- as.numeric(difftime(raw_data$claim_dt, raw_data$dob, units = "days")) %/% 365

# Policy duration
raw_data$policy_duration_days <- as.numeric(raw_data$policy_end_dt - raw_data$policy_start_dt)

# Time between claim and admission
raw_data$claim_to_admit_days <- as.numeric(raw_data$claim_dt - raw_data$admit_dt)

raw_data <- raw_data |> 
  mutate(claim_before_admission = case_when(
    claim_to_admit_days < 0 ~ 1,
    claim_to_admit_days >= 0 ~ 0
  ))

# Time between claim and discharge
raw_data$claim_since_discharge_days <- as.numeric(raw_data$claim_dt - raw_data$discharge_dt)

# Length of hospital stay
raw_data$hospital_stay_days <- as.numeric(raw_data$discharge_dt - raw_data$admit_dt)

# Time to payment
raw_data$payment_delay_days <- as.numeric(raw_data$payment_dt - raw_data$claim_dt)

# Claim made close to expiry?
raw_data$days_to_policy_expiry <- as.numeric(raw_data$policy_end_dt - raw_data$claim_dt)
raw_data$near_expiry_claim <- raw_data$days_to_policy_expiry <= 7

# Claim made early in policy?
raw_data$days_since_policy_start <- as.numeric(raw_data$claim_dt - raw_data$policy_start_dt)
raw_data$early_claim <- raw_data$days_since_policy_start <= 7

# Claim reported on weekend?
raw_data$claim_weekday <- weekdays(raw_data$claim_dt)
raw_data$claim_on_weekend <- raw_data$claim_weekday %in% c("Saturday", "Sunday")


## ---- Ratios -----------------------------------------------------------------

raw_data$copay_to_claim_ratio <- raw_data$copayment/raw_data$claim_amt

raw_data$settled_to_claim_ratio <- raw_data$settle_amt/raw_data$claim_amt

raw_data$claim_to_insured_ratio <- raw_data$claim_amt/raw_data$sum_insured

raw_data$claim_amt_per_day <- raw_data$claim_amt/raw_data$hospital_stay_days

raw_data <- raw_data |> 
  mutate(
    copay_to_claim_ratio = if_else(is.na(copay_to_claim_ratio), 0, copay_to_claim_ratio),
    settled_to_claim_ratio = if_else(is.na(settled_to_claim_ratio), 0, settled_to_claim_ratio)
  )


## ---- Recommendation Variable ------------------------------------------------ 
raw_data <- raw_data |> 
  mutate(recommendation = as.factor(recommendation))

raw_data <- raw_data |> 
  mutate(recommendation_dummy = case_when(
    recommendation == "Genuine" ~ 0,
    recommendation == "Investigate" | recommendation == "Discuss" ~ 1
  ))
raw_data <- raw_data |> 
  relocate(recommendation_dummy, .after = recommendation)

raw_data <- raw_data |> 
  mutate(recommendation_num = case_when(
    recommendation == "Genuine" ~ 0,
    recommendation == "Discuss" ~ 1,
    recommendation == "Investigate" ~ 2
  ))

raw_data <- raw_data |> 
  relocate(recommendation_num, .after = recommendation_dummy)


colSums(is.na(raw_data))



# ---- Infinite Values ---------------------------------------------------------
sapply(raw_data, function(x) sum(!is.finite(x)))

impute_inf_with_percentile <- function(vec, p = 0.99) {
  
  inf_idx <- is.infinite(vec) & vec > 0
  minus_inf_idx <- is.infinite(vec) & vec < 0
  
  pct_99 <- quantile(vec[is.finite(vec)], probs = p, na.rm = TRUE)
  
  vec[inf_idx] <- pct_99
  
  vec[minus_inf_idx] <- NA
  
  vec[is.nan(vec)] <- NA
  
  return(vec)
}

raw_data$claim_amt_per_day <- impute_inf_with_percentile(raw_data$claim_amt_per_day)
raw_data$claim_to_insured_ratio <- impute_inf_with_percentile(raw_data$claim_to_insured_ratio)

##########  Numeric fraud var
raw_data$fraud <- as.numeric(raw_data$fraud)
table(raw_data$fraud)

# ---- Separate and Save Data --------------------------------------------------
use_data <- raw_data[raw_data$holdout == 0, ]
to_predict_data  <- raw_data[raw_data$holdout == 1, ]

table(use_data$fraud, useNA = "ifany")
table(to_predict_data$fraud, useNA = "ifany")

######  Save data
save(raw_data, file = "data/raw_data.RData")
save(use_data, file = "data/use_data.RData")
save(to_predict_data, file = "data/to_predict_data.RData")

summary(use_data$claim_amt_per_day)

# ---- Data Exploration  -------------------------------------------------------

## ---- Exploring the recommendation var ---------------------------------------
rm(list=ls())
load("data/use_data.RData")

use_data$recommendation <- as.factor(use_data$recommendation)
use_data$fraud <- as.factor(use_data$fraud)

table(use_data$recommendation, use_data$fraud)

fraud_rate <- mean(use_data$fraud == 1)
flagged_rate <- mean(use_data$recommendation_dummy == 1)
industry_low <- 0.10
industry_high <- 0.20

rate_df <- data.frame(
  Type = c("Expert Classification", "Flagged by Company", "Industry Rate (Low)", "Industry Rate (High)"),
  Rate = c(fraud_rate, flagged_rate, industry_low, industry_high)
)

fraud_vs_recommenation_bar <- ggplot(rate_df, aes(x = Type, y = Rate, fill = Type)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("#08306B", "#2171B5", "#6BAED6", "#C6DBEF")) +
  labs(
    title = "Fraud Rate Detection at MediMaybe",
    y = "Rate",
    x = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")
fraud_vs_recommenation_bar


## ---- Creating a Pseudo Policy ID --------------------------------------------

use_data$pseudo_policy_id <- paste(
  use_data$sex,
  use_data$dob,
  use_data$policy_start_dt,
  use_data$policy_end_dt,
  use_data$policy_type,
  use_data$prod_code,
  use_data$sum_insured,
  sep = "_"
)


head(use_data$pseudo_policy_id, 10)

use_data |> 
  count(pseudo_policy_id) |> 
  filter(n > 1) |> 
  nrow()

n_distinct(use_data$pseudo_policy_id)


## ---- Rule-by-Rule Analysis --------------------------------------------------

#1. Non-network hospital
use_data <- use_data |> 
  mutate(rule1_non_network = case_when(
    hospital_networked == "Y" ~ 0,
    hospital_networked == "N" ~ 1,
    .default = 0))

rule1_table <- prop.table(table(use_data$rule1_non_network, use_data$fraud), 1)
t_test_result_rule1 <- t.test(as.numeric(fraud)-1 ~ rule1_non_network, data = use_data)
t_test_result_rule1
#p-value = 0.33

#2. Multiple claims
use_data <- use_data |> 
  group_by(pseudo_policy_id) |> 
  mutate(rule2_multiple_claims = n() > 1) |> 
  ungroup()
use_data$rule2_multiple_claims <- as.numeric(use_data$rule2_multiple_claims)

rule2_table <- prop.table(table(use_data$rule2_multiple_claims, use_data$fraud), 1)
rule2_table
t_test_result_rule2 <- t.test(as.numeric(fraud)-1 ~ rule2_multiple_claims, data = use_data)
t_test_result_rule2
#p-value = 0.82

#3. group claims from same hospital
group_claims_df <- use_data |> 
  group_by(hospital_id, claim_dt) |>
  summarise(
    n_claims = n(),
    .groups = "drop"
  )

use_data <- use_data |> 
  left_join(group_claims_df, by = c("hospital_id", "claim_dt")) |> 
  mutate(rule3_group_claim = ifelse(n_claims >= 3, 1, 0)) |> 
  select(-n_claims)
rm(group_claims_df)

rule3_table <- prop.table(table(use_data$rule3_group_claim, use_data$fraud), 1)
rule3_table
t_test_result_rule3 <- t.test(as.numeric(fraud)-1 ~ rule3_group_claim, data = use_data)
t_test_result_rule3
#p-value = 0.78

#4 & #5. claim near expiry date or start date
use_data |>
  ggplot(aes(x = days_since_policy_start)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "white") +
  labs(title = "Days Since Policy Start")

use_data |>
  ggplot(aes(x = days_to_policy_expiry)) +
  geom_histogram(bins = 50, fill = "lightblue", color = "white") +
  labs(title = "Days to Policy Expiry")

quantile(use_data$days_since_policy_start, probs = seq(0.05, 0.9, by = 0.05), na.rm = TRUE)
quantile(use_data$days_to_policy_expiry, probs = seq(0.05, 0.9, by = 0.05), na.rm = TRUE)


use_data <- use_data |> 
  mutate(
    rule5_near_start = if_else(days_since_policy_start <= 30, 1, 0),
    rule4_near_expiry = if_else(days_to_policy_expiry <= 14, 1, 0)
  )

rule4_table <- prop.table(table(use_data$rule4_near_expiry, use_data$fraud), 1)
rule5_table <- prop.table(table(use_data$rule5_near_start, use_data$fraud), 1)
rule4_table
rule5_table

t_test_result_rule4 <- t.test(as.numeric(fraud)-1 ~ rule4_near_expiry, data = use_data)
t_test_result_rule4
#p-value = 0.0066 !!

t_test_result_rule5 <- t.test(as.numeric(fraud)-1 ~ rule5_near_start, data = use_data)
t_test_result_rule5
#p-value = 0.71

#6. No pre- or post claim?
use_data <- use_data |> 
  mutate(rule6_no_prepost = case_when(
    pre_hosp_exp <= 0 | post_hosp_exp <= 0 ~ 1,
    .default = 0
  ))

rule6_table <- prop.table(table(use_data$rule6_no_prepost, use_data$fraud), 1)
rule6_table
# This is suspicious!!!

t_test_result_rule6 <- t.test(as.numeric(fraud)-1 ~ rule6_no_prepost, data = use_data)
t_test_result_rule6
# p-value = 0.00046

#7 Misinformation in post
quantile(use_data$sum_insured, probs = seq(0.05, 0.9, by = 0.05), na.rm = TRUE)

use_data <- use_data |> 
  rowwise() |> 
  mutate(has_negative = any(c_across(where(is.numeric)) < 0, na.rm = TRUE)) |> 
  ungroup()

use_data <- use_data |> 
  mutate(rule7_misinformation = case_when(
    has_negative == TRUE ~ 1,
    sum_insured == 0 | claim_amt == 0 | discharge_dt < admit_dt ~ 1,
    nursing_chg == 0 & surgery_chg == 0 & cons_fee == 0 & test_chg == 0 & pharmacy_cost == 0 & other_chg == 0 ~ 1,
    post_hosp_exp > 0 & hospital_stay_days <= 0 ~ 1,
    pre_hosp_exp > 0 & hospital_stay_days <= 0 ~ 1,
    age_at_claim < - 3 ~ 1,
    .default = 0
  ))

rule7_table <- prop.table(table(use_data$rule7_misinformation, use_data$fraud), 1)
rule7_table

t_test_result_rule7 <- t.test(as.numeric(fraud)-1 ~ rule7_misinformation, data = use_data)
t_test_result_rule7
#p-value = 0.062  --> at the 10% level


# 8 Weekend claim
use_data <- use_data |> 
  rename(rule8_weekend = claim_on_weekend) |> 
  mutate(rule8_weekend = as.numeric(rule8_weekend))
rule8_table <- prop.table(table(use_data$rule8_weekend, use_data$fraud), 1)
rule8_table

t_test_result_rule8 <- t.test(as.numeric(fraud)-1 ~ rule8_weekend, data = use_data)
t_test_result_rule8
#p-value = 0.65

# 9&10 costlier investigations
cost_vars = c("nursing_chg", "surgery_chg", "cons_fee", "test_chg", "pharmacy_cost",
              "other_chg", "other_chg_non_hosp")

percentiles <- sapply(use_data[cost_vars], function(x) quantile(x, 0.95, na.rm = TRUE))

use_data <- use_data |>
  rowwise() |>
  mutate(rule9_costly = any(c_across(all_of(cost_vars)) > percentiles)) |>
  ungroup()
use_data$rule9_costly <- as.numeric(use_data$rule9_costly)

rule9_table <- prop.table(table(use_data$rule9_costly, use_data$fraud), 1)
rule9_table

t_test_result_rule9 <- t.test(as.numeric(fraud)-1 ~ rule9_costly, data = use_data)
t_test_result_rule9
#p-value = 0.58

# 11 Claim before discharge?
use_data <- use_data |> 
  mutate(rule11_day_before_discharge = case_when(
           claim_since_discharge_days < -1 ~ 1,
           .default = 0
         ))
rule11_table <- prop.table(table(use_data$rule11_day_before_discharge, use_data$fraud), 1)
rule11_table

t_test_result_rule11 <- t.test(as.numeric(fraud)-1 ~ rule11_day_before_discharge, data = use_data)
t_test_result_rule11
#p-value = 0.49

# 12 Claim more than 2 days after admission?
use_data <- use_data |> 
  mutate(rule12_claim_after_48h = case_when(
    claim_to_admit_days < 2 ~ 1,
    .default = 0
  ))

rule12_table <- prop.table(table(use_data$rule12_claim_after_48h, use_data$fraud), 1)
rule12_table

t_test_result_rule12 <- t.test(as.numeric(fraud)-1 ~ rule12_claim_after_48h, data = use_data)
t_test_result_rule12
#p-value = 0.48

### ---- Table: Fraud Rates by Rule Y/N ----------------------------------------
rule_vars <- names(select(use_data, starts_with("rule")))

get_fraud_share <- function(varname) {
  tab <- prop.table(table(use_data[[varname]], use_data$fraud), 1)
  shares <- as.numeric(tab[, "1"])
  levels <- rownames(tab)
  
  tibble(
    level = levels,
    fraud_share = shares
  )
}


fraud_share_df <- map_dfr(rule_vars, function(v) {
  get_fraud_share(v) |>
    mutate(variable = v)
})


fraud_share_wide <- fraud_share_df |>
  pivot_wider(
    names_from = level,
    values_from = fraud_share,
    names_prefix = "level_"
  ) |>
  mutate(diff = abs(level_1 - level_0)) |> 
  rename(Variable = variable,
         No = level_0,
         Yes = level_1,
         Difference = diff)
fraud_share_wide

kable(fraud_share_wide, format = "latex", digits = 4, booktabs = TRUE, caption = "Fraud Rates by Rule-Based Flags")

rule_vars

save(use_data, file = "data/use_data.RData")


### ---- Check most telling Rule combinations ----------------------------------
explore_data <- use_data
explore_data$fraud <- as.numeric(explore_data$fraud) -1
table(explore_data$fraud)
class((explore_data$fraud))

rules_df <- explore_data |> 
  select(all_of(rule_vars), fraud)

fraud_rules <- rules_df |> 
  filter(fraud == 1) |> 
  select(-fraud)

long_rules <- fraud_rules |> 
  mutate(obs_id = row_number()) |> 
  pivot_longer(-obs_id, names_to = "rule", values_to = "value") |> 
  filter(value == 1)

rule_pairs <- long_rules |>
  group_by(obs_id) |>
  summarise(rules = list(rule), .groups = "drop") |>
  mutate(rule_count = lengths(rules)) |>
  filter(rule_count >= 2) |>
  mutate(pairs = map(rules, ~combn(.x, 2, simplify = FALSE))) |>
  select(pairs) |>
  unnest(pairs) |>
  mutate(pair = map_chr(pairs, ~paste(sort(.x), collapse = " & "))) |>
  count(pair, sort = TRUE)

head(rule_pairs, 10)




## ---- Grouping Variables -----------------------------------------------------

explore_data <- explore_data |> 
  mutate(
    claim_month   = floor_date(claim_dt, "month"),
    claim_quarter = floor_date(claim_dt, "quarter"),
    age_group     = cut(age_at_claim,
                        breaks = c(-4, 25, 40, 55, 70, Inf),
                        labels = c("<25","25–40","40–55","55–70","70+"))
  )

## ---- Fraud Rates per calender month -----------------------------------------
monthly_summary <- explore_data |> 
  group_by(claim_month) |> 
  summarise(
    total_claims = n(),
    fraud_rate   = mean(fraud)
  )

claims_per_month <- ggplot(monthly_summary, aes(x = claim_month)) +
  geom_line(aes(y = total_claims / 1000), color = "steelblue", size = 1.2) +
  labs(title = "Monthly Claims",
       y = "Total Claims\n\n(in thousand)",
       x = "Calender Month") +
  xlim(as.Date("2007-12-01"), as.Date("2011-03-01")) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(plot.title = element_text(size = 20),
        axis.title.y = element_text(size = 12, angle = 0, vjust = 0.45),
        axis.title.x = element_text(size = 10))

fraudrate_per_month <- ggplot(monthly_summary, aes(x = claim_month)) +
  geom_line(aes(y = fraud_rate * 100), color = "firebrick", size = 1.2) +
  scale_y_continuous(
    name = "Fraud Rate in %") +
  ylim(18, 25) +
  xlim(as.Date("2007-12-01"), as.Date("2011-03-01")) +
  labs(title = "Monthly Fraud Rate",
       y = "Fraud Rate in %",
       x = "Calender Month") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(plot.title = element_text(size = 20),
        axis.title.y = element_text(size = 12, angle = 0, vjust = 0.5),
        axis.title.x = element_text(size = 10))

analysis_time <- claims_per_month / fraudrate_per_month
analysis_time
ggsave("figures/monthly_claims_and_fraudrate.png", plot = analysis_time,
       width = 10, height = 7.9, dpi = 320)


## ---- Smoothed Fraud Rates per Hosital/ZIP code ------------------------------
p0 <- mean(explore_data$fraud)    
m  <- 50

hospital_smooth <- explore_data |>
  group_by(hospital_id) |>
  summarise(
    n_claims   = n(),
    fraud_obs  = sum(fraud),
    raw_rate   = fraud_obs / n_claims
  ) |>
  mutate(
    smooth_rate_hospital = (fraud_obs + m * p0) / (n_claims + m)
  ) |>
  arrange(desc(smooth_rate_hospital))

zip_smooth <- explore_data |>
  group_by(hos_zipcode) |>
  summarise(
    n_claims   = n(),
    fraud_obs  = sum(fraud),
    raw_rate   = fraud_obs / n_claims
  ) |>
  mutate(
    smooth_rate_zip = (fraud_obs + m * p0) / (n_claims + m)
  ) |>
  arrange(desc(smooth_rate_zip))

head(hospital_smooth)
tail(hospital_smooth)
head(zip_smooth)
tail(zip_smooth)

hospital_smooth |> 
  ggplot(aes(x = smooth_rate_hospital)) +
  geom_density()

hospital_smooth |> 
  arrange(desc(n_claims)) |> 
  head()

use_data |> 
  mutate(fraud = as.numeric(fraud)) |> 
  group_by(hospital_id) |> 
  summarise(n_claims = n(),
            fraud_obs = sum(fraud),
            fraud_rate = fraud_obs/n_claims) |> 
  arrange(desc(n_claims)) |>  
  head()

## ---- Cluster Hospitals by fraud rate & n_claims -----------------------------
explore_data <- explore_data |> 
  left_join(
    hospital_smooth |>  select(hospital_id, smooth_rate_hospital),
    by = "hospital_id"
  )

hosp_feats <- hospital_smooth |> 
  select(hospital_id, n_claims, smooth_rate_hospital)

# Scale & cluster
hosp_scaled <- scale(hosp_feats |> select(-hospital_id))
set.seed(123)
km <- kmeans(hosp_scaled, centers = 4, nstart = 50)

hosp_feats$cluster <- factor(km$cluster)

# Quick visualization
ggplot(hosp_feats, aes(x = n_claims, y = smooth_rate_hospital, color = cluster)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  labs(
    title = "Hospital Clusters by Volume & Smoothed Fraud Rate",
    x = "Claims (log scale)",
    y = "Smoothed Fraud Rate"
  ) +
  theme_minimal()

####  Get Clusters into main data
hosp_cluster <- hosp_feats |> 
  select(hospital_id, cluster)

explore_data <- explore_data |> 
  left_join(hosp_cluster, by = "hospital_id")


cluster_claim_counts <- explore_data |> 
  count(cluster) |> 
  rename(n_claims = n)

print(cluster_claim_counts)


# ---- Descriptive Analysis  ---------------------------------------------------

## ---- Regional Analysis ------------------------------------------------------

explore_data <- explore_data |> 
  left_join(
    zip_smooth |>  select(hos_zipcode, smooth_rate_zip),
    by = "hos_zipcode"
  )


zip_smooth |>
  mutate(region = str_sub(as.character(hos_zipcode), 1, 2)) |>
  distinct(region) |>
  count()

zip_smooth <- zip_smooth |>
  mutate(region = str_sub(as.character(hos_zipcode), 1, 2)) |>
  group_by(region) |>
  mutate(region_mean_smooth_rate_zip = mean(smooth_rate_zip, na.rm = TRUE)) |>
  ungroup()

# Summarize mean fraud rate per prefix
prefix_counts <- zip_smooth |>
  mutate(region = str_sub(as.character(hos_zipcode), 1, 2)) |>
  count(region, name = "n_zipcodes")

prefix_summary <- zip_smooth |>
  group_by(region) |>
  summarise(region_mean_smooth_rate_zip = mean(smooth_rate_zip, na.rm = TRUE), .groups = "drop") |>
  left_join(prefix_counts, by = "region") |>
  arrange(desc(region_mean_smooth_rate_zip))

explore_data <- explore_data |>
  mutate(region = str_sub(as.character(hos_zipcode), 1, 2))

prefix_additional <- explore_data |>
  group_by(region) |>
  summarise(
    n_claims = n(),
    n_hospitals = n_distinct(hospital_id),
    .groups = "drop"
  )

regional_summary <- prefix_summary |>
  left_join(prefix_additional, by = "region")

head(regional_summary, 15)
tail(regional_summary, 15)

######### Redo whole thing at broader level, using earlier stuff, so DON'T DELETE PREVIOUS LINES
regional_summary <- explore_data |>
  group_by(region) |>
  summarise(
    n_claims = n(),
    n_frauds = sum(fraud, na.rm = TRUE),
    .groups = "drop"
  )

overall_fraud_rate <- mean(explore_data$fraud, na.rm = TRUE)

regional_summary <- regional_summary |>
  mutate(
    smooth_rate_region = (n_frauds + 5 * overall_fraud_rate) / (n_claims + 5)
  )

extra_stats <- explore_data |>
  group_by(region) |>
  summarise(
    n_zipcodes = n_distinct(hos_zipcode),
    n_hospitals = n_distinct(hospital_id),
    .groups = "drop"
  )

regional_summary <- regional_summary |>
  left_join(extra_stats, by = "region") |> 
  left_join(prefix_summary, by = "region") |> 
  select(-n_zipcodes.y) |> 
  rename(n_zipcodes = n_zipcodes.x)

regional_summary |> 
  arrange(desc(region_mean_smooth_rate_zip)) |> 
  head(10)

regional_summary |> 
  arrange(desc(region_mean_smooth_rate_zip)) |> 
  tail(10)

cor(regional_summary$smooth_rate_region, regional_summary$region_mean_smooth_rate_zip)

regional_summary <- regional_summary |>
  mutate(
    z_smooth_region = scale(smooth_rate_region)[, 1],
    z_region_mean_smooth_zip = scale(region_mean_smooth_rate_zip)[, 1]
  )

regional_summary <- regional_summary |> 
  mutate(raw_fraud_rate = n_frauds/n_claims)

regional_summary <- regional_summary[, c(1,10,4,7,8,9,2,3,5, 6)]
save(regional_summary, file = "data/regional_summary.RData")

### Scatter

regiona_claims_vs_fraud_scatter <- regional_summary |>
  ggplot(aes(x = region_mean_smooth_rate_zip, y = n_claims)) +
  geom_point(size = 1.5, alpha = 0.6, color = "#E34A33") +
  geom_smooth(method = "lm", se = TRUE, color = "black", size = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title    = "Claims Volume vs. ZIP-Level Smoothed Fraud Rate",
    subtitle = "Linear fit (solid) and identity line (dashed)",
    x        = "Mean ZIP-Level Smoothed Fraud Rate",
    y        = "Number of Claims"
  ) +
  ylim(0, 690) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.title       = element_text(face = "bold", size = 24, hjust = 0.5),
    plot.subtitle    = element_text(size = 18, color = "grey40", hjust = 0.5),
    axis.title.x     = element_text(size = 14),
    axis.title.y     = element_text(size = 14),
    axis.text        = element_text(size = 12),
    legend.position  = "none"
  )
ggsave(regiona_claims_vs_fraud_scatter,
       file="figures/regional_claims_vs_fraud_scatter.png",
       width = 12, height = 10, bg = "white",units = "in", dpi = 320)


### Bars
extreme_regions <- regional_summary |>
  arrange(desc(region_mean_smooth_rate_zip)) |>
  slice_head(n = 10) |>
  bind_rows(
    regional_summary |>
      arrange(region_mean_smooth_rate_zip) |>
      slice_head(n = 10)
  ) |>
  mutate(
    region = fct_rev(fct_reorder(region, region_mean_smooth_rate_zip, .desc = TRUE))
  )

extreme_long <- extreme_regions |>
  select(region, region_mean_smooth_rate_zip, smooth_rate_region) |>
  pivot_longer(
    cols = c(region_mean_smooth_rate_zip, smooth_rate_region),
    names_to  = "method",
    values_to = "rate"
  ) |>
  mutate(
    method = factor(
      method,
      levels = c("smooth_rate_region", "region_mean_smooth_rate_zip")
    )
  )

region_bars <- ggplot(extreme_long, aes(x = region, y = rate, fill = method)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  geom_hline(yintercept = 0.217, linetype = "dashed", color = "grey30", size = 0.8) +
  scale_fill_manual(
    values = c(
      "region_mean_smooth_rate_zip" = "#4A90E2",
      "smooth_rate_region"          = "#7FC97F"
    ),
    labels = c(
      "region_mean_smooth_rate_zip" = "Smoothed at ZIP level (strong prior)",
      "smooth_rate_region"          = "Smoothed at region level (weak prior)"
    )
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    title    = "Top & Bottom 10 Regions by ZIP-Level Smoothed Fraud Rate",
    subtitle = "Dashed line shows overall mean fraud rate (0.217)",
    x        = "Region (ZIP Prefix)",
    y        = "Smoothed Fraud Rate",
    fill     = NULL
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.title     = element_text(face = "bold", size = 24, hjust = 0.5),
    plot.subtitle  = element_text(color = "grey40", size = 18, hjust = 0.5),
    axis.title.y = element_text(size = 12),
    legend.position           = "inside",
    legend.position.inside    = c(0.88, 0.25),
    legend.justification.inside = c(1, 0),
    legend.background         = element_rect(fill = alpha("white", 0.7), colour = NA),
    legend.text               = element_text(size = 14),
    legend.key.size           = unit(1.2, "lines")
  )
ggsave(region_bars, file="figures/region_bars.png", width = 12, height = 10,
       bg = "white",units = "in", dpi = 320)

summary(regional_summary$region_mean_smooth_rate_zip)
summary(regional_summary$smooth_rate_region)


## ---- Age Group Analysis -----------------------------------------------------
age_group_fraud_rate <- explore_data |> 
  select(age_at_claim, age_group, fraud) |> 
  group_by(age_group) |> 
  summarise(
    n_claims = n(),
    n_frauds = sum(fraud, na.rm = TRUE),
    fraud_rate_by_age = n_frauds/n_claims,
    .groups = "drop"
  )
knitr::kable(age_group_fraud_rate, format = "latex", booktabs = TRUE, digits = 2)

# Seems as if people above 55 might be more fraudulent,
# but certainly not statistically significant

explore_data <- explore_data |>
  mutate(over55 = ifelse(age_at_claim < 55, "under_55", "55_or_older"))

t_test_result_age <- t.test(fraud ~ over55, data = explore_data)
t_test_result_age
# p-value = 0.12, so not statistically significant


## ---- Cluster at policy level ----############################################
rule_cols <- explore_data |> 
  select(starts_with("rule"))

n_rules_triggered <- rowSums(rule_cols, na.rm = TRUE)

explore_data <- explore_data |> 
  mutate(n_rules_triggered = n_rules_triggered)

patient_summary <- explore_data |>
  group_by(pseudo_policy_id) |>
  summarise(
    n_claims = n(),
    n_frauds = sum(fraud, na.rm = TRUE),
    fraud_rate = mean(fraud, na.rm = TRUE),
    total_claimed = sum(claim_amt, na.rm = TRUE),
    avg_claim_amt = mean(claim_amt, na.rm = TRUE),
    total_settled = sum(settle_amt, na.rm = TRUE),
    avg_settle_amt = mean(settle_amt, na.rm = TRUE),
    total_insured = sum(sum_insured, na.rm = TRUE),
    total_rules_triggered = sum(n_rules_triggered, na.rm = TRUE),
    avg_rules_triggered = mean(n_rules_triggered, na.rm = TRUE)
  ) |>
  ungroup() |> 
  arrange(desc(n_claims))

summary(patient_summary$fraud_rate)

# create flags
patient_summary <- patient_summary |>
  mutate(
    high_fraud_rate = fraud_rate > 0.5 & n_claims >= 2,
    high_n_claims = n_claims > quantile(n_claims, 0.95),
    large_claim_behavior = avg_claim_amt > quantile(avg_claim_amt, 0.95),
    n_claims = as.numeric(n_claims)
  )

# Cluster using k-means
clust_vars <- patient_summary |>
  select(-c(pseudo_policy_id, n_frauds, total_claimed, total_settled,
            total_rules_triggered, starts_with("high"), large_claim_behavior))

str(clust_vars)
clust_scaled <- scale(clust_vars)

set.seed(123)
patient_kmeans <- kmeans(clust_scaled, centers = 5)

patient_summary$cluster <- as.factor(patient_kmeans$cluster)

# analyze clusters
patient_summary |>
  group_by(cluster) |>
  summarise(
    mean_claims_amt = mean(n_claims),
    mean_fraud_rate = mean(fraud_rate),
    mean_claim_amt  = mean(avg_claim_amt),
    mean_rules_triggered = mean(n_rules_triggered)
  )


# Claim frequency vs. fraud rate
patient_summary |> 
  filter(n_claims >= 10 & fraud_rate < 0.75) |> 
  ggplot(aes(x = n_claims, y = fraud_rate)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  labs(title = "Fraud Rate vs Number of Claims per Patient")

# Total claimed vs. total settled
ggplot(patient_summary, aes(x = total_claimed, y = total_settled)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  ylim(0, 8000000) +
  labs(title = "Total Claimed vs Settled per Patient")


# ---- Cost Analysis of Recommendation Var Model -------------------------------

explore_data <- explore_data |> 
  mutate(
    classification = case_when(
      recommendation_dummy == 1 & fraud == 1 ~ "TP",
      recommendation_dummy == 0 & fraud == 0 ~ "TN",
      recommendation_dummy == 1 & fraud == 0 ~ "FP",
      recommendation_dummy == 0 & fraud == 1 ~ "FN",
      TRUE ~ NA_character_
    )
  )

explore_data |> 
  count(classification)

cat("Recall:      ", sum(explore_data$classification == "TP")/sum(explore_data$fraud==1), "\n")
cat("Specificity: ", sum(explore_data$classification == "TN")/sum(explore_data$fraud==0), "\n")

fn_settle_total <- explore_data |> 
  filter(classification == "FN") |> 
  summarise(total_FN_settle = sum(settle_amt, na.rm = TRUE))

fn_settle_total
# Total cost is 63270740 EUR

sum(explore_data$settle_amt, na.rm = T)


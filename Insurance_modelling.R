##########################   Title: Business Analytics  ########################
#########################    Insurance Business Report
################################    Modelling
#############################   Jan Bohnenstengel  


# ---- Setup -------------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(randomForest)
library(ggridges)
library(glmnet)
library(xgboost)
library(caret)
library(pROC)
library(shapviz)
library(ggplot2)
library(SHAPforxgboost)
library(Matrix)
library(ggridges)
library(viridis)
library(sysfonts)
library(showtext)
library(rpart)


rm(list = ls())

font_add_google(name = "Roboto Condensed",
                family = "Roboto Condensed")

showtext.auto()
showtext_opts(dpi = 320)

setwd("C:/Users/janni/Uni lokal/WU/Semester_2/insurance_claims_git")

load("data/use_data.RData")
load("data/to_predict_data.RData")
load("data/raw_data.RData")

class(to_predict_data$fraud)


# ---- Accuracy Measures -------------------------------------------------------

accuracy_score <- function(predicted, actual) {
  mean(predicted == actual)
}

recall_score <- function(predicted, actual) {
  true_positives <- sum(predicted == 1 & actual == 1)
  total_fraud <- sum(actual == 1)
  if (total_fraud == 0) return(NA)
  true_positives / total_fraud
}

business_score <- function(predicted, actual) {
  acc <- accuracy_score(predicted, actual)
  rec <- recall_score(predicted, actual)
  acc * rec
}

specificity_score <- function(predicted, actual) {
  true_negatives <- sum(predicted == 0 & actual == 0)
  total_nonfraud <- sum(actual == 0)
  if (total_nonfraud == 0) return(NA)
  true_negatives / total_nonfraud
}


# ---- Random Forest -----------------------------------------------------------

colSums(is.na(use_data))

use_data$fraud <- as.factor(use_data$fraud)
  
rf_data <- use_data
  
set.seed(42)
folds <- createFolds(rf_data$fraud, k = 5)

rf_oof_probs <- rep(NA, nrow(rf_data))
  
for (i in 1:5) {
    cat("Processing fold", i, "\n")
    
    valid_idx <- folds[[i]]
    train_fold <- rf_data[-valid_idx, ]
    valid_fold <- rf_data[valid_idx, ]
    
    rf_model <- randomForest(
      fraud ~ .,
      data = train_fold |>  select(-holdout_order, -holdout),
      ntree = 150,
      maxnodes = 140,
      classwt = c("0" = 1, "1" = 4)
    )
    
    rf_oof_probs[valid_idx] <- predict(rf_model, newdata = valid_fold, type = "prob")[, 2]
}
  
# Classify Probs (0.5 threshold)
rf_oof_preds <- ifelse(rf_oof_probs > 0.45, 1, 0)

actual <- as.numeric(rf_data$fraud) - 1
  
rf_roc <- roc(actual, rf_oof_probs)
rf_auc <- auc(rf_roc)
  
rf_acc <- accuracy_score(rf_oof_preds, actual)
rf_rec <- recall_score(rf_oof_preds, actual)
rf_biz <- business_score(rf_oof_preds, actual)

cat("Random Forest CV Evaluation:\n")
cat("AUC:            ", round(rf_auc, 4), "\n")
cat("Accuracy:       ", round(rf_acc, 4), "\n")
cat("Recall:         ", round(rf_rec, 4), "\n")
cat("Business Score: ", round(rf_biz, 4), "\n")
  
  

# ---- XGBoost model -----------------------------------------------------------

model_data <- use_data |> 
    select(!c(hospital_id, hos_zipcode, has_negative, pseudo_policy_id,
              starts_with("rule")))

model_data$fraud <- as.numeric(use_data$fraud) -1
table(model_data$fraud)
class(model_data$fraud)
  
x <- model.matrix(fraud ~ . - holdout_order - holdout, data = model_data)[, -1]
y <- model_data$fraud
  
set.seed(42)
folds <- createFolds(y, k = 5)

  
xgb_oof_probs <- rep(NA, length(y))
  
for (i in 1:5) {
    cat("Processing fold", i, "\n")
    
    valid_idx <- folds[[i]]
    train_idx <- setdiff(1:length(y), valid_idx)
    
    dtrain <- xgb.DMatrix(data = x[train_idx, ], label = y[train_idx])
    dvalid <- xgb.DMatrix(data = x[valid_idx, ])
    
    # scale_pos_weight to balance classes
    scale_pos_weight <- sum(y[train_idx] == 0) / sum(y[train_idx] == 1)
    
    xgb_model <- xgboost(
      data = dtrain,
      objective = "binary:logistic",
      eval_metric = "auc",
      scale_pos_weight = scale_pos_weight,
      max_depth = 6,
      eta = 0.1,
      nrounds = 200,
      verbose = 0
    )
    
    xgb_oof_probs[valid_idx] <- predict(xgb_model, dvalid)
}
  
# Classify Probs (0.45 threshold)
xgb_preds <- ifelse(xgb_oof_probs > 0.45, 1, 0)

xgb_auc <- auc(roc(y, xgb_oof_probs))
xgb_acc <- accuracy_score(xgb_preds, y)
xgb_rec <- recall_score(xgb_preds, y)
xgb_biz <- business_score(xgb_preds, y)
xgb_spe <- specificity_score(xgb_preds, y)

cat("XGBoost CV Evaluation:\n")
cat("AUC:            ", round(xgb_auc, 4), "\n")
cat("Accuracy:       ", round(xgb_acc, 4), "\n")
cat("Recall:         ", round(xgb_rec, 4), "\n")
cat("Specificity:    ", round(xgb_spe, 4), "\n")
cat("Business Score: ", round(xgb_biz, 4), "\n")

  
thresholds <- seq(0.1, 0.9, by = 0.01)
results <- data.frame(thresh = thresholds, acc = NA, rec = NA, biz = NA)

for (i in seq_along(thresholds)) {
  preds <- ifelse(xgb_oof_probs > thresholds[i], 1, 0)
  results$acc[i] <- accuracy_score(preds, y)
  results$rec[i] <- recall_score(preds, y)
  results$biz[i] <- business_score(preds, y)
}

# Plot
ggplot(results, aes(x = thresh)) +
  geom_line(aes(y = acc, color = "Accuracy")) +
  geom_line(aes(y = rec, color = "Recall")) +
  geom_line(aes(y = biz, color = "Business Score")) +
  labs(title = "Model Performance by Threshold", y = "Score", x = "Threshold") +
  theme_minimal() +
  scale_color_manual(values = c("Accuracy" = "blue", "Recall" = "darkgreen", "Business Score" = "red"))

# Importance of variables
importance_matrix <- xgb.importance(
  feature_names = colnames(x),
  model = xgb_model
)

xgb.plot.importance(importance_matrix, top_n = 20, measure = "Gain", rel_to_first = TRUE)
head(importance_matrix, 10)
write.csv(importance_matrix, "xgboost_feature_importance.csv", row.names = FALSE)


# ----  Predictions for .txt-file to hand in -----------------------------------

# x_test <- model.matrix(~ . - holdout_order - holdout, data = to_predict_data)[, -1]
# 
# dtest <- xgb.DMatrix(data = x_test)
# predicted_probs <- predict(xgb_model, newdata = dtest)
# 
# predicted_fraud <- ifelse(predicted_probs > 0.45, 1, 0)
# 
# to_predict_data$predicted_fraud <- predicted_fraud
# 
# to_predict_data <- to_predict_data[order(to_predict_data$holdout_order), ]
# 
# write.table(to_predict_data$predicted_fraud,
#             file = "fraud_predictions.txt",
#             row.names = FALSE,
#             col.names = FALSE,
#             quote = FALSE)


# ----  SHAP graph -------------------------------------------------------------

## ---- Preparations for SHAP Graph --------------------------------------------
dtrain_full <- xgb.DMatrix(data = x, label = y)
scale_pos_weight_full <- sum(y == 0) / sum(y == 1)

final_model <- xgboost(
  data = dtrain_full,
  objective = "binary:logistic",
  eval_metric = "auc",
  scale_pos_weight = scale_pos_weight_full,
  max_depth = 6,
  eta = 0.1,
  nrounds = 200,
  verbose = 0
)

shap_values <- shap.values(xgb_model = final_model, X_train = x)
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = x)

top_feats <- shap_long |> 
  group_by(variable) |> 
  summarise(mean_abs = mean(abs(value), na.rm = TRUE)) |> 
  arrange(desc(mean_abs)) |> 
  slice_head(n=20) |> 
  pull(variable)

shap_samp <- shap_long |> 
  filter(variable %in% top_feats) |> 
  group_by(variable) |> 
  slice_sample(n = 5000) |> 
  ungroup()

ranked <- shap_samp |> 
  group_by(variable) |> 
  summarise(mean_abs = mean(abs(value), na.rm = TRUE)) |> 
  arrange(mean_abs)

shap_samp <- shap_samp |> 
  mutate(variable = factor(variable, levels = ranked$variable))

## ---- SHAP Plot --------------------------------------------------------------
ggplot(shap_samp, aes(x = value, y = variable)) +
  geom_jitter(aes(colour = value),
              size = 0.9, width = 0.15, alpha = 0.5) +
  geom_violin(scale = "width", trim = FALSE, fill = "grey90",alpha = 0.5, colour = NA) +
  scale_colour_viridis_c(name = "Feature\nSHAP value", option = "magma") +
  scale_y_discrete(labels = c(
    payment_delay_days = "Payment delay after claim",
    claim_to_admit_days = "Time between admission and claim",
    payment_dt = "Date of settlement",
    discharge_dt = "Date of discharge form hospital",
    admit_dt = "Date of admission to hospital",
    recommendationGenuine = "Flagged as genuine by us",
    claim_to_insured_ratio = "Claim amount to insured sum ratio",
    settled_to_claim_ratio = "Settled Amount to claim amount ratio",
    claim_amt_per_day = "Claim amount relative to days in hospital",
    claim_dt = "Date of claim",
    claim_amt = "Claim amount",
    days_since_policy_start = "Time between policy start and claim",
    dob = "Date of birth",
    cons_fee= "Doctor consultation charges",
    pharmacy_cost = "Cost od meds during treatment",
    settle_amt = "Settlement amount",
    test_chg = "Cost for tests during treatment",
    policy_start_dt = "Start date of policy",
    surgery_chg = "Charges for surgeries",
    days_to_policy_expiry = "Time to policy expiry"
  )) +
  labs(
    x = "SHAP value",
    y = "",
    title = "Importance Analysis of Variables\n(SHAP distributions)")  +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 13),
    axis.text.y = element_text(size = 12),
    plot.margin = unit(c(10, 10, 10, 10), "pt")
  )
ggsave("figures/shap_violin.png", width = 12, height = 12, bg = "white",units = "in", dpi = 320)



# ----  Basic tree based on rules to Compare  ----------------------------------

rule_vars <- names(select(use_data, starts_with("rule")))
tree_data <- use_data |> 
  select(all_of(rule_vars), fraud)
  
set.seed(123)
folds <- createFolds(tree_data$fraud, k = 5)
  
# Storage for predictions and results
tree_oof_preds <- rep(NA, nrow(tree_data))
tree_oof_probs <- rep(NA, nrow(tree_data))
  
for (i in seq_along(folds)) {
  cat("Processing fold", i, "\n")
    
  valid_idx <- folds[[i]]
  train_idx <- setdiff(seq_len(nrow(tree_data)), valid_idx)
  
  train_data <- tree_data[train_idx, ]
  valid_data <- tree_data[valid_idx, ]
    
  tree_model <- rpart(fraud ~ ., data = train_data, method = "class", cp = 0.001)
    
  prob_preds <- predict(tree_model, newdata = valid_data, type = "prob")[, 2]
  class_preds <- ifelse(prob_preds > 0.217, 1, 0) # 0.2169999 all 1s, 0.217 all 0s

  tree_oof_probs[valid_idx] <- prob_preds
  tree_oof_preds[valid_idx] <- class_preds
}

accuracy_score <- function(pred, actual) mean(pred == actual)
  
recall_score <- function(pred, actual) {
  sum(pred == 1 & actual == 1) / sum(actual == 1)
}
  
business_score <- function(pred, actual) {
  acc <- accuracy_score(pred, actual)
  rec <- recall_score(pred, actual)
  acc * rec
}

actuals <- tree_data$fraud
  
library(pROC)
tree_auc <- auc(roc(actuals, tree_oof_probs))
tree_acc <- accuracy_score(tree_oof_preds, actuals)
tree_rec <- recall_score(tree_oof_preds, actuals)
tree_biz <- business_score(tree_oof_preds, actuals)
  
cat("Decision Tree CV Evaluation:\n")
cat("AUC:            ", round(tree_auc, 4), "\n")
cat("Accuracy:       ", round(tree_acc, 4), "\n")
cat("Recall:         ", round(tree_rec, 4), "\n")
cat("Business Score: ", round(tree_biz, 4), "\n")

 

# ----  Score model based on rules  --------------------------------------------
# (to check whether my rules and the recommendation align)
  
# Assign points for each rule
score_data <- use_data |> 
 mutate(rule_score = rowSums(across(all_of(rule_vars))))

score_data$fraud <- as.numeric(score_data$fraud) - 1
table(score_data$fraud)
  
# Threshold
score_data <- score_data |> 
    mutate(rule_flag = ifelse(rule_score >= 7, 1, 0))

# Evaluate manually
accuracy_score(score_data$rule_flag, score_data$fraud)
recall_score(score_data$rule_flag, score_data$fraud)
specificity_score(score_data$rule_flag, score_data$fraud)
  
### Compare with recommendation_dummy
accuracy_score(score_data$recommendation_dummy, score_data$fraud)
recall_score(score_data$recommendation_dummy, score_data$fraud)
specificity_score(score_data$recommendation_dummy, score_data$fraud)

cor(score_data$recommendation_dummy, score_data$fraud)
cor(score_data$rule_flag, score_data$fraud)
cor(score_data$recommendation_dummy, score_data$rule_flag)


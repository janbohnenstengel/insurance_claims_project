library(shiny)
library(tidyverse)

rm(list=ls())

setwd("C:/Users/janni/Uni lokal/WU/Semester_2/insurance_claims_git")
load("data/use_data.RData")

ui <- navbarPage(
  titlePanel("Summary Statistics on the Insurance Data"),
  tabsetPanel(
    tabPanel("Plots",
             fluidPage(
               style = "margin-top: 20px;",
               fluidRow(
                 column(12, plotOutput("plot1"))),
               fluidRow(
                 column(4,
                        sliderInput("bin", "Select Number of Bins", value = 20,
                                    min = 5, max = 50)),
                 column(4, selectInput("variable", "Select Variable", choices =
                          c("sum_insured_log", "claim_amt_log",
                            "settle_amt_log"))),
                 column(4, selectInput("colour", "Select Colour", choices =
                          c("red", "gold", "violet"), selected = "violet")))
             )
            ),
   tabPanel("Summary Tables",
              fluidPage(
                style = "margin-top: 20px;",
                fluidRow(
                  column(12, h4("Claim Amount Summary Statistics")),
                  column(12, tableOutput("table1"))
                ),
                fluidRow(
                  column(12, h4("Fraud and Recommendation Relationship")),
                  column(12, tableOutput("fraud_recommendation"))
                )
              )
  )
))

server <- function(input, output, session) {
  variable_labels <- c(
    sum_insured_log = "Sum Insured (log)",
    claim_amt_log = "Claim Amount (log)",
    settle_amt_log = "Settlement Amount (log)"
  )

  output$plot1 <- renderPlot({
    plot_data <- use_data |> filter(sum_insured < 50000)
    ggplot(plot_data, aes(x=.data[[input$variable]])) +
      geom_histogram(bins = input$bin,
                     fill = input$colour,
                     colour = "black") +
      labs(x = variable_labels[[input$variable]],
           y = "Frequency",
           title = paste("Distribution of", variable_labels[[input$variable]])) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 19, face = "bold"),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))})
  output$table1 <- renderTable(use_data |> 
                                  summarise("Min" = min(claim_amt),
                                            "Mean" = mean(claim_amt),
                                            "Median" = median(claim_amt),
                                            "Max" = max(claim_amt)))
  output$fraud_recommendation <- renderTable({
      df <- as.data.frame(table(Fraud = use_data$fraud,
                                Recommendation = use_data$recommendation))
      df})
}

shinyApp(ui, server)

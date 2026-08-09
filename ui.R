#
# Generate a multi-variate regression model for the mtcars data.
# run the application by clicking 'Run App' above.
#
# 1. Shiny app with a slider to view the change in MSE on changing the slope
# of the dependent variables. 
#
# 2. Shiny table to view the final summary in a table with the key variables in first column,
# whether significant or not in the second column, and how much measure in units
# of the dependent variable changes the outcome variable by 1% in the third column, 
# average level of dependent variable in fourth column

# 3. Dropdown to select a dependent variable in the model and visualize
# the linear model to measure the minimum and maximum after which we get diminishing returns

library(shiny)
library(ggplot2)
library(dplyr)

# Load data explicitly to ensure environment visibility
data(mtcars)

# Define UI
ui <- fluidPage(
    titlePanel("Mtcars Multivariable Regression Explorer"),
    
    sidebarLayout(
        sidebarPanel(
            h4("Model Inputs & Adjustments"),
            
            # Dropdown to select variables for correlation analysis
            selectInput("corr_var", 
                        label = "Select Variable for Correlation with MPG:",
                        choices = c("Transmission (am)" = "am", 
                                    "Weight (wt)" = "wt", 
                                    "1/4 Mile Time (qsec)" = "qsec"),
                        selected = "wt"),
            
            hr(),
            h4("Interactive Slope Modification"),
            p("Adjust the slider to change the weight (wt) slope coefficient and see how it impacts predicted MPG compared to the actual data."),
            
            # UI Output to dynamically generate the slider using actual model parameters safely
            uiOutput("slope_slider_ui")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Correlation & Model Summary",
                         h3("Correlation Analysis"),
                         textOutput("corr_text"),
                         plotOutput("corr_plot", height = "300px"),
                         
                         hr(),
                         h3("Final Model Summary Table"),
                         verbatimTextOutput("model_summary")),
                
                tabPanel("Interactive Prediction Plot",
                         h3("Impact of Changing 'wt' Slope on MPG"),
                         p("The blue line represents predictions using the adjusted slope. The grey dashed line represents the original OLS model predictions."),
                         
                         # Error Metrics Callout Box
                         wellPanel(
                             style = "background-color: #fdf6e2; border-color: #f39c12;",
                             h4(tags$b("Model Error Analytics")),
                             htmlOutput("error_metrics")
                         ),
                         
                         plotOutput("prediction_plot"))
            )
        )
    )
)
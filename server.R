

server <- function(input, output, session) {
    
    # Reactive baseline model to guarantee it runs inside the server session scope
    base_fit <- lm(mpg ~ am + wt + qsec, data = mtcars)
    base_coefs <- coef(base_fit)
    
    # Calculate Baseline Error (Residual Sum of Squares) once
    base_rss <- sum(residuals(base_fit)^2)
    
    # Dynamically render the slider inside the server so it safely references base_coefs
    output$slope_slider_ui <- renderUI({
        wt_estimate <- round(base_coefs["wt"], 2)
        sliderInput("wt_slope",
                    label = "Adjust 'wt' Slope Coefficient:",
                    min = round(wt_estimate * 2, 2), 
                    max = 0,                             
                    value = wt_estimate,   
                    step = 0.05)
    })
    
    # 1. Calculate and display correlation
    output$corr_text <- renderText({
        req(input$corr_var)
        r_val <- cor(mtcars$mpg, mtcars[[input$corr_var]]) 
        var_names <- c(am = "Transmission (am)", wt = "Weight (wt)", qsec = "1/4 Mile Time (qsec)")
        paste0("The Pearson correlation coefficient between MPG and ", var_names[input$corr_var], " is: ", round(r_val, 4))
    })
    
    # Visualizing the selected correlation
    output$corr_plot <- renderPlot({
        req(input$corr_var)
        ggplot(mtcars, aes(x = !!sym(input$corr_var), y = mpg)) +
            geom_point(color = "#2c3e50", size = 3, alpha = 0.7) +
            geom_smooth(method = "lm", color = "#e74c3c", se = TRUE) +
            theme_minimal() +
            labs(title = paste("MPG vs", input$corr_var), y = "Miles Per Gallon (mpg)", x = input$corr_var)
    })
    
    # 2. Display the summary table of the final model (FIXED BLOCK)
    output$model_summary <- renderPrint({
        # Explicitly print the summary so it does not get cleared by subsequent operations
        print(summary(base_fit))
        
        # Calculate and print explicit RSS safely right underneath it
        raw_rss <- sum(residuals(base_fit)^2)
        cat("\n==========================================================\n")
        cat("EXTRA MODEL METRICS:\n")
        cat("Explicit Baseline Residual Sum of Squares (RSS):", round(raw_rss, 4), "\n")
        cat("==========================================================\n")
    })
    
    # Real-Time Error Calculation and HTML display
    output$error_metrics <- renderUI({
        req(input$wt_slope)
        
        # Replicate predictions with custom slope
        custom_coefs <- base_coefs
        custom_coefs["wt"] <- input$wt_slope
        
        pred_custom <- custom_coefs["(Intercept)"] + 
            (custom_coefs["am"] * mtcars$am) + 
            (custom_coefs["wt"] * mtcars$wt) + 
            (custom_coefs["qsec"] * mtcars$qsec)
        
        # Calculate Custom RSS
        custom_rss <- sum((mtcars$mpg - pred_custom)^2)
        rss_difference <- custom_rss - base_rss
        percent_increase <- (rss_difference / base_rss) * 100
        
        # Generate HTML text output based on variation
        if (abs(rss_difference) < 0.01) {
            HTML("<p style='color: green;'><b>Current state matches OLS!</b> The model is optimized. Residual Sum of Squares (RSS): <b>", round(custom_rss, 2), "</b></p>")
        } else {
            HTML(paste0(
                "<p>Original OLS Model Error (RSS): <b>", round(base_rss, 2), "</b></p>",
                "<p>Custom Model Error (RSS): <b style='color: #c0392b;'>", round(custom_rss, 2), "</b></p>",
                "<p>The change increased prediction error by <b>+", round(rss_difference, 2), "</b> units ",
                "(<span style='color: #c0392b;'><b>+", round(percent_increase, 1), "%</b></span> worse than OLS).</p>"
            ))
        }
    })
    
    # 3. Dynamic prediction plot based on the custom wt slope slider
    output$prediction_plot <- renderPlot({
        req(input$wt_slope) 
        
        custom_coefs <- base_coefs
        custom_coefs["wt"] <- input$wt_slope
        
        plot_data <- mtcars
        plot_data$pred_base <- predict(base_fit)
        plot_data$pred_custom <- custom_coefs["(Intercept)"] + 
            (custom_coefs["am"] * plot_data$am) + 
            (custom_coefs["wt"] * plot_data$wt) + 
            (custom_coefs["qsec"] * plot_data$qsec)
        
        ggplot(plot_data, aes(x = wt, y = mpg)) +
            geom_point(aes(color = as.factor(am)), size = 3, alpha = 0.6) +
            geom_line(aes(y = pred_base), color = "grey50", linetype = "dashed", linewidth = 1) +
            geom_line(aes(y = pred_custom), color = "#0073C2FF", linewidth = 1.2) +
            scale_color_manual(values = c("#E74C3C", "#2ECC71"), labels = c("Automatic", "Manual"), name = "Transmission") +
            theme_minimal() +
            labs(title = paste("Effect of Weight on MPG (Adjusted Slope:", input$wt_slope, ")"),
                 subtitle = "Dashed line = Original Model | Solid Blue Line = Adjusted Model",
                 x = "Weight (1000 lbs)",
                 y = "Miles Per Gallon (mpg)")
    })
}

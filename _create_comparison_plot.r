create_comparison_plot <- function(data, expected_var, observed_var, 
                                 rel_diff_var = NULL, # New parameter for rel_diff
                                 title, y_label, x_label = "Observed Value",
                                 nbins = 25, font_family = "Roboto",
                                 output_path = NULL) {
    
    # Create formula for regression
    formula_str <- as.formula(paste0(expected_var, " ~ ", observed_var))
    
    # Run binned scatter regression
    result <- binsreg(
        y = data[[expected_var]],
        x = data[[observed_var]],
        binspos = "qs",
        ci = TRUE,
        polyreg = 1,
        nbins = nbins
    )
    
    # Calculate summary statistics
    lm_fit <- lm(formula_str, data = data)
    coef <- coef(lm_fit)[2]
    r2 <- summary(lm_fit)$r.squared
    mean_diff <- mean(abs(data[[expected_var]] - data[[observed_var]]))
    
    # Calculate relative difference based on provided variable or default calculation
    if (!is.null(rel_diff_var)) {
        mean_rel_diff <- mean(data[[rel_diff_var]], na.rm = TRUE) * 100
    } else {
        rel_diff <- abs(data[[expected_var]] - data[[observed_var]])/data[[observed_var]]
        mean_rel_diff <- mean(rel_diff, na.rm = TRUE) * 100
    }
    
    # Print key statistics
    cat(sprintf("\nRegression Results for %s:\n", title))
    cat(sprintf("Coefficient: %.3f\n", coef))
    cat(sprintf("R-squared: %.3f\n", r2))
    cat(sprintf("Mean Absolute Difference: %.3f\n", mean_diff))
    cat(sprintf("Mean Relative Difference: %.1f%%\n", mean_rel_diff))
    
    # Customize plot
    result$bins_plot <- result$bins_plot +
        labs(
            title = title,
            subtitle = sprintf(
                "β = %.3f, R² = %.3f\nMean Absolute Difference: %.3f, Mean Relative Difference: %.1f%%",
                coef, r2, mean_diff, mean_rel_diff
            ),
            x = x_label,
            y = y_label
        ) +
        theme_minimal() +
        theme(
            text = element_text(family = font_family),
            plot.title = element_text(face = "bold", size = 14),
            plot.subtitle = element_text(size = 10, color = "#666666"),
            axis.title = element_text(size = 12)
        )
    
    # Save plot if output path is provided
    if (!is.null(output_path)) {
        ggsave(
            output_path,
            result$bins_plot,
            width = 10,
            height = 8,
            dpi = 300,
            bg = "white"
        )
    }
    
    # Return results
    return(list(
        plot = result$bins_plot,
        statistics = list(
            coefficient = coef,
            r_squared = r2,
            mean_diff = mean_diff,
            mean_rel_diff = mean_rel_diff
        )
    ))
}

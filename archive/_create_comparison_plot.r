create_comparison_plot <- function(data, expected_var, observed_var, 
                                 rel_diff_var = NULL, # New parameter for rel_diff
                                 title, y_label, x_label = "Observed value",
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
    
    # Stats text for inside plot
    stats_text <- sprintf(
        "β = %.3f\nR² = %.3f\nMean rel. diff. = %.1f%%",
        coef, r2, mean_rel_diff
    )
    
    # Customize plot
    result$bins_plot <- result$bins_plot +
        geom_point(size = 3) +     # Makes dots bigger
        geom_line(linewidth = 2) + # Makes line thicker
        labs(
            title = title,
            x = x_label,
            y = y_label
        ) +
        # Add stats annotation
        annotate(
            "text",
            x = min(data[[observed_var]], na.rm = TRUE),
            y = 0.9*max(data[[expected_var]], na.rm = TRUE),
            label = stats_text,
            hjust = 0,
            vjust = 1,
            size = 5,
            family = font_family
        ) +
        theme_minimal() +
        theme(
            text = element_text(family = font_family, size = 18),
            plot.title = element_text(face = "bold", size = 18),
            axis.title = element_text(size = 18),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
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
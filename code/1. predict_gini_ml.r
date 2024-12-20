#-------------------------------------------------------------
#* Author: Pablo Garcia Guzman
#* Project: validation metrics for www.comparatuingreso.es
#* This script: predicts missing Ginis using ML
#-------------------------------------------------------------

packages_to_load <- c(
    "tidyverse",
    "data.table",
    "ineAtlas",
    "fixest",
    "xgboost",
    "caret",
    "fst"
)

package.check <- lapply(
  packages_to_load,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)

lapply(packages_to_load, require, character=T)

#-------------------------------------------------------------------

# ------------------------- Prepare data ---------------------------
atlas_all <- merge(
    setDT(ineAtlas::get_atlas("income", "tract")),
    setDT(ineAtlas::get_atlas("demographics", "tract"))
) %>%
    filter(year == 2022)

# verify % of missings for each variable in atlas_all 
missing_percent <- atlas_all %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100))

print(missing_percent)

gini <- setDT(ineAtlas::get_atlas("gini_p80p20", "tract")) %>%
    filter(year == 2022) %>%
    select(tract_code, gini, p80p20)

atlas_all <- merge(atlas_all, gini, by = "tract_code")

# First impute net_income_equiv where missing
data_clean <- atlas_all %>%
    # Calculate national ratio for imputation
    mutate(
        # Create dependency ratio
        dependency_ratio = (pct_under18 + pct_over65)/(100 - pct_under18 - pct_over65),
        # Create province factor
        prov_code = factor(prov_code)
    ) %>%
    group_by(prov_code) %>%
    mutate(
        # Calculate provincial ratio
        ratio = weighted.mean(net_income_equiv/net_income_pc, w = population, na.rm = TRUE),
        # Impute net_income_equiv where missing
        net_income_equiv = if_else(
            is.na(net_income_equiv) & !is.na(net_income_pc),
            net_income_pc * ratio,
            net_income_equiv
        )
    ) %>%
    ungroup()

# ------------------------- Model training ---------------------------

# 1. OLS
model_data <- data_clean %>%
    filter(!is.na(gini),              
           !is.na(net_income_equiv), 
           !is.na(dependency_ratio),  
           !is.na(mean_age),
           !is.na(pct_single_hh)) %>%
    mutate(
        log_income_equiv = log(net_income_equiv),
    )

# Get predictions from OLS
model1 <- feols(
    gini ~ log_income_equiv + 
          dependency_ratio + mean_age + pct_single_hh + mean_hh_size + population | prov_code,
    data = model_data
)

prediction_data <- data_clean %>%
    filter(is.na(gini)) %>%  # Get observations without Gini
    filter(!is.na(net_income_equiv),
           !is.na(dependency_ratio),
           !is.na(mean_age),
           !is.na(pct_single_hh)) %>%
    mutate(log_income_equiv = log(net_income_equiv))

ols_predictions <- predict(model1, newdata = prediction_data)

# Print prediction metrics
model_data$predicted_gini <- fitted(model1)

cat("\nPrediction Performance:\n")
performance <- model_data %>%
    summarise(
        rmse = sqrt(mean((gini - predicted_gini)^2)),
        mae = mean(abs(gini - predicted_gini)),
        mape = mean(abs(gini - predicted_gini)/gini)*100
    )

# 2. XGBoost with cross-validation
# Prepare data for XGBoost
features <- c("log_income_equiv", "dependency_ratio", "mean_age", 
              "pct_single_hh", "pct_under18", "mean_hh_size", "population")

# Create training matrix
train_x <- model_data %>%
    select(all_of(features)) %>%
    as.matrix()

train_y <- model_data$gini

# Create cross-validation folds
set.seed(123)
folds <- createFolds(train_y, k = 5, list = TRUE)

# Function to calculate metrics
calc_metrics <- function(actual, predicted) {
    data.frame(
        rmse = sqrt(mean((actual - predicted)^2)),
        mae = mean(abs(actual - predicted)),
        mape = mean(abs(actual - predicted)/actual)*100
    )
}

# Cross-validate XGBoost
xgb_cv_results <- lapply(folds, function(test_idx) {
    train_idx <- setdiff(seq_along(train_y), test_idx)
    
    # Train XGBoost
    xgb_model <- xgboost(
        data = train_x[train_idx,],
        label = train_y[train_idx],
        nrounds = 100,
        max_depth = 6,
        eta = 0.3,
        objective = "reg:squarederror",
        verbose = 0
    )
    
    # Get predictions
    pred <- predict(xgb_model, train_x[test_idx,])
    
    # Calculate metrics
    calc_metrics(train_y[test_idx], pred)
})

# Average CV results
cv_results <- bind_rows(xgb_cv_results) %>%
    summarise(across(everything(), mean))

# Train final XGBoost model on all data
final_xgb <- xgboost(
    data = train_x,
    label = train_y,
    nrounds = 100,
    max_depth = 6,
    eta = 0.3,
    objective = "reg:squarederror",
    verbose = 0
)

# Get XGBoost predictions for missing values
prediction_matrix <- prediction_data %>%
    select(all_of(features)) %>%
    as.matrix()

xgb_predictions <- predict(final_xgb, prediction_matrix)

# Compare model performance
cat("\nOLS Performance (original):\n")
print(performance)

cat("\nXGBoost Performance (cross-validated):\n")
print(cv_results)

# Combine tract codes with XGBoost predictions
predicted_gini <- data.frame(
    tract_code = prediction_data$tract_code,
    gini = xgb_predictions
)

# Save to file
write.fst(predicted_gini, "data-raw/gini_predicted.fst")

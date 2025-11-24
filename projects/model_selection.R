################################################################################
# STAT 525
# Script: model_selection.R
# Purpose: Variable Selection Methods
# Author: Alexandra Y. Chang
################################################################################

rm(list = ls())

library(readr)
library(dplyr)
library(MASS)     # for stepAIC
library(leaps)    # for all-subset selection

# --- Load model frame with complete cases (created in regression.R) ----------
model_data <- read_csv(
  "/Users/al8xi8/Documents/GitHub/STAT525_Purdue2025/projects/stat525_project_withresid.csv"
)

# Convert categorical variables (needed again here)
model_data <- model_data %>%
  mutate(
    female = factor(female),
    race   = factor(race),
    edu    = factor(edu, ordered = TRUE)
  )

# --- Fit FULL MODEL again (needed for stepwise procedures) --------------------

full_mod <- lm(
  pky ~ age + female + race + edu +
    smkyears + avecpd + qtyears +
    bmi +
    copd + diab + heartattack + heartdisease +
    hypertension + stroke + kidney + liver +
    fmhist,
  data = model_data
)

summary(full_mod)

################################################################################
# 1. BACKWARD ELIMINATION (start with full model)
################################################################################

backward_mod <- stepAIC(full_mod,
                        direction = "backward",
                        trace = FALSE)

summary(backward_mod)

################################################################################
# 2. FORWARD SELECTION (start with intercept only)
################################################################################

empty_mod <- lm(pky ~ 1, data = model_data)

forward_mod <- stepAIC(empty_mod,
                       scope = list(lower = empty_mod, upper = full_mod),
                       direction = "forward",
                       trace = FALSE)

summary(forward_mod)

################################################################################
# 3. STEPWISE SELECTION (both forward and backward)
################################################################################

stepwise_mod <- stepAIC(full_mod,
                        direction = "both",
                        trace = FALSE)

summary(stepwise_mod)

################################################################################
# 4. COMPARE MODELS USING AIC / BIC
################################################################################

model_AICs <- c(
  full      = AIC(full_mod),
  backward  = AIC(backward_mod),
  forward   = AIC(forward_mod),
  stepwise  = AIC(stepwise_mod)
)

model_BICs <- c(
  full      = BIC(full_mod),
  backward  = BIC(backward_mod),
  forward   = BIC(forward_mod),
  stepwise  = BIC(stepwise_mod)
)

model_AICs
model_BICs

################################################################################
# 5. ALL-SUBSET REGRESSION (Adjusted R² and Cp)
################################################################################

# regsubsets() cannot directly handle factor predictors,
# so convert factors to numeric codes:
numeric_data <- model_data %>%
  mutate(
    female = as.numeric(female) - 1,   # Female=1, Male=0
    race   = as.numeric(race),         # 1–4
    edu    = as.numeric(edu)           # 1–6 ordered levels
  )

# Fit all-subset regression using formula interface
leaps_fit <- regsubsets(
  pky ~ .,              # response: pky, predictors: all others
  data  = numeric_data,
  nbest = 1,            # best model of each size
  nvmax = ncol(numeric_data) - 1
)

leaps_summary <- summary(leaps_fit)

# Extract model-selection statistics
adj_r2_vals <- leaps_summary$adjr2
cp_vals     <- leaps_summary$cp
bic_vals    <- leaps_summary$bic

# Display the statistics
adj_r2_vals
cp_vals
bic_vals

# Identify the best models according to each criterion
best_adjR2 <- which.max(adj_r2_vals)
best_Cp    <- which.min(cp_vals)
best_BIC   <- which.min(bic_vals)

best_adjR2
best_Cp
best_BIC


################################################################################
# 6. CHOOSE FINAL MODEL
# (Based on AIC/BIC/AdjR2/Cp and consistency across methods)
################################################################################

# Typically stepwise or backward is chosen
final_mod <- stepwise_mod

summary(final_mod)

save(final_mod,
     file = "/Users/al8xi8/Documents/GitHub/STAT525_Purdue2025/projects/final_mod.RData")


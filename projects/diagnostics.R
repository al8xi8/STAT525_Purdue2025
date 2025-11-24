################################################################################
# STAT 525
# Script: diagnostics.R
# Purpose: Regression Diagnostics on FINAL Selected Model
# Author: Alexandra Y. Chang
################################################################################

rm(list = ls())

library(readr)
library(dplyr)
library(ggplot2)
library(car)     # influenceIndexPlot, influencePlot
library(MASS)    # for studres()
library(stats)

# --- Load model_data with complete cases --------------------------------------
model_data <- read_csv(
  "/Users/al8xi8/Documents/GitHub/STAT525_Purdue2025/projects/stat525_project_withresid.csv"
)

# --- Load final model ---------------------------------------------------------
load("/Users/al8xi8/Documents/GitHub/STAT525_Purdue2025/projects/final_mod.RData")

################################################################################
# 1. RESIDUALS & FITTED VALUES
################################################################################

resid_final  <- residuals(final_mod)
fitted_final <- fitted(final_mod)

model_data$resid_final  <- resid_final
model_data$fitted_final <- fitted_final


################################################################################
# 2. RESIDUAL PLOTS (Ch. 10)
################################################################################

# Residuals vs Fitted
ggplot(model_data, aes(x = fitted_final, y = resid_final)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, col = "red") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted Values",
       y = "Residuals")

# Histogram of residuals
ggplot(model_data, aes(x = resid_final)) +
  geom_histogram(color = "black", fill = "lightgray", bins = 40) +
  labs(title = "Histogram of Residuals")

# Q–Q plot
qqnorm(resid_final, main = "Normal Q-Q Plot")
qqline(resid_final, col = "red")


################################################################################
# 3. RESIDUALS VS EACH PREDICTOR (Ch. 10)
################################################################################

predictors <- c("age", "race", "smkyears", "avecpd", "bmi",
                "copd", "diab", "heartattack", "hypertension",
                "kidney", "liver")

for (p in predictors) {
  print(
    ggplot(model_data, aes_string(x = p, y = "resid_final")) +
      geom_point(alpha = 0.5) +
      geom_hline(yintercept = 0, col = "red") +
      labs(title = paste("Residuals vs", p),
           y = "Residuals")
  )
}


################################################################################
# 4. STUDENTIZED DELETED RESIDUALS (Ch. 11)
################################################################################

stud_del_resid <- rstudent(final_mod)
model_data$stud_del <- stud_del_resid

# Plot studentized deleted residuals
ggplot(model_data, aes(x = fitted_final, y = stud_del)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = c(-2, 2), col = "red", linetype = "dashed") +
  labs(title = "Studentized Deleted Residuals",
       y = "Studentized Deleted Residual")

# Identify large > |2|
which(abs(stud_del_resid) > 2)


################################################################################
# 5. LEVERAGE (Hat Values) (Ch. 12)
################################################################################

leverage_vals <- hatvalues(final_mod)
model_data$leverage <- leverage_vals

plot(leverage_vals,
     main = "Leverage Values",
     ylab = "Hat Value",
     pch = 20)

which(leverage_vals > 2 * mean(leverage_vals))


################################################################################
# 6. COOK'S DISTANCE (Ch. 12)
################################################################################

cook_vals <- cooks.distance(final_mod)
model_data$cooksD <- cook_vals

plot(cook_vals,
     main = "Cook's Distance",
     ylab = "Cook's D",
     pch = 20)

which(cook_vals > 4 / nrow(model_data))


################################################################################
# 7. DFFITS (Ch. 12)
################################################################################

dffits_vals <- dffits(final_mod)
model_data$dffits <- dffits_vals

plot(dffits_vals,
     main = "DFFITS",
     ylab = "DFFITS",
     pch = 20)

which(abs(dffits_vals) > 2 * sqrt(ncol(model_data) / nrow(model_data)))


################################################################################
# 8. PRESS STATISTIC (Ch. 11)
################################################################################

PRESS <- sum((resid_final / (1 - leverage_vals))^2)
PRESS

################################################################################
# 9. Summary of Influential Observations
################################################################################

influential_points <- list(
  big_residuals     = which(abs(stud_del_resid) > 2),
  high_leverage     = which(leverage_vals > 2 * mean(leverage_vals)),
  high_cooksD       = which(cook_vals > 4 / nrow(model_data)),
  high_dffits       = which(abs(dffits_vals) > 2 * sqrt(ncol(model_data)/nrow(model_data)))
)

influential_points


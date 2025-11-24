################################################################################
# STAT 525
# Script: regression.R
# Purpose: Basic Analysis / Full Multiple Regression Model
# Author: Alexandra Y. Chang
################################################################################

rm(list = ls())

# --- Load libraries -----------------------------------------------------------
library(readr)
library(dplyr)
library(car)      # for VIF

# --- Load STAT 525 subset dataset --------------------------------------------
stat525_project <- read_csv(
  "/Users/al8xi8/Documents/GitHub/STAT525_Purdue2025/projects/stat525_project.csv"
)

# Quick check of data
dim(stat525_project)
head(stat525_project)
summary(stat525_project)

# --- Convert categorical variables to factors ---------------------------------

stat525_project <- stat525_project %>%
  mutate(
    female = factor(female, levels = c(0, 1),
                    labels = c("Male", "Female")),
    race   = factor(race,
                    levels = c(0, 1, 2, 3),
                    labels = c("NH_White", "NH_Black", "Hispanic", "Other")),
    # treat education as ordered factor (1 = lowest, 6 = highest)
    edu    = factor(edu,
                    levels = c(1, 2, 3, 4, 5, 6),
                    ordered = TRUE)
  )

# --- Fit FULL multiple regression model ---------------------------------------
# Response: pky (pack-years)
# Predictors: demographics, smoking vars, BMI, comorbidities, family history

full_mod <- lm(
  pky ~ age + female + race + edu +
    smkyears + avecpd + qtyears +
    bmi +
    copd + diab + heartattack + heartdisease +
    hypertension + stroke + kidney + liver +
    fmhist,
  data = stat525_project
)

# --- Basic output -------------------------------------------------------------

# Regression summary: coefficients, t-tests, R^2, etc.
summary(full_mod)

# ANOVA table for overall F-test
anova(full_mod)

# --- Multicollinearity: VIF ---------------------------------------------------
vif(full_mod)

# --- Save residuals and fitted values for diagnostics.R -----------------------

model_data <- full_mod$model
model_data$resid_full  <- resid(full_mod)
model_data$fitted_full <- fitted(full_mod)

write_csv(
  stat525_project,
  "/Users/al8xi8/Documents/GitHub/STAT525_Purdue2025/projects/stat525_project_withresid.csv"
)

save(full_mod,
     file = "/Users/al8xi8/Documents/GitHub/STAT525_Purdue2025/projects/full_mod.RData")



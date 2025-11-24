################################################################################
# STAT 525
# Script: recode.R
# Purpose: Recode NHIS variables
# Author: Alexandra Y. Chang
################################################################################

rm(list = ls())

# --- Load Libraries and Packages ---
library(readr)
library(dplyr)
library(tidyr)
install.packages("here")
library(here)

# --- Load Data ---
getwd()
setwd("/Users/al8xi8/Documents/GitHub/nhis_LCrisks")

nhis_lcrisks <- read_csv("datasets/nhis_lcrisks.csv")
View(nhis_lcrisks)

################################################################################
# --- 1. DEMOGRAPHICS -----------------------------------------------------------
################################################################################

# AGE
nhis_lcrisks$age <- nhis_lcrisks$AGE
nhis_lcrisks$age[nhis_lcrisks$age %in% c(997, 998, 999)] <- NA

# SEX → female
nhis_lcrisks$female <- ifelse(
  nhis_lcrisks$SEX %in% c(7, 8, 9), NA,
  ifelse(nhis_lcrisks$SEX == 2, 1,
         ifelse(nhis_lcrisks$SEX == 1, 0, NA))
)

################################################################################
# --- 2. SMOKING VARIABLES ------------------------------------------------------
################################################################################

### Smoking Duration (smkyears)
nhis_lcrisks$smkyears <- NA

current <- which(!is.na(nhis_lcrisks$SMOKESTATUS2) &
                   nhis_lcrisks$SMOKESTATUS2 %in% c(10, 11, 12, 13))

former  <- which(!is.na(nhis_lcrisks$SMOKESTATUS2) &
                   nhis_lcrisks$SMOKESTATUS2 == 20)

never   <- which(!is.na(nhis_lcrisks$SMOKESTATUS2) &
                   nhis_lcrisks$SMOKESTATUS2 == 30)

# Current smokers
nhis_lcrisks$smkyears[current] <-
  nhis_lcrisks$age[current] - nhis_lcrisks$SMOKAGEREG[current]

# Former smokers
nhis_lcrisks$smkyears[former] <-
  nhis_lcrisks$age[former] - nhis_lcrisks$SMOKAGEREG[former] -
  nhis_lcrisks$QUITYRS[former]

# Never smokers
nhis_lcrisks$smkyears[never] <- 0

nhis_lcrisks$smkyears[nhis_lcrisks$smkyears < 0] <- NA


### Years Since Quit (qtyears)
nhis_lcrisks$qtyears <- NA
valid_q <- which(!is.na(nhis_lcrisks$QUITYRS) &
                   nhis_lcrisks$QUITYRS >= 0 &
                   nhis_lcrisks$QUITYRS <= 70)

nhis_lcrisks$qtyears[valid_q] <- nhis_lcrisks$QUITYRS[valid_q]
nhis_lcrisks$qtyears[current] <- 0     # current smokers = 0


### Average Cigarettes Per Day (avecpd)
nhis_lcrisks$avecpd <- NA

# Current smokers
nhis_lcrisks$avecpd[current] <- ifelse(
  nhis_lcrisks$CIGSDAY1[current] >= 1 &
    nhis_lcrisks$CIGSDAY1[current] <= 95,
  nhis_lcrisks$CIGSDAY1[current],
  ifelse(
    nhis_lcrisks$CIGSDAY2[current] >= 1 &
      nhis_lcrisks$CIGSDAY2[current] <= 95,
    nhis_lcrisks$CIGSDAY2[current],
    NA)
)

# Former smokers
nhis_lcrisks$avecpd[former] <- ifelse(
  nhis_lcrisks$CIGSDAYFS[former] >= 1 &
    nhis_lcrisks$CIGSDAYFS[former] <= 94,
  nhis_lcrisks$CIGSDAYFS[former],
  NA
)

# Never smokers
nhis_lcrisks$avecpd[never] <- 0

################################################################################
# --- 3. RACE/ETHNICITY ---------------------------------------------------------
################################################################################

nhis_lcrisks$race <- NA

# Hispanic
nhis_lcrisks$race[nhis_lcrisks$HISPYN == 2] <- 2

# Non-Hispanic White
nhis_lcrisks$race[
  nhis_lcrisks$HISPYN == 1 & nhis_lcrisks$RACENEW == 100
] <- 0

# Non-Hispanic Black
nhis_lcrisks$race[
  nhis_lcrisks$HISPYN == 1 & nhis_lcrisks$RACENEW == 200
] <- 1

# Non-Hispanic Other
nhis_lcrisks$race[
  nhis_lcrisks$HISPYN == 1 &
    nhis_lcrisks$RACENEW %in%
    c(300, 400, 500, 510, 520, 540, 541, 542)
] <- 3

# Missing
nhis_lcrisks$race[
  nhis_lcrisks$HISPYN %in% c(7, 8, 9) |
    nhis_lcrisks$RACENEW %in% c(530, 997, 998, 999)
] <- NA

################################################################################
# --- 4. COPD RECODING (Harmonized across years) --------------------------------
################################################################################

nhis_lcrisks$copd <- NA

# Post-2019: COPDEV
copd_2019 <- which(nhis_lcrisks$YEAR >= 2019)
nhis_lcrisks$copd[copd_2019[nhis_lcrisks$COPDEV[copd_2019] == 2]] <- 1
nhis_lcrisks$copd[copd_2019[nhis_lcrisks$COPDEV[copd_2019] == 1]] <- 0

# 2010–2018: EMPHYSEMEV or COPDEV
copd_pre2019 <- which(nhis_lcrisks$YEAR <= 2018)
nhis_lcrisks$copd[copd_pre2019[
  nhis_lcrisks$EMPHYSEMEV[copd_pre2019] == 2 |
    nhis_lcrisks$COPDEV[copd_pre2019] == 2
]] <- 1

nhis_lcrisks$copd[copd_pre2019[
  nhis_lcrisks$EMPHYSEMEV[copd_pre2019] == 1 &
    nhis_lcrisks$COPDEV[copd_pre2019] == 1
]] <- 0

################################################################################
# --- 5. FAMILY HISTORY ----------------------------------------------------------
################################################################################

parents <- nhis_lcrisks[, c("BFLGCAN", "BMLGCAN")]
parents[parents %in% c(0, 7, 8, 9)] <- NA

parent_pos <- rowSums(parents == 2, na.rm = TRUE)
parent_na  <- rowSums(is.na(parents))

nhis_lcrisks$fmhist <- ifelse(parent_na == 2, NA, parent_pos)

################################################################################
# --- 6. BMI & EDUCATION ---------------------------------------------------------
################################################################################

# BMI
nhis_lcrisks$bmi <- nhis_lcrisks$BMICALC
nhis_lcrisks$bmi[nhis_lcrisks$bmi %in% c(0, 996.0)] <- NA

# Education
nhis_lcrisks$edu <- NA
nhis_lcrisks$edu[nhis_lcrisks$EDUC %in% c(100:116)] <- 1
nhis_lcrisks$edu[nhis_lcrisks$EDUC %in% c(200, 201, 202)] <- 2
nhis_lcrisks$edu[nhis_lcrisks$EDUC %in% c(302, 303)] <- 3
nhis_lcrisks$edu[nhis_lcrisks$EDUC %in% c(300, 301)] <- 4
nhis_lcrisks$edu[nhis_lcrisks$EDUC == 400] <- 5
nhis_lcrisks$edu[nhis_lcrisks$EDUC %in% c(500, 501, 502, 503, 505)] <- 6
nhis_lcrisks$edu[nhis_lcrisks$EDUC %in% c(000, 504, 996, 997, 998, 999)] <- NA

################################################################################
# --- 7. COMORBIDITIES (Binary recoding) ----------------------------------------
################################################################################

comorb_vars <- list(
  CANCEREV   = "prshist",
  HYPERTENEV = "hypertension",
  CHEARTDIEV = "chd",
  ANGIPECEV  = "angina",
  HEARTATTEV = "heartattack",
  HEARTCONEV = "heartdisease",
  STROKEV    = "stroke",
  DIABETICEV = "diab",
  CRONBRONYR = "bron",
  KIDNEYWKYR = "kidney",
  LIVERCONYR = "liver",
  EQUIPMENT  = "spaceq"
)

for (orig_var in names(comorb_vars)) {
  new_var <- comorb_vars[[orig_var]]
  nhis_lcrisks[[new_var]] <- NA
  
  if (orig_var == "DIABETICEV") {
    nhis_lcrisks[[new_var]][nhis_lcrisks[[orig_var]] %in% c(1, 3)] <- 0
    nhis_lcrisks[[new_var]][nhis_lcrisks[[orig_var]] == 2] <- 1
  } else {
    nhis_lcrisks[[new_var]][nhis_lcrisks[[orig_var]] == 1] <- 0
    nhis_lcrisks[[new_var]][nhis_lcrisks[[orig_var]] == 2] <- 1
  }
}

################################################################################
# --- 8. PACK-YEARS (pky) -------------------------------------------------------
################################################################################

nhis_lcrisks$pky <- NA

valid_pky <- !is.na(nhis_lcrisks$avecpd) &
  !is.na(nhis_lcrisks$smkyears)

nhis_lcrisks$pky[valid_pky] <-
  (nhis_lcrisks$avecpd[valid_pky] * nhis_lcrisks$smkyears[valid_pky]) / 20

nhis_lcrisks$pky[nhis_lcrisks$pky < 0] <- NA


################################################################################

# --- Create STAT 525 analysis dataset -----------------------------------------
stat525_project <- nhis_lcrisks %>%
  select(
    pky,                      # response variable
    age, female, race, edu,   # demographics
    smkyears, avecpd, qtyears,# smoking variables
    bmi,                      # bmi
    copd, diab, heartattack, heartdisease,
    hypertension, stroke, kidney, liver,
    fmhist                    # family history of cancer
  ) %>%
  filter(age >= 18) %>%       # adults only
  filter(!is.na(pky)) %>%     # must have pack-years
  filter(pky >= 0)            # no negative values

# --- Save to your STAT 525 project folder -------------------------------------
write_csv(
  stat525_project,
  "/Users/al8xi8/Documents/GitHub/STAT525_Purdue2025/projects/stat525_project.csv")

stat525_project <- read_csv("~/Documents/GitHub/STAT525_Purdue2025/projects/stat525_project.csv")
View(stat525_project)

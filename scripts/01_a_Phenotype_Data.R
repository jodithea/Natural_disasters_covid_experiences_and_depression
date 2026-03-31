### EFFECTS OF NATURAL DISASTER AND COVID EXPERIENCES ADJUSTING FOR MDD-PRS ON DEPRESSION SYMPTOMS - SETUP DATA ###

## ---- LoadPackages
library(haven)        # for importing SPSS files
library(labelled)     # for looking at metadata stored as variable labels
library(readxl)       # read in excel files
library(EnvStats)     # for sample size on ggplot2 plots
library(glmmTMB)      # to fit GLMMs, and to allow dispersion parameter to vary with chosen explanatory variables
library(VGAM)         # to fit ordinal regression models
library(MuMIn)        # to calculate AICc
library(MASS)         # for negative binomial and proportional odds models
library(performance)  # for model diagnostics
library(see)          # needed for performance model diagnostics
library(ggfortify)    # needed for autoplot model diagnostics
library(DHARMa)       # for residuals and model diagnostics
library(generalhoslem)# for model diagnostics for proportional odds model in polr
library(brant)        # for model diagnostics for proportional odds model in polr
library(gofcat)       # for model diagnostic for models in VGAM
library(effects)      # for nice summary of model results (allEffects)
library(sjPlot)       # for summary plot of model results
library(ggplotify)    # to use patchwork on lattice plots (created by allEffects)
library(broom.mixed)  # for tidy output
library(emmeans)      # for estimating marginal means
library(patchwork)    # for putting together plots into a figure
library(ggeffects)    # create tidy data frames from marginal data
library(knitr)        # for kable tables
library(ggvenn)       # for venn diagram
library(ggforce)      # ggplot circles
library(Hmisc)        # power analyses
library(knitr)        # for kable tables
library(tidyverse)
## ----

## ---- ReadInData
# Read in tidied AGDS data
load(file = "/path_one/AGDS/R/data/AGDS_data.RData")

# Read in tidied AGDS follow-up data
load(file = "/path_one/AGDS/R/data/AGDS_followup_data.RData")

# Read in tidied AGDS covid wave 1 data
load(file = "/path_one/AGDS/R/data/AGDS_COVIDwave1_data.RData")

# Read in tidied AGDS covid wave 2 data
load(file = "/path_one/AGDS/R/data/AGDS_COVIDwave2_data.RData")


# Date of birth data for each participant in covid waves 1 and 2 (to calculate age)
DOB_COVIDwave1 <- read_delim(file = "data/AGDS_covidwave1_dateofbirth.csv")
DOB_COVIDwave2 <- read_delim(file = "data/AGDS_covidwave2_dateofbirth.csv")

# Read in key to match aprticipants across surveys
id_map <- read_csv(file = "/path_one/AGDS/R/data_combine_datasets/AGDS_studycode_key.csv")

# Read in list of withdrawals
withdrawals <- read_excel("/path_one/AGDS/R/data_combine_datasets/AGDS_Withdrawals_03102024.xlsx")
## ----

## ---- CombineData
# Use id_map to get a standardised ID for each participant which can then be used to merge all datasets

# Step 1: Join each dataset with id_map to get the standardised ID (column FINALID)
AGDS_baseline_std <- AGDS_data %>%
  rename(STUDYCODE = STUDYID) %>%           # baseline uses STUDYID
  left_join(id_map, by = "STUDYCODE") %>%
  mutate(FINALID = coalesce(FINALID, STUDYCODE))   # if FINALID is missing, use the original ID (i.e. this participant is not in the id_map)

AGDS_followup_std <- AGDS_followup_data %>%
  select(-STUDYCODE) %>%                    # STUDYCODE col in follow-up is empty
  rename(STUDYCODE = StudyCode.0) %>%       # follow-up uses StudyCode.0
  left_join(id_map, by = "STUDYCODE") %>%
  mutate(FINALID = coalesce(FINALID, STUDYCODE))   # if FINALID is missing, use the original ID (i.e. this participant is not in the id_map)

AGDS_covid1_std <- AGDS_COVIDwave1_data %>%
  select(-STUDYCODE) %>%                    # STUDYCODE col in covid1 is empty
  rename(STUDYCODE = STUDYCODE_ID) %>%      # covid1 uses STUDYCODE_ID
  left_join(id_map, by = "STUDYCODE") %>%
  mutate(FINALID = coalesce(FINALID, STUDYCODE))   # if FINALID is missing, use the original ID (i.e. this participant is not in the id_map)

AGDS_covid2_std <- AGDS_COVIDwave2_data %>%
  rename(STUDYCODE = StudyCode) %>%         # covid2 uses StudyCode
  left_join(id_map, by = "STUDYCODE") %>%
  mutate(FINALID = coalesce(FINALID, STUDYCODE))   # if FINALID is missing, use the original ID (i.e. this participant is not in the id_map)

AGDS_covid1_dob_std <- DOB_COVIDwave1 %>%
  rename(STUDYCODE = STUDYCODE_ID) %>%      # dob file uses STUDYCODE_ID
  left_join(id_map, by = "STUDYCODE") %>%
  mutate(FINALID = coalesce(FINALID, STUDYCODE))   # if FINALID is missing, use the original ID (i.e. this participant is not in the id_map)

AGDS_covid2_dob_std <- DOB_COVIDwave2 %>%
  rename(STUDYCODE = STUDYCODE_ID) %>%      # dob file uses STUDYCODE_ID
  left_join(id_map, by = "STUDYCODE") %>%
  mutate(FINALID = coalesce(FINALID, STUDYCODE))   # if FINALID is missing, use the original ID (i.e. this participant is not in the id_map)

# Step 2: Now that all participants use the same standardised IDs (FINALID), join all datasets

# Put all standardised datasets into a named list
datasets <- list(
  baseline   = AGDS_baseline_std,
  followup   = AGDS_followup_std,
  covid1     = AGDS_covid1_std,
  covid2     = AGDS_covid2_std,
  covid1dob  = AGDS_covid1_dob_std,
  covid2dob  = AGDS_covid2_dob_std
)

# Clean and add suffix to columns for each dataset (so can determine which dataset each column originated from)
datasets_cleaned <- imap(datasets, function(df, dataset_name) {
  df %>%
    select(-STUDYCODE) %>%  # drop old studycode if present
    rename_with(~ paste0(.x, "_", dataset_name), -FINALID)  # add suffix to all columns except FINALID
})

# Merge all datasets by FINALID
AGDS_all <- reduce(datasets_cleaned, full_join, by = "FINALID") %>% 
  select(FINALID, everything())

# Remove individuals that have asked to be withdrawn
withdrawals_std <- withdrawals %>%
  rename(STUDYCODE = STUDYCODE_ID) %>%      # withdrawals file uses STUDYCODE_ID
  left_join(id_map, by = "STUDYCODE") %>%
  mutate(FINALID = coalesce(FINALID, STUDYCODE))   # if FINALID is missing, use the original ID (i.e. this participant is not in the id_map)

AGDS_all <- AGDS_all %>% 
  anti_join(withdrawals_std, by = join_by(FINALID))

# Remove labels from data frame (models can't handle them still being there)
# user_na_to_na = TRUE to convert user defined missing values to NA - don't think I need this but include just to be safe
phen_data <- remove_labels(AGDS_all, user_na_to_na = TRUE)

save(phen_data, file = "data/phen_data.RData")
## ----

## ---- NewPhenotypes
# Create new phenotypes with all surveys now joined in to one dataframe

## AGE ##
# Using dob in covid 1 and 2
phen_data <- phen_data %>% 
  mutate(StartDate_covid1 = as.Date(StartDate_covid1),
         AGE_covid1 = floor(as.numeric(StartDate_covid1 - dmy(dateofbirth_covid1dob))/365.25),
         StartDate_covid2 = as.Date(StartDate_covid2),
         AGE_covid2 = floor(as.numeric(StartDate_covid2 - dmy(dateofbirth_covid2dob))/365.25)) 


## AGDS BASELINE NATURAL DISASTERS ##

# Create new columns for defining natural disaster exposure in relation to timing of depression
# NDEXPOS_DEPRWAFTER = yes (experienced a ND but only if worst depression occurred after the first ND experience.) No (never experienced ND)
# NDEXPOSME_DEPRWAFTER = as above but restricted to ND exposure happened to me
# NDEXPOS_DEPRFAFTER = yes (experienced a ND but only if first depression occurred after the first ND experience.) No (never experienced ND)
# NDEXPOSME_DEPRFAFTER = as above but restricted to ND exposure happened to me

phen_data <- phen_data %>% 
  mutate(NDEXPOS_DEPRWAFTER = case_when(
    NDEXPOS_baseline == "Yes" & NDFDEPRW_baseline == "after" ~ "Yes",
    NDEXPOS_baseline == "No" ~ "No"
  ),
  NDEXPOS_DEPRWAFTER = factor(NDEXPOS_DEPRWAFTER, levels = c('No', "Yes")),
  
  NDEXPOSME_DEPRWAFTER = case_when(
    NDEXPOSME_baseline == "Yes" & NDFDEPRW_baseline == "after" ~ "Yes",
    NDEXPOSME_baseline == "No" ~ "No"
  ),
  NDEXPOSME_DEPRWAFTER = factor(NDEXPOSME_DEPRWAFTER, levels = c('No', "Yes")),
  
  NDEXPOS_DEPRFAFTER = case_when(
    NDEXPOS_baseline == "Yes" & NDFDEPRF_baseline == "after" ~ "Yes",
    NDEXPOS_baseline == "No" ~ "No"
  ),
  NDEXPOS_DEPRFAFTER = factor(NDEXPOS_DEPRFAFTER, levels = c('No', "Yes")),
  
  NDEXPOSME_DEPRFAFTER = case_when(
    NDEXPOSME_baseline == "Yes" & NDFDEPRF_baseline == "after" ~ "Yes",
    NDEXPOSME_baseline == "No" ~ "No"
  ),
  NDEXPOSME_DEPRFAFTER = factor(NDEXPOSME_DEPRFAFTER, levels = c('No', "Yes")))

## COVID WAVE 1 AND WAVE 2 COVID EXPERIENCES ##
# COVIDALLBIN = Experienced any frightening, horrible or traumatic experiences related to COVID-19.
#   Yes = happened to me, witnessed it, learned about it or part of my job
#   No = No
# COVIDMEBIN = Experienced any frightening, horrible or traumatic experiences related to COVID-19.
#   Yes = happened to me
#   No = No, witnessed it, learned about it or part of my job
# If answered yes in wave 1 and/or 2 = yes
# If answered no in wave 1 and wave 2 = no
# Use NUMSYMPALL from wave 2 for all

phen_data <- phen_data %>% 
  mutate(COVIDALLBIN_C1C2 = case_when(
    COVIDALLBIN_covid1==1 | COVIDALLBIN_covid2==1 ~ 1,
    COVIDALLBIN_covid1==0 & COVIDALLBIN_covid2==0 ~ 0
  ),
  COVIDALLBIN_C1C2 = factor(COVIDALLBIN_C1C2, levels = c('0', '1')),
  COVIDMEBIN_C1C2 = case_when(
    COVIDMEBIN_covid1==1 | COVIDMEBIN_covid2==1 ~ 1,
    COVIDMEBIN_covid1==0 & COVIDMEBIN_covid2==0 ~ 0
  ),
  COVIDMEBIN_C1C2 = factor(COVIDMEBIN_C1C2, levels = c('0', '1')))

## BUSHFIRE/FLODD FROM FOLLOW-UP AND COVID WAVE 1 AND COVID WAVE 2 ##
# BUSHFLOODHORR = whether participant experienced any frightening, horrible or traumatic experiences related to the bushfires and flooding over the past year (binary)
# Create new variable combining bushfire/flood and covid experiences
# Bushfire/flood and COVID = yes to bushfires/floods AND covid wave 1 and/or wave 2
# COVID only = yes to covid wave 1 and/or 2 and not to bushfire/flood
# Bushfire/flood only = yes to bushfire/flood and no to covid wave 1 and 2
# No = no to bushfires/floods and covid wave 1 and covid wave 2

phen_data <- phen_data %>%
  mutate(COVIDALL_BUSHFLOOD = case_when(
    COVIDALLBIN_C1C2==1 & BUSHFLOODHORR_followup==1 ~ "Bushfire/flood and COVID",
    COVIDALLBIN_C1C2==1 & BUSHFLOODHORR_followup==0 ~ "COVID only",
    COVIDALLBIN_C1C2==0 & BUSHFLOODHORR_followup==1 ~ "Bushfire/flood only",
    COVIDALLBIN_C1C2==0 & BUSHFLOODHORR_followup==0 ~ "No"
  ),
  COVIDALL_BUSHFLOOD = factor(COVIDALL_BUSHFLOOD, levels = c('No', 'Bushfire/flood only', 'COVID only', 'Bushfire/flood and COVID')),
  COVIDME_BUSHFLOOD = case_when(
    COVIDMEBIN_C1C2==1 & BUSHFLOODHORR_followup==1 ~ "Bushfire/flood and COVID",
    COVIDMEBIN_C1C2==1 & BUSHFLOODHORR_followup==0 ~ "COVID only",
    COVIDMEBIN_C1C2==0 & BUSHFLOODHORR_followup==1 ~ "Bushfire/flood only",
    COVIDMEBIN_C1C2==0 & BUSHFLOODHORR_followup==0 ~ "No"
  ),
  COVIDME_BUSHFLOOD = factor(COVIDME_BUSHFLOOD, levels = c('No', 'Bushfire/flood only', 'COVID only', 'Bushfire/flood and COVID')),
  COVIDALL_BUSHFLOOD_both = case_when(
    COVIDALL_BUSHFLOOD == "No" ~ "No",
    COVIDALL_BUSHFLOOD == "Bushfire/flood and COVID" ~ "Yes"
  ),
  COVIDALL_BUSHFLOOD_both = factor(COVIDALL_BUSHFLOOD_both, levels = c("No", "Yes")),
  COVIDME_BUSHFLOOD_both = case_when(
    COVIDME_BUSHFLOOD == "No" ~ "No",
    COVIDME_BUSHFLOOD == "Bushfire/flood and COVID" ~ "Yes"
  ),
  COVIDME_BUSHFLOOD_both = factor(COVIDME_BUSHFLOOD_both, levels = c("No", "Yes"))
  )



save(phen_data, file = "data/phen_data.RData")
## -----

## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----

## ---- MissingnessNDFirst
# ND (first)
# Subset only to individuals that started baseline survey + started follow-up survey + have genetic data
# Then calculate missingness for relevant variables: NUMSYMPALL_followup, AGE_baseline, NDEXPOS_DEPRFAFTER (should be no missingness for variables derived from genetci data: PCs, sex, PGS)

# have genetic data (6,450 missiing PGS data) and participated in the follow-up survey (answered at least 1 q.) (12,733 did not answer at least one follow-up q.)

# If in dataset then started baseline survey
# Determine who completed at least on q. in the followup survey 
df_ND_covid <- df_ND_covid %>%
  mutate(num_followup_qs_answered = rowSums(!is.na(select(., ends_with("_followup")))),
         started_followup = case_when(
           num_followup_qs_answered > 0 ~ TRUE,
           num_followup_qs_answered == 0 ~ FALSE
         ))

df_ND_covid %>% 
  count(started_followup)
# 9,098 people answered at least 1 question in the follow-up survey (12,733 didn't answer any qs in follow-up survey)

df_ND_covid %>% 
  count(is.na(PRS_stand_genetic), is.na(SEX_genetic), is.na(PC1_genetic)) %>% 
  kable()
# 15,302 people have data for PRS, PC and sex
# 6,450 people don't have data for PRS, PC and sex
# 79 people don't have sex data but do have PRS and PC data

# Eligible participants
df_ND_covid <- df_ND_covid %>% 
  mutate(eligible_NDfirst = as.vector(!is.na(PRS_stand_genetic) & started_followup == TRUE))

df_ND_covid %>% count(eligible_NDfirst)
# 8,119 people eligible for any ND (first) analyses (started baseline survey + started follow-up survey + have genotype data)

# Now look at numbers missing for variables of interest
df_ND_covid <- df_ND_covid %>%
  mutate(NDEXPOS_deprfirst_dontmeettimingdefinition = case_when(
    (NDEXPOS_baseline == "Yes" & NDFDEPRF_baseline == "before") | 
      (NDEXPOS_baseline == "Yes" & NDFDEPRF_baseline == "same year") ~ TRUE
  ),
  NDEXPOS_deprfirst_missing_NDexpos = is.na(NDEXPOS_baseline),
  NDEXPOS_deprfirst_missing_timing = case_when(
    (NDEXPOS_baseline == "Yes" & is.na(NDFDEPRF_baseline)) ~ TRUE
  )
  )

df_ND_covid %>%
  filter(eligible_NDfirst == TRUE) %>%
  summarise(
    n = n(),
    NUMSYMPALL_missing = sum(is.na(NUMSYMPALL_followup)),
    NUMSYMPALL_missing_perc = sum(is.na(NUMSYMPALL_followup))/n * 100,
    AGE_missing = sum(is.na(AGE_baseline)),
    AGE_missing_perc = sum(is.na(AGE_baseline))/n * 100,
    NDEXPOS_missing = sum(is.na(NDEXPOS_DEPRFAFTER)),
    NDEXPOS_missing_perc = sum(is.na(NDEXPOS_DEPRFAFTER))/n * 100,
    NDEXPOS_dontmeettimingdefinition = sum(NDEXPOS_deprfirst_dontmeettimingdefinition == TRUE, na.rm = TRUE),
    NDEXPOS_dontmeettimingdefinition_perc = sum(NDEXPOS_deprfirst_dontmeettimingdefinition == TRUE, na.rm = TRUE)/n * 100,
    NDEXPOS_missing_NDexpos = sum(is.na(NDEXPOS_baseline)),
    NDEXPOS_missing_NDexpos_perc = sum(is.na(NDEXPOS_baseline))/n * 100,
    NDEXPOS_missing_timing = sum(NDEXPOS_deprfirst_missing_timing == TRUE, na.rm = TRUE),
    NDEXPOS_missing_timing_perc = sum(NDEXPOS_deprfirst_missing_timing == TRUE, na.rm = TRUE)/n * 100,
    SEX_missing = sum(is.na(SEX_genetic)),
    SEX_missing_perc = sum(is.na(SEX_genetic))/n * 100) %>% 
  t() %>% 
  kable(caption = "Comparison of each variable across participants missing data and not missing data for the natural disaster exposure variable")




# Compare variables across missing vs observed ND exposure (not including doesn't meet temporal definition, i.e. missingness is because didnt answer questions)
test_data <- df_ND_covid %>%
  filter(eligible_NDfirst) %>%
  mutate(
    ND_missing = case_when(
      NDEXPOS_deprfirst_missing_NDexpos == TRUE ~ "no_answer",
      NDEXPOS_deprfirst_missing_timing == TRUE ~ "no_answer",
      !is.na(NDEXPOS_DEPRFAFTER) ~ "observed",
      NDEXPOS_deprfirst_dontmeettimingdefinition == TRUE ~ "dont_meet_temporal_definition"
    ))

test_data %>% 
  group_by(ND_missing) %>%
  summarise(
    n = n(),
    age = mean(AGE_baseline, na.rm = TRUE),
    female = mean(SEX_genetic == "Female", na.rm = TRUE),
    pgs = mean(PRS_stand_genetic, na.rm = TRUE),
    num_depression_symptoms = mean(NUMSYMPALL_followup, na.rm = TRUE)
  ) %>% 
  kable

test_data <- test_data %>% 
  filter_out(ND_missing == "dont_meet_temporal_definition") %>% 
  mutate(ND_missing = case_when(
    ND_missing == "no_answer" ~ TRUE,
    ND_missing == "observed" ~ FALSE
  ))

# Age
age_model <- glm(ND_missing ~ AGE_baseline,
                 family = binomial(link = logit),
                 data = test_data)

age_result <- tidy(age_model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "AGE_baseline")

# PGS
pgs_model <- glm(ND_missing ~ PRS_stand_genetic,
                 family = binomial(link = logit),
                 data = test_data)

pgs_result <- tidy(pgs_model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "PRS_stand_genetic")

# Symptoms
symptom_model <- glm(ND_missing ~ NUMSYMPALL_followup,
                     family = binomial(link = logit),
                     data = test_data)

symptom_result <- tidy(symptom_model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "NUMSYMPALL_followup")

# Sex
sex_model <- glm(ND_missing ~ SEX_genetic,
                 family = binomial(link = logit),
                 data = test_data)

sex_result <- tidy(sex_model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "SEX_geneticFemale")

age_result %>% bind_rows(sex_result, pgs_result, symptom_result) %>% 
  rename(OR = estimate,
         SE = std.error,
         Statistic = statistic,
         Lower_CI = conf.low,
         Upper_CI = conf.high,
         P = p.value) %>% 
  mutate(Padj = p.adjust(P, method = "BH", n = 4)) %>% 
  select(term, OR, SE, Lower_CI, Upper_CI, Statistic, P, Padj) %>% 
  kable(caption = "Comparison of each variable across participants with data and without data for the any natural disaster exposure variable")



# Compare variables across missing vs observed depression symptoms
df_ND_covid %>%
  filter(eligible_NDfirst) %>%
  mutate(
    symptoms_missing = is.na(NUMSYMPALL_followup)
  ) %>%
  group_by(symptoms_missing) %>%
  summarise(
    n = n(),
    age = mean(AGE_baseline, na.rm = TRUE),
    female = mean(SEX_genetic == "Female", na.rm = TRUE),
    pgs = mean(PRS_stand_genetic, na.rm = TRUE),
    ND_exposed = mean(NDEXPOS_DEPRFAFTER == "Yes", na.rm = TRUE)
  ) %>% 
  kable()

# Age
age_model <- glm(is.na(NUMSYMPALL_followup) ~ AGE_baseline,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_NDfirst))

age_result <- tidy(age_model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "AGE_baseline")

# PGS
pgs_model <- glm(is.na(NUMSYMPALL_followup) ~ PRS_stand_genetic,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_NDfirst))

pgs_result <- tidy(pgs_model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "PRS_stand_genetic")

# ND exposure
nd_model <- glm(is.na(NUMSYMPALL_followup) ~ NDEXPOS_DEPRFAFTER,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_NDfirst))

nd_result <- tidy(nd_model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "NDEXPOS_DEPRFAFTERYes")

# Sex
sex_model <- glm(is.na(NUMSYMPALL_followup) ~ SEX_genetic,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_NDfirst))

sex_result <- tidy(sex_model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "SEX_geneticFemale")

age_result %>% bind_rows(sex_result, pgs_result, nd_result) %>% 
  rename(OR = estimate,
         SE = std.error,
         Statistic = statistic,
         Lower_CI = conf.low,
         Upper_CI = conf.high,
         P = p.value) %>% 
  mutate(Padj = p.adjust(P, method = "BH", n = 4)) %>% 
  select(term, OR, SE, Lower_CI, Upper_CI, Statistic, P, Padj) %>% 
  kable(caption = "Comparison of each variable across participants with data and without data for the outcome variable (number depression symptoms)")
## ----




## ----
# Compare variables across missing vs observed ND exposure 
df_ND_covid %>%
  filter(eligible_NDfirst) %>%
  mutate(
    ND_missing = is.na(NDEXPOS_DEPRFAFTER)) %>%
  group_by(ND_missing) %>%
  summarise(
    n = n(),
    age = mean(AGE_baseline, na.rm = TRUE),
    female = mean(SEX_genetic == "Female", na.rm = TRUE),
    pgs = mean(PRS_stand_genetic, na.rm = TRUE),
    num_depression_symptoms = mean(NUMSYMPALL_followup, na.rm = TRUE)
  ) %>% 
  kable

# Age
summary(glm(is.na(NDEXPOS_DEPRFAFTER) ~ AGE_baseline,
            family = binomial(link = logit),
            data = df_ND_covid %>% filter(eligible_NDfirst)))

# PGS
summary(glm(is.na(NDEXPOS_DEPRFAFTER) ~ PRS_stand_genetic,
            family = binomial(link = logit),
            data = df_ND_covid %>% filter(eligible_NDfirst)))

# Symptoms
summary(glm(is.na(NDEXPOS_DEPRFAFTER) ~ NUMSYMPALL_followup,
            family = binomial(link = logit),
            data = df_ND_covid %>% filter(eligible_NDfirst)))

# Sex
summary(glm(is.na(NDEXPOS_DEPRFAFTER) ~ SEX_genetic,
            family = binomial(link = logit),
            data = df_ND_covid %>% filter(eligible_NDfirst)))
## ----


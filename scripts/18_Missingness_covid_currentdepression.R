## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----

## ---- MissingnessCOVID
# 2019 bushfires/floods
# Subset only to individuals that started COVID wave 1 survey + started COVID wave 2 survey + have genetic data
# Then calculate missingness for relevant variables: NUMSYMPALL_covid2_cat, AGE_covid2, COVIDALLBIN_C1C2 (should be no missingness for variables derived from genetci data: PCs, sex, PGS)


# Determine who completed at least on q. in the covid waves 1 and 2 surveys
df_ND_covid <- df_ND_covid %>%
  mutate(num_covid1_qs_answered = rowSums(!is.na(select(., ends_with("_covid1")))),
         started_covid1 = case_when(
           num_covid1_qs_answered > 0 ~ TRUE,
           num_covid1_qs_answered == 0 ~ FALSE
         ),
         num_covid2_qs_answered = rowSums(!is.na(select(., ends_with("_covid2")))),
         started_covid2 = case_when(
           num_covid2_qs_answered > 0 ~ TRUE,
           num_covid2_qs_answered == 0 ~ FALSE
         ))

df_ND_covid %>% 
  count(started_covid1)
# 6,122 people answered at least 1 question in COVID wave 1 (15,709 didnt answer any qs in covid wave 1 survey)
df_ND_covid %>% 
  count(started_covid2)
# 5,374 people answered at least 1 question in COVID wave 2 (16,457 didnt answer any qs in covid wave 2 survey)

# Eligible participants
df_ND_covid <- df_ND_covid %>% 
  mutate(eligible_covid2 = as.vector(!is.na(PRS_stand_genetic) & started_covid1 == TRUE & started_covid2 == TRUE))

df_ND_covid %>% count(eligible_covid2)
# 3,620 people eligible for covid analyses (started COVID wave 1 survey + started covid wave 2 survey + have genotype data)

# Now look at numbers missing for variables of interest
df_ND_covid %>%
  filter(eligible_covid2 == TRUE) %>%
  summarise(
    n = n(),
    NUMSYMPALL_missing = sum(is.na(NUMSYMPALL_covid2_cat)),
    NUMSYMPALL_missing_perc = sum(is.na(NUMSYMPALL_covid2_cat))/n * 100,
    AGE_missing = sum(is.na(AGE_covid2)),
    AGE_missing_perc = sum(is.na(AGE_covid2))/n * 100,
    Covid_missing = sum(is.na(COVIDALLBIN_C1C2)),
    Covid_missing_perc = sum(is.na(COVIDALLBIN_C1C2))/n * 100,
    SEX_missing = sum(is.na(SEX_genetic)),
    SEX_missing_perc = sum(is.na(SEX_genetic))/n * 100) %>% 
  t() %>% 
  kable(caption = "Comparison of each variable across participants missing data and not missing data for the COVID-19 traumatic experiences variable")
## ----



## ---- CompareMissingnessCovidCovidExperience
# Compare variables across missing vs observed covid experience
df_ND_covid %>%
  filter(eligible_covid2) %>%
  mutate(
    covid_missing = is.na(COVIDALLBIN_C1C2)
  ) %>%
  group_by(covid_missing) %>%
  summarise(
    n = n(),
    age = mean(AGE_covid2, na.rm = TRUE),
    female = mean(SEX_genetic == "Female", na.rm = TRUE),
    pgs = mean(PRS_stand_genetic, na.rm = TRUE),
    num_depr_symptoms = mean(NUMSYMPALL_covid2, na.rm = TRUE)
  ) %>% 
  kable()

# Age
summary(glm(is.na(COVIDALLBIN_C1C2) ~ AGE_covid2,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_covid2)))

# PGS
summary(glm(is.na(COVIDALLBIN_C1C2) ~ PRS_stand_genetic,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_covid2)))

# Num depression symptoms
summary(glm(is.na(COVIDALLBIN_C1C2) ~ NUMSYMPALL_covid2,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_covid2)))

# Sex
summary(glm(is.na(COVIDALLBIN_C1C2) ~ SEX_genetic,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_covid2)))
## ----
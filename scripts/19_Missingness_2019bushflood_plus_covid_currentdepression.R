## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----

## ---- MissingnessBushFloodCOVID
# 2019 bushfires/floods
# Subset only to individuals that started follow-up survey + COVID wave 1 survey + started COVID wave 2 survey + have genetic data
# Then calculate missingness for relevant variables: NUMSYMPALL_covid2_cat, AGE_covid2, COVIDALL_BUSHFLOOD_both (should be no missingness for variables derived from genetci data: PCs, sex, PGS)


# Determine who completed at least on q. in the covid waves 1 and 2 surveys
df_ND_covid <- df_ND_covid %>%
  mutate(num_followup_qs_answered = rowSums(!is.na(select(., ends_with("_followup")))),
         started_followup = case_when(
           num_followup_qs_answered > 0 ~ TRUE,
           num_followup_qs_answered == 0 ~ FALSE
         ),
         num_covid1_qs_answered = rowSums(!is.na(select(., ends_with("_covid1")))),
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
  count(started_followup)
# 9,098 people answered at least 1 question in the follow-up survey (12,733 didn't answer any qs in follow-up survey)
df_ND_covid %>% 
  count(started_covid1)
# 6,122 people answered at least 1 question in COVID wave 1 (15,709 didnt answer any qs in covid wave 1 survey)
df_ND_covid %>% 
  count(started_covid2)
# 5,374 people answered at least 1 question in COVID wave 2 (16,457 didnt answer any qs in covid wave 2 survey)

# Eligible participants
df_ND_covid <- df_ND_covid %>% 
  mutate(eligible_covid2 = as.vector(!is.na(PRS_stand_genetic) & started_followup == TRUE & started_covid1 == TRUE & started_covid2 == TRUE))

df_ND_covid %>% count(eligible_covid2)
# 3,304 people eligible for bush/flood plus covid analyses (started followup survey + started COVID wave 1 survey + started covid wave 2 survey + have genotype data)

# Now look at numbers missing for variables of interest
df_ND_covid %>%
  filter(eligible_covid2 == TRUE) %>%
  summarise(
    n = n(),
    NUMSYMPALL_missing = sum(is.na(NUMSYMPALL_covid2_cat)),
    NUMSYMPALL_missing_perc = sum(is.na(NUMSYMPALL_covid2_cat))/n * 100,
    AGE_missing = sum(is.na(AGE_covid2)),
    AGE_missing_perc = sum(is.na(AGE_covid2))/n * 100,
    Bushflood_Covid_missing = sum(is.na(COVIDALL_BUSHFLOOD_both)),
    Bushflood_Covid_missing_perc = sum(is.na(COVIDALL_BUSHFLOOD_both))/n * 100,
    SEX_missing = sum(is.na(SEX_genetic)),
    SEX_missing_perc = sum(is.na(SEX_genetic))/n * 100) %>% 
  t() %>% 
  kable(caption = "Comparison of each variable across participants missing data and not missing data for the combined bushfires/floods and COVID-19 variable")




# Compare variables across missing vs observed bushflood _ covid experience
df_ND_covid %>%
  filter(eligible_covid2) %>%
  mutate(
    bushfloodcovid_missing = is.na(COVIDALL_BUSHFLOOD_both)
  ) %>%
  group_by(bushfloodcovid_missing) %>%
  summarise(
    n = n(),
    age = mean(AGE_covid2, na.rm = TRUE),
    female = mean(SEX_genetic == "Female", na.rm = TRUE),
    pgs = mean(PRS_stand_genetic, na.rm = TRUE),
    num_depr_symptoms = mean(NUMSYMPALL_covid2, na.rm = TRUE)
  ) %>% 
  kable(caption = "Comparison of each variable across participants missing data and not missing data for the combined bushfires/floods and COVID-19 variable")

# Age
age_model <- glm(is.na(COVIDALLBIN_C1C2) ~ AGE_covid2,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_covid2))

age_result <- tidy(age_model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "AGE_covid2")

# PGS
pgs_model <- glm(is.na(COVIDALLBIN_C1C2) ~ PRS_stand_genetic,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_covid2))

pgs_result <- tidy(pgs_model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "PRS_stand_genetic")

# Num depression symptoms
symptoms_model <- glm(is.na(COVIDALLBIN_C1C2) ~ NUMSYMPALL_covid2,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_covid2))

symptoms_result <- tidy(symptoms_model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "NUMSYMPALL_covid2")

# Sex
sex_model <- glm(is.na(COVIDALLBIN_C1C2) ~ SEX_genetic,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_covid2))

sex_result <- tidy(sex_model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "SEX_geneticFemale")

age_result %>% bind_rows(sex_result, pgs_result, symptoms_result) %>% 
  rename(OR = estimate,
         SE = std.error,
         Statistic = statistic,
         Lower_CI = conf.low,
         Upper_CI = conf.high,
         P = p.value) %>% 
  mutate(Padj = p.adjust(P, method = "BH", n = 4)) %>% 
  select(term, OR, SE, Lower_CI, Upper_CI, Statistic, P, Padj) %>% 
  kable(caption = "Comparison of each variable across participants with data and without data for the combined bushfires/floods and COVID-19 variable")
## ----
## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----

## ---- MissingnessBushFlood
# 2019 bushfires/floods
# Subset only to individuals that started follow-up survey + started COVID wave 1 survey + have genetic data
# Calculated num current symptoms from COVID Wave 1 survey -  participants were radomly assigned to 2 conditions - asked about covid-19 experiences in the last 2 weeks, or asked about covid-19 experiences in the worst period (month) identified between jan 2020 and feb 2021.
# Thus, only participants assigned to 1st condition were asked about current depression symptoms and are eligible
# So subset to only individuals that started follow-up survey + started COVID wave 1 survey + were subset to condition 1 in COVID wave 1 survey (i.e. col 'groupB_covid1' has '2w') + have genetic data
# Then calculate missingness for relevant variables: NUMSYMPALL_covid1_cat, AGE_followup, BUSHFLOODHORR_followup (should be no missingness for variables derived from genetic data: PCs, sex, PGS)


# Determine who completed at least on q. in the followup survey and covid wave 1 survey
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
         ))

df_ND_covid %>% 
  count(started_followup)
# 9,098 people answered at least 1 question in the follow-up survey (12,733 didn't answer any qs in follow-up survey)
df_ND_covid %>% 
  count(started_covid1)
# 6,122 people answered at least 1 question in COVID wave 1 (15,709 didnt answer any qs in covid wave 1 survey)
df_ND_covid %>% 
  count(groupB_covid1)
# 3,029 people aswered questions on last 2 weeks for COVID wave 1

# Eligible participants
df_ND_covid <- df_ND_covid %>% 
  mutate(eligible_covid1 = as.vector(!is.na(PRS_stand_genetic) & started_followup == TRUE & started_covid1 == TRUE & groupB_covid1 == "2w"))

df_ND_covid %>% count(eligible_covid1)
# 2,342 people eligible for bushfires/floods analyses (started follow-up survey + started COVID wave 1 survey + randomly assigned to qs asking about last 2 weeks in COVID wave 1 survey + have genotype data)

# Now look at numbers missing for variables of interest
df_ND_covid %>%
  filter(eligible_covid1 == TRUE) %>%
  summarise(
    n = n(),
    NUMSYMPALL_missing = sum(is.na(NUMSYMPALL_covid1_cat)),
    NUMSYMPALL_missing_perc = sum(is.na(NUMSYMPALL_covid1_cat))/n * 100,
    AGE_missing = sum(is.na(AGE_followup)),
    AGE_missing_perc = sum(is.na(AGE_followup))/n * 100,
    Bushflood_missing = sum(is.na(BUSHFLOODHORR_followup)),
    Bushflood_missing_perc = sum(is.na(BUSHFLOODHORR_followup))/n * 100,
    SEX_missing = sum(is.na(SEX_genetic)),
    SEX_missing_perc = sum(is.na(SEX_genetic))/n * 100) %>% 
  t() %>% 
  kable(caption = "Comparison of each variable across participants missing data and not missing data for the bushfires/flooding traumatic experiences variable")
## ----


## ---- CompareMissingnessBushFloodDepressionSymptoms
# Compare variables across missing vs observed depression symptoms
df_ND_covid %>%
  filter(eligible_covid1) %>%
  mutate(
    symptoms_missing = is.na(NUMSYMPALL_covid1_cat)
  ) %>%
  group_by(symptoms_missing) %>%
  summarise(
    n = n(),
    age = mean(AGE_followup, na.rm = TRUE),
    female = mean(SEX_genetic == "Female", na.rm = TRUE),
    pgs = mean(PRS_stand_genetic, na.rm = TRUE),
    bushflood_experience = mean(BUSHFLOODHORR_followup == "1", na.rm = TRUE)
  ) %>% 
  kable()

# Age
summary(glm(is.na(NUMSYMPALL_covid1_cat) ~ AGE_followup,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_covid1)))

# PGS
summary(glm(is.na(NUMSYMPALL_covid1_cat) ~ PRS_stand_genetic,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_covid1)))

# Bushfires/floods experience
summary(glm(is.na(NUMSYMPALL_covid1_cat) ~ BUSHFLOODHORR_followup,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_covid1)))

# Sex
summary(glm(is.na(NUMSYMPALL_covid1_cat) ~ SEX_genetic,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_covid1)))
## ----


## ---- CompareMissingnessBushFloodBushFlood
# Compare variables across missing vs observed bushfire/flood experience
df_ND_covid %>%
  filter(eligible_covid1) %>%
  mutate(
    bushflood_missing = is.na(BUSHFLOODHORR_followup)
  ) %>%
  group_by(bushflood_missing) %>%
  summarise(
    n = n(),
    age = mean(AGE_followup, na.rm = TRUE),
    female = mean(SEX_genetic == "Female", na.rm = TRUE),
    pgs = mean(PRS_stand_genetic, na.rm = TRUE),
    num_symptoms = mean(NUMSYMPALL_covid1, na.rm = TRUE)
  ) %>% 
  kable()

# Age
summary(glm(is.na(BUSHFLOODHORR_followup) ~ AGE_followup,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_covid1)))

# PGS
summary(glm(is.na(BUSHFLOODHORR_followup) ~ PRS_stand_genetic,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_covid1)))

# Number depression symptoms
summary(glm(is.na(BUSHFLOODHORR_followup) ~ NUMSYMPALL_covid1,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_covid1)))

# Sex
summary(glm(is.na(BUSHFLOODHORR_followup) ~ SEX_genetic,
            family = binomial(link = logit),
            data = df_ND_covid %>%
              filter(eligible_covid1)))
## ----

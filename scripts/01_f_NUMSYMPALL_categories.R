# Group no. depressive symptoms into categories to use in ordinal regression

## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----

## ---- CodeNUMSYMPALLCategories

df_ND_covid <- df_ND_covid %>% 
  mutate(NUMSYMPALL_baseline_cat = case_when(
    NUMSYMPALL_baseline == 0 ~ "zero",
    NUMSYMPALL_baseline == 1 | NUMSYMPALL_baseline == 2 | NUMSYMPALL_baseline == 3 ~ "low",
    NUMSYMPALL_baseline == 4 | NUMSYMPALL_baseline == 5 | NUMSYMPALL_baseline == 6 ~ "medium",
    NUMSYMPALL_baseline == 7 | NUMSYMPALL_baseline == 8 | NUMSYMPALL_baseline == 9 ~ "high"
  ),
  NUMSYMPALL_baseline_cat = factor(NUMSYMPALL_baseline_cat, levels = c("zero", "low", "medium", "high"), ordered = TRUE),
  NUMSYMPALL_followup_cat = case_when(
    NUMSYMPALL_followup == 0 ~ "zero",
    NUMSYMPALL_followup == 1 | NUMSYMPALL_followup == 2 | NUMSYMPALL_followup == 3 ~ "low",
    NUMSYMPALL_followup == 4 | NUMSYMPALL_followup == 5 | NUMSYMPALL_followup == 6 ~ "medium",
    NUMSYMPALL_followup == 7 | NUMSYMPALL_followup == 8 | NUMSYMPALL_followup == 9 ~ "high"
  ),
  NUMSYMPALL_followup_cat = factor(NUMSYMPALL_followup_cat, levels = c("zero", "low", "medium", "high"), ordered = TRUE),
  NUMSYMPALL_covid1_cat = case_when(
    NUMSYMPALL_covid1 == 0 ~ "zero",
    NUMSYMPALL_covid1 == 1 | NUMSYMPALL_covid1 == 2 | NUMSYMPALL_covid1 == 3 ~ "low",
    NUMSYMPALL_covid1 == 4 | NUMSYMPALL_covid1 == 5 | NUMSYMPALL_covid1 == 6 ~ "medium",
    NUMSYMPALL_covid1 == 7 | NUMSYMPALL_covid1 == 8 | NUMSYMPALL_covid1 == 9 ~ "high"
  ),
  NUMSYMPALL_covid1_cat = factor(NUMSYMPALL_covid1_cat, levels = c("zero", "low", "medium", "high"), ordered = TRUE),
  NUMSYMPALL_covid2_cat = case_when(
    NUMSYMPALL_covid2 == 0 ~ "zero",
    NUMSYMPALL_covid2 == 1 | NUMSYMPALL_covid2 == 2 | NUMSYMPALL_covid2 == 3 ~ "low",
    NUMSYMPALL_covid2 == 4 | NUMSYMPALL_covid2 == 5 | NUMSYMPALL_covid2 == 6 ~ "medium",
    NUMSYMPALL_covid2 == 7 | NUMSYMPALL_covid2 == 8 | NUMSYMPALL_covid2 == 9 ~ "high"
  ),
  NUMSYMPALL_covid2_cat = factor(NUMSYMPALL_covid2_cat, levels = c("zero", "low", "medium", "high"), ordered = TRUE)
  )

## ----

## ---- SaveData
save(df_ND_covid, file = "data/df_ND_covid.RData")
## ----
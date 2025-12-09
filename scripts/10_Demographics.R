# Table of demographics of analysis dataset

## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----

## ---- TotalDemographics
# Function to process data - print demographics for each analytical dataset

summarise_data <- function(outcome, age, predictor, title) {
  
  n_age <- df_ND_covid %>% 
    drop_na({{outcome}}, SEX_genetic, {{age}}, matches("^PC.*_genetic$"), {{predictor}}) %>% 
    summarise(N_count = n(),
              Mean_age = mean({{age}}),
              SD_age = sd({{age}}),
              Min_age = min({{age}}),
              Max_age = max({{age}})) %>% 
    mutate(
      N_percent = N_count / sum(N_count) * 100
    ) %>% 
    relocate(N_percent, .after = N_count)
  
  n_outcome <- df_ND_covid %>%
    drop_na({{outcome}}, SEX_genetic, {{age}}, matches("^PC.*_genetic$"), {{predictor}}) %>% 
    group_by({{outcome}}) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = {{outcome}},
      values_from = count,
      values_fill = 0
    ) %>%
    rowwise() %>%
    mutate(
      total = sum(c_across(where(is.numeric))),  # sum of all category counts
      across(
        .cols = -c(total), 
        .fns = ~ .x / total * 100,
        .names = "{.col}_percent"
      )
    ) %>%
    select(-total) %>%
    ungroup() %>%
    rename_with(
      ~ paste0(.x, "_count"),
      .cols = where(is.numeric) & !ends_with("_percent")
    )
  
  n_predictor <- df_ND_covid %>%
    drop_na({{outcome}}, SEX_genetic, {{age}}, matches("^PC.*_genetic$"), {{predictor}}) %>% 
    group_by({{predictor}}) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = {{predictor}},
      values_from = count,
      values_fill = 0
    ) %>%
    rowwise() %>%
    mutate(
      total = sum(c_across(where(is.numeric))),  # sum of all category counts
      across(
        .cols = -c(total), 
        .fns = ~ .x / total * 100,
        .names = "{.col}_percent"
      )
    ) %>%
    select(-total) %>%
    ungroup() %>%
    rename_with(
      ~ paste0(.x, "_count"),
      .cols = where(is.numeric) & !ends_with("_percent")
    )
  
  
  n_age %>%
    bind_cols(n_outcome) %>% 
    bind_cols(n_predictor) %>% 
    kable(caption = title)
}


summarise_data(NUMSYMPALL_baseline_cat, AGE_baseline, NDEXPOS_DEPRWAFTER, "Natural disaster experience, worst depressive episode")
summarise_data(NUMSYMPALL_followup_cat, AGE_baseline, NDEXPOS_DEPRFAFTER, "Natural disaster experience, first depressive episode")
summarise_data(NUMSYMPALL_covid1_cat, AGE_followup, BUSHFLOODHORR_followup, "Horrible 2019 bushfires/floods experience, current symptoms (1 year later)")
summarise_data(NUMSYMPALL_covid2_cat, AGE_covid2, COVIDALLBIN_C1C2, "Horrible covid experience, current symptoms")
summarise_data(NUMSYMPALL_covid2_cat, AGE_covid2, COVIDALL_BUSHFLOOD_both, "Horrible 2019 bushfires/floods and covid experience, current symptoms")
## ----

## ---- SexDemographics
# Function to process data - print demographics for each analytical dataset split by sex

summarise_data <- function(outcome, age, predictor, title) {

  n_age <- df_ND_covid %>% 
    drop_na({{outcome}}, SEX_genetic, {{age}}, matches("^PC.*_genetic$"), {{predictor}}) %>% 
    group_by(SEX_genetic) %>% 
    summarise(N_count = n(),
              Mean_age = mean({{age}}),
              SD_age = sd({{age}}),
              Min_age = min({{age}}),
              Max_age = max({{age}})) %>% 
    mutate(
      N_percent = N_count / sum(N_count) * 100
    ) %>% 
    relocate(N_percent, .after = N_count)
  
  n_outcome <- df_ND_covid %>%
    drop_na({{outcome}}, SEX_genetic, {{age}}, matches("^PC.*_genetic$"), {{predictor}}) %>% 
    group_by(SEX_genetic, {{outcome}}) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = {{outcome}},
      values_from = count,
      values_fill = 0
    ) %>%
    rowwise() %>%
    mutate(
      total = sum(c_across(where(is.numeric))),  # sum of all category counts
      across(
        .cols = -c(SEX_genetic, total), 
        .fns = ~ .x / total * 100,
        .names = "{.col}_percent"
      )
    ) %>%
    select(-total) %>%
    ungroup() %>%
    rename_with(
      ~ paste0(.x, "_count"),
      .cols = where(is.numeric) & !ends_with("_percent")
    )
  
  n_predictor <- df_ND_covid %>%
    drop_na({{outcome}}, SEX_genetic, {{age}}, matches("^PC.*_genetic$"), {{predictor}}) %>% 
    group_by(SEX_genetic, {{predictor}}) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = {{predictor}},
      values_from = count,
      values_fill = 0
    ) %>%
    rowwise() %>%
    mutate(
      total = sum(c_across(where(is.numeric))),  # sum of all category counts
      across(
        .cols = -c(SEX_genetic, total), 
        .fns = ~ .x / total * 100,
        .names = "{.col}_percent"
      )
    ) %>%
    select(-total) %>%
    ungroup() %>%
    rename_with(
      ~ paste0(.x, "_count"),
      .cols = where(is.numeric) & !ends_with("_percent")
    )
  
  
  n_age %>%
    left_join(n_outcome, by = "SEX_genetic") %>% 
    left_join(n_predictor, by = "SEX_genetic") %>% 
    kable(caption = title)
}

summarise_data(NUMSYMPALL_baseline_cat, AGE_baseline, NDEXPOS_DEPRWAFTER, "Natural disaster experience, worst depressive episode")
summarise_data(NUMSYMPALL_followup_cat, AGE_baseline, NDEXPOS_DEPRFAFTER, "Natural disaster experience, first depressive episode")
summarise_data(NUMSYMPALL_covid1_cat, AGE_followup, BUSHFLOODHORR_followup, "Horrible 2019 bushfires/floods experience, current symptoms (1 year later)")
summarise_data(NUMSYMPALL_covid2_cat, AGE_covid2, COVIDALLBIN_C1C2, "Horrible covid experience, current symptoms")
summarise_data(NUMSYMPALL_covid2_cat, AGE_covid2, COVIDALL_BUSHFLOOD_both, "Horrible 2019 bushfires/floods and covid experience, current symptoms")
## ----

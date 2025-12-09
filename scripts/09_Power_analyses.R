# Determine minimum detectable OR in each of the proposed analyses

## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----

## ---- Power
# Function to calculate smallest detectable OR for a given outcome
calc_smallest_or <- function(data, outcome, predictor, age, title) {
  
  df_subset <- data %>% 
    drop_na({{outcome}}, SEX_genetic, {{age}}, matches("^PC.*_genetic$"), {{predictor}})
  
  alpha <- 0.05
  
  # Get outcome probabilities
  p <- df_subset %>%
    count({{outcome}}) %>%  
    mutate(prop = n / sum(n)) %>%       
    pull(prop)
  
  # Determine n1 and n2 from the predictor categories
  predictor_counts <- df_subset %>%
    group_by({{predictor}}) %>%
    summarise(n = n(), .groups = "drop")
  n1 <- as.numeric(predictor_counts$n[1])
  n2 <- as.numeric(predictor_counts$n[2])
  
  # Power function
  power_for_or <- function(or) {
    pop <- popower(p = p, 
                   odds.ratio = or, 
                   n1 = n1, 
                   n2 = n2, 
                   alpha = alpha)
    pop$power
  }
  
  results <- tibble(
    or = seq(1, 4, by = 0.0001)
  ) %>%
    mutate(power = map_dbl(or, power_for_or)) %>%
    filter(power > 0.8) %>%
    slice_min(or)    # take the smallest OR with power > 0.8
  
  results %>% 
    kable(caption = title)
  
}


calc_smallest_or(df_ND_covid, NUMSYMPALL_baseline_cat, NDEXPOS_DEPRWAFTER, AGE_baseline, "Natural disaster experience, worst depressive episode")
calc_smallest_or(df_ND_covid %>% filter(SEX_genetic == "Female"), NUMSYMPALL_baseline_cat, NDEXPOS_DEPRWAFTER, AGE_baseline, "Natural disaster experience, worst depressive episode (females only)")
calc_smallest_or(df_ND_covid %>% filter(SEX_genetic == "Male"), NUMSYMPALL_baseline_cat, NDEXPOS_DEPRWAFTER, AGE_baseline, "Natural disaster experience, worst depressive episode (males only)")


calc_smallest_or(df_ND_covid, NUMSYMPALL_followup_cat, NDEXPOS_DEPRFAFTER, AGE_baseline, "Natural disaster experience, first depressive episode")
calc_smallest_or(df_ND_covid %>% filter(SEX_genetic == "Female"), NUMSYMPALL_followup_cat, NDEXPOS_DEPRFAFTER, AGE_baseline, "Natural disaster experience, first depressive episode (females only)")
calc_smallest_or(df_ND_covid %>% filter(SEX_genetic == "Male"), NUMSYMPALL_followup_cat, NDEXPOS_DEPRFAFTER, AGE_baseline, "Natural disaster experience, first depressive episode (males only)")


calc_smallest_or(df_ND_covid, NUMSYMPALL_covid1_cat, BUSHFLOODHORR_followup, AGE_followup, "Horrible 2019 bushfires/floods experience, current symptoms (1 year later)")
calc_smallest_or(df_ND_covid %>% filter(SEX_genetic == "Female"), NUMSYMPALL_covid1_cat, BUSHFLOODHORR_followup, AGE_followup, "Horrible 2019 bushfires/floods experience, current symptoms (1 year later) (females only)")
calc_smallest_or(df_ND_covid %>% filter(SEX_genetic == "Male"), NUMSYMPALL_covid1_cat, BUSHFLOODHORR_followup, AGE_followup, "Horrible 2019 bushfires/floods experience, current symptoms (1 year later) (males only)")


calc_smallest_or(df_ND_covid, NUMSYMPALL_covid2_cat, COVIDALLBIN_C1C2, AGE_covid2, "Horrible covid experience, current symptoms")
calc_smallest_or(df_ND_covid %>% filter(SEX_genetic == "Female"), NUMSYMPALL_covid2_cat, COVIDALLBIN_C1C2, AGE_covid2, "Horrible covid experience, current symptoms (females only)")
calc_smallest_or(df_ND_covid %>% filter(SEX_genetic == "Male"), NUMSYMPALL_covid2_cat, COVIDALLBIN_C1C2, AGE_covid2, "Horrible covid experience, current symptoms (males only)")


calc_smallest_or(df_ND_covid, NUMSYMPALL_covid2_cat, COVIDALL_BUSHFLOOD_both, AGE_covid2, "Horrible 2019 bushfires/floods and covid experience, current symptoms")
calc_smallest_or(df_ND_covid %>% filter(SEX_genetic == "Female"), NUMSYMPALL_covid2_cat, COVIDALL_BUSHFLOOD_both, AGE_covid2, "Horrible 2019 bushfires/floods and covid experience, current symptoms (females only)")
calc_smallest_or(df_ND_covid %>% filter(SEX_genetic == "Male"), NUMSYMPALL_covid2_cat, COVIDALL_BUSHFLOOD_both, AGE_covid2, "Horrible 2019 bushfires/floods and covid experience, current symptoms (males only)")
## ----
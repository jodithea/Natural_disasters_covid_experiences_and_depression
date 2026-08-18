# Run sensitivity analyses including overall trauma burden (minus ND) as a covariate 


## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----


## ---- CreateTraumaBurden
# For each of 16 qs (17 items excluding natural disaster item): 1 = if happened to me / witness / learned/ part of my job, 0 = doesn't apply
# Sum so get 0 - 16 trauma burden
# LEC5EXA1 = natural disaster happened to me, LEC5EXA2 = witnessed it = LEC5EXA3 = learned about it, LEC5EXA4 = part of my job, LEC5EXA5 = not sure, LEC5EXA6 = doesn't apply
# LEAC5EXB1-6 = fire or explosion, etc until Q.


# Event letters B-Q (exclude natural disaster item A)
event_letters <- LETTERS[2:17]

# Create one binary variable per event
for (event in event_letters) {
  
  exposure_cols <- paste0("LEC5EX", event, 1:4, "_baseline")
  doesnt_apply_col <- paste0("LEC5EX", event, "6_baseline")
  
  df_ND_covid <- df_ND_covid %>%
    mutate(
      !!paste0("LEC_", event, "_BIN") :=
        case_when(
          # Any exposure endorsed
          rowSums(across(all_of(exposure_cols)) == 1,
                  na.rm = TRUE) > 0 ~ 1,
          
          # Doesn't apply endorsed
          .data[[doesnt_apply_col]] == 1 ~ 0,
          
          TRUE ~ NA_real_
        )
    )
}

# Sum across all non-disaster trauma types
df_ND_covid <- df_ND_covid %>%
  mutate(
    TRAUMA_BURDEN = rowSums(
      select(., starts_with("LEC_"), ends_with("_BIN")),
      na.rm = FALSE
    )
  )

# Checks
a <- df_ND_covid %>% 
  select(starts_with("LEC_"), ends_with("_BIN"), TRAUMA_BURDEN)

b <- df_ND_covid %>% 
  select(LEC5EXB1_baseline, LEC5EXB2_baseline, LEC5EXB3_baseline, LEC5EXB4_baseline, LEC5EXB5_baseline, LEC5EXB6_baseline, LEC_B_BIN) 

df_ND_covid %>% 
  count(TRAUMA_BURDEN)

save(df_ND_covid, file = "data/df_ND_covid.RData")
## ----


## ---- FitModelswithTraumaBurden
NDworse_PO_trauma <- VGAM::vglm(NUMSYMPALL_baseline_cat ~ 
                           PRS_stand_genetic +
                           NDEXPOS_DEPRWAFTER +
                           AGE_baseline +
                           SEX_genetic +
                           PC1_genetic +
                           PC2_genetic +
                           PC3_genetic +
                           PC4_genetic +
                           PC5_genetic +
                           PC6_genetic +
                           PC7_genetic +
                           PC8_genetic +
                           PC9_genetic + 
                           PC10_genetic + 
                             TRAUMA_BURDEN,
                         family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                         data = df_ND_covid)

NDFirst_PO_trauma <- VGAM::vglm(NUMSYMPALL_followup_cat ~ 
                           PRS_stand_genetic +
                           NDEXPOS_DEPRFAFTER +
                           AGE_baseline +
                           SEX_genetic +
                           PC1_genetic +
                           PC2_genetic +
                           PC3_genetic +
                           PC4_genetic +
                           PC5_genetic +
                           PC6_genetic +
                           PC7_genetic +
                           PC8_genetic +
                           PC9_genetic + 
                           PC10_genetic + 
                           TRAUMA_BURDEN,
                         family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                         data = df_ND_covid)

Twenty19BushFlood_PO_trauma <- VGAM::vglm(NUMSYMPALL_covid1_cat ~ 
                                     PRS_stand_genetic +
                                     BUSHFLOODHORR_followup +
                                     AGE_followup +
                                     SEX_genetic +
                                     PC1_genetic +
                                     PC2_genetic +
                                     PC3_genetic +
                                     PC4_genetic +
                                     PC5_genetic +
                                     PC6_genetic +
                                     PC7_genetic +
                                     PC8_genetic +
                                     PC9_genetic + 
                                     PC10_genetic + 
                                     TRAUMA_BURDEN,
                                   family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                   data = df_ND_covid)

Covid_PO_trauma <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                         PRS_stand_genetic +
                         COVIDALLBIN_C1C2 +
                         AGE_covid2 +
                         SEX_genetic +
                         PC1_genetic +
                         PC2_genetic +
                         PC3_genetic +
                         PC4_genetic +
                         PC5_genetic +
                         PC6_genetic +
                         PC7_genetic +
                         PC8_genetic +
                         PC9_genetic + 
                         PC10_genetic + 
                           TRAUMA_BURDEN,
                       family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                       data = df_ND_covid)

Twenty19BushFloodCovidBoth_PO_trauma <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                              PRS_stand_genetic +
                                              COVIDALL_BUSHFLOOD_both +
                                              AGE_covid2 +
                                              SEX_genetic +
                                              PC1_genetic +
                                              PC2_genetic +
                                              PC3_genetic +
                                              PC4_genetic +
                                              PC5_genetic +
                                              PC6_genetic +
                                              PC7_genetic +
                                              PC8_genetic +
                                              PC9_genetic + 
                                              PC10_genetic + 
                                              TRAUMA_BURDEN,
                                            family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                            data = df_ND_covid)

trauma_models <- ls(pattern = "*_trauma$")
save(list = trauma_models, file = "models/trauma_models.RData")
## ----

## ---- SummaryDataTraumaBurdenModels
# Function to process data
process_data <- function(model, model_name) {
  CI <- as.data.frame(exp(confint(model))) %>% 
    rownames_to_column() %>% 
    rename('lower_95_CI_of_OR' = "2.5 %",
           'upper_95_CI_of_OR' = "97.5 %")
  
  df <- as.data.frame(coef(summary(model))) %>% 
    rownames_to_column(var = 'rowname') %>% 
    mutate(OR = exp(Estimate)) %>% 
    rename('P' = 'Pr(>|z|)') %>% 
    mutate(Model = model_name)
  
  result <- df %>% 
    full_join(CI)
  
  return(result)
}

# Process data 
ND_worst <- process_data(NDworse_PO_trauma, "ND_worst")
ND_First <- process_data(NDFirst_PO_trauma, "ND_first")
Twenty19BushFlood <- process_data(Twenty19BushFlood_PO_trauma, "Twenty19BushFlood")
Covid <- process_data(Covid_PO_trauma, "Covid")
Twenty19BushFloodCovidBoth <- process_data(Twenty19BushFloodCovidBoth_PO_trauma, "Twenty19BushFloodCovidBoth")

# Combine the results
results_df <- bind_rows(ND_worst, ND_First, Twenty19BushFlood, Covid, Twenty19BushFloodCovidBoth)


# Subset df and put in order for graph
order <- c('NDEXPOS_DEPRWAFTERYes', 
           'NDEXPOS_DEPRFAFTERYes', 
           'BUSHFLOODHORR_followup1', 
           'COVIDALLBIN_C1C21', 
           'COVIDALL_BUSHFLOOD_bothYes')
order <- rev(order)

results_df <- results_df %>% 
  filter(rowname == 'NDEXPOS_DEPRWAFTERYes' | 
           rowname == 'NDEXPOS_DEPRFAFTERYes' | 
           rowname == 'BUSHFLOODHORR_followup1' | 
           rowname == 'COVIDALLBIN_C1C21' | 
           rowname == 'COVIDALL_BUSHFLOOD_bothYes') %>% 
  mutate(P_adjusted = p.adjust(P, method = "BH", n = 5)) %>%
  mutate(P_adjusted = ifelse(P_adjusted >= 0.009, sprintf("%.2f", P_adjusted), sprintf("%.0e", P_adjusted))) %>% 
  select(rowname, Model, OR, lower_95_CI_of_OR, upper_95_CI_of_OR, "z value", P, P_adjusted, Estimate, "Std. Error")

results_df %>%  kable()
## ----
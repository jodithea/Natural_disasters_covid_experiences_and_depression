# Run sensitivity analyses using linear models to check efefct of categorising no. depressive symptoms


## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----


## ---- FitLinearModels
NDworse_linear <- lm(NUMSYMPALL_baseline ~ 
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
                       PC10_genetic,
                     data = df_ND_covid)

NDFirst_linear <- lm(NUMSYMPALL_followup ~ 
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
                       PC10_genetic,
                     data = df_ND_covid)

Twenty19BushFlood_linear <- lm(NUMSYMPALL_covid1 ~ 
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
                                     PC10_genetic,
                                   data = df_ND_covid)


Covid_linear <- lm(NUMSYMPALL_covid2 ~ 
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
                         PC10_genetic,
                       data = df_ND_covid)


Twenty19BushFloodCovidBoth_linear <- lm(NUMSYMPALL_covid2 ~ 
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
                                              PC10_genetic,
                                            data = df_ND_covid)

linear_models <- ls(pattern = "*_linear$")
save(list = linear_models, file = "models/linear_models.RData")

## ----


## ---- SummaryDataLinearModels
ND_worst <- tidy(NDworse_linear, conf.int = T) %>% 
  mutate(Model = "ND_worst")

ND_first <- tidy(NDFirst_linear, conf.int = T) %>% 
  mutate(Model = "ND_first")

Twenty19BushFlood <- tidy(Twenty19BushFlood_linear, conf.int = T) %>% 
  mutate(Model = "Twenty19BushFlood")

Covid <- tidy(Covid_linear, conf.int = T) %>% 
  mutate(Model = "Covid")

Twenty19BushFloodCovidBoth <- tidy(Twenty19BushFloodCovidBoth_linear, conf.int = T) %>% 
  mutate(Model = "Twenty19BushFloodCovidBoth")

# Combine the results
NUMSYMPALL_linear_results_df <- bind_rows(ND_worst, 
                                      ND_first,
                                      Twenty19BushFlood, 
                                      Covid,
                                      Twenty19BushFloodCovidBoth)

NUMSYMPALL_linear_results_df %>% 
  filter(term == 'NDEXPOS_DEPRWAFTERYes' | 
           term == 'NDEXPOS_DEPRFAFTERYes' | 
           term == 'BUSHFLOODHORR_followup1' | 
           term == 'COVIDALLBIN_C1C21' | 
           term == 'COVIDALL_BUSHFLOOD_bothYes') %>% 
  mutate(P_adjusted = p.adjust(p.value, method = "BH", n = 5)) %>% 
  select(Model, term, estimate, conf.low, conf.high, statistic, p.value, P_adjusted) %>% 
  kable()


## ----


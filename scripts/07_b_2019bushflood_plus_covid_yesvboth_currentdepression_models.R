# Fit models: effect of 2019 bushfires/floods AND covid experience on no. depressive symptoms during current episode 

## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----

## ---- Twenty19BushFloodCovidBothFitModelsPO
Twenty19BushFloodCovidBoth_PO <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                                     PRS_stand_genetic +
                                                     COVIDALL_BUSHFLOOD_both +
                                                     AGE_covid2 +
                                                     SEX_genetic +
                                                     PC1_genetic +
                                                     PC2_genetic +
                                                     PC3_genetic +
                                                     PC4_genetic,
                                                   family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                   data = df_ND_covid)
                                                   
Twenty19BushFloodCovidBoth_female_PO <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                                 PRS_stand_genetic +
                                                 COVIDALL_BUSHFLOOD_both +
                                                 AGE_covid2 +
                                                 PC1_genetic +
                                                 PC2_genetic +
                                                 PC3_genetic +
                                                 PC4_genetic,
                                               family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                               data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

Twenty19BushFloodCovidBoth_male_PO <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                               PRS_stand_genetic +
                                               COVIDALL_BUSHFLOOD_both +
                                               AGE_covid2 +
                                               PC1_genetic +
                                               PC2_genetic +
                                               PC3_genetic +
                                               PC4_genetic,
                                             family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                             data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])
## ----

## ---- Twenty19BushFloodCovidBothFitModelsPartPO
Twenty19BushFloodCovidBoth_PartPO_NDexpos <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                                                 PRS_stand_genetic +
                                                                 COVIDALL_BUSHFLOOD_both +
                                                                 AGE_covid2 +
                                                                 SEX_genetic +
                                                                 PC1_genetic +
                                                                 PC2_genetic +
                                                                 PC3_genetic +
                                                                 PC4_genetic,
                                                               family = VGAM::cumulative(logitlink, parallel = F ~ COVIDALL_BUSHFLOOD_both, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                               data = df_ND_covid)

Twenty19BushFloodCovidBoth_PartPO_PRS <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                                             PRS_stand_genetic +
                                                             COVIDALL_BUSHFLOOD_both +
                                                             AGE_covid2 +
                                                             SEX_genetic +
                                                             PC1_genetic +
                                                             PC2_genetic +
                                                             PC3_genetic +
                                                             PC4_genetic,
                                                           family = VGAM::cumulative(logitlink, parallel = F ~ PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                           data = df_ND_covid)

Twenty19BushFloodCovidBoth_PartPO_NDexpos_PRS <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                                                     PRS_stand_genetic +
                                                                     COVIDALL_BUSHFLOOD_both +
                                                                     AGE_covid2 +
                                                                     SEX_genetic +
                                                                     PC1_genetic +
                                                                     PC2_genetic +
                                                                     PC3_genetic +
                                                                     PC4_genetic,
                                                                   family = VGAM::cumulative(logitlink, parallel = F ~ COVIDALL_BUSHFLOOD_both + PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                                   data = df_ND_covid)


Twenty19BushFloodCovidBoth_female_PartPO_NDexpos <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                                             PRS_stand_genetic +
                                                             COVIDALL_BUSHFLOOD_both +
                                                             AGE_covid2 +
                                                             PC1_genetic +
                                                             PC2_genetic +
                                                             PC3_genetic +
                                                             PC4_genetic,
                                                           family = VGAM::cumulative(logitlink, parallel = F ~ COVIDALL_BUSHFLOOD_both, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                           data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

Twenty19BushFloodCovidBoth_female_PartPO_PRS <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                                         PRS_stand_genetic +
                                                         COVIDALL_BUSHFLOOD_both +
                                                         AGE_covid2 +
                                                         PC1_genetic +
                                                         PC2_genetic +
                                                         PC3_genetic +
                                                         PC4_genetic,
                                                       family = VGAM::cumulative(logitlink, parallel = F ~ PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                       data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

Twenty19BushFloodCovidBoth_female_PartPO_NDexpos_PRS <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                                                 PRS_stand_genetic +
                                                                 COVIDALL_BUSHFLOOD_both +
                                                                 AGE_covid2 +
                                                                 PC1_genetic +
                                                                 PC2_genetic +
                                                                 PC3_genetic +
                                                                 PC4_genetic,
                                                               family = VGAM::cumulative(logitlink, parallel = F ~ COVIDALL_BUSHFLOOD_both + PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                               data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

Twenty19BushFloodCovidBoth_male_PartPO_NDexpos <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                                           PRS_stand_genetic +
                                                           COVIDALL_BUSHFLOOD_both +
                                                           AGE_covid2 +
                                                           PC1_genetic +
                                                           PC2_genetic +
                                                           PC3_genetic +
                                                           PC4_genetic,
                                                         family = VGAM::cumulative(logitlink, parallel = F ~ COVIDALL_BUSHFLOOD_both, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                         data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])

Twenty19BushFloodCovidBoth_male_PartPO_PRS <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                                       PRS_stand_genetic +
                                                       COVIDALL_BUSHFLOOD_both +
                                                       AGE_covid2 +
                                                       PC1_genetic +
                                                       PC2_genetic +
                                                       PC3_genetic +
                                                       PC4_genetic,
                                                     family = VGAM::cumulative(logitlink, parallel = F ~ PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                     data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])

Twenty19BushFloodCovidBoth_male_PartPO_NDexpos_PRS <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                                               PRS_stand_genetic +
                                                               COVIDALL_BUSHFLOOD_both +
                                                               AGE_covid2 +
                                                               PC1_genetic +
                                                               PC2_genetic +
                                                               PC3_genetic +
                                                               PC4_genetic,
                                                             family = VGAM::cumulative(logitlink, parallel = F ~ COVIDALL_BUSHFLOOD_both + PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                             data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])
## ----


## ---- Twenty19BushFloodCovidBothSaveModels
PO_models <- ls(pattern = "^Twenty19BushFloodCovidBoth.*_PO$")
save(list = PO_models, file = "models/Twenty19BushFloodCovidBoth_PO_models.RData")

PartPO_models <- ls(pattern = "^Twenty19BushFloodCovidBoth.*_PartPO.*")
save(list = PartPO_models, file = "models/Twenty19BushFloodCovidBoth_PartPO_models.RData")
## ----

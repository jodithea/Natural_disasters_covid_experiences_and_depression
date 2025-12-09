# Fit models: effect of Natural disaster experience on no. depressive symptoms if first depressive episode occurred after the Natural disaster

## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----

## ---- NDFirstFitModelsPO
NDFirst_PO <- VGAM::vglm(NUMSYMPALL_followup_cat ~ 
                                  PRS_stand_genetic +
                                  NDEXPOS_DEPRFAFTER +
                                  AGE_baseline +
                                  SEX_genetic +
                                  PC1_genetic +
                                  PC2_genetic +
                                  PC3_genetic +
                                  PC4_genetic,
                                family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                data = df_ND_covid)

NDFirst_female_PO <- VGAM::vglm(NUMSYMPALL_followup_cat ~ 
                                  PRS_stand_genetic +
                                  NDEXPOS_DEPRFAFTER +
                                  AGE_baseline +
                                  PC1_genetic +
                                  PC2_genetic +
                                  PC3_genetic +
                                  PC4_genetic,
                                family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

NDFirst_male_PO <- VGAM::vglm(NUMSYMPALL_followup_cat ~ 
                                PRS_stand_genetic +
                                NDEXPOS_DEPRFAFTER +
                                AGE_baseline +
                                PC1_genetic +
                                PC2_genetic +
                                PC3_genetic +
                                PC4_genetic,
                              family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                              data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])
## ----

## ---- NDFirstFitModelsPartPO
NDFirst_PartPO_NDexpos <- VGAM::vglm(NUMSYMPALL_followup_cat ~ 
                                             PRS_stand_genetic +
                                             NDEXPOS_DEPRFAFTER +
                                             AGE_baseline +
                                             SEX_genetic +
                                             PC1_genetic +
                                             PC2_genetic +
                                             PC3_genetic +
                                             PC4_genetic,
                                           family = VGAM::cumulative(logitlink, parallel = F ~ NDEXPOS_DEPRFAFTER, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                           data = df_ND_covid)

NDFirst_PartPO_PRS <- VGAM::vglm(NUMSYMPALL_followup_cat ~ 
                                          PRS_stand_genetic +
                                          NDEXPOS_DEPRFAFTER +
                                          AGE_baseline +
                                          SEX_genetic +
                                          PC1_genetic +
                                          PC2_genetic +
                                          PC3_genetic +
                                          PC4_genetic,
                                        family = VGAM::cumulative(logitlink, parallel = F ~ PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                        data = df_ND_covid)

NDFirst_PartPO_NDexpos_PRS <- VGAM::vglm(NUMSYMPALL_followup_cat ~ 
                                                  PRS_stand_genetic +
                                                  NDEXPOS_DEPRFAFTER +
                                                  AGE_baseline +
                                                  SEX_genetic +
                                                  PC1_genetic +
                                                  PC2_genetic +
                                                  PC3_genetic +
                                                  PC4_genetic,
                                                family = VGAM::cumulative(logitlink, parallel = F ~ NDEXPOS_DEPRFAFTER + PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                data = df_ND_covid)



NDFirst_female_PartPO_NDexpos <- VGAM::vglm(NUMSYMPALL_followup_cat ~ 
                                              PRS_stand_genetic +
                                              NDEXPOS_DEPRFAFTER +
                                              AGE_baseline +
                                              PC1_genetic +
                                              PC2_genetic +
                                              PC3_genetic +
                                              PC4_genetic,
                                            family = VGAM::cumulative(logitlink, parallel = F ~ NDEXPOS_DEPRFAFTER, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                            data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

NDFirst_female_PartPO_PRS <- VGAM::vglm(NUMSYMPALL_followup_cat ~ 
                                          PRS_stand_genetic +
                                          NDEXPOS_DEPRFAFTER +
                                          AGE_baseline +
                                          PC1_genetic +
                                          PC2_genetic +
                                          PC3_genetic +
                                          PC4_genetic,
                                        family = VGAM::cumulative(logitlink, parallel = F ~ PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                        data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

NDFirst_female_PartPO_NDexpos_PRS <- VGAM::vglm(NUMSYMPALL_followup_cat ~ 
                                                  PRS_stand_genetic +
                                                  NDEXPOS_DEPRFAFTER +
                                                  AGE_baseline +
                                                  PC1_genetic +
                                                  PC2_genetic +
                                                  PC3_genetic +
                                                  PC4_genetic,
                                                family = VGAM::cumulative(logitlink, parallel = F ~ NDEXPOS_DEPRFAFTER + PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

NDFirst_male_PartPO_NDexpos <- VGAM::vglm(NUMSYMPALL_followup_cat ~ 
                                            PRS_stand_genetic +
                                            NDEXPOS_DEPRFAFTER +
                                            AGE_baseline +
                                            PC1_genetic +
                                            PC2_genetic +
                                            PC3_genetic +
                                            PC4_genetic,
                                          family = VGAM::cumulative(logitlink, parallel = F ~ NDEXPOS_DEPRFAFTER, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                          data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])

NDFirst_male_PartPO_PRS <- VGAM::vglm(NUMSYMPALL_followup_cat ~ 
                                        PRS_stand_genetic +
                                        NDEXPOS_DEPRFAFTER +
                                        AGE_baseline +
                                        PC1_genetic +
                                        PC2_genetic +
                                        PC3_genetic +
                                        PC4_genetic,
                                      family = VGAM::cumulative(logitlink, parallel = F ~ PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                      data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])

NDFirst_male_PartPO_NDexpos_PRS <- VGAM::vglm(NUMSYMPALL_followup_cat ~ 
                                                PRS_stand_genetic +
                                                NDEXPOS_DEPRFAFTER +
                                                AGE_baseline +
                                                PC1_genetic +
                                                PC2_genetic +
                                                PC3_genetic +
                                                PC4_genetic,
                                              family = VGAM::cumulative(logitlink, parallel = F ~ NDEXPOS_DEPRFAFTER + PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                              data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])
## ----

## ---- NDFirstSaveModels
PO_models <- ls(pattern = "^NDFirst.*_PO$")
save(list = PO_models, file = "models/NDFirst_PO_models.RData")

PartPO_models <- ls(pattern = "^NDFirst.*_PartPO.*")
save(list = PartPO_models, file = "models/NDFirst_PartPO_models.RData")
## ----

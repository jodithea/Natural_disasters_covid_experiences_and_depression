# Fit models: effect of Natural disaster experience on no. depressive symptoms if worst depressive episode occurred after the Natural disaster

## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----

## ---- NDWorseFitModelsPO
NDworse_PO <- VGAM::vglm(NUMSYMPALL_baseline_cat ~ 
                                  PRS_stand_genetic +
                                  NDEXPOS_DEPRWAFTER +
                                  AGE_baseline +
                                  SEX_genetic +
                                  PC1_genetic +
                                  PC2_genetic +
                                  PC3_genetic +
                                  PC4_genetic,
                                family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                data = df_ND_covid)

NDworse_female_PO <- VGAM::vglm(NUMSYMPALL_baseline_cat ~ 
                PRS_stand_genetic +
                NDEXPOS_DEPRWAFTER +
                AGE_baseline +
                PC1_genetic +
                PC2_genetic +
                PC3_genetic +
                PC4_genetic,
              family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
              data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

NDworse_male_PO <- VGAM::vglm(NUMSYMPALL_baseline_cat ~ 
                               PRS_stand_genetic +
                               NDEXPOS_DEPRWAFTER +
                               AGE_baseline +
                               PC1_genetic +
                               PC2_genetic +
                               PC3_genetic +
                               PC4_genetic,
                             family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                             data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])
## ----

## ---- NDWorseFitModelsPartPO
NDworse_PartPO_NDexpos <- VGAM::vglm(NUMSYMPALL_baseline_cat ~ 
                                              PRS_stand_genetic +
                                              NDEXPOS_DEPRWAFTER +
                                              AGE_baseline +
                                              SEX_genetic +
                                              PC1_genetic +
                                              PC2_genetic +
                                              PC3_genetic +
                                              PC4_genetic,
                                            family = VGAM::cumulative(logitlink, parallel = F ~ NDEXPOS_DEPRWAFTER, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                            data = df_ND_covid)

NDworse_PartPO_PRS <- VGAM::vglm(NUMSYMPALL_baseline_cat ~ 
                                          PRS_stand_genetic +
                                          NDEXPOS_DEPRWAFTER +
                                          AGE_baseline +
                                          SEX_genetic +
                                          PC1_genetic +
                                          PC2_genetic +
                                          PC3_genetic +
                                          PC4_genetic,
                                        family = VGAM::cumulative(logitlink, parallel = F ~ PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                        data = df_ND_covid)

NDworse_PartPO_NDexpos_PRS <- VGAM::vglm(NUMSYMPALL_baseline_cat ~ 
                                                  PRS_stand_genetic +
                                                  NDEXPOS_DEPRWAFTER +
                                                  AGE_baseline +
                                                  SEX_genetic +
                                                  PC1_genetic +
                                                  PC2_genetic +
                                                  PC3_genetic +
                                                  PC4_genetic,
                                                family = VGAM::cumulative(logitlink, parallel = F ~ NDEXPOS_DEPRWAFTER + PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                data = df_ND_covid)

NDworse_female_PartPO_NDexpos <- VGAM::vglm(NUMSYMPALL_baseline_cat ~ 
                                            PRS_stand_genetic +
                                            NDEXPOS_DEPRWAFTER +
                                            AGE_baseline +
                                            PC1_genetic +
                                            PC2_genetic +
                                            PC3_genetic +
                                            PC4_genetic,
                                          family = VGAM::cumulative(logitlink, parallel = F ~ NDEXPOS_DEPRWAFTER, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                          data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

NDworse_female_PartPO_PRS <- VGAM::vglm(NUMSYMPALL_baseline_cat ~ 
                                        PRS_stand_genetic +
                                        NDEXPOS_DEPRWAFTER +
                                        AGE_baseline +
                                        PC1_genetic +
                                        PC2_genetic +
                                        PC3_genetic +
                                        PC4_genetic,
                                      family = VGAM::cumulative(logitlink, parallel = F ~ PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                      data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

NDworse_female_PartPO_NDexpos_PRS <- VGAM::vglm(NUMSYMPALL_baseline_cat ~ 
                                                PRS_stand_genetic +
                                                NDEXPOS_DEPRWAFTER +
                                                AGE_baseline +
                                                PC1_genetic +
                                                PC2_genetic +
                                                PC3_genetic +
                                                PC4_genetic,
                                              family = VGAM::cumulative(logitlink, parallel = F ~ NDEXPOS_DEPRWAFTER + PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                              data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

NDworse_male_PartPO_NDexpos <- VGAM::vglm(NUMSYMPALL_baseline_cat ~ 
                                PRS_stand_genetic +
                                NDEXPOS_DEPRWAFTER +
                                AGE_baseline +
                                PC1_genetic +
                                PC2_genetic +
                                PC3_genetic +
                                PC4_genetic,
                              family = VGAM::cumulative(logitlink, parallel = F ~ NDEXPOS_DEPRWAFTER, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                              data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])

NDworse_male_PartPO_PRS <- VGAM::vglm(NUMSYMPALL_baseline_cat ~ 
                                            PRS_stand_genetic +
                                            NDEXPOS_DEPRWAFTER +
                                            AGE_baseline +
                                            PC1_genetic +
                                            PC2_genetic +
                                            PC3_genetic +
                                            PC4_genetic,
                                          family = VGAM::cumulative(logitlink, parallel = F ~ PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                          data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])

NDworse_male_PartPO_NDexpos_PRS <- VGAM::vglm(NUMSYMPALL_baseline_cat ~ 
                                            PRS_stand_genetic +
                                            NDEXPOS_DEPRWAFTER +
                                            AGE_baseline +
                                            PC1_genetic +
                                            PC2_genetic +
                                            PC3_genetic +
                                            PC4_genetic,
                                          family = VGAM::cumulative(logitlink, parallel = F ~ NDEXPOS_DEPRWAFTER + PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                          data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])
## ----

## ---- NDWorseSaveModels
PO_models <- ls(pattern = "^NDworse.*_PO$")
save(list = PO_models, file = "models/NDworse_PO_models.RData")

PartPO_models <- ls(pattern = "^NDworse.*_PartPO.*")
save(list = PartPO_models, file = "models/NDworse_PartPO_models.RData")
## ----

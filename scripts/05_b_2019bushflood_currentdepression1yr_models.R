# Fit models: Effect of 2019 bushfires/floods experience on no. depressive symptoms during current episode measured 1 year later

## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----

## ---- Twenty19BushFloodFitModelsPO
Twenty19BushFlood_PO <- VGAM::vglm(NUMSYMPALL_covid1_cat ~ 
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
                                          family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                          data = df_ND_covid)

Twenty19BushFlood_female_PO <- VGAM::vglm(NUMSYMPALL_covid1_cat ~ 
                                  PRS_stand_genetic +
                                  BUSHFLOODHORR_followup +
                                  AGE_followup +
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
                                family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

Twenty19BushFlood_male_PO <- VGAM::vglm(NUMSYMPALL_covid1_cat ~ 
                                PRS_stand_genetic +
                                BUSHFLOODHORR_followup +
                                AGE_followup +
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
                              family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                              data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])
## ----

## ---- Twenty19BushFloodFitModelsPartPO
Twenty19BushFlood_PartPO_NDexpos <- VGAM::vglm(NUMSYMPALL_covid1_cat ~ 
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
                                                      family = VGAM::cumulative(logitlink, parallel = F ~ BUSHFLOODHORR_followup, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                      data = df_ND_covid)

Twenty19BushFlood_PartPO_PRS <- VGAM::vglm(NUMSYMPALL_covid1_cat ~ 
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
                                                  family = VGAM::cumulative(logitlink, parallel = F ~ PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                  data = df_ND_covid)

Twenty19BushFlood_PartPO_NDexpos_PRS <- VGAM::vglm(NUMSYMPALL_covid1_cat ~ 
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
                                                          family = VGAM::cumulative(logitlink, parallel = F ~ BUSHFLOODHORR_followup + PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                          data = df_ND_covid)

Twenty19BushFlood_female_PartPO_NDexpos <- VGAM::vglm(NUMSYMPALL_covid1_cat ~ 
                                              PRS_stand_genetic +
                                              BUSHFLOODHORR_followup +
                                              AGE_followup +
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
                                            family = VGAM::cumulative(logitlink, parallel = F ~ BUSHFLOODHORR_followup, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                            data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

Twenty19BushFlood_female_PartPO_PRS <- VGAM::vglm(NUMSYMPALL_covid1_cat ~ 
                                          PRS_stand_genetic +
                                          BUSHFLOODHORR_followup +
                                          AGE_followup +
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
                                        family = VGAM::cumulative(logitlink, parallel = F ~ PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                        data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

Twenty19BushFlood_female_PartPO_NDexpos_PRS <- VGAM::vglm(NUMSYMPALL_covid1_cat ~ 
                                                  PRS_stand_genetic +
                                                  BUSHFLOODHORR_followup +
                                                  AGE_followup +
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
                                                family = VGAM::cumulative(logitlink, parallel = F ~ BUSHFLOODHORR_followup + PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

Twenty19BushFlood_male_PartPO_NDexpos <- VGAM::vglm(NUMSYMPALL_covid1_cat ~ 
                                            PRS_stand_genetic +
                                            BUSHFLOODHORR_followup +
                                            AGE_followup +
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
                                          family = VGAM::cumulative(logitlink, parallel = F ~ BUSHFLOODHORR_followup, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                          data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])

Twenty19BushFlood_male_PartPO_PRS <- VGAM::vglm(NUMSYMPALL_covid1_cat ~ 
                                        PRS_stand_genetic +
                                        BUSHFLOODHORR_followup +
                                        AGE_followup +
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
                                      family = VGAM::cumulative(logitlink, parallel = F ~ PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                      data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])

Twenty19BushFlood_male_PartPO_NDexpos_PRS <- VGAM::vglm(NUMSYMPALL_covid1_cat ~ 
                                                PRS_stand_genetic +
                                                BUSHFLOODHORR_followup +
                                                AGE_followup +
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
                                              family = VGAM::cumulative(logitlink, parallel = F ~ BUSHFLOODHORR_followup + PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                              data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])
## ----


## ---- Twenty19BushFloodSaveModels
PO_models <- ls(pattern = "^Twenty19BushFlood.*_PO$")
save(list = PO_models, file = "models/Twenty19BushFlood_PO_models.RData")

PartPO_models <- ls(pattern = "^Twenty19BushFlood.*_PartPO.*")
save(list = PartPO_models, file = "models/Twenty19BushFlood_PartPO_models.RData")

## ----
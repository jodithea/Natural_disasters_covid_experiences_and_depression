# Fit models: effect of covid experience on no. depressive symptoms during current episode 

## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----

## ---- CovidFitModelsPO
Covid_PO <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                PRS_stand_genetic +
                                COVIDALLBIN_C1C2 +
                                AGE_covid2 +
                                SEX_genetic +
                                PC1_genetic +
                                PC2_genetic +
                                PC3_genetic +
                                PC4_genetic,
                              family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                              data = df_ND_covid)


Covid_female_PO <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                            PRS_stand_genetic +
                                            COVIDALLBIN_C1C2 +
                                            AGE_covid2 +
                                            PC1_genetic +
                                            PC2_genetic +
                                            PC3_genetic +
                                            PC4_genetic,
                                          family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                          data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

Covid_male_PO <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                          PRS_stand_genetic +
                                          COVIDALLBIN_C1C2 +
                                          AGE_covid2 +
                                          PC1_genetic +
                                          PC2_genetic +
                                          PC3_genetic +
                                          PC4_genetic,
                                        family = VGAM::cumulative(logitlink, parallel = T, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                        data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])
## ----

## ---- CovidFitModelsPartPO
Covid_PartPO_NDexpos <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                            PRS_stand_genetic +
                                            COVIDALLBIN_C1C2 +
                                            AGE_covid2 +
                                            SEX_genetic +
                                            PC1_genetic +
                                            PC2_genetic +
                                            PC3_genetic +
                                            PC4_genetic,
                                          family = VGAM::cumulative(logitlink, parallel = F ~ COVIDALLBIN_C1C2, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                          data = df_ND_covid)

Covid_PartPO_PRS <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                        PRS_stand_genetic +
                                        COVIDALLBIN_C1C2 +
                                        AGE_covid2 +
                                        SEX_genetic +
                                        PC1_genetic +
                                        PC2_genetic +
                                        PC3_genetic +
                                        PC4_genetic,
                                      family = VGAM::cumulative(logitlink, parallel = F ~ PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                      data = df_ND_covid)

Covid_PartPO_NDexpos_PRS <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                                PRS_stand_genetic +
                                                COVIDALLBIN_C1C2 +
                                                AGE_covid2 +
                                                SEX_genetic +
                                                PC1_genetic +
                                                PC2_genetic +
                                                PC3_genetic +
                                                PC4_genetic,
                                              family = VGAM::cumulative(logitlink, parallel = F ~ COVIDALLBIN_C1C2 + PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                              data = df_ND_covid)


Covid_female_PartPO_NDexpos <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                                        PRS_stand_genetic +
                                                        COVIDALLBIN_C1C2 +
                                                        AGE_covid2 +
                                                        PC1_genetic +
                                                        PC2_genetic +
                                                        PC3_genetic +
                                                        PC4_genetic,
                                                      family = VGAM::cumulative(logitlink, parallel = F ~ COVIDALLBIN_C1C2, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                      data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

Covid_female_PartPO_PRS <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                                    PRS_stand_genetic +
                                                    COVIDALLBIN_C1C2 +
                                                    AGE_covid2 +
                                                    PC1_genetic +
                                                    PC2_genetic +
                                                    PC3_genetic +
                                                    PC4_genetic,
                                                  family = VGAM::cumulative(logitlink, parallel = F ~ PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                  data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

Covid_female_PartPO_NDexpos_PRS <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                                            PRS_stand_genetic +
                                                            COVIDALLBIN_C1C2 +
                                                            AGE_covid2 +
                                                            PC1_genetic +
                                                            PC2_genetic +
                                                            PC3_genetic +
                                                            PC4_genetic,
                                                          family = VGAM::cumulative(logitlink, parallel = F ~ COVIDALLBIN_C1C2 + PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                          data = df_ND_covid[df_ND_covid$SEX_genetic == "Female", ])

Covid_male_PartPO_NDexpos <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                                      PRS_stand_genetic +
                                                      COVIDALLBIN_C1C2 +
                                                      AGE_covid2 +
                                                      PC1_genetic +
                                                      PC2_genetic +
                                                      PC3_genetic +
                                                      PC4_genetic,
                                                    family = VGAM::cumulative(logitlink, parallel = F ~ COVIDALLBIN_C1C2, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                    data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])

Covid_male_PartPO_PRS <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                                  PRS_stand_genetic +
                                                  COVIDALLBIN_C1C2 +
                                                  AGE_covid2 +
                                                  PC1_genetic +
                                                  PC2_genetic +
                                                  PC3_genetic +
                                                  PC4_genetic,
                                                family = VGAM::cumulative(logitlink, parallel = F ~ PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])

Covid_male_PartPO_NDexpos_PRS <- VGAM::vglm(NUMSYMPALL_covid2_cat ~ 
                                                          PRS_stand_genetic +
                                                          COVIDALLBIN_C1C2 +
                                                          AGE_covid2 +
                                                          PC1_genetic +
                                                          PC2_genetic +
                                                          PC3_genetic +
                                                          PC4_genetic,
                                                        family = VGAM::cumulative(logitlink, parallel = F ~ COVIDALLBIN_C1C2 + PRS_stand_genetic, reverse = T), # with PO (package pammtools masks VGAM cumulative so specify from VGAM)
                                                        data = df_ND_covid[df_ND_covid$SEX_genetic == "Male", ])
## ----


## ---- CovidSaveModels
PO_models <- ls(pattern = "^Covid.*_PO$")
save(list = PO_models, file = "models/Covid_PO_models.RData")

PartPO_models <- ls(pattern = "^Covid.*_PartPO.*")
save(list = PartPO_models, file = "models/Covid_PartPO_models.RData")
## ----

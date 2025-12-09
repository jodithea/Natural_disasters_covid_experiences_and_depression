# Check models: Effect of 2019 bushfires/floods experience on no. depressive symptoms during current episode measured 1 year later

## ---- LoadData
load(file = "data/df_ND_covid.RData")

load(file = "models/Twenty19BushFlood_PO_models.RData")
load(file = "models/Twenty19BushFlood_PartPO_models.RData")
## ----

## ---- Twenty19BushFloodCheckModelsPOBothSexes
### Test goodness of fit ###

# lipsitz test  
lipsitz(Twenty19BushFlood_PO)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
pulkroben(Twenty19BushFlood_PO, test = c("chisq","deviance"), tables = FALSE)

# Hosmer-Lemeshow test 
hosmerlem(Twenty19BushFlood_PO)


### Test proportional odds assumption ###

# Brant Wald Test
# A generalized ordinal logistic regression model is approximated and compared to the calculated proportional odds model (a generalized ordinal logistic regression model is simply a relaxing of the proportional odds model to allow for different coefficients at each level of the ordinal outcome variable.)
# A Wald test is a hypothesis test of the significance of the difference in model coefficients, producing a chi-square statistic. A low p-value in a Brant-Wald test is an indicator that the coefficient does not satisfy the proportional odds assumption. 
# A p-value of less than 0.05 on this test, particularly on the Omnibus plus at least one of the variables should be interpreted as a failure of the proportional odds assumption.

brant.test(Twenty19BushFlood_PO)

# Sighting coefficients of stratified binomial models - want each to be similar
# create binary variables for every combo of the 3 categories
x <- df_ND_covid %>% 
  mutate(SYMPzeroplus = case_when(
    NUMSYMPALL_covid1_cat == 'zero' ~ 0,
    NUMSYMPALL_covid1_cat == 'low' | NUMSYMPALL_covid1_cat == 'med' | NUMSYMPALL_covid1_cat == 'high' ~ 1
  ),
  SYMPlowplus = case_when(
    NUMSYMPALL_covid1_cat == 'zero' | NUMSYMPALL_covid1_cat == 'low' ~ 0,
    NUMSYMPALL_covid1_cat == 'medium' |  NUMSYMPALL_covid1_cat == 'high' ~ 1),
  SYMPmedplus = case_when(
    NUMSYMPALL_covid1_cat == 'zero' | NUMSYMPALL_covid1_cat == 'low' | NUMSYMPALL_covid1_cat == 'medium' ~ 0,
    NUMSYMPALL_covid1_cat == 'high' ~ 1)
  )


# Create logistic regression for each
zeroplus_model <- glm(SYMPzeroplus ~ 
                        PRS_stand_genetic +
                        BUSHFLOODHORR_followup +
                        AGE_followup +
                        SEX_genetic +
                        PC1_genetic +
                        PC2_genetic +
                        PC3_genetic +
                        PC4_genetic,
                      family = binomial(link = 'logit'),
                      data = x)

lowplus_model <- glm(SYMPlowplus ~ 
                       PRS_stand_genetic +
                       BUSHFLOODHORR_followup +
                       AGE_followup +
                       SEX_genetic +
                       PC1_genetic +
                       PC2_genetic +
                       PC3_genetic +
                       PC4_genetic,
                     family = binomial(link = 'logit'),
                     data = x)

medplus_model <- glm(SYMPmedplus ~ 
                       PRS_stand_genetic +
                       BUSHFLOODHORR_followup +
                       AGE_followup +
                       SEX_genetic +
                       PC1_genetic +
                       PC2_genetic +
                       PC3_genetic +
                       PC4_genetic,
                     family = binomial(link = 'logit'),
                     data = x)


# display the coefficients of all models and examine the difference between them.
# coefficients (i.e. the slopes) should be similar for all models to hold the proportional odds assumption

# table
coeffs <- data.frame(
  'ordinal' = coef(Twenty19BushFlood_PO)[-c(1,2,3)],
  'binary zero vs low and med and high' = lowplus_model$coefficients[-1],
  'binary zero and low vs med and high' = lowplus_model$coefficients[-1],
  'binary zero and low and med vs high' = medplus_model$coefficients[-1])

coeffs %>% 
  kable()

coeffs2 <- coeffs %>% 
  rownames_to_column() %>% 
  pivot_longer(cols = 2:5)

ggplot(coeffs2, aes(x = value, y = rowname, group = name, colour = name)) +
  geom_jitter(height = 0.2) +
  labs(x = "coefficient", y = "predictors")

coeffs3 <- coeffs2 %>% 
  filter(rowname == 'PRS_stand_genetic' | rowname == 'BUSHFLOODHORR_followup1' | rowname == "AGE_followup")

ggplot(coeffs3, aes(x = value, y = rowname, group = name, colour = name)) +
  geom_jitter(height = 0.2) +
  labs(x = "coefficient", y = "predictors")
## ----


## ---- Twenty19BushFloodCheckModelsPOFemale
### Test goodness of fit ###

# lipsitz test  
lipsitz(Twenty19BushFlood_female_PO)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
pulkroben(Twenty19BushFlood_female_PO, test = c("chisq","deviance"), tables = FALSE)

# Hosmer-Lemeshow test 
hosmerlem(Twenty19BushFlood_female_PO)


### Test proportional odds assumption ###

# Brant Wald Test
# A generalized ordinal logistic regression model is approximated and compared to the calculated proportional odds model (a generalized ordinal logistic regression model is simply a relaxing of the proportional odds model to allow for different coefficients at each level of the ordinal outcome variable.)
# A Wald test is a hypothesis test of the significance of the difference in model coefficients, producing a chi-square statistic. A low p-value in a Brant-Wald test is an indicator that the coefficient does not satisfy the proportional odds assumption. 
# A p-value of less than 0.05 on this test, particularly on the Omnibus plus at least one of the variables should be interpreted as a failure of the proportional odds assumption.

brant.test(Twenty19BushFlood_female_PO)

# Sighting coefficients of stratified binomial models - want each to be similar
# create binary variables for every combo of the 3 categories
x <- df_ND_covid %>% 
  mutate(SYMPzeroplus = case_when(
    NUMSYMPALL_covid1_cat == 'zero' ~ 0,
    NUMSYMPALL_covid1_cat == 'low' | NUMSYMPALL_covid1_cat == 'med' | NUMSYMPALL_covid1_cat == 'high' ~ 1
  ),
    SYMPlowplus = case_when(
    NUMSYMPALL_covid1_cat == 'zero' | NUMSYMPALL_covid1_cat == 'low' ~ 0,
    NUMSYMPALL_covid1_cat == 'medium' |  NUMSYMPALL_covid1_cat == 'high' ~ 1),
    SYMPmedplus = case_when(
      NUMSYMPALL_covid1_cat == 'zero' | NUMSYMPALL_covid1_cat == 'low' | NUMSYMPALL_covid1_cat == 'medium' ~ 0,
      NUMSYMPALL_covid1_cat == 'high' ~ 1)
  )


# Create logistic regression for each
zeroplus_model <- glm(SYMPzeroplus ~ 
                       PRS_stand_genetic +
                       BUSHFLOODHORR_followup +
                       AGE_followup +
                       PC1_genetic +
                       PC2_genetic +
                       PC3_genetic +
                       PC4_genetic,
                     family = binomial(link = 'logit'),
                     data = x[x$SEX_genetic == "Female", ])

lowplus_model <- glm(SYMPlowplus ~ 
                       PRS_stand_genetic +
                       BUSHFLOODHORR_followup +
                       AGE_followup +
                       PC1_genetic +
                       PC2_genetic +
                       PC3_genetic +
                       PC4_genetic,
                     family = binomial(link = 'logit'),
                     data = x[x$SEX_genetic == "Female", ])

medplus_model <- glm(SYMPmedplus ~ 
                       PRS_stand_genetic +
                       BUSHFLOODHORR_followup +
                       AGE_followup +
                       PC1_genetic +
                       PC2_genetic +
                       PC3_genetic +
                       PC4_genetic,
                     family = binomial(link = 'logit'),
                     data = x[x$SEX_genetic == "Female", ])


# display the coefficients of all models and examine the difference between them.
# coefficients (i.e. the slopes) should be similar for all models to hold the proportional odds assumption

# table
coeffs <- data.frame(
  'ordinal' = coef(Twenty19BushFlood_female_PO)[-c(1,2,3)],
  'binary zero vs low and med and high' = lowplus_model$coefficients[-1],
  'binary zero and low vs med and high' = lowplus_model$coefficients[-1],
  'binary zero and low and med vs high' = medplus_model$coefficients[-1])

coeffs %>% 
  kable()

coeffs2 <- coeffs %>% 
  rownames_to_column() %>% 
  pivot_longer(cols = 2:5)

ggplot(coeffs2, aes(x = value, y = rowname, group = name, colour = name)) +
  geom_jitter(height = 0.2) +
  labs(x = "coefficient", y = "predictors")

coeffs3 <- coeffs2 %>% 
  filter(rowname == 'PRS_stand_genetic' | rowname == 'BUSHFLOODHORR_followup1' | rowname == "AGE_followup")

ggplot(coeffs3, aes(x = value, y = rowname, group = name, colour = name)) +
  geom_jitter(height = 0.2) +
  labs(x = "coefficient", y = "predictors")
## ----

## ---- Twenty19BushFloodCheckModelsPOMale
### Test goodness of fit ###

# lipsitz test  
lipsitz(Twenty19BushFlood_male_PO)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
pulkroben(Twenty19BushFlood_male_PO, test = c("chisq","deviance"), tables = FALSE)

# Hosmer-Lemeshow test 
hosmerlem(Twenty19BushFlood_male_PO)


### Test proportional odds assumption ###

# Brant Wald Test
# A generalized ordinal logistic regression model is approximated and compared to the calculated proportional odds model (a generalized ordinal logistic regression model is simply a relaxing of the proportional odds model to allow for different coefficients at each level of the ordinal outcome variable.)
# A Wald test is a hypothesis test of the significance of the difference in model coefficients, producing a chi-square statistic. A low p-value in a Brant-Wald test is an indicator that the coefficient does not satisfy the proportional odds assumption. 
# A p-value of less than 0.05 on this test, particularly on the Omnibus plus at least one of the variables should be interpreted as a failure of the proportional odds assumption.

brant.test(Twenty19BushFlood_male_PO)

# Sighting coefficients of stratified binomial models - want each to be similar
# create binary variables for every combo of the 3 categories
x <- df_ND_covid %>% 
  mutate(SYMPzeroplus = case_when(
    NUMSYMPALL_covid1_cat == 'zero' ~ 0,
    NUMSYMPALL_covid1_cat == 'low' | NUMSYMPALL_covid1_cat == 'med' | NUMSYMPALL_covid1_cat == 'high' ~ 1
  ),
  SYMPlowplus = case_when(
    NUMSYMPALL_covid1_cat == 'zero' | NUMSYMPALL_covid1_cat == 'low' ~ 0,
    NUMSYMPALL_covid1_cat == 'medium' |  NUMSYMPALL_covid1_cat == 'high' ~ 1),
  SYMPmedplus = case_when(
    NUMSYMPALL_covid1_cat == 'zero' | NUMSYMPALL_covid1_cat == 'low' | NUMSYMPALL_covid1_cat == 'medium' ~ 0,
    NUMSYMPALL_covid1_cat == 'high' ~ 1)
  )


# Create logistic regression for each
zeroplus_model <- glm(SYMPzeroplus ~ 
                        PRS_stand_genetic +
                        BUSHFLOODHORR_followup +
                        AGE_followup +
                        PC1_genetic +
                        PC2_genetic +
                        PC3_genetic +
                        PC4_genetic,
                      family = binomial(link = 'logit'),
                      data = x[x$SEX_genetic == "Male", ])

lowplus_model <- glm(SYMPlowplus ~ 
                       PRS_stand_genetic +
                       BUSHFLOODHORR_followup +
                       AGE_followup +
                       PC1_genetic +
                       PC2_genetic +
                       PC3_genetic +
                       PC4_genetic,
                     family = binomial(link = 'logit'),
                     data = x[x$SEX_genetic == "Male", ])

medplus_model <- glm(SYMPmedplus ~ 
                       PRS_stand_genetic +
                       BUSHFLOODHORR_followup +
                       AGE_followup +
                       PC1_genetic +
                       PC2_genetic +
                       PC3_genetic +
                       PC4_genetic,
                     family = binomial(link = 'logit'),
                     data = x[x$SEX_genetic == "Male", ])


# display the coefficients of all models and examine the difference between them.
# coefficients (i.e. the slopes) should be similar for all models to hold the proportional odds assumption

# table
coeffs <- data.frame(
  'ordinal' = coef(Twenty19BushFlood_male_PO)[-c(1,2,3)],
  'binary zero vs low and med and high' = lowplus_model$coefficients[-1],
  'binary zero and low vs med and high' = lowplus_model$coefficients[-1],
  'binary zero and low and med vs high' = medplus_model$coefficients[-1])

coeffs %>% 
  kable()

coeffs2 <- coeffs %>% 
  rownames_to_column() %>% 
  pivot_longer(cols = 2:5)

ggplot(coeffs2, aes(x = value, y = rowname, group = name, colour = name)) +
  geom_jitter(height = 0.2) +
  labs(x = "coefficient", y = "predictors")

coeffs3 <- coeffs2 %>% 
  filter(rowname == 'PRS_stand_genetic' | rowname == 'BUSHFLOODHORR_followup1' | rowname == "AGE_followup")

ggplot(coeffs3, aes(x = value, y = rowname, group = name, colour = name)) +
  geom_jitter(height = 0.2) +
  labs(x = "coefficient", y = "predictors")
## ----

## ---- Twenty19BushFloodCheckModelsPartPOBothSexes
# LRT to compare partial PO to PO models
lrtest(Twenty19BushFlood_PO, Twenty19BushFlood_PartPO_NDexpos)
lrtest(Twenty19BushFlood_PO, Twenty19BushFlood_PartPO_PRS)
lrtest(Twenty19BushFlood_PO, Twenty19BushFlood_PartPO_NDexpos_PRS)


### Test goodness of fit ###

# lipsitz test  
lipsitz(Twenty19BushFlood_PartPO_NDexpos)
lipsitz(Twenty19BushFlood_PartPO_PRS)
lipsitz(Twenty19BushFlood_PartPO_NDexpos_PRS)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
pulkroben(Twenty19BushFlood_PartPO_NDexpos, test = c("chisq","deviance"), tables = FALSE)
pulkroben(Twenty19BushFlood_PartPO_PRS, test = c("chisq","deviance"), tables = FALSE)
pulkroben(Twenty19BushFlood_PartPO_NDexpos_PRS, test = c("chisq","deviance"), tables = FALSE)

# Hosmer-Lemeshow test 
hosmerlem(Twenty19BushFlood_PartPO_NDexpos)
hosmerlem(Twenty19BushFlood_PartPO_PRS)
hosmerlem(Twenty19BushFlood_PartPO_PRS)
## ----

## ---- Twenty19BushFloodCheckModelsPartPOFemale
# LRT to compare partial PO to PO models
lrtest(Twenty19BushFlood_female_PO, Twenty19BushFlood_female_PartPO_NDexpos)
lrtest(Twenty19BushFlood_female_PO, Twenty19BushFlood_female_PartPO_PRS)
lrtest(Twenty19BushFlood_female_PO, Twenty19BushFlood_female_PartPO_NDexpos_PRS)


### Test goodness of fit ###

# lipsitz test  
lipsitz(Twenty19BushFlood_female_PartPO_NDexpos)
lipsitz(Twenty19BushFlood_female_PartPO_PRS)
lipsitz(Twenty19BushFlood_female_PartPO_NDexpos_PRS)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
pulkroben(Twenty19BushFlood_female_PartPO_NDexpos, test = c("chisq","deviance"), tables = FALSE)
pulkroben(Twenty19BushFlood_female_PartPO_PRS, test = c("chisq","deviance"), tables = FALSE)
pulkroben(Twenty19BushFlood_female_PartPO_NDexpos_PRS, test = c("chisq","deviance"), tables = FALSE)

# Hosmer-Lemeshow test 
hosmerlem(Twenty19BushFlood_female_PartPO_NDexpos)
hosmerlem(Twenty19BushFlood_female_PartPO_PRS)
hosmerlem(Twenty19BushFlood_female_PartPO_PRS)
## ----

## ---- Twenty19BushFloodCheckModelsPartPOMale
# LRT to compare partial PO to PO models
lrtest(Twenty19BushFlood_male_PO, Twenty19BushFlood_male_PartPO_NDexpos)
lrtest(Twenty19BushFlood_male_PO, Twenty19BushFlood_male_PartPO_PRS)
lrtest(Twenty19BushFlood_male_PO, Twenty19BushFlood_male_PartPO_NDexpos_PRS)


### Test goodness of fit ###

# lipsitz test  
lipsitz(Twenty19BushFlood_male_PartPO_NDexpos)
lipsitz(Twenty19BushFlood_male_PartPO_PRS)
lipsitz(Twenty19BushFlood_male_PartPO_NDexpos_PRS)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
pulkroben(Twenty19BushFlood_male_PartPO_NDexpos, test = c("chisq","deviance"), tables = FALSE)
pulkroben(Twenty19BushFlood_male_PartPO_PRS, test = c("chisq","deviance"), tables = FALSE)
pulkroben(Twenty19BushFlood_male_PartPO_NDexpos_PRS, test = c("chisq","deviance"), tables = FALSE)

# Hosmer-Lemeshow test 
hosmerlem(Twenty19BushFlood_male_PartPO_NDexpos)
hosmerlem(Twenty19BushFlood_male_PartPO_PRS)
hosmerlem(Twenty19BushFlood_male_PartPO_PRS)

## ----

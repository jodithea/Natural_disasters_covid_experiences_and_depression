# Check models: effect of Natural disaster experience on no. depressive symptoms if first depressive episode occurred after the Natural disaster

## ---- LoadData
load(file = "data/df_ND_covid.RData")

load(file = "models/NDFirst_PO_models.RData")
load(file = "models/NDFirst_PartPO_models.RData")
## ----

## ---- NDFirstCheckModelsPOBothSexes
### Test goodness of fit ###

# lipsitz test  
lipsitz(NDFirst_PO)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
pulkroben(NDFirst_PO, test = c("chisq","deviance"), tables = FALSE)

# Hosmer-Lemeshow test 
hosmerlem(NDFirst_PO)


### Test proportional odds assumption ###

# Brant Wald Test
# A generalized ordinal logistic regression model is approximated and compared to the calculated proportional odds model (a generalized ordinal logistic regression model is simply a relaxing of the proportional odds model to allow for different coefficients at each level of the ordinal outcome variable.)
# A Wald test is a hypothesis test of the significance of the difference in model coefficients, producing a chi-square statistic. A low p-value in a Brant-Wald test is an indicator that the coefficient does not satisfy the proportional odds assumption. 
# A p-value of less than 0.05 on this test, particularly on the Omnibus plus at least one of the variables should be interpreted as a failure of the proportional odds assumption.

brant.test(NDFirst_PO)

# Sighting coefficients of stratified binomial models - want each to be similar
# create binary variables for every combo of the 3 categories
x <- df_ND_covid %>% 
  mutate(SYMPlowplus = case_when(
    NUMSYMPALL_baseline_cat == 'low' ~ 0,
    NUMSYMPALL_baseline_cat == 'medium' |  NUMSYMPALL_baseline_cat == 'high' ~ 1),
    SYMPmedplus = case_when(
      NUMSYMPALL_baseline_cat == 'low' | NUMSYMPALL_baseline_cat == 'medium' ~ 0,
      NUMSYMPALL_baseline_cat == 'high' ~ 1)
  )


# Create logistic regression for each
lowplus_model <- glm(SYMPlowplus ~ 
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
                     family = binomial(link = 'logit'),
                     data = x)

medplus_model <- glm(SYMPmedplus ~ 
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
                     family = binomial(link = 'logit'),
                     data = x)


# display the coefficients of all models and examine the difference between them.
# coefficients (i.e. the slopes) should be similar for all models to hold the proportional odds assumption

# table
coeffs <- data.frame(
  'ordinal' = coef(NDFirst_PO)[-c(1,2)],
  'binary low vs med and high' = lowplus_model$coefficients[-1],
  'binary low and med vs high' = medplus_model$coefficients[-1])

coeffs %>% 
  kable()

coeffs2 <- coeffs %>% 
  rownames_to_column() %>% 
  pivot_longer(cols = 2:4)

ggplot(coeffs2, aes(x = value, y = rowname, group = name, colour = name)) +
  geom_jitter(height = 0.2) +
  labs(x = "coefficient", y = "predictors")

coeffs3 <- coeffs2 %>% 
  filter(rowname == 'PRS_stand_genetic' | rowname == 'NDEXPOS_DEPRFAFTERYes' | rowname == "AGE_baseline")

ggplot(coeffs3, aes(x = value, y = rowname, group = name, colour = name)) +
  geom_jitter(height = 0.2) +
  labs(x = "coefficient", y = "predictors")
## ----

## ---- NDFirstCheckModelsPOFemale
### Test goodness of fit ###

# lipsitz test  
lipsitz(NDFirst_female_PO)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
pulkroben(NDFirst_female_PO, test = c("chisq","deviance"), tables = FALSE)

# Hosmer-Lemeshow test 
hosmerlem(NDFirst_female_PO)


### Test proportional odds assumption ###

# Brant Wald Test
# A generalized ordinal logistic regression model is approximated and compared to the calculated proportional odds model (a generalized ordinal logistic regression model is simply a relaxing of the proportional odds model to allow for different coefficients at each level of the ordinal outcome variable.)
# A Wald test is a hypothesis test of the significance of the difference in model coefficients, producing a chi-square statistic. A low p-value in a Brant-Wald test is an indicator that the coefficient does not satisfy the proportional odds assumption. 
# A p-value of less than 0.05 on this test, particularly on the Omnibus plus at least one of the variables should be interpreted as a failure of the proportional odds assumption.

brant.test(NDFirst_female_PO)

# Sighting coefficients of stratified binomial models - want each to be similar
# create binary variables for every combo of the 3 categories
x <- df_ND_covid %>% 
  mutate(SYMPlowplus = case_when(
    NUMSYMPALL_baseline_cat == 'low' ~ 0,
    NUMSYMPALL_baseline_cat == 'medium' |  NUMSYMPALL_baseline_cat == 'high' ~ 1),
    SYMPmedplus = case_when(
      NUMSYMPALL_baseline_cat == 'low' | NUMSYMPALL_baseline_cat == 'medium' ~ 0,
      NUMSYMPALL_baseline_cat == 'high' ~ 1)
  )


# Create logistic regression for each
lowplus_model <- glm(SYMPlowplus ~ 
                       PRS_stand_genetic +
                       NDEXPOS_DEPRFAFTER +
                       AGE_baseline +
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
                     family = binomial(link = 'logit'),
                     data = x[x$SEX_genetic == "Female", ])

medplus_model <- glm(SYMPmedplus ~ 
                       PRS_stand_genetic +
                       NDEXPOS_DEPRFAFTER +
                       AGE_baseline +
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
                     family = binomial(link = 'logit'),
                     data = x[x$SEX_genetic == "Female", ])


# display the coefficients of all models and examine the difference between them.
# coefficients (i.e. the slopes) should be similar for all models to hold the proportional odds assumption

# table
coeffs <- data.frame(
  'ordinal' = coef(NDFirst_female_PO)[-c(1,2)],
  'binary low vs med and high' = lowplus_model$coefficients[-1],
  'binary low and med vs high' = medplus_model$coefficients[-1])

coeffs %>% 
  kable()

coeffs2 <- coeffs %>% 
  rownames_to_column() %>% 
  pivot_longer(cols = 2:4)

ggplot(coeffs2, aes(x = value, y = rowname, group = name, colour = name)) +
  geom_jitter(height = 0.2) +
  labs(x = "coefficient", y = "predictors")

coeffs3 <- coeffs2 %>% 
  filter(rowname == 'PRS_stand_genetic' | rowname == 'NDEXPOS_DEPRFAFTERYes' | rowname == "AGE_baseline")

ggplot(coeffs3, aes(x = value, y = rowname, group = name, colour = name)) +
  geom_jitter(height = 0.2) +
  labs(x = "coefficient", y = "predictors")
## ----

## ---- NDFirstCheckModelsPOMale
### Test goodness of fit ###

# lipsitz test  
lipsitz(NDFirst_male_PO)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
pulkroben(NDFirst_male_PO, test = c("chisq","deviance"), tables = FALSE)

# Hosmer-Lemeshow test 
hosmerlem(NDFirst_male_PO)


### Test proportional odds assumption ###

# Brant Wald Test
# A generalized ordinal logistic regression model is approximated and compared to the calculated proportional odds model (a generalized ordinal logistic regression model is simply a relaxing of the proportional odds model to allow for different coefficients at each level of the ordinal outcome variable.)
# A Wald test is a hypothesis test of the significance of the difference in model coefficients, producing a chi-square statistic. A low p-value in a Brant-Wald test is an indicator that the coefficient does not satisfy the proportional odds assumption. 
# A p-value of less than 0.05 on this test, particularly on the Omnibus plus at least one of the variables should be interpreted as a failure of the proportional odds assumption.

brant.test(NDFirst_male_PO)

# Sighting coefficients of stratified binomial models - want each to be similar
# create binary variables for every combo of the 3 categories
x <- df_ND_covid %>% 
  mutate(SYMPlowplus = case_when(
    NUMSYMPALL_baseline_cat == 'low' ~ 0,
    NUMSYMPALL_baseline_cat == 'medium' |  NUMSYMPALL_baseline_cat == 'high' ~ 1),
    SYMPmedplus = case_when(
      NUMSYMPALL_baseline_cat == 'low' | NUMSYMPALL_baseline_cat == 'medium' ~ 0,
      NUMSYMPALL_baseline_cat == 'high' ~ 1)
  )


# Create logistic regression for each
lowplus_model <- glm(SYMPlowplus ~ 
                       PRS_stand_genetic +
                       NDEXPOS_DEPRFAFTER +
                       AGE_baseline +
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
                     family = binomial(link = 'logit'),
                     data = x[x$SEX_genetic == "Male", ])

medplus_model <- glm(SYMPmedplus ~ 
                       PRS_stand_genetic +
                       NDEXPOS_DEPRFAFTER +
                       AGE_baseline +
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
                     family = binomial(link = 'logit'),
                     data = x[x$SEX_genetic == "Male", ])


# display the coefficients of all models and examine the difference between them.
# coefficients (i.e. the slopes) should be similar for all models to hold the proportional odds assumption

# table
coeffs <- data.frame(
  'ordinal' = coef(NDFirst_male_PO)[-c(1,2)],
  'binary low vs med and high' = lowplus_model$coefficients[-1],
  'binary low and med vs high' = medplus_model$coefficients[-1])

coeffs %>% 
  kable()

coeffs2 <- coeffs %>% 
  rownames_to_column() %>% 
  pivot_longer(cols = 2:4)

ggplot(coeffs2, aes(x = value, y = rowname, group = name, colour = name)) +
  geom_jitter(height = 0.2) +
  labs(x = "coefficient", y = "predictors")

coeffs3 <- coeffs2 %>% 
  filter(rowname == 'PRS_stand_genetic' | rowname == 'NDEXPOS_DEPRFAFTERYes' | rowname == "AGE_baseline")

ggplot(coeffs3, aes(x = value, y = rowname, group = name, colour = name)) +
  geom_jitter(height = 0.2) +
  labs(x = "coefficient", y = "predictors")
## ----


## ---- NDFirstCheckModelsPartPOBothSexes
# LRT to compare partial PO to PO models
lrtest(NDFirst_PO, NDFirst_PartPO_NDexpos)
lrtest(NDFirst_PO, NDFirst_PartPO_PRS)
lrtest(NDFirst_PO, NDFirst_PartPO_NDexpos_PRS)


### Test goodness of fit ###

# lipsitz test  
lipsitz(NDFirst_PartPO_NDexpos)
lipsitz(NDFirst_PartPO_PRS)
lipsitz(NDFirst_PartPO_NDexpos_PRS)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
pulkroben(NDFirst_PartPO_NDexpos, test = c("chisq","deviance"), tables = FALSE)
pulkroben(NDFirst_PartPO_PRS, test = c("chisq","deviance"), tables = FALSE)
pulkroben(NDFirst_PartPO_NDexpos_PRS, test = c("chisq","deviance"), tables = FALSE)

# Hosmer-Lemeshow test 
hosmerlem(NDFirst_PartPO_NDexpos)
hosmerlem(NDFirst_PartPO_PRS)
hosmerlem(NDFirst_PartPO_PRS)
## ----



## ---- NDFirstCheckModelsPartPOFemale
# LRT to compare partial PO to PO models
lrtest(NDFirst_female_PO, NDFirst_female_PartPO_NDexpos)
lrtest(NDFirst_female_PO, NDFirst_female_PartPO_PRS)
lrtest(NDFirst_female_PO, NDFirst_female_PartPO_NDexpos_PRS)


### Test goodness of fit ###

# lipsitz test  
lipsitz(NDFirst_female_PartPO_NDexpos)
lipsitz(NDFirst_female_PartPO_PRS)
lipsitz(NDFirst_female_PartPO_NDexpos_PRS)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
pulkroben(NDFirst_female_PartPO_NDexpos, test = c("chisq","deviance"), tables = FALSE)
pulkroben(NDFirst_female_PartPO_PRS, test = c("chisq","deviance"), tables = FALSE)
pulkroben(NDFirst_female_PartPO_NDexpos_PRS, test = c("chisq","deviance"), tables = FALSE)

# Hosmer-Lemeshow test 
hosmerlem(NDFirst_female_PartPO_NDexpos)
hosmerlem(NDFirst_female_PartPO_PRS)
hosmerlem(NDFirst_female_PartPO_PRS)
## ----

## ---- NDFirstCheckModelsPartPOMale
# LRT to compare partial PO to PO models
lrtest(NDFirst_male_PO, NDFirst_male_PartPO_NDexpos)
lrtest(NDFirst_male_PO, NDFirst_male_PartPO_PRS)
lrtest(NDFirst_male_PO, NDFirst_male_PartPO_NDexpos_PRS)


### Test goodness of fit ###

# lipsitz test  
lipsitz(NDFirst_male_PartPO_NDexpos)
lipsitz(NDFirst_male_PartPO_PRS)
lipsitz(NDFirst_male_PartPO_NDexpos_PRS)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
pulkroben(NDFirst_male_PartPO_NDexpos, test = c("chisq","deviance"), tables = FALSE)
pulkroben(NDFirst_male_PartPO_PRS, test = c("chisq","deviance"), tables = FALSE)
pulkroben(NDFirst_male_PartPO_NDexpos_PRS, test = c("chisq","deviance"), tables = FALSE)

# Hosmer-Lemeshow test 
hosmerlem(NDFirst_male_PartPO_NDexpos)
hosmerlem(NDFirst_male_PartPO_PRS)
hosmerlem(NDFirst_male_PartPO_PRS)

## ----

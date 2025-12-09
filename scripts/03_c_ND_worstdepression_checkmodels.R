# Check models: effect of Natural disaster experience on no. depressive symptoms if worst depressive episode occurred after the Natural disaster

## ---- LoadData
load(file = "data/df_ND_covid.RData")

load(file = "models/NDworse_PO_models.RData")
load(file = "models/NDworse_PartPO_models.RData")
## ----

## ---- NDWorseCheckModelsPOBothSexes
### Test goodness of fit ###

# lipsitz test  
lipsitz(NDworse_PO)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
pulkroben(NDworse_PO, test = c("chisq","deviance"), tables = FALSE)

# Hosmer-Lemeshow test 
hosmerlem(NDworse_PO)


### Test proportional odds assumption ###

# Brant Wald Test
# A generalized ordinal logistic regression model is approximated and compared to the calculated proportional odds model (a generalized ordinal logistic regression model is simply a relaxing of the proportional odds model to allow for different coefficients at each level of the ordinal outcome variable.)
# A Wald test is a hypothesis test of the significance of the difference in model coefficients, producing a chi-square statistic. A low p-value in a Brant-Wald test is an indicator that the coefficient does not satisfy the proportional odds assumption. 
# A p-value of less than 0.05 on this test, particularly on the Omnibus plus at least one of the variables should be interpreted as a failure of the proportional odds assumption.

brant.test(NDworse_PO)

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
                       NDEXPOS_DEPRWAFTER +
                       AGE_baseline +
                       SEX_genetic +
                       PC1_genetic +
                       PC2_genetic +
                       PC3_genetic +
                       PC4_genetic,
                     family = binomial(link = 'logit'),
                     data = x)

medplus_model <- glm(SYMPmedplus ~ 
                       PRS_stand_genetic +
                       NDEXPOS_DEPRWAFTER +
                       AGE_baseline +
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
  'ordinal' = coef(NDworse_PO)[-c(1,2)],
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
  filter(rowname == 'PRS_stand_genetic' | rowname == 'NDEXPOS_DEPRWAFTERYes' | rowname == "AGE_baseline")

ggplot(coeffs3, aes(x = value, y = rowname, group = name, colour = name)) +
  geom_jitter(height = 0.2) +
  labs(x = "coefficient", y = "predictors")
## ----

## ---- NDWorseCheckModelsPOFemale
### Test goodness of fit ###

# lipsitz test  
lipsitz(NDworse_female_PO)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
pulkroben(NDworse_female_PO, test = c("chisq","deviance"), tables = FALSE)

# Hosmer-Lemeshow test 
hosmerlem(NDworse_female_PO)


### Test proportional odds assumption ###

# Brant Wald Test
# A generalized ordinal logistic regression model is approximated and compared to the calculated proportional odds model (a generalized ordinal logistic regression model is simply a relaxing of the proportional odds model to allow for different coefficients at each level of the ordinal outcome variable.)
# A Wald test is a hypothesis test of the significance of the difference in model coefficients, producing a chi-square statistic. A low p-value in a Brant-Wald test is an indicator that the coefficient does not satisfy the proportional odds assumption. 
# A p-value of less than 0.05 on this test, particularly on the Omnibus plus at least one of the variables should be interpreted as a failure of the proportional odds assumption.

brant.test(NDworse_female_PO)

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
                       NDEXPOS_DEPRWAFTER +
                       AGE_baseline +
                       PC1_genetic +
                       PC2_genetic +
                       PC3_genetic +
                       PC4_genetic,
                     family = binomial(link = 'logit'),
                     data = x[x$SEX_genetic == "Female", ])

medplus_model <- glm(SYMPmedplus ~ 
                         PRS_stand_genetic +
                         NDEXPOS_DEPRWAFTER +
                         AGE_baseline +
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
  'ordinal' = coef(NDworse_female_PO)[-c(1,2)],
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
  filter(rowname == 'PRS_stand_genetic' | rowname == 'NDEXPOS_DEPRWAFTERYes' | rowname == "AGE_baseline")

ggplot(coeffs3, aes(x = value, y = rowname, group = name, colour = name)) +
  geom_jitter(height = 0.2) +
  labs(x = "coefficient", y = "predictors")
## ----

## ---- NDWorseCheckModelsPOMale
### Test goodness of fit ###

# lipsitz test  
lipsitz(NDworse_male_PO)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
pulkroben(NDworse_male_PO, test = c("chisq","deviance"), tables = FALSE)

# Hosmer-Lemeshow test 
hosmerlem(NDworse_male_PO)


### Test proportional odds assumption ###

# Brant Wald Test
# A generalized ordinal logistic regression model is approximated and compared to the calculated proportional odds model (a generalized ordinal logistic regression model is simply a relaxing of the proportional odds model to allow for different coefficients at each level of the ordinal outcome variable.)
# A Wald test is a hypothesis test of the significance of the difference in model coefficients, producing a chi-square statistic. A low p-value in a Brant-Wald test is an indicator that the coefficient does not satisfy the proportional odds assumption. 
# A p-value of less than 0.05 on this test, particularly on the Omnibus plus at least one of the variables should be interpreted as a failure of the proportional odds assumption.

brant.test(NDworse_male_PO)

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
                       NDEXPOS_DEPRWAFTER +
                       AGE_baseline +
                       PC1_genetic +
                       PC2_genetic +
                       PC3_genetic +
                       PC4_genetic,
                     family = binomial(link = 'logit'),
                     data = x[x$SEX_genetic == "Male", ])

medplus_model <- glm(SYMPmedplus ~ 
                       PRS_stand_genetic +
                       NDEXPOS_DEPRWAFTER +
                       AGE_baseline +
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
  'ordinal' = coef(NDworse_male_PO)[-c(1,2)],
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
  filter(rowname == 'PRS_stand_genetic' | rowname == 'NDEXPOS_DEPRWAFTERYes' | rowname == "AGE_baseline")

ggplot(coeffs3, aes(x = value, y = rowname, group = name, colour = name)) +
  geom_jitter(height = 0.2) +
  labs(x = "coefficient", y = "predictors")
## ----

## ---- NDWorseCheckModelsPartPOBothSexes
# LRT to compare partial PO to PO models
lrtest(NDworse_PO, NDworse_PartPO_NDexpos)
lrtest(NDworse_PO, NDworse_PartPO_PRS)
lrtest(NDworse_PO, NDworse_PartPO_NDexpos_PRS)


### Test goodness of fit ###

# lipsitz test  
lipsitz(NDworse_PartPO_NDexpos)
lipsitz(NDworse_PartPO_PRS)
lipsitz(NDworse_PartPO_NDexpos_PRS)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
pulkroben(NDworse_PartPO_NDexpos, test = c("chisq","deviance"), tables = FALSE)
pulkroben(NDworse_PartPO_PRS, test = c("chisq","deviance"), tables = FALSE)
pulkroben(NDworse_PartPO_NDexpos_PRS, test = c("chisq","deviance"), tables = FALSE)

# Hosmer-Lemeshow test 
hosmerlem(NDworse_PartPO_NDexpos)
hosmerlem(NDworse_PartPO_PRS)
hosmerlem(NDworse_PartPO_PRS)
## ----


## ---- NDWorseCheckModelsPartPOFemale
# LRT to compare partial PO to PO models
lrtest(NDworse_female_PO, NDworse_female_PartPO_NDexpos)
lrtest(NDworse_female_PO, NDworse_female_PartPO_PRS)
lrtest(NDworse_female_PO, NDworse_female_PartPO_NDexpos_PRS)


### Test goodness of fit ###

# lipsitz test  
lipsitz(NDworse_female_PartPO_NDexpos)
lipsitz(NDworse_female_PartPO_PRS)
lipsitz(NDworse_female_PartPO_NDexpos_PRS)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
pulkroben(NDworse_female_PartPO_NDexpos, test = c("chisq","deviance"), tables = FALSE)
pulkroben(NDworse_female_PartPO_PRS, test = c("chisq","deviance"), tables = FALSE)
pulkroben(NDworse_female_PartPO_NDexpos_PRS, test = c("chisq","deviance"), tables = FALSE)

# Hosmer-Lemeshow test 
hosmerlem(NDworse_female_PartPO_NDexpos)
hosmerlem(NDworse_female_PartPO_PRS)
hosmerlem(NDworse_female_PartPO_PRS)
## ----

## ---- NDWorseCheckModelsPartPOMale
# LRT to compare partial PO to PO models
lrtest(NDworse_male_PO, NDworse_male_PartPO_NDexpos)
lrtest(NDworse_male_PO, NDworse_male_PartPO_PRS)
lrtest(NDworse_male_PO, NDworse_male_PartPO_NDexpos_PRS)


### Test goodness of fit ###

# lipsitz test  
lipsitz(NDworse_male_PartPO_NDexpos)
lipsitz(NDworse_male_PartPO_PRS)
lipsitz(NDworse_male_PartPO_NDexpos_PRS)

# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
pulkroben(NDworse_male_PartPO_NDexpos, test = c("chisq","deviance"), tables = FALSE)
pulkroben(NDworse_male_PartPO_PRS, test = c("chisq","deviance"), tables = FALSE)
pulkroben(NDworse_male_PartPO_NDexpos_PRS, test = c("chisq","deviance"), tables = FALSE)

# Hosmer-Lemeshow test 
hosmerlem(NDworse_male_PartPO_NDexpos)
hosmerlem(NDworse_male_PartPO_PRS)
hosmerlem(NDworse_male_PartPO_PRS)

## ----
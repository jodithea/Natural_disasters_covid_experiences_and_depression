# Model investigation: effect of Natural disaster experience on no. depressive symptoms if first depressive episode occurred after the Natural disaster

## ---- LoadData
load(file = "data/df_ND_covid.RData")

load(file = "models/NDFirst_PO_models.RData")
load(file = "models/NDFirst_PartPO_models.RData")
## ----

## ---- NDFirstResultsBothSexes
summary(NDFirst_PO)

# Create nice summary of results, including back transformation to give OR and calculating 95% CI
CI <- as.data.frame(confint(NDFirst_PO)) %>% 
  rownames_to_column() %>% 
  rename('lower_95_CI_of_beta' = "2.5 %",
         'upper_95_CI_of_beta' = "97.5 %") %>% 
  mutate(lower_95_CI_of_OR = exp(lower_95_CI_of_beta),
         upper_95_CI_of_OR = exp(upper_95_CI_of_beta))

beta <- as.data.frame(coef(NDFirst_PO)) %>%
  rownames_to_column() %>% 
  rename("beta" = "coef(NDFirst_PO)") %>% 
  mutate(OR = exp(beta))

results <- beta %>% 
  full_join(CI) %>% 
  relocate(rowname, beta, lower_95_CI_of_beta, upper_95_CI_of_beta, OR, lower_95_CI_of_OR, upper_95_CI_of_OR)

results %>% kable()
## ----

## ---- NDFirstResultsPlotsBothSexes
plot_model(NDFirst_PO, 
           type = 'est', 
           show.values = T)

plot_model(NDFirst_PO, 
           type = 'est', 
           show.values = T,
           terms = c('PRS_stand_genetic', 'NDEXPOS_DEPRFAFTERYes'))
## ----

## ---- NDFirstResultsFemale
summary(NDFirst_female_PO)

# Create nice summary of results, including back transformation to give OR and calculating 95% CI
CI <- as.data.frame(confint(NDFirst_female_PO)) %>% 
  rownames_to_column() %>% 
  rename('lower_95_CI_of_beta' = "2.5 %",
         'upper_95_CI_of_beta' = "97.5 %") %>% 
  mutate(lower_95_CI_of_OR = exp(lower_95_CI_of_beta),
         upper_95_CI_of_OR = exp(upper_95_CI_of_beta))

beta <- as.data.frame(coef(NDFirst_female_PO)) %>%
  rownames_to_column() %>% 
  rename("beta" = "coef(NDFirst_female_PO)") %>% 
  mutate(OR = exp(beta))

results <- beta %>% 
  full_join(CI) %>% 
  relocate(rowname, beta, lower_95_CI_of_beta, upper_95_CI_of_beta, OR, lower_95_CI_of_OR, upper_95_CI_of_OR)

results %>% kable()
## ----

## ---- NDFirstResultsPlotsFemale
plot_model(NDFirst_female_PO, 
           type = 'est', 
           show.values = T)

plot_model(NDFirst_female_PO, 
           type = 'est', 
           show.values = T,
           terms = c('PRS_stand_genetic', 'NDEXPOS_DEPRFAFTERYes'))
## ----

## ---- NDFirstResultsMale
summary(NDFirst_male_PO)

# Create nice summary of results, including back transformation to give OR and calculating 95% CI
CI <- as.data.frame(confint(NDFirst_male_PO)) %>% 
  rownames_to_column() %>% 
  rename('lower_95_CI_of_beta' = "2.5 %",
         'upper_95_CI_of_beta' = "97.5 %") %>% 
  mutate(lower_95_CI_of_OR = exp(lower_95_CI_of_beta),
         upper_95_CI_of_OR = exp(upper_95_CI_of_beta))

beta <- as.data.frame(coef(NDFirst_male_PO)) %>%
  rownames_to_column() %>% 
  rename("beta" = "coef(NDFirst_male_PO)") %>% 
  mutate(OR = exp(beta))

results <- beta %>% 
  full_join(CI) %>% 
  relocate(rowname, beta, lower_95_CI_of_beta, upper_95_CI_of_beta, OR, lower_95_CI_of_OR, upper_95_CI_of_OR)

results %>% kable()
## ----

## ---- NDFirstResultsPlotsMale
plot_model(NDFirst_male_PO, 
           type = 'est', 
           show.values = T)

plot_model(NDFirst_male_PO, 
           type = 'est', 
           show.values = T,
           terms = c('PRS_stand_genetic', 'NDEXPOS_DEPRFAFTERYes'))
## ----


## ---- NDFirstResultsFemalevsMale
female_beta <- coef(summary(NDFirst_female_PO))["NDEXPOS_DEPRFAFTERYes", "Estimate"]
female_SE <- coef(summary(NDFirst_female_PO))["NDEXPOS_DEPRFAFTERYes", "Std. Error"]
male_beta <- coef(summary(NDFirst_male_PO))["NDEXPOS_DEPRFAFTERYes", "Estimate"]
male_SE <- coef(summary(NDFirst_male_PO))["NDEXPOS_DEPRFAFTERYes", "Std. Error"]

Z <- (female_beta - male_beta) / sqrt((female_SE)^2 + (male_SE)^2)
p <- 2 * pnorm(abs(Z), mean = 0, sd = 1, lower.tail = FALSE)

message <- sprintf("Association of ND experience on no. symptoms during first depressive episode in females compared to males, Z = %.6f, p = %.6f", Z, p)
print(message)
## ----


# Look into making a marginal effects plot (normal plot_model don't work on VGAM objects)
# margeff <- as.data.frame(margeff(NDFirst_female_PO))

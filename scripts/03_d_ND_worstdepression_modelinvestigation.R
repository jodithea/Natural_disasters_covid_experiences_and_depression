# Model investigation: effect of Natural disaster experience on no. depressive symptoms if worst depressive episode occurred after the Natural disaster

## ---- LoadData
load(file = "data/df_ND_covid.RData")

load(file = "models/NDworse_PO_models.RData")
load(file = "models/NDworse_PartPO_models.RData")
## ----
## ---- NDWorseResultsBothSexes
summary(NDworse_PO)

# Create nice summary of results, including back transformation to give OR and calculating 95% CI
CI <- as.data.frame(confint(NDworse_PO)) %>% 
  rownames_to_column() %>% 
  rename('lower_95_CI_of_beta' = "2.5 %",
         'upper_95_CI_of_beta' = "97.5 %") %>% 
  mutate(lower_95_CI_of_OR = exp(lower_95_CI_of_beta),
         upper_95_CI_of_OR = exp(upper_95_CI_of_beta))

beta <- as.data.frame(coef(NDworse_PO)) %>%
  rownames_to_column() %>% 
  rename("beta" = "coef(NDworse_PO)") %>% 
  mutate(OR = exp(beta))

results <- beta %>% 
  full_join(CI) %>% 
  relocate(rowname, beta, lower_95_CI_of_beta, upper_95_CI_of_beta, OR, lower_95_CI_of_OR, upper_95_CI_of_OR)

results %>% kable()
## ----

## ---- NDWorseResultsPlotsBothSexes
plot_model(NDworse_PO, 
           type = 'est', 
           show.values = T)

plot_model(NDworse_PO, 
           type = 'est', 
           show.values = T,
           terms = c('PRS_stand_genetic', 'NDEXPOS_DEPRWAFTERYes'))
## ----

## ---- NDWorseResultsFemale
summary(NDworse_female_PO)

# Create nice summary of results, including back transformation to give OR and calculating 95% CI
CI <- as.data.frame(confint(NDworse_female_PO)) %>% 
  rownames_to_column() %>% 
  rename('lower_95_CI_of_beta' = "2.5 %",
         'upper_95_CI_of_beta' = "97.5 %") %>% 
  mutate(lower_95_CI_of_OR = exp(lower_95_CI_of_beta),
         upper_95_CI_of_OR = exp(upper_95_CI_of_beta))

beta <- as.data.frame(coef(NDworse_female_PO)) %>%
  rownames_to_column() %>% 
  rename("beta" = "coef(NDworse_female_PO)") %>% 
  mutate(OR = exp(beta))

results <- beta %>% 
  full_join(CI) %>% 
  relocate(rowname, beta, lower_95_CI_of_beta, upper_95_CI_of_beta, OR, lower_95_CI_of_OR, upper_95_CI_of_OR)

results %>% kable()
## ----

## ---- NDWorseResultsPlotsFemale
plot_model(NDworse_female_PO, 
           type = 'est', 
           show.values = T)

plot_model(NDworse_female_PO, 
           type = 'est', 
           show.values = T,
           terms = c('PRS_stand_genetic', 'NDEXPOS_DEPRWAFTERYes'))
## ----

## ---- NDWorseResultsMale
summary(NDworse_male_PO)

# Create nice summary of results, including back transformation to give OR and calculating 95% CI
CI <- as.data.frame(confint(NDworse_male_PO)) %>% 
  rownames_to_column() %>% 
  rename('lower_95_CI_of_beta' = "2.5 %",
         'upper_95_CI_of_beta' = "97.5 %") %>% 
  mutate(lower_95_CI_of_OR = exp(lower_95_CI_of_beta),
         upper_95_CI_of_OR = exp(upper_95_CI_of_beta))

beta <- as.data.frame(coef(NDworse_male_PO)) %>%
  rownames_to_column() %>% 
  rename("beta" = "coef(NDworse_male_PO)") %>% 
  mutate(OR = exp(beta))

results <- beta %>% 
  full_join(CI) %>% 
  relocate(rowname, beta, lower_95_CI_of_beta, upper_95_CI_of_beta, OR, lower_95_CI_of_OR, upper_95_CI_of_OR)

results %>% kable()
## ----

## ---- NDWorseResultsPlotsMale
plot_model(NDworse_male_PO, 
           type = 'est', 
           show.values = T)

plot_model(NDworse_male_PO, 
           type = 'est', 
           show.values = T,
           terms = c('PRS_stand_genetic', 'NDEXPOS_DEPRWAFTERYes'))
## ----

## ---- NDWorseResultsFemalevsMale
female_beta <- coef(summary(NDworse_female_PO))["NDEXPOS_DEPRWAFTERYes", "Estimate"]
female_SE <- coef(summary(NDworse_female_PO))["NDEXPOS_DEPRWAFTERYes", "Std. Error"]
male_beta <- coef(summary(NDworse_male_PO))["NDEXPOS_DEPRWAFTERYes", "Estimate"]
male_SE <- coef(summary(NDworse_male_PO))["NDEXPOS_DEPRWAFTERYes", "Std. Error"]

Z <- (female_beta - male_beta) / sqrt((female_SE)^2 + (male_SE)^2)
p <- 2 * pnorm(abs(Z), mean = 0, sd = 1, lower.tail = FALSE)

message <- sprintf("Association of ND experience on no. symptoms during worst depressive episode in females compared to males, Z = %.6f, p = %.6f", Z, p)
print(message)
## ----

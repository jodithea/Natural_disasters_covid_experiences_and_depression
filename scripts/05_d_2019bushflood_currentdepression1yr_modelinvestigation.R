# Model investigation: Effect of 2019 bushfires/floods experience on no. depressive symptoms during current episode measured 1 year later

## ---- LoadData
load(file = "data/df_ND_covid.RData")

load(file = "models/Twenty19BushFlood_PO_models.RData")
load(file = "models/Twenty19BushFlood_PartPO_models.RData")
## ----

## ---- Twenty19BushFloodResultsBothSexes
summary(Twenty19BushFlood_PO)

# Create nice summary of results, including back transformation to give OR and calculating 95% CI
CI <- as.data.frame(confint(Twenty19BushFlood_PO)) %>% 
  rownames_to_column() %>% 
  rename('lower_95_CI_of_beta' = "2.5 %",
         'upper_95_CI_of_beta' = "97.5 %") %>% 
  mutate(lower_95_CI_of_OR = exp(lower_95_CI_of_beta),
         upper_95_CI_of_OR = exp(upper_95_CI_of_beta))

beta <- as.data.frame(coef(Twenty19BushFlood_PO)) %>%
  rownames_to_column() %>% 
  rename("beta" = "coef(Twenty19BushFlood_PO)") %>% 
  mutate(OR = exp(beta))

results <- beta %>% 
  full_join(CI) %>% 
  relocate(rowname, beta, lower_95_CI_of_beta, upper_95_CI_of_beta, OR, lower_95_CI_of_OR, upper_95_CI_of_OR)

results %>% kable()
## ----

## ---- Twenty19BushFloodResultsPlotsBothSexes
plot_model(Twenty19BushFlood_PO, 
           type = 'est', 
           show.values = T)

plot_model(Twenty19BushFlood_PO, 
           type = 'est', 
           show.values = T,
           terms = c('PRS_stand_genetic', 'BUSHFLOODHORR_followup1'))
## ----


## ---- Twenty19BushFloodResultsFemale
summary(Twenty19BushFlood_female_PO)

# Create nice summary of results, including back transformation to give OR and calculating 95% CI
CI <- as.data.frame(confint(Twenty19BushFlood_female_PO)) %>% 
  rownames_to_column() %>% 
  rename('lower_95_CI_of_beta' = "2.5 %",
         'upper_95_CI_of_beta' = "97.5 %") %>% 
  mutate(lower_95_CI_of_OR = exp(lower_95_CI_of_beta),
         upper_95_CI_of_OR = exp(upper_95_CI_of_beta))

beta <- as.data.frame(coef(Twenty19BushFlood_female_PO)) %>%
  rownames_to_column() %>% 
  rename("beta" = "coef(Twenty19BushFlood_female_PO)") %>% 
  mutate(OR = exp(beta))

results <- beta %>% 
  full_join(CI) %>% 
  relocate(rowname, beta, lower_95_CI_of_beta, upper_95_CI_of_beta, OR, lower_95_CI_of_OR, upper_95_CI_of_OR)

results %>% kable()
## ----

## ---- Twenty19BushFloodResultsPlotsFemale
plot_model(Twenty19BushFlood_female_PO, 
           type = 'est', 
           show.values = T)

plot_model(Twenty19BushFlood_female_PO, 
           type = 'est', 
           show.values = T,
           terms = c('PRS_stand_genetic', 'BUSHFLOODHORR_followup1'))
## ----

## ---- Twenty19BushFloodResultsMale
summary(Twenty19BushFlood_male_PO)

# Create nice summary of results, including back transformation to give OR and calculating 95% CI
CI <- as.data.frame(confint(Twenty19BushFlood_male_PO)) %>% 
  rownames_to_column() %>% 
  rename('lower_95_CI_of_beta' = "2.5 %",
         'upper_95_CI_of_beta' = "97.5 %") %>% 
  mutate(lower_95_CI_of_OR = exp(lower_95_CI_of_beta),
         upper_95_CI_of_OR = exp(upper_95_CI_of_beta))

beta <- as.data.frame(coef(Twenty19BushFlood_male_PO)) %>%
  rownames_to_column() %>% 
  rename("beta" = "coef(Twenty19BushFlood_male_PO)") %>% 
  mutate(OR = exp(beta))

results <- beta %>% 
  full_join(CI) %>% 
  relocate(rowname, beta, lower_95_CI_of_beta, upper_95_CI_of_beta, OR, lower_95_CI_of_OR, upper_95_CI_of_OR)

results %>% kable()
## ----

## ---- Twenty19BushFloodResultsPlotsMale
plot_model(Twenty19BushFlood_male_PO, 
           type = 'est', 
           show.values = T)

plot_model(Twenty19BushFlood_male_PO, 
           type = 'est', 
           show.values = T,
           terms = c('PRS_stand_genetic', 'BUSHFLOODHORR_followup1'))
## ----

## ---- Twenty19BushFloodResultsFemalevsMale
female_beta <- coef(summary(Twenty19BushFlood_female_PO))["BUSHFLOODHORR_followup1", "Estimate"]
female_SE <- coef(summary(Twenty19BushFlood_female_PO))["BUSHFLOODHORR_followup1", "Std. Error"]
male_beta <- coef(summary(Twenty19BushFlood_male_PO))["BUSHFLOODHORR_followup1", "Estimate"]
male_SE <- coef(summary(Twenty19BushFlood_male_PO))["BUSHFLOODHORR_followup1", "Std. Error"]

Z <- (female_beta - male_beta) / sqrt((female_SE)^2 + (male_SE)^2)
p <- 2 * pnorm(abs(Z), mean = 0, sd = 1, lower.tail = FALSE)

message <- sprintf("Association of horrible 2019 bushfires/floods experience on no. symptoms currenlty (1 year later) in females compared to males, Z = %.6f, p = %.6f", Z, p)
print(message)
## ----


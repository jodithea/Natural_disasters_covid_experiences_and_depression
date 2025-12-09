# Model investigation: Effect of 2019 bushfires/floods experience on no. depressive symptoms during current episode measured 1 year later

## ---- LoadData
load(file = "data/df_ND_covid.RData")

load(file = "models/Twenty19BushFloodCovidBoth_PO_models.RData")
load(file = "models/Twenty19BushFloodCovidBoth_PartPO_models.RData")
## ----

## ---- Twenty19BushFloodCovidBothResultsBothSexes
summary(Twenty19BushFloodCovidBoth_PO)

# Create nice summary of results, including back transformation to give OR and calculating 95% CI
CI <- as.data.frame(confint(Twenty19BushFloodCovidBoth_PO)) %>% 
  rownames_to_column() %>% 
  rename('lower_95_CI_of_beta' = "2.5 %",
         'upper_95_CI_of_beta' = "97.5 %") %>% 
  mutate(lower_95_CI_of_OR = exp(lower_95_CI_of_beta),
         upper_95_CI_of_OR = exp(upper_95_CI_of_beta))

beta <- as.data.frame(coef(Twenty19BushFloodCovidBoth_PO)) %>%
  rownames_to_column() %>% 
  rename("beta" = "coef(Twenty19BushFloodCovidBoth_PO)") %>% 
  mutate(OR = exp(beta))

results <- beta %>% 
  full_join(CI) %>% 
  relocate(rowname, beta, lower_95_CI_of_beta, upper_95_CI_of_beta, OR, lower_95_CI_of_OR, upper_95_CI_of_OR)

results %>% kable()
## ----

## ---- Twenty19BushFloodCovidBothResultsPlotsBothSexes
plot_model(Twenty19BushFloodCovidBoth_PO, 
           type = 'est', 
           show.values = T)

plot_model(Twenty19BushFloodCovidBoth_PO, 
           type = 'est', 
           show.values = T,
           terms = c('PRS_stand_genetic', 'COVIDALL_BUSHFLOOD_bothYes'))
## ----

## ---- Twenty19BushFloodCovidBothResultsFemale
summary(Twenty19BushFloodCovidBoth_female_PO)

# Create nice summary of results, including back transformation to give OR and calculating 95% CI
CI <- as.data.frame(confint(Twenty19BushFloodCovidBoth_female_PO)) %>% 
  rownames_to_column() %>% 
  rename('lower_95_CI_of_beta' = "2.5 %",
         'upper_95_CI_of_beta' = "97.5 %") %>% 
  mutate(lower_95_CI_of_OR = exp(lower_95_CI_of_beta),
         upper_95_CI_of_OR = exp(upper_95_CI_of_beta))

beta <- as.data.frame(coef(Twenty19BushFloodCovidBoth_female_PO)) %>%
  rownames_to_column() %>% 
  rename("beta" = "coef(Twenty19BushFloodCovidBoth_female_PO)") %>% 
  mutate(OR = exp(beta))

results <- beta %>% 
  full_join(CI) %>% 
  relocate(rowname, beta, lower_95_CI_of_beta, upper_95_CI_of_beta, OR, lower_95_CI_of_OR, upper_95_CI_of_OR)

results %>% kable()
## ----

## ---- Twenty19BushFloodCovidBothResultsPlotsFemale
plot_model(Twenty19BushFloodCovidBoth_female_PO, 
           type = 'est', 
           show.values = T)

plot_model(Twenty19BushFloodCovidBoth_female_PO, 
           type = 'est', 
           show.values = T,
           terms = c('PRS_stand_genetic', 'COVIDALL_BUSHFLOOD_bothYes'))
## ----

## ---- Twenty19BushFloodCovidBothResultsMale
summary(Twenty19BushFloodCovidBoth_male_PO)

# Create nice summary of results, including back transformation to give OR and calculating 95% CI
CI <- as.data.frame(confint(Twenty19BushFloodCovidBoth_male_PO)) %>% 
  rownames_to_column() %>% 
  rename('lower_95_CI_of_beta' = "2.5 %",
         'upper_95_CI_of_beta' = "97.5 %") %>% 
  mutate(lower_95_CI_of_OR = exp(lower_95_CI_of_beta),
         upper_95_CI_of_OR = exp(upper_95_CI_of_beta))

beta <- as.data.frame(coef(Twenty19BushFloodCovidBoth_male_PO)) %>%
  rownames_to_column() %>% 
  rename("beta" = "coef(Twenty19BushFloodCovidBoth_male_PO)") %>% 
  mutate(OR = exp(beta))

results <- beta %>% 
  full_join(CI) %>% 
  relocate(rowname, beta, lower_95_CI_of_beta, upper_95_CI_of_beta, OR, lower_95_CI_of_OR, upper_95_CI_of_OR)

results %>% kable()
## ----

## ---- Twenty19BushFloodCovidBothResultsPlotsMale
plot_model(Twenty19BushFloodCovidBoth_male_PO, 
           type = 'est', 
           show.values = T)

plot_model(Twenty19BushFloodCovidBoth_male_PO, 
           type = 'est', 
           show.values = T,
           terms = c('PRS_stand_genetic', 'COVIDALL_BUSHFLOOD_bothYes'))
## ----

## ---- Twenty19BushFloodCovidBothResultsFemalevsMale
female_beta <- coef(summary(Twenty19BushFloodCovidBoth_female_PO))["COVIDALL_BUSHFLOOD_bothYes", "Estimate"]
female_SE <- coef(summary(Twenty19BushFloodCovidBoth_female_PO))["COVIDALL_BUSHFLOOD_bothYes", "Std. Error"]
male_beta <- coef(summary(Twenty19BushFloodCovidBoth_male_PO))["COVIDALL_BUSHFLOOD_bothYes", "Estimate"]
male_SE <- coef(summary(Twenty19BushFloodCovidBoth_male_PO))["COVIDALL_BUSHFLOOD_bothYes", "Std. Error"]

Z <- (female_beta - male_beta) / sqrt((female_SE)^2 + (male_SE)^2)
p <- 2 * pnorm(abs(Z), mean = 0, sd = 1, lower.tail = FALSE)

message <- sprintf("Association of horrible 2019 bushfires/floods AND covid experiences on no. symptoms currently in females compared to males, Z = %.6f, p = %.6f", Z, p)
print(message)
## ----

# Look into making a marginal effects plot (normal plot_model don't work on VGAM objects)
# margeff <- as.data.frame(margeff(Twenty19BushFloodCovidBoth_female_PO))
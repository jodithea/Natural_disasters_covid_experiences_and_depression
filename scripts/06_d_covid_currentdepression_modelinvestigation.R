# Model investigation: Effect of 2019 bushfires/floods experience on no. depressive symptoms during current episode measured 1 year later

## ---- LoadData
load(file = "data/df_ND_covid.RData")

load(file = "models/Covid_PO_models.RData")
load(file = "models/Covid_PartPO_models.RData")
## ----

## ---- CovidResultsBothSexes
summary(Covid_PO)

# Create nice summary of results, including back transformation to give OR and calculating 95% CI
CI <- as.data.frame(confint(Covid_PO)) %>% 
  rownames_to_column() %>% 
  rename('lower_95_CI_of_beta' = "2.5 %",
         'upper_95_CI_of_beta' = "97.5 %") %>% 
  mutate(lower_95_CI_of_OR = exp(lower_95_CI_of_beta),
         upper_95_CI_of_OR = exp(upper_95_CI_of_beta))

beta <- as.data.frame(coef(Covid_PO)) %>%
  rownames_to_column() %>% 
  rename("beta" = "coef(Covid_PO)") %>% 
  mutate(OR = exp(beta))

results <- beta %>% 
  full_join(CI) %>% 
  relocate(rowname, beta, lower_95_CI_of_beta, upper_95_CI_of_beta, OR, lower_95_CI_of_OR, upper_95_CI_of_OR)

results %>% kable()
## ----

## ---- CovidResultsPlotsBothSexes
plot_model(Covid_PO, 
           type = 'est', 
           show.values = T)

plot_model(Covid_PO, 
           type = 'est', 
           show.values = T,
           terms = c('PRS_stand_genetic', 'COVIDALLBIN_C1C21'))
## ----


## ---- CovidResultsFemale
summary(Covid_female_PO)

# Create nice summary of results, including back transformation to give OR and calculating 95% CI
CI <- as.data.frame(confint(Covid_female_PO)) %>% 
  rownames_to_column() %>% 
  rename('lower_95_CI_of_beta' = "2.5 %",
         'upper_95_CI_of_beta' = "97.5 %") %>% 
  mutate(lower_95_CI_of_OR = exp(lower_95_CI_of_beta),
         upper_95_CI_of_OR = exp(upper_95_CI_of_beta))

beta <- as.data.frame(coef(Covid_female_PO)) %>%
  rownames_to_column() %>% 
  rename("beta" = "coef(Covid_female_PO)") %>% 
  mutate(OR = exp(beta))

results <- beta %>% 
  full_join(CI) %>% 
  relocate(rowname, beta, lower_95_CI_of_beta, upper_95_CI_of_beta, OR, lower_95_CI_of_OR, upper_95_CI_of_OR)

results %>% kable()
## ----

## ---- CovidResultsPlotsFemale
plot_model(Covid_female_PO, 
           type = 'est', 
           show.values = T)

plot_model(Covid_female_PO, 
           type = 'est', 
           show.values = T,
           terms = c('PRS_stand_genetic', 'COVIDALLBIN_C1C21'))
## ----

## ---- CovidResultsMale
summary(Covid_male_PO)

# Create nice summary of results, including back transformation to give OR and calculating 95% CI
CI <- as.data.frame(confint(Covid_male_PO)) %>% 
  rownames_to_column() %>% 
  rename('lower_95_CI_of_beta' = "2.5 %",
         'upper_95_CI_of_beta' = "97.5 %") %>% 
  mutate(lower_95_CI_of_OR = exp(lower_95_CI_of_beta),
         upper_95_CI_of_OR = exp(upper_95_CI_of_beta))

beta <- as.data.frame(coef(Covid_male_PO)) %>%
  rownames_to_column() %>% 
  rename("beta" = "coef(Covid_male_PO)") %>% 
  mutate(OR = exp(beta))

results <- beta %>% 
  full_join(CI) %>% 
  relocate(rowname, beta, lower_95_CI_of_beta, upper_95_CI_of_beta, OR, lower_95_CI_of_OR, upper_95_CI_of_OR)

results %>% kable()
## ----

## ---- CovidResultsPlotsMale
plot_model(Covid_male_PO, 
           type = 'est', 
           show.values = T)

plot_model(Covid_male_PO, 
           type = 'est', 
           show.values = T,
           terms = c('PRS_stand_genetic', 'COVIDALLBIN_C1C21'))
## ----

## ---- CovidResultsFemalevsMale
female_beta <- coef(summary(Covid_female_PO))["COVIDALLBIN_C1C21", "Estimate"]
female_SE <- coef(summary(Covid_female_PO))["COVIDALLBIN_C1C21", "Std. Error"]
male_beta <- coef(summary(Covid_male_PO))["COVIDALLBIN_C1C21", "Estimate"]
male_SE <- coef(summary(Covid_male_PO))["COVIDALLBIN_C1C21", "Std. Error"]

Z <- (female_beta - male_beta) / sqrt((female_SE)^2 + (male_SE)^2)
p <- 2 * pnorm(abs(Z), mean = 0, sd = 1, lower.tail = FALSE)

message <- sprintf("Association of horrible covid experience on no. symptoms currenlty in females compared to males, Z = %.6f, p = %.6f", Z, p)
print(message)
## ----


# Look into making a marginal effects plot (normal plot_model don't work on VGAM objects)
# margeff <- as.data.frame(margeff(Covid_female_PO))
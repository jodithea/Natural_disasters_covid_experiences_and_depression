## Compare association of ND/COVID to no. symptoms in females and males

## ---- LoadData
load(file = "data/df_ND_covid.RData")

# Models
## ND (worst depression)
load(file = "models/NDworse_PO_models.RData")
load(file = "models/NDworse_PartPO_models.RData")

## ND (first depression)
load(file = "models/NDFirst_PO_models.RData")
load(file = "models/NDFirst_PartPO_models.RData")

## 2019 bushfires/floods (current depression 1 year later)
load(file = "models/Twenty19BushFlood_PO_models.RData")
load(file = "models/Twenty19BushFlood_PartPO_models.RData")

## COVID (current depression)
load(file = "models/Covid_PO_models.RData")
load(file = "models/Covid_PartPO_models.RData")

## 2019 Bushfires/Floods AND Covid (Current depression)
load(file = "models/Twenty19BushFloodCovidBoth_PO_models.RData")
load(file = "models/Twenty19BushFloodCovidBoth_PartPO_models.RData")
## ----

## ----MalevsFemale

# Function to process data
process_data <- function(model_female, model_male, predictor) {
  female_beta <- coef(summary(model_female))[predictor, "Estimate"]
  female_SE <- coef(summary(model_female))[predictor, "Std. Error"]
  male_beta <- coef(summary(model_male))[predictor, "Estimate"]
  male_SE <- coef(summary(model_male))[predictor, "Std. Error"]
  
  Z <- (female_beta - male_beta) / sqrt((female_SE)^2 + (male_SE)^2)
  p <- 2 * pnorm(abs(Z), mean = 0, sd = 1, lower.tail = FALSE) 
  
  return(data.frame(Predictor = predictor, Z = Z, p = p))
}

# process data for each model
ND_worst_female <- process_data(NDworse_female_PO, NDworse_male_PO, "NDEXPOS_DEPRWAFTERYes")

ND_First_female <- process_data(NDFirst_female_PO, NDFirst_male_PO, "NDEXPOS_DEPRFAFTERYes")

Twenty19BushFlood_female <- process_data(Twenty19BushFlood_female_PO, Twenty19BushFlood_male_PO, "BUSHFLOODHORR_followup1")

Covid_female <- process_data(Covid_female_PO, Covid_male_PO, "COVIDALLBIN_C1C21")

Twenty19BushFloodCovidBoth <- process_data(Twenty19BushFloodCovidBoth_female_PO, Twenty19BushFloodCovidBoth_male_PO, "COVIDALL_BUSHFLOOD_bothYes")


femalevsmale <- ND_worst_female %>% 
  full_join(ND_First_female) %>% 
  full_join(Twenty19BushFlood_female) %>% 
  full_join(Covid_female) %>% 
  full_join(Twenty19BushFloodCovidBoth) %>% 
  mutate(P_adjusted = p.adjust(p, method = "BH", n = 5))

femalevsmale %>% 
  kable()

## ----

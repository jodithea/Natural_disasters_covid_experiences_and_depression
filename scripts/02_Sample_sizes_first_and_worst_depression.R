## CALCULATE SAMPLE SIZES TO DECIDE WHAT ANALYSES TO DO ##

## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----


## ---- SampleSizeFirstWorstDeprSame
# How many people report first and worst depression as the same event

df_ND_covid %>% 
  filter(WORSEAGE_baseline == FIRSTAGE_followup) %>% 
  summarise(n=n()) %>% 
  kable(caption = "No. participants whose first depressive episode was also their worst depressive episode")
## ----
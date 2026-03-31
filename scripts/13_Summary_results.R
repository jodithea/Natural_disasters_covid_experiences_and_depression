# Output tables summarising all results from regression analyses
## ---- LoadData
load(file = "data/NUMSYMPALL_PO_results_df.RData")
load(file = "data/NUMSYMPALL_PO_plotting_df.RData")
## ----


## ---- SummaryTableND
NUMSYMPALL_PO_plotting_df %>% 
  select(Model, Sex, OR, lower_95_CI_of_OR, upper_95_CI_of_OR, "z value", P, P_adjusted) %>% 
  kable(caption = "Results from all regressions - natural disaster exposure or bushfires/flooding or COVID as the predictor")
## ----

## ---- SummaryTablePGS
NUMSYMPALL_PO_results_df %>% 
  filter(rowname == 'PRS_stand_genetic') %>% 
  mutate(P_adjusted = p.adjust(P, method = "BH", n = 15)) %>%
  select(Model, Sex, OR, lower_95_CI_of_OR, upper_95_CI_of_OR, "z value", P, P_adjusted) %>% 
  kable(caption = "Results from all regressions - polygenic score as the predictor")
## ----
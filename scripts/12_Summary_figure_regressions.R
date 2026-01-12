# Create overall figure summarising all results from regression analyses

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

## ---- SummaryData
# Function to process data
process_data <- function(model, sex_name, model_name) {
  CI <- as.data.frame(exp(confint(model))) %>% 
    rownames_to_column() %>% 
    rename('lower_95_CI_of_OR' = "2.5 %",
           'upper_95_CI_of_OR' = "97.5 %")
  
  df <- as.data.frame(coef(summary(model))) %>% 
    rownames_to_column(var = 'rowname') %>% 
    mutate(OR = exp(Estimate)) %>% 
    rename('P' = 'Pr(>|z|)') %>% 
    mutate(Sex = sex_name,
           Model = model_name)
  
  result <- df %>% 
    full_join(CI)
  
  return(result)
}

# Process data for female and male for each model
ND_worst_both <- process_data(NDworse_PO, "Both", "ND_worst")
ND_worst_female <- process_data(NDworse_female_PO, "Female", "ND_worst")
ND_worst_male <- process_data(NDworse_male_PO, "Male", "ND_worst")

ND_First_both <- process_data(NDFirst_PO, "Both", "ND_first")
ND_First_female <- process_data(NDFirst_female_PO, "Female", "ND_first")
ND_First_male <- process_data(NDFirst_male_PO, "Male", "ND_first")

Twenty19BushFlood_both <- process_data(Twenty19BushFlood_PO, "Both", "Twenty19BushFlood")
Twenty19BushFlood_female <- process_data(Twenty19BushFlood_female_PO, "Female", "Twenty19BushFlood")
Twenty19BushFlood_male <- process_data(Twenty19BushFlood_male_PO, "Male", "Twenty19BushFlood")

Covid_both <- process_data(Covid_PO, "Both", "Covid")
Covid_female <- process_data(Covid_female_PO, "Female", "Covid")
Covid_male <- process_data(Covid_male_PO, "Male", "Covid")

Twenty19BushFloodCovidBoth_both <- process_data(Twenty19BushFloodCovidBoth_PO, "Both", "Twenty19BushFloodCovidBoth")
Twenty19BushFloodCovidBoth_female <- process_data(Twenty19BushFloodCovidBoth_female_PO, "Female", "Twenty19BushFloodCovidBoth")
Twenty19BushFloodCovidBoth_male <- process_data(Twenty19BushFloodCovidBoth_male_PO, "Male", "Twenty19BushFloodCovidBoth")


# Combine the results
NUMSYMPALL_PO_results_df <- bind_rows(ND_worst_both, ND_worst_female, ND_worst_male,
                              ND_First_both, ND_First_female, ND_First_male,
                              Twenty19BushFlood_both, Twenty19BushFlood_female, Twenty19BushFlood_male,
                              Covid_both, Covid_female, Covid_male,
                              Twenty19BushFloodCovidBoth_both, Twenty19BushFloodCovidBoth_female, Twenty19BushFloodCovidBoth_male)


# Subset df and put in order for graph
order <- c('NDEXPOS_DEPRWAFTERYes', 
           'NDEXPOS_DEPRFAFTERYes', 
           'BUSHFLOODHORR_followup1', 
           'COVIDALLBIN_C1C21', 
           'COVIDALL_BUSHFLOOD_bothYes')
order <- rev(order)

NUMSYMPALL_PO_plotting_df <- NUMSYMPALL_PO_results_df %>% 
  filter(rowname == 'NDEXPOS_DEPRWAFTERYes' | 
           rowname == 'NDEXPOS_DEPRFAFTERYes' | 
           rowname == 'BUSHFLOODHORR_followup1' | 
           rowname == 'COVIDALLBIN_C1C21' | 
           rowname == 'COVIDALL_BUSHFLOOD_bothYes') %>% 
  mutate(Significance = case_when(
    P >= 0.05 ~ "Non-significant",
    P < 0.05 ~ "Uncorrected (P < 0.05)"
  )) %>% 
  mutate(P_adjusted = p.adjust(P, method = "BH", n = 15)) %>%
  # mutate(P = ifelse(P >= 0.009, sprintf("%.2f", P), sprintf("%.0e", P))) %>% 
  mutate(P_adjusted = ifelse(P_adjusted >= 0.009, sprintf("%.2f", P_adjusted), sprintf("%.0e", P_adjusted))) %>%
  mutate(rowname = factor(rowname, levels = order),
         Sex = factor(Sex, levels = c("Both", "Female", "Male")))

save(NUMSYMPALL_PO_results_df, file = "data/NUMSYMPALL_PO_results_df.RData")
save(NUMSYMPALL_PO_plotting_df, file = "data/NUMSYMPALL_PO_plotting_df.RData")
## ----

## ---- POMainPlot
# Main plot
plot_main <- ggplot(NUMSYMPALL_PO_plotting_df %>% 
                      filter(rowname != 'NDEXPOS_DEPRWAFTERYes'), 
                    aes(x = OR, y = rowname, colour = Sex, shape = Sex)) +
  geom_pointrange(aes(x = OR, xmin = lower_95_CI_of_OR, 
                      xmax = upper_95_CI_of_OR),
                  position = position_dodge(width = -0.5)) +
  geom_text(aes(x = OR, y = rowname, label = P_adjusted), 
            position = position_dodge(width = -0.5), 
            vjust = 0, 
            hjust = -0.5,
            show.legend = FALSE) +
  geom_vline(xintercept = 1,
             colour = "grey",
             linetype = 'dotted') +
  scale_colour_manual(values = c("Both" = "#287d8e",
                                 "Female" = "#eae51a",
                                 "Male" = "#440154")) +
  scale_shape_manual(values = c("Both" = 15,
                                "Female" = 16,
                                "Male" = 17)) +
  scale_y_discrete("",
                   breaks = c('NDEXPOS_DEPRFAFTERYes', 
                              'BUSHFLOODHORR_followup1', 
                              'COVIDALLBIN_C1C21', 
                              'COVIDALL_BUSHFLOOD_bothYes'),
                   labels = c('Any natural disaster \n(first depressive episode)',
                              '2019 bushfires/floods',
                              'COVID-19',
                              '2019 bushfires/floods \nand COVID-19')) +
  scale_x_continuous('Odds Ratio', 
                     lim = c(0.45,4.05), 
                     breaks = c(0.5, 1, 2, 4), 
                     labels = c('0.5', '1', '2', '4'), 
                     trans=scales::log2_trans()) +
  theme_classic() +
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 10, colour = "black", margin = margin(10,0,0,0)),
        axis.title.y = element_text(size = 10, colour = "black", margin = margin(0,10,0,0)),
        legend.text = element_text(size = 8, colour = "black"),
        legend.title = element_text(size = 10, colour = "black"),
        legend.position = "top")

plot_main
## ----

## ---- POSensitivityPlot
# Sensitivity analysis graph of ND with worst depressive episode occurring after 
plot_sensitivity <- ggplot(NUMSYMPALL_PO_plotting_df %>% 
                             filter(rowname == 'NDEXPOS_DEPRWAFTERYes'), 
       aes(x = OR, y = rowname, colour = Sex, shape = Sex)) +
  geom_pointrange(aes(x = OR, xmin = lower_95_CI_of_OR, 
                      xmax = upper_95_CI_of_OR),
                  position = position_dodge(width = -0.5)) +
  geom_text(aes(x = OR, y = rowname, label = P_adjusted), 
            position = position_dodge(width = -0.5), 
            vjust = 0, 
            hjust = -0.5,
            show.legend = FALSE) +
  geom_vline(xintercept = 1,
             colour = "grey",
             linetype = 'dotted') +
  scale_colour_manual(values = c("Both" = "#287d8e",
                                 "Female" = "#eae51a",
                                 "Male" = "#440154")) +
  scale_shape_manual(values = c("Both" = 15,
                                "Female" = 16,
                                "Male" = 17)) +
  scale_y_discrete("",
                   breaks = c('NDEXPOS_DEPRWAFTERYes'),
                   labels = c('Any natural disaster \n(worst depressive episode)')) +
  scale_x_continuous('Odds Ratio', 
                     lim = c(0.45,4.05), 
                     breaks = c(0.5, 1, 2, 4), 
                     labels = c('0.5', '1', '2', '4'), 
                     trans=scales::log2_trans()) +
  theme_classic() +
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 10, colour = "black", margin = margin(10,0,0,0)),
        axis.title.y = element_text(size = 10, colour = "black", margin = margin(0,10,0,0)),
        legend.text = element_text(size = 8, colour = "black"),
        legend.title = element_text(size = 10, colour = "black"),
        legend.position = "top")

plot_sensitivity
## ----

## ---- SaveFigures
ggsave(plot_main, width = 18, height = 21, dpi = 300, unit = "cm", 
       file = "figures/Figure_main_NDfirst_bushflood_covid_bushfloodcovid_both_female_male.png")

ggsave(plot_main, width = 18, height = 21, dpi = 300, unit = "cm", 
       file = "figures/Figure_main_NDfirst_bushflood_covid_bushfloodcovid_both_female_male.pdf",
       device = cairo_pdf)



ggsave(plot_sensitivity, width = 18, height = 8, dpi = 300, unit = "cm", 
       file = "figures/Figure_sensitivity_NDworst_both_female_male.png")

ggsave(plot_sensitivity, width = 18, height = 8, dpi = 300, unit = "cm", 
       file = "figures/Figure_sensitivity_NDworst_both_female_male.pdf",
       device = cairo_pdf)
## ----


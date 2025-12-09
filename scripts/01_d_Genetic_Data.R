# Add PRS, genetic sex, 10 PCs

## ---- LoadData
load(file = "data/phen_data.RData")
load(file = "data/AGDS_relatedness.RData")

AGDS_sex_PCs <- read.table(file = "data/AGDS_all4surveys_phenotype_IDs_remapper_PCs.txt", header = T)
AGDS_PRS <- read.table(file = "/path_one/Reference/PRS/MDD_Adams2025/Depression_Adams_2025_LOO_SBRC.profile", header = T)
## ----

## ---- CombinePhenotypeSexPCs
# Format genetic sex and PCs data
AGDS_sex_PCs <- AGDS_sex_PCs %>% 
  mutate(SEX = case_when(
    SEX == 1 ~ "Male",
    SEX == 2 ~ "Female" 
  ),
  SEX = factor(SEX, levels = c("Male", "Female"))) %>% 
  rename(FINALID = SUPPLIED_ID) %>% 
  select(FINALID, SEX, contains("PC"), IID) %>% 
  rename_with(~ paste(., "genetic", sep = "_"), -FINALID)

# Format PRS data
AGDS_PRS <- AGDS_PRS %>% 
  rename(IID_genetic = IID,
         PRS_genetic = SCORESUM) %>% 
  select(IID_genetic, PRS_genetic)

# Merge genetic sex + PCs data with PRS data
AGDS_sex_PCs_PRS <- AGDS_sex_PCs %>% 
  left_join(AGDS_PRS)

# Merge genetic data (sex, PCs, PRS) with phenotype data (all 4 surveys combined)
phen_gen_PRS_data <- AGDS_sex_PCs_PRS %>% 
  full_join(phen_data)
  
  
save(phen_gen_PRS_data, file = "data/phen_gen_PRS_data.RData")
## ----


## ---- RemoveRelated
AGDS_remove_related_list <- AGDS_relatedness %>% 
  filter(PI_HAT > 0.2) %>% 
  select(IID1) %>% 
  distinct()
save(AGDS_remove_related_list, file = "data/AGDS_remove_related_list.RData")

# Combine phen_gen_PRS_data and remove_list so new dataset doesn't include IIDs in remove_list
phen_gen_PRS_data_relatedremoved <- phen_gen_PRS_data %>% 
  anti_join(AGDS_remove_related_list, by = c('IID_genetic' = "IID1"))


save(phen_gen_PRS_data_relatedremoved, file = "data/phen_gen_PRS_data_relatedremoved.RData")
## ----

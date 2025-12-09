## ---- LoadData
load(file = "data/phen_data.RData")
## ----

## ---- Remapper
# make list of AGDS IDs to calculate relatedness and PRS

# Save AGDS IDs
# One of the IDs has a bracket at the end "MDD12843)". There is another ID "MDD12843" so removed "MDD12843)"
AGDS_phenotype_IDs <- phen_data %>% 
  select(FINALID) %>% 
  rename(QID = FINALID) %>% 
  filter(QID != "MDD12843)")

# save data for remapper
write.table(AGDS_phenotype_IDs, "data/AGDS_all4surveys_phenotype_IDs.txt", sep = "\t", row.names = FALSE, quote = FALSE)
# Then run through remapper for FID and IID
## ----
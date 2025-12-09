# Look at relatedness of individuals

## ---- LoadData
AGDS_relatedness <- read.table(file = "/path_one/Reference/Relatedness/AGDS_relatedness.genome", header = T)

save(AGDS_relatedness, file = "data/AGDS_relatedness.RData")
## ----

## ---- Relatedness
hist(AGDS_relatedness$PI_HAT, breaks = 100)

# Number with pihat > 0.25 (2nd degree relatedness)
AGDS_relatedness %>% 
  filter(PI_HAT > 0.25) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. participants with relatedness pihat > 0.25 (2nd degree)")

# Number with pihat > 0.20 (2nd degree relatedness but with lower cut-off to account for variability)
AGDS_relatedness %>% 
  filter(PI_HAT > 0.20) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. participants with relatedness pihat > 0.20 (2nd degree with lower cut-off to account for variability)")


# Number with pihat > 0.125 (3rd degree relatedness)
AGDS_relatedness %>% 
  filter(PI_HAT > 0.125) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. participants with relatedness pihat > 0.125 (3rd degree)")

# Number with pihat > 0.10 (3rd degree relatedness but with lower cut-off to account for variability)
AGDS_relatedness %>% 
  filter(PI_HAT > 0.10) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. participants with relatedness pihat > 0.10 (3rd degree with lower cut-off to account for variability)")

# Some individuals are paired with a 2nd participant more than once
AGDS_relatedness %>%
  group_by(IID1) %>%
  summarise(num_rows = n()) %>%
  filter(num_rows > 1) %>%
  summarise(num_individuals = n()) %>% 
  kable(caption = "No. of participants in IID1 that are in more than one pairing")

AGDS_relatedness %>%
  group_by(IID2) %>%
  summarise(num_rows = n()) %>%
  filter(num_rows > 1) %>%
  summarise(num_individuals = n()) %>% 
  kable(caption = "No. of participants in IID2 that are in more than one pairing")
## ----

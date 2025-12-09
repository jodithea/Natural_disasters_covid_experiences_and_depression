# Check PRS Data

## ---- LoadData
load(file = "data/phen_gen_PRS_data_relatedremoved.RData")
## ----

## ---- StandardisePRS
# standardise PRS values within data using
df_ND_covid <- phen_gen_PRS_data_relatedremoved %>% 
  mutate(PRS_stand_genetic = scale(PRS_genetic, center = TRUE, scale = TRUE))

save(df_ND_covid, file = "data/df_ND_covid.RData")
## ----

## ---- PRSDistribution
a <- ggplot(df_ND_covid, aes(x = PRS_genetic)) +
  geom_histogram()

b <- ggplot(df_ND_covid, aes(x = PRS_stand_genetic)) +
  geom_histogram()

a + b

df_ND_covid %>%
  summarise(mean_PRS = mean(PRS_genetic, na.rm = TRUE),
            mean_PRS_standardised = mean(PRS_stand_genetic, na.rm = TRUE),
            sd_PRS = sd(PRS_genetic, na.rm = TRUE),
            sd_PRS_standardised = sd(PRS_stand_genetic, na.rm = TRUE)) %>% 
  kable()

df_ND_covid %>% 
  group_by(SEX_genetic) %>%
  drop_na(SEX_genetic) %>% 
  summarise(mean_PRS = mean(PRS_genetic, na.rm = TRUE),
            mean_PRS_standardised = mean(PRS_stand_genetic, na.rm = TRUE),
            sd_PRS = sd(PRS_genetic, na.rm = TRUE),
            sd_PRS_standardised = sd(PRS_stand_genetic, na.rm = TRUE)) %>% 
  kable()
## ----

## ---- CheckPRSCorrelation
# No. symptoms during worst episode (original survey)
model_NUMSYMPALL_baseline <- lm(NUMSYMPALL_baseline ~ PRS_stand_genetic + SEX_genetic + AGE_baseline + PC1_genetic + PC2_genetic + PC3_genetic + PC4_genetic,
                    data = df_ND_covid)
summary(model_NUMSYMPALL_baseline)

# No. symptoms during first episode (follow-up survey)
model_NUMSYMPALL_followup <- lm(NUMSYMPALL_followup ~ PRS_stand_genetic + SEX_genetic + AGE_followup + PC1_genetic + PC2_genetic + PC3_genetic + PC4_genetic,
                         data = df_ND_covid)
summary(model_NUMSYMPALL_followup)

# No. symptoms currently (covid wave 1 survey)
model_NUMSYMPALL_covid1 <- lm(NUMSYMPALL_covid1 ~ PRS_stand_genetic + SEX_genetic + AGE_covid1 + PC1_genetic + PC2_genetic + PC3_genetic + PC4_genetic,
                          data = df_ND_covid)
summary(model_NUMSYMPALL_covid1)

# No. symptoms currently (covid wave 2 survey)
model_NUMSYMPALL_covid2 <- lm(NUMSYMPALL_covid2 ~ PRS_stand_genetic + SEX_genetic + AGE_covid2 + PC1_genetic + PC2_genetic + PC3_genetic + PC4_genetic,
                          data = df_ND_covid)
summary(model_NUMSYMPALL_covid2)

# Duration of worst episode
model_TIMEWKS_baseline <- lm(TIMEWKS_baseline ~ PRS_stand_genetic + SEX_genetic + AGE_baseline + PC1_genetic + PC2_genetic + PC3_genetic + PC4_genetic,
                         data = df_ND_covid)
summary(model_TIMEWKS_baseline)

# Age of first episode
model_FIRSTAGE_followup <- lm(FIRSTAGE_followup ~ PRS_stand_genetic + SEX_genetic + AGE_baseline + PC1_genetic + PC2_genetic + PC3_genetic + PC4_genetic,
                      data = df_ND_covid)
summary(model_FIRSTAGE_followup)
## ----


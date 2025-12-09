# Explore effect of 2019 bushfires/floods experience on no. depressive symptoms during current episode measured 1 year later

## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----

## ---- Twenty19BushFloodSampleSize
df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid1, SEX_genetic, AGE_followup, matches("^PC.*_genetic$"), BUSHFLOODHORR_followup, PRS_stand_genetic) %>% 
  count(BUSHFLOODHORR_followup) %>%
  kable(caption = "No. participants experienced any frightening, horrible or traumatic experiences related to the bushfires and flooding over the past year")

df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid1, SEX_genetic, AGE_followup, matches("^PC.*_genetic$"), BUSHFLOODHORR_followup, PRS_stand_genetic) %>% 
  filter(SEX_genetic == "Female") %>% 
  count(BUSHFLOODHORR_followup) %>%
  kable(caption = "No. female participants experienced any frightening, horrible or traumatic experiences related to the bushfires and flooding over the past year")

df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid1, SEX_genetic, AGE_followup, matches("^PC.*_genetic$"), BUSHFLOODHORR_followup, PRS_stand_genetic) %>% 
  filter(SEX_genetic == "Male") %>% 
  count(BUSHFLOODHORR_followup) %>%
  kable(caption = "No. male participants experienced any frightening, horrible or traumatic experiences related to the bushfires and flooding over the past year")
## ----

## ---- Twenty19BushFloodDistribution
ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid1) & !is.na(df_ND_covid$SEX_genetic),], 
             aes(y = NUMSYMPALL_covid1, colour = SEX_genetic)) +
  geom_boxplot()


p1 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid1) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           !is.na(df_ND_covid$BUSHFLOODHORR_followup), ], 
             aes (x = NUMSYMPALL_covid1, colour = BUSHFLOODHORR_followup, fill = BUSHFLOODHORR_followup)) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  ggtitle("Both sexes")

p2 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid1) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           (df_ND_covid$SEX_genetic == "Female") & 
                           !is.na(df_ND_covid$BUSHFLOODHORR_followup), ], 
             aes (x = NUMSYMPALL_covid1, colour = BUSHFLOODHORR_followup, fill = BUSHFLOODHORR_followup)) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  ggtitle("Female")

p3 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid1) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           (df_ND_covid$SEX_genetic == "Male") & 
                           !is.na(df_ND_covid$BUSHFLOODHORR_followup), ], 
             aes (x = NUMSYMPALL_covid1, colour = BUSHFLOODHORR_followup, fill = BUSHFLOODHORR_followup)) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  ggtitle("Male")

p1 / (p2 | p3)



p1 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid1_cat) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           !is.na(df_ND_covid$BUSHFLOODHORR_followup), ], 
             aes (x = NUMSYMPALL_covid1_cat, colour = BUSHFLOODHORR_followup, fill = BUSHFLOODHORR_followup)) +
  geom_bar(position = "dodge") +
  ggtitle("Both sexes")

p2 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid1_cat) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           (df_ND_covid$SEX_genetic == "Female") & 
                           !is.na(df_ND_covid$BUSHFLOODHORR_followup), ], 
             aes (x = NUMSYMPALL_covid1_cat, colour = BUSHFLOODHORR_followup, fill = BUSHFLOODHORR_followup)) +
  geom_bar(position = "dodge") +
  ggtitle("Female")

p3 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid1_cat) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           (df_ND_covid$SEX_genetic == "Male") & 
                           !is.na(df_ND_covid$BUSHFLOODHORR_followup), ], 
             aes (x = NUMSYMPALL_covid1_cat, colour = BUSHFLOODHORR_followup, fill = BUSHFLOODHORR_followup)) +
  geom_bar(position = "dodge") +
  ggtitle("Male")

p1 / (p2 | p3)
## ----

## ---- Twenty19BushFloodBoxplot
ggplot(df_ND_covid[!is.na(df_ND_covid$SEX_genetic),], aes(x = as.factor(BUSHFLOODHORR_followup), y = NUMSYMPALL_covid1, colour = SEX_genetic)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.4, jitter.height = 0.4), alpha = 0.1) +
  stat_n_text() +
  labs(x = "Experienced a ND",  
       y = "Number of depressive symptoms") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  theme_classic()

ggplot(df_ND_covid[!is.na(df_ND_covid$SEX_genetic) & !is.na(df_ND_covid$NUMSYMPALL_covid1_cat),], aes(x = as.factor(BUSHFLOODHORR_followup), fill = NUMSYMPALL_covid1_cat)) +
  geom_bar(position = "fill") +
  # stat_n_text() +
  labs(x = "Experienced a ND",  
       y = "Number of depressive symptoms") +
  facet_wrap(~ SEX_genetic) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme_classic()
## ----

# Explore effect of Natural disaster experience on no. depressive symptoms if worst depressive episode occurred after the Natural disaster

## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----

## ---- NDWorseSampleSize
df_ND_covid %>% 
  drop_na(NUMSYMPALL_baseline, SEX_genetic, AGE_baseline, matches("^PC.*_genetic$"), NDEXPOS_DEPRWAFTER, PRS_stand_genetic) %>% 
  count(NDEXPOS_DEPRWAFTER) %>%
  kable(caption = "No. participants exposed to a natural disaster \n (Yes = ND happened to me, witnessed it, learned about it or part of my job - only if **worst** depression occurred after ND experience \nNo = never experienced ND)")

df_ND_covid %>% 
  drop_na(NUMSYMPALL_baseline, SEX_genetic, AGE_baseline, matches("^PC.*_genetic$"), NDEXPOS_DEPRWAFTER, PRS_stand_genetic) %>% 
  filter(SEX_genetic == "Female") %>% 
  count(NDEXPOS_DEPRWAFTER) %>%
  kable(caption = "No. female participants exposed to a natural disaster \n (Yes = ND happened to me, witnessed it, learned about it or part of my job - only if **worst** depression occurred after ND experience \nNo = never experienced ND)")

df_ND_covid %>% 
  drop_na(NUMSYMPALL_baseline, SEX_genetic, AGE_baseline, matches("^PC.*_genetic$"), NDEXPOS_DEPRWAFTER, PRS_stand_genetic) %>% 
  filter(SEX_genetic == "Male") %>% 
  count(NDEXPOS_DEPRWAFTER) %>%
  kable(caption = "No. male participants exposed to a natural disaster \n (Yes = ND happened to me, witnessed it, learned about it or part of my job - only if **worst** depression occurred after ND experience \nNo = never experienced ND)")
## ----

## ---- NDWorseDistribution
ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_baseline) & !is.na(df_ND_covid$SEX_genetic),], 
             aes(y = NUMSYMPALL_baseline, colour = SEX_genetic)) +
  geom_boxplot()


ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_baseline) & !is.na(df_ND_covid$SEX_genetic),], 
             aes (x = NUMSYMPALL_baseline, colour = SEX_genetic, fill = SEX_genetic)) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9))


p1 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_baseline) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           !is.na(df_ND_covid$NDEXPOS_DEPRWAFTER), ], 
             aes (x = NUMSYMPALL_baseline, colour = NDEXPOS_DEPRWAFTER, fill = NDEXPOS_DEPRWAFTER)) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9)) +
  ggtitle("Both sexes")

p2 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_baseline) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           (df_ND_covid$SEX_genetic == "Female") & 
                           !is.na(df_ND_covid$NDEXPOS_DEPRWAFTER), ], 
             aes (x = NUMSYMPALL_baseline, colour = NDEXPOS_DEPRWAFTER, fill = NDEXPOS_DEPRWAFTER)) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9)) +
  ggtitle("Female")

p3 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_baseline) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           (df_ND_covid$SEX_genetic == "Male") & 
                           !is.na(df_ND_covid$NDEXPOS_DEPRWAFTER), ], 
             aes (x = NUMSYMPALL_baseline, colour = NDEXPOS_DEPRWAFTER, fill = NDEXPOS_DEPRWAFTER)) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9)) +
  ggtitle("Male")



p1 / (p2 | p3)



p1 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_baseline_cat) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           !is.na(df_ND_covid$NDEXPOS_DEPRWAFTER), ], 
             aes (x = NUMSYMPALL_baseline_cat, colour = NDEXPOS_DEPRWAFTER, fill = NDEXPOS_DEPRWAFTER)) +
  geom_bar(position = "dodge") +
  ggtitle("Both sexes")


p2 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_baseline_cat) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           (df_ND_covid$SEX_genetic == "Female") & 
                           !is.na(df_ND_covid$NDEXPOS_DEPRWAFTER), ], 
             aes (x = NUMSYMPALL_baseline_cat, colour = NDEXPOS_DEPRWAFTER, fill = NDEXPOS_DEPRWAFTER)) +
  geom_bar(position = "dodge") +
  ggtitle("Female")

p3 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_baseline_cat) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           (df_ND_covid$SEX_genetic == "Male") & 
                           !is.na(df_ND_covid$NDEXPOS_DEPRWAFTER), ], 
             aes (x = NUMSYMPALL_baseline_cat, colour = NDEXPOS_DEPRWAFTER, fill = NDEXPOS_DEPRWAFTER)) +
  geom_bar(position = "dodge") +
  ggtitle("Male")



p1 / (p2 | p3)
## ----

## ---- NDWorseBoxplot
ggplot(df_ND_covid[!is.na(df_ND_covid$SEX_genetic),], aes(x = as.factor(NDEXPOS_DEPRWAFTER), y = NUMSYMPALL_baseline, colour = SEX_genetic)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.4, jitter.height = 0.4), alpha = 0.1) +
  stat_n_text() +
  labs(x = "Experienced a ND",  
       y = "Number of depressive symptoms") +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme_classic()


ggplot(df_ND_covid[!is.na(df_ND_covid$SEX_genetic) & !is.na(df_ND_covid$NUMSYMPALL_baseline_cat),], aes(x = as.factor(NDEXPOS_DEPRWAFTER), fill = NUMSYMPALL_baseline_cat)) +
  geom_bar(position = "fill") +
  # stat_n_text() +
  labs(x = "Experienced a ND",  
       y = "Number of depressive symptoms") +
  facet_wrap(~ SEX_genetic) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme_classic()


## ----

# Explore effect of 2019 bushfires/floods AND covid experience on no. depressive symptoms during current episode 

## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----

## ---- 19BushFloodCovidBothSampleSize
df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid2, SEX_genetic, AGE_covid2, matches("^PC.*_genetic$"), COVIDALL_BUSHFLOOD_both, PRS_stand_genetic) %>% 
  count(COVIDALL_BUSHFLOOD_both) %>%
  kable(caption = "No. participants that experienced any frightening, horrible or traumatic experiences related to COVID (wave 1 and/or 2) and the bushfires and flooding over the past year \n (Yes = yes to covid wave 1 and/or 2 AND yes to bushfires/floods, No = no to covid wave 1 and 2 and bushfires/floods")

df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid2, SEX_genetic, AGE_covid2, matches("^PC.*_genetic$"), COVIDALL_BUSHFLOOD_both, PRS_stand_genetic) %>% 
  filter(SEX_genetic == "Female") %>% 
  count(COVIDALL_BUSHFLOOD_both) %>%
  kable(caption = "No. female participants that experienced any frightening, horrible or traumatic experiences related to COVID (wave 1 and/or 2) and the bushfires and flooding over the past year \n (Yes = yes to covid wave 1 and/or 2 AND yes to bushfires/floods, No = no to covid wave 1 and 2 and bushfires/floods")

df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid2, SEX_genetic, AGE_covid2, matches("^PC.*_genetic$"), COVIDALL_BUSHFLOOD_both, PRS_stand_genetic) %>% 
  filter(SEX_genetic == "Male") %>% 
  count(COVIDALL_BUSHFLOOD_both) %>%
  kable(caption = "No. male participants that experienced any frightening, horrible or traumatic experiences related to COVID (wave 1 and/or 2) and the bushfires and flooding over the past year \n (Yes = yes to covid wave 1 and/or 2 AND yes to bushfires/floods, No = no to covid wave 1 and 2 and bushfires/floods")
## ----

## ---- 19BushFloodCovidBothDistribution
ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid2) & !is.na(df_ND_covid$SEX_genetic),], 
             aes(y = NUMSYMPALL_covid2, colour = SEX_genetic)) +
  geom_boxplot()


p1 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid2) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           !is.na(df_ND_covid$COVIDALL_BUSHFLOOD_both), ], 
             aes (x = NUMSYMPALL_covid2, colour = as.factor(COVIDALL_BUSHFLOOD_both), fill = as.factor(COVIDALL_BUSHFLOOD_both))) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  ggtitle("Both sexes")


p2 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid2) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           (df_ND_covid$SEX_genetic == "Female") & 
                           !is.na(df_ND_covid$COVIDALL_BUSHFLOOD_both), ], 
             aes (x = NUMSYMPALL_covid2, colour = as.factor(COVIDALL_BUSHFLOOD_both), fill = as.factor(COVIDALL_BUSHFLOOD_both))) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  ggtitle("Female")

p3 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid2) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           (df_ND_covid$SEX_genetic == "Male") & 
                           !is.na(df_ND_covid$COVIDALL_BUSHFLOOD_both), ], 
             aes (x = NUMSYMPALL_covid2, colour = as.factor(COVIDALL_BUSHFLOOD_both), fill = as.factor(COVIDALL_BUSHFLOOD_both))) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  ggtitle("Male")

p1 / (p2 | p3)

p1 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid2_cat) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           !is.na(df_ND_covid$COVIDALL_BUSHFLOOD_both), ], 
             aes (x = NUMSYMPALL_covid2_cat, colour = COVIDALL_BUSHFLOOD_both, fill = COVIDALL_BUSHFLOOD_both)) +
  geom_bar(position = "dodge") +
  ggtitle("Both sexes")

p2 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid2_cat) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           (df_ND_covid$SEX_genetic == "Female") & 
                           !is.na(df_ND_covid$COVIDALL_BUSHFLOOD_both), ], 
             aes (x = NUMSYMPALL_covid2_cat, colour = COVIDALL_BUSHFLOOD_both, fill = COVIDALL_BUSHFLOOD_both)) +
  geom_bar(position = "dodge") +
  ggtitle("Female")

p3 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid2_cat) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           (df_ND_covid$SEX_genetic == "Male") & 
                           !is.na(df_ND_covid$COVIDALL_BUSHFLOOD_both), ], 
             aes (x = NUMSYMPALL_covid2_cat, colour = COVIDALL_BUSHFLOOD_both, fill = COVIDALL_BUSHFLOOD_both)) +
  geom_bar(position = "dodge") +
  ggtitle("Male")

p1 / (p2 | p3)
## ----

## ---- 19BushFloodCovidBothBoxplot
ggplot(df_ND_covid[!is.na(df_ND_covid$SEX_genetic),], aes(x = as.factor(COVIDALL_BUSHFLOOD_both), y = NUMSYMPALL_covid2, colour = SEX_genetic)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.4, jitter.height = 0.4), alpha = 0.1) +
  stat_n_text() +
  labs(x = "Experienced a ND",  
       y = "Number of depressive symptoms") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  theme_classic()

ggplot(df_ND_covid[!is.na(df_ND_covid$SEX_genetic) & !is.na(df_ND_covid$NUMSYMPALL_covid2_cat),], aes(x = as.factor(COVIDALL_BUSHFLOOD_both), fill = NUMSYMPALL_covid2_cat)) +
  geom_bar(position = "fill") +
  # stat_n_text() +
  labs(x = "Experienced a horrible or traumatic 2019 bushfires/floods and/or COVID experience",  
       y = "Number of depressive symptoms") +
  facet_wrap(~ SEX_genetic) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme_classic()
## ----

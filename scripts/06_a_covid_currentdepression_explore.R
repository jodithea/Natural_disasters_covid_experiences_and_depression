# Explore effect of covid experience on no. depressive symptoms during current episode 

## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----

## ---- CovidSampleSize
df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid2, SEX_genetic, AGE_covid2, matches("^PC.*_genetic$"), COVIDALLBIN_C1C2, PRS_stand_genetic) %>% 
  count(COVIDALLBIN_C1C2) %>%
  kable(caption = "No. participants that experienced any frightening, horrible or traumatic experiences related to COVID \n Yes = happened to me, witnessed it, learned about it or part of my job in wave 1 and/or wave 2, No = No in wave 1 and wave 2")

df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid2, SEX_genetic, AGE_covid2, matches("^PC.*_genetic$"), COVIDALLBIN_C1C2, PRS_stand_genetic) %>% 
  filter(SEX_genetic == "Female") %>% 
  count(COVIDALLBIN_C1C2) %>%
  kable(caption = "No. female participants that experienced any frightening, horrible or traumatic experiences related to COVID \n Yes = happened to me, witnessed it, learned about it or part of my job in wave 1 and/or wave 2, No = No in wave 1 and wave 2")

df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid2, SEX_genetic, AGE_covid2, matches("^PC.*_genetic$"), COVIDALLBIN_C1C2, PRS_stand_genetic) %>% 
  filter(SEX_genetic == "Male") %>% 
  count(COVIDALLBIN_C1C2) %>%
  kable(caption = "No. male participants that experienced any frightening, horrible or traumatic experiences related to COVID \n Yes = happened to me, witnessed it, learned about it or part of my job in wave 1 and/or wave 2, No = No in wave 1 and wave 2")
## ----

## ---- CovidDistribution
ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid2) & !is.na(df_ND_covid$SEX_genetic),], 
             aes(y = NUMSYMPALL_covid2, colour = SEX_genetic)) +
  geom_boxplot()

p1 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid2) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           !is.na(df_ND_covid$COVIDALLBIN_C1C2), ], 
             aes (x = NUMSYMPALL_covid2, colour = as.factor(COVIDALLBIN_C1C2), fill = as.factor(COVIDALLBIN_C1C2))) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  ggtitle("Both sexes")

p2 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid2) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           (df_ND_covid$SEX_genetic == "Female") & 
                           !is.na(df_ND_covid$COVIDALLBIN_C1C2), ], 
             aes (x = NUMSYMPALL_covid2, colour = as.factor(COVIDALLBIN_C1C2), fill = as.factor(COVIDALLBIN_C1C2))) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  ggtitle("Female")

p3 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid2) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           (df_ND_covid$SEX_genetic == "Male") & 
                           !is.na(df_ND_covid$COVIDALLBIN_C1C2), ], 
             aes (x = NUMSYMPALL_covid2, colour = as.factor(COVIDALLBIN_C1C2), fill = as.factor(COVIDALLBIN_C1C2))) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  ggtitle("Male")

p1 / (p2 | p3)


p1 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid2_cat) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           !is.na(df_ND_covid$COVIDALLBIN_C1C2), ], 
             aes (x = NUMSYMPALL_covid2_cat, colour = COVIDALLBIN_C1C2, fill = COVIDALLBIN_C1C2)) +
  geom_bar(position = "dodge") +
  ggtitle("Both sexes")

p2 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid2_cat) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           (df_ND_covid$SEX_genetic == "Female") & 
                           !is.na(df_ND_covid$COVIDALLBIN_C1C2), ], 
             aes (x = NUMSYMPALL_covid2_cat, colour = COVIDALLBIN_C1C2, fill = COVIDALLBIN_C1C2)) +
  geom_bar(position = "dodge") +
  ggtitle("Female")

p3 <- ggplot(df_ND_covid[!is.na(df_ND_covid$NUMSYMPALL_covid2_cat) & 
                           !is.na(df_ND_covid$SEX_genetic) & 
                           (df_ND_covid$SEX_genetic == "Male") & 
                           !is.na(df_ND_covid$COVIDALLBIN_C1C2), ], 
             aes (x = NUMSYMPALL_covid2_cat, colour = COVIDALLBIN_C1C2, fill = COVIDALLBIN_C1C2)) +
  geom_bar(position = "dodge") +
  ggtitle("Male")

p1 / (p2 | p3)
## ----

## ---- CovidBoxplot
ggplot(df_ND_covid[!is.na(df_ND_covid$SEX_genetic),], aes(x = as.factor(COVIDALLBIN_C1C2), y = NUMSYMPALL_covid2, colour = SEX_genetic)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.4, jitter.height = 0.4), alpha = 0.1) +
  stat_n_text() +
  labs(x = "Experienced a horrible or traumatic COVID experience",  
       y = "Number of depressive symptoms") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  theme_classic()

ggplot(df_ND_covid[!is.na(df_ND_covid$SEX_genetic) & !is.na(df_ND_covid$NUMSYMPALL_covid2_cat),], aes(x = as.factor(COVIDALLBIN_C1C2), fill = NUMSYMPALL_covid2_cat)) +
  geom_bar(position = "fill") +
  # stat_n_text() +
  labs(x = "Experienced a horrible or traumatic COVID experience",  
       y = "Number of depressive symptoms") +
  facet_wrap(~ SEX_genetic) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme_classic()
## ----

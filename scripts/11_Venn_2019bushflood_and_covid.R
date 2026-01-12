# Create venn diagram comparing people who had a horrible experience in 2019 bushfires/floods and/or covid

## ---- LoadData
load(file = "data/df_ND_covid.RData")
## ----

## ---- Venn
# Venn diagram for both sexes

# Data: sizes of each group
# Bushfire/flood exclusive
one <- df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid2, SEX_genetic, AGE_covid2, matches("^PC.*_genetic$"), COVIDALL_BUSHFLOOD, PRS_stand_genetic) %>% 
  filter(COVIDALL_BUSHFLOOD == "Bushfire/flood only") %>% 
  nrow() 
# COVID exclusive
two <- df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid2, SEX_genetic, AGE_covid2, matches("^PC.*_genetic$"), COVIDALL_BUSHFLOOD, PRS_stand_genetic) %>% 
  filter(COVIDALL_BUSHFLOOD == "COVID only") %>% 
  nrow()
# Both bushfire/flood and COVID
overlap <- df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid2, SEX_genetic, AGE_covid2, matches("^PC.*_genetic$"), COVIDALL_BUSHFLOOD, PRS_stand_genetic) %>% 
  filter(COVIDALL_BUSHFLOOD == "Bushfire/flood and COVID") %>% 
  nrow()  
# None experiences
none <- df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid2, SEX_genetic, AGE_covid2, matches("^PC.*_genetic$"), COVIDALL_BUSHFLOOD, PRS_stand_genetic) %>% 
  filter(COVIDALL_BUSHFLOOD == "No") %>% 
  nrow()  

# Calculate circle areas
area_one <- one + overlap
area_two <- two + overlap

# Convert areas to radii (since area = π * r^2, radius = sqrt(area / π))
r_1 <- sqrt(area_one / pi)
r_2 <- sqrt(area_two / pi)

# Calculate the proportional overlap distance
overlap_ratio <- overlap / min(area_one, area_two)
d <- (r_1 + r_2) * (1 - overlap_ratio / 2)  # Adjust overlap distance

# Create circle data with calculated positions
circle_data <- data.frame(
  x = c(0, d),  # x-coordinates for each circle
  y = c(0, 0),  # Keep y-coordinates the same for both circles
  radius = c(r_1, r_2),
  group = c("bushflood", "covid")
)

# Plot the Euler diagram with numbers positioned correctly
venn_bushflood_covid <- ggplot(circle_data) +
  geom_circle(aes(x0 = x, y0 = y, r = radius, fill = group), alpha = 0.6, color = "black") +
  scale_fill_manual(values = c("#20a386", "#287d8e")) +
  coord_fixed() +
  # Add labels inside each circle and in the overlapping section
  # bushfires/floods only
  annotate("text",
           x = circle_data$x[1] - r_1 / 2,
           y = circle_data$y[1],
           label = one,
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  annotate("text",
           x = circle_data$x[1] - r_1 / 2,
           y = circle_data$y[1] - 2,
           label = paste0(
             round(one/(one + two + overlap + none)*100,2),
             "%"),
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  # COVID only
  annotate("text",
           x = circle_data$x[2] + r_2 / 2,
           y = circle_data$y[2],
           label = two,
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  annotate("text",
           x = circle_data$x[2] + r_2 / 2,
           y = circle_data$y[2] - 2,
           label = paste0(
             round(two/(one + two + overlap + none)*100,2),
             "%"),
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  # Both bushfires/floods and covid
  annotate("text",
           x = mean(circle_data$x) - 3.5,
           y = circle_data$y[1],
           label = overlap,
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  annotate("text",
           x = mean(circle_data$x) - 3.5,
           y = circle_data$y[1] - 2,
           label = paste0(
             round(overlap/(one + two + overlap + none)*100,2),
             "%"),
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  # Oustide of circles for no. experiencing none
  annotate("text",
           x = mean(circle_data$x) - 3.5,         
           y = -max(r_1, r_2) - 2,         
           label = none,
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  annotate("text",
           x = mean(circle_data$x) - 3.5,         
           y = -max(r_1, r_2) - 4,         
           label = paste0(
             round(none/(one + two + overlap + none)*100,2),
             "%"),
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  # Add labels above the circles
  annotate("text",
           x = circle_data$x[1],
           y = circle_data$y[1] + r_1 + 5,
           label = "Bushfires/floods",
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  annotate("text",
           x = circle_data$x[2],
           y = circle_data$y[2] + r_2 + 5,
           label = "COVID-19",
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  ggtitle("Both sexes") +
  theme_classic() +
  theme(text = element_text(family = "Arial"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 10))

# venn_bushflood_covid



# Venn diagram for females

# Data: sizes of each group
# Bushfire/flood exclusive
one <- df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid2, SEX_genetic, AGE_covid2, matches("^PC.*_genetic$"), COVIDALL_BUSHFLOOD, PRS_stand_genetic) %>% 
  filter(SEX_genetic == "Female") %>% 
  filter(COVIDALL_BUSHFLOOD == "Bushfire/flood only") %>% 
  nrow() 
# COVID exclusive
two <- df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid2, SEX_genetic, AGE_covid2, matches("^PC.*_genetic$"), COVIDALL_BUSHFLOOD, PRS_stand_genetic) %>% 
  filter(SEX_genetic == "Female") %>%
  filter(COVIDALL_BUSHFLOOD == "COVID only") %>% 
  nrow()
# Both bushfire/flood and COVID
overlap <- df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid2, SEX_genetic, AGE_covid2, matches("^PC.*_genetic$"), COVIDALL_BUSHFLOOD, PRS_stand_genetic) %>% 
  filter(SEX_genetic == "Female") %>%
  filter(COVIDALL_BUSHFLOOD == "Bushfire/flood and COVID") %>% 
  nrow()  
# None experiences
none <- df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid2, SEX_genetic, AGE_covid2, matches("^PC.*_genetic$"), COVIDALL_BUSHFLOOD, PRS_stand_genetic) %>% 
  filter(SEX_genetic == "Female") %>%
  filter(COVIDALL_BUSHFLOOD == "No") %>% 
  nrow()  

# Calculate circle areas
area_one <- one + overlap
area_two <- two + overlap

# Convert areas to radii (since area = π * r^2, radius = sqrt(area / π))
r_1 <- sqrt(area_one / pi)
r_2 <- sqrt(area_two / pi)

# Calculate the proportional overlap distance
overlap_ratio <- overlap / min(area_one, area_two)
d <- (r_1 + r_2) * (1 - overlap_ratio / 2)  # Adjust overlap distance

# Create circle data with calculated positions
circle_data <- data.frame(
  x = c(0, d),  # x-coordinates for each circle
  y = c(0, 0),  # Keep y-coordinates the same for both circles
  radius = c(r_1, r_2),
  group = c("bushflood", "covid")
)

# Plot the Euler diagram with numbers positioned correctly
venn_bushflood_covid_female <- ggplot(circle_data) +
  geom_circle(aes(x0 = x, y0 = y, r = radius, fill = group), alpha = 0.6, color = "black") +
  scale_fill_manual(values = c("#eae51a", "#B8DE29FF")) +
  coord_fixed() +
  # Add labels inside each circle and in the overlapping section
  # Bushfires/floods only
  annotate("text",
           x = circle_data$x[1] - r_1 / 2 + 1,
           y = circle_data$y[1],
           label = one,
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  annotate("text",
           x = circle_data$x[1] - r_1 / 2 + 1,
           y = circle_data$y[1] - 3,
           label = paste0(
             round(one/(one + two + overlap + none)*100,2),
             "%"),
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  # COVID only
  annotate("text",
           x = circle_data$x[2] + r_2 / 2,
           y = circle_data$y[2],
           label = two,
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  annotate("text",
           x = circle_data$x[2] + r_2 / 2,
           y = circle_data$y[2] - 3,
           label = paste0(
             round(two/(one + two + overlap + none)*100,2),
             "%"),
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  # Bushfires/floods and COVID
  annotate("text",
           x = mean(circle_data$x) - 3.2,
           y = circle_data$y[1],
           label = overlap,
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  annotate("text",
           x = mean(circle_data$x) - 5.2,
           y = circle_data$y[1] - 11,
           label = paste0(
             round(overlap/(one + two + overlap + none)*100,2),
             "%"),
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  annotate("segment",
           x = mean(circle_data$x) - 3.2,
           xend = mean(circle_data$x) - 5.2,
           y = circle_data$y[1] - 2,
           yend = circle_data$y[1] - 10,
           linewidth = 0.4,
           colour = "black") +
  # Add label oustide of circles for no. experiencing none
  annotate("text",
           x = mean(circle_data$x) - 3.2,         
           y = -max(r_1, r_2) - 2,         
           label = none,
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  annotate("text",
           x = mean(circle_data$x) - 3.2,         
           y = -max(r_1, r_2) - 5,         
           label = paste0(
             round(none/(one + two + overlap + none)*100,2),
             "%"),
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  # Add labels above the circles
  annotate("text",
           x = circle_data$x[1],
           y = circle_data$y[1] + r_1 + 5,
           label = "Bushfires/floods",
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  annotate("text",
           x = circle_data$x[2],
           y = circle_data$y[2] + r_2 + 5,
           label = "COVID-19",
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  ggtitle("Female") +
  theme_classic() +
  theme(text = element_text(family = "Arial"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 10))

# venn_bushflood_covid_female




# Venn diagram for males

# Data: sizes of each group
# Bushfire/flood exclusive
one <- df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid2, SEX_genetic, AGE_covid2, matches("^PC.*_genetic$"), COVIDALL_BUSHFLOOD, PRS_stand_genetic) %>% 
  filter(SEX_genetic == "Male") %>% 
  filter(COVIDALL_BUSHFLOOD == "Bushfire/flood only") %>% 
  nrow() 
# COVID exclusive
two <- df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid2, SEX_genetic, AGE_covid2, matches("^PC.*_genetic$"), COVIDALL_BUSHFLOOD, PRS_stand_genetic) %>% 
  filter(SEX_genetic == "Male") %>%
  filter(COVIDALL_BUSHFLOOD == "COVID only") %>% 
  nrow()
# Both bushfire/flood and COVID
overlap <- df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid2, SEX_genetic, AGE_covid2, matches("^PC.*_genetic$"), COVIDALL_BUSHFLOOD, PRS_stand_genetic) %>% 
  filter(SEX_genetic == "Male") %>%
  filter(COVIDALL_BUSHFLOOD == "Bushfire/flood and COVID") %>% 
  nrow()  
# None experiences
none <- df_ND_covid %>% 
  drop_na(NUMSYMPALL_covid2, SEX_genetic, AGE_covid2, matches("^PC.*_genetic$"), COVIDALL_BUSHFLOOD, PRS_stand_genetic) %>% 
  filter(SEX_genetic == "Male") %>%
  filter(COVIDALL_BUSHFLOOD == "No") %>% 
  nrow()  

# Calculate circle areas
area_one <- one + overlap
area_two <- two + overlap

# Convert areas to radii (since area = π * r^2, radius = sqrt(area / π))
r_1 <- sqrt(area_one / pi)
r_2 <- sqrt(area_two / pi)

# Calculate the proportional overlap distance
overlap_ratio <- overlap / min(area_one, area_two)
d <- (r_1 + r_2) * (1 - overlap_ratio / 2)  # Adjust overlap distance

# Create circle data with calculated positions
circle_data <- data.frame(
  x = c(0, d),  # x-coordinates for each circle
  y = c(0, 0),  # Keep y-coordinates the same for both circles
  radius = c(r_1, r_2),
  group = c("bushflood", "covid")
)

# Plot the Euler diagram with numbers positioned correctly
venn_bushflood_covid_male <- ggplot(circle_data) +
  geom_circle(aes(x0 = x, y0 = y, r = radius, fill = group), alpha = 0.6, color = "black") +
  scale_fill_manual(values = c("#453781FF", "#440154")) +
  coord_fixed() +
  # Add labels inside each circle and in the overlapping section
  # Bushfires/floods only
  annotate("text",
           x = circle_data$x[1] - r_1 / 2 + 1,
           y = circle_data$y[1],
           label = one,
           size = 7 / 2.845276, color = "white", family = "Arial", fontface = "bold") +
  annotate("text",
           x = circle_data$x[1] - r_1 / 2 + 1,
           y = circle_data$y[1] - 2,
           label = paste0(
             round(one/(one + two + overlap + none)*100,2),
             "%"),
           size = 7 / 2.845276, color = "white", family = "Arial", fontface = "bold") +
  # COVID only
  annotate("text",
           x = circle_data$x[2] + r_2 / 2,
           y = circle_data$y[2],
           label = two,
           size = 7 / 2.845276, color = "white", family = "Arial", fontface = "bold") +
  annotate("text",
           x = circle_data$x[2] + r_2 / 2,
           y = circle_data$y[2] - 2,
           label = paste0(
             round(two/(one + two + overlap + none)*100,2),
             "%"),
           size = 7 / 2.845276, color = "white", family = "Arial", fontface = "bold") +
  # Bushfires/floods and COVID
  annotate("text",
           x = mean(circle_data$x - 1.2),
           y = circle_data$y[1],
           label = overlap,
           size = 7 / 2.845276, color = "white", family = "Arial", fontface = "bold") +
  annotate("text",
           x = mean(circle_data$x - 2.2),
           y = circle_data$y[1] - 6,
           label = paste0(
             round(overlap/(one + two + overlap + none)*100,2),
             "%"),
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  annotate("segment",
           x = mean(circle_data$x) - 1.2,
           xend = mean(circle_data$x) - 2.2,
           y = circle_data$y[1] - 1,
           yend = circle_data$y[1] - 5,
           linewidth = 0.4,
           colour = "black") +
  # Add label oustide of circles for no. experiencing none
  annotate("text",
           x = mean(circle_data$x) - 1.2,         
           y = -max(r_1, r_2) - 2,         
           label = none,
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  annotate("text",
           x = mean(circle_data$x) - 1.2,         
           y = -max(r_1, r_2) - 4,         
           label = paste0(
             round(none/(one + two + overlap + none)*100,2),
             "%"),
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  # Add labels above the circles
  annotate("text",
           x = circle_data$x[1],
           y = circle_data$y[1] + r_1 + 5,
           label = "Bushfires/floods",
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  annotate("text",
           x = circle_data$x[2],
           y = circle_data$y[2] + r_2 + 5,
           label = "COVID-19",
           size = 7 / 2.845276, color = "black", family = "Arial", fontface = "bold") +
  ggtitle("Male") +
  theme_classic() +
  theme(text = element_text(family = "Arial"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 10))

# venn_bushflood_covid_male



# Panelled Figure
figure <- venn_bushflood_covid /
  (venn_bushflood_covid_female | venn_bushflood_covid_male) +
  plot_annotation(tag_levels = "A")

figure
## ----

## ---- SaveVenn
ggsave(figure, width = 18, height = 21, dpi = 300, unit = "cm", 
       file = "figures/Venn_bushflood_covid_both_female_male.png")

ggsave(figure, width = 18, height = 21, dpi = 300, unit = "cm", 
       file = "figures/Venn_bushflood_covid_both_female_male.pdf",
       device = cairo_pdf)
## ----

###Code for plotting global floating kelp forest protection status and coverage
##Figure 1b,c and Supplementary Figures 1-4
#Nur Arafeh Dalmau, nadalmau@stanford.edu_ Updated January 2025.
# Manuscript Arafeh-Dalmau et al. 2025. Intensifying marine heatwaves and limited protection threaten floating kelp forests globally. Nature Communications

library(ggplot2)
library(tidyverse)


#########Load data

Protection_All <- read.csv("Data/Outputs/Protection Status/protection_status_by_country_realm_province_ecoregion.csv")
Protection_Realm <- read.csv("Data/Outputs/Protection Status/protection_status_by_realm.csv")
Protection_Country <- read.csv("Data/Outputs/Protection Status/protection_status_by_country.csv") 


########Prepare data

#Estimate Global Protection Coverage
Area_Total <- Protection_Country %>%
  summarise(Area = sum(kelp_area_km2))

##Estimate Global Protection Coverage without Southern Ocean
Area_Total_SO <- Protection_All %>%
  filter (!(realm == "Southern\nOcean")) %>%
  summarise(Area = sum(kelp_area_km2))

##Estimate Global Area Protected by LFP category
Protection <- Protection_Country %>%
  filter (!(lfp_cat == "None")) %>% group_by(lfp_cat) %>% 
  summarise(Protection = sum(kelp_area_km2)/Area_Total*100) %>%
  arrange(desc(Protection))  # Arrange by descending order of Area

##Estimate Global Area Protected by LFP category without Southern Ocean
Protection_SO <- Protection_Realm %>%
  filter(!(REALM == "Southern Ocean")) %>% filter (!(lfp_cat == "None")) %>%  group_by(lfp_cat) %>% 
  summarise(Protection = sum(kelp_area_km2)/Area_Total_SO*100) %>%
  arrange(desc(Protection))  # Arrange by descending order of Area

##Estimate MPA type coverage for each ecoregion
Ecoregion_MPA_Area <- Protection_All %>% group_by(ecoregion, lfp_cat) %>% 
  summarise(Area = sum(kelp_area_km2)) %>%
  arrange(desc(Area))  # Arrange by descending order of Area

##Estimate kelp coverage by ecoregion
Ecoregion_Area <- Protection_All %>%
  group_by(realm, ecoregion) %>% 
  summarise(Area = sum(kelp_area_km2)) %>%
  arrange(desc(Area))  # Arrange by descending order of Area

### Estimate Percentage protection of kelp by each ecoregion and LFP
merged_eco <- merge(Ecoregion_Area, Ecoregion_MPA_Area, by = "ecoregion", all = TRUE)  %>%
  mutate(Per=(Area.y/Area.x)*100)


##########Figures

##Figure 3b-c

Fig_3b <- ggplot(Protection) +
  geom_col(aes(x = factor(lfp_cat, levels = c("Least", "Less", "Moderately", "Heavily", "Most")), y = Protection$Area, fill = lfp_cat)) +
  scale_fill_manual(values = c("Least" = "#CA0220",
                               "Less" = "#EC846E",
                               "Moderately" = "#73B2FF",
                               "Heavily" = "#0084A8",
                               "Most" = "#002673")) +  # Assigning custom colors
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),  # Remove x-axis minor gridlines
        panel.grid.major.x = element_blank(),  # Remove x-axis major gridlines
        axis.line.x = element_line(color = "black"),
        legend.position = "none") +  # Remove the legend
  ylim(0, 15) +
  labs(x = "Level of Fishing Protection", y = "Protection (%)")  # Adding axis labels

Fig_3b


Fig_3c <- ggplot(Protection_SO) +
  geom_col(aes(x = factor(lfp_cat, levels = c("Least", "Less", "Moderately", "Heavily", "Most")), y = Protection$Area, fill = lfp_cat)) +
  scale_fill_manual(values = c("Least" = "#CA0220",
                               "Less" = "#EC846E",
                               "Moderately" = "#73B2FF",
                               "Heavily" = "#0084A8",
                               "Most" = "#002673")) +  # Assigning custom colors
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),  # Remove x-axis minor gridlines
        panel.grid.major.x = element_blank(),  # Remove x-axis major gridlines
        axis.line.x = element_line(color = "black"),
        legend.position = "none") +  # Remove the legend
  ylim(0, 15) +
  labs(x = "Level of Fishing Protection", y = "Protection (%)")  # Adding axis labels

Fig_3c


##Suplementary Figure 1 and 4

FigS1 <- ggplot(Ecoregion_MPA_Area) +
  geom_col(aes(Area, reorder(ecoregion, Area)), fill = "#4682B4") +  # Color by Realm
  theme_bw() +
  theme(panel.grid.minor.y = element_blank(),  # Remove y-axis minor gridlines
        panel.grid.major.y = element_blank(),  # Remove y-axis major gridlines
        axis.line.y = element_line(color = "black")) +  # Set y-axis line color
  labs(y = "Ecoregion", x = "Area")  # Change y-axis to "Ecoregion"

FigS1


FigS4 <- ggplot(merged_eco) +
  geom_col(aes(x = reorder(ecoregion, Area.x),  # Reorder ecoregions alphabetically in reverse
               y = Per, 
               fill = factor(lfp_cat, levels = rev(c("Most", "Heavily", "Moderately", "Less", "Least", "None"))))) +
  scale_fill_manual(values = c("Most" = "#002673",
                               "Heavily" = "#0084A8",
                               "Moderately" = "#73B2FF",
                               "Less" = "#EC846E",
                               "Least" = "#CA0220",
                               "None" = "gray")) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),  # Remove x-axis minor gridlines
        panel.grid.major.x = element_blank(),  # Remove x-axis major gridlines
        axis.line.x = element_line(color = "black"),
        legend.position = "none") +  # Remove the legend
  coord_flip() +
  labs(x = "Ecoregion", y = "Protection (%)")  # Change x-axis and y-axis labels

FigS4

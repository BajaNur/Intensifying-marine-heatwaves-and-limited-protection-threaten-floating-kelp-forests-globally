###Code for plotting Relationship between threat posed by future marine heatwaves and level of protection for floating kelp forests
##Figure 5a,b
#Nur Arafeh Dalmau, nadalmau@stanford.edu_ Updated January 2025.
# Manuscript Arafeh-Dalmau et al. 2025. Intensifying marine heatwaves and limited protection threaten floating kelp forests globally. Nature Communications


library(dplyr)
library(ggplot2)
library(viridis)
library(ggrepel)
library(cowplot)

##Load data of protection status and Cumulative MHW Intensity

Protection_All <- read.csv("Data/Outputs/Protection Status/protection_status_by_country_realm_province_ecoregion.csv")
MHWs <- read.csv("Data/Outputs/Ecoregions_mean_MHWs.csv")

### Estimate total area of protection for Highly and moderately MPAs by ecoregion
Ecoregion_MPA_Area <- Protection_All %>%
  mutate(
    new_lfp_cat = case_when(
      lfp_cat %in% c("Least", "Less", "None") ~ "None_Full_Partial_MPA",
      lfp_cat == "Moderately" ~ "Moderately_MPA",
      lfp_cat %in% c("Most", "Heavily") ~ "Highly_MPA",
      TRUE ~ lfp_cat  # Retain the original value if not in the specified categories
    )
  ) %>%  group_by(ecoregion, new_lfp_cat) %>% 
  summarise(Protection = sum(kelp_area_km2)) %>%
  arrange(desc(Protection))  # Arrange by descending order of Area

##Estimate kelp coverage by ecoregion
Ecoregion_Area <- Protection_All %>%
  group_by(realm, ecoregion) %>% 
  summarise(Area = sum(kelp_area_km2)) %>%
  arrange(desc(Area)) 

### Join both datasets
Kelp_MPA_join <- left_join(Ecoregion_Area, Ecoregion_MPA_Area, by = "ecoregion") %>%
  mutate(Percentage = (Protection/Area*100)) %>% filter (new_lfp_cat == "Moderately_MPA" |
                                                  new_lfp_cat == "Highly_MPA")

## Estimate only Highly (Fig5a)
Kelp_MPAs_Highly <- Kelp_MPA_join %>% filter (new_lfp_cat == "Highly_MPA")

### Need to add 0s to those ecoregions with no Protection
Kelp_MPAs_Highly2 <- left_join(Ecoregion_Area, Kelp_MPAs_Highly, by = "ecoregion") %>% 
  dplyr:: select(ecoregion,Percentage)
Kelp_MPAs_Highly2[is.na(Kelp_MPAs_Highly2)] = 0 

Kelp_Highly <- left_join(Ecoregion_Area, Kelp_MPAs_Highly2, by="ecoregion")  %>% 
  rename(Ecoregion = ecoregion, Percentage_Highly = Percentage)

## Estimate Moderately and Highly combined (Fig 5b)
Kelp_MPAs_Moderately_Highly <- Kelp_MPA_join %>% group_by (ecoregion) %>% summarise (Percentage_Moderately_Highly = sum(Percentage))

### Need to add 0s to those ecoregions with no Protection
Kelp_MPAs_Moderately_Highly2 <- left_join(Ecoregion_Area, Kelp_MPAs_Moderately_Highly, by = "ecoregion") %>%
  rename(Ecoregion = ecoregion)
Kelp_MPAs_Moderately_Highly2[is.na(Kelp_MPAs_Moderately_Highly2)] = 0 

# Join both data frames (Kelp_Highly and Kelp_MPAs_Moderately_Highly2)
Final_Kelp_Data <- left_join(Kelp_Highly, Kelp_MPAs_Moderately_Highly2, by = "Ecoregion") %>%
  dplyr::select(-realm.x, -realm.y, -Area.y) %>% rename (Area = Area.x)


#######MHW prepration. Use intermediate climate scenario (SSP2-45)
SSP245 <- MHWs %>% dplyr::select(Ecoregion, Sho_245_raster, Mid_245_raster, Long_245_raster)

Kelp_Final_MPAs <- left_join(SSP245, Final_Kelp_Data, by = "Ecoregion") 


# Rename Ecoregions for space in plot
replacements <- c(
  "Southern California Bight" = "S. California Bight",
  "Puget Trough/Georgia Basin" = "Puget Trough",
  "North American Pacific Fijordland" = "N. American Pac. Fjorland",
  "Channels and Fjords of Southern Chile" = "Channels & Fjords S. Chile",
  "Eastern Bering Sea" = "E. Bering Sea",
  "Three Kings-North Cape" = "North Cape",
  "Heard and Macdonald Islands" = "Heard & Macdonald Island",
  "Bounty and Antipodes Islands" = "Bounty & Antipodes Island",
  "Oregon, Washington, Vancouver Coast and Shelf" = "Oregon to Vancouver Coast & Shelf",
  "Northern California" = "N. California",
  "Central Peru" = "C. Peru",
  "Central Chile" = "C. Chile",
  "South Georgia" = "S. Georgia",
  "South New Zealand" = "S. New Zealand",
  "North Cape" = "N. Cape",
  "Prince Edward Islands" = "Prince Edward Isl.",
  "Crozet Islands" = "Crozet Isl.",
  "Kerguelen Islands" = "Kerguelen Isl.",
  "Macquarie Island" = "Macquarie Isl.",
  "Auckland Island" = "Auckland Isl.",
  "Campbell Island" = "Campbell Isl.",
  "Western Bassian" = "W. Bassian",
  "Agulhas Bank" = "Agulhas",
  "Patagonian Shelf" = "Patagonian",
  "Northeastern New Zealand" = "N.E. New Zealand",
  "Central New Zealand" = "C. New Zealand",
  "Chatham Island" = "Chatham Isl.",
  "Tristan Gough" = "Tristan"
)

# Apply the replacements
Kelp_Final_MPAs$Ecoregion <- recode(Kelp_Final_MPAs$Ecoregion, !!!replacements)


# Plot

p1 <- ggplot(Kelp_Final_MPAs, aes(x = Percentage_Highly, y = Mid_245_raster, size = Area, color = Mid_245_raster, label = Ecoregion)) +
  geom_point(alpha = 0.5) +
  scale_size(range = c(1, 18), guide = FALSE) +
  scale_color_viridis_c() +
  labs(x = "Protected ecoregion (%)", y = "Cummulative MHWs Intensity (°C days/year)") +
  #geom_text_repel(size = 2, fontface = "bold", nudge_x = -0.08, nudge_y = 3) +
  guides(color = FALSE) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 8, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.y = element_blank(),  # Remove y-axis label
    panel.grid.major = element_line(colour = "gray", size = 0.25),  # Customize major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank()) +
  scale_x_continuous(expand = c(0.18, 0)) +
  scale_y_continuous(expand = c(0.16, 0))


p1



p2 <- ggplot(Kelp_Final_MPAs, aes(x = Percentage_Moderately_Highly, y = Mid_245_raster, size = Area, color = Mid_245_raster, label = Ecoregion)) +
  geom_point(alpha = 0.5) +
  scale_size(range = c(1, 18), guide = FALSE) +
  scale_color_viridis_c() +
  labs(x = "Protected ecoregion (%)", y = "Cummulative MHWs Intensity (°C days/year)") +
  geom_text_repel(size = 2, fontface = "bold", nudge_x = -0.08, nudge_y = 3) +
  guides(color = FALSE) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 8, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    panel.grid.major = element_line(colour = "gray", size = 0.25),  # Customize major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank()) +
  scale_x_continuous(expand = c(0.18, 0)) +
  scale_y_continuous(expand = c(0.16, 0))


p2


plot_grid(p1, p2, ncol = 1, align = "v", axis = "lr", vjust = 1)


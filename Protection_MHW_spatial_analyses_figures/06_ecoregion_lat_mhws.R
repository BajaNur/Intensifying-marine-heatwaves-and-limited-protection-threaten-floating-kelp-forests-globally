####Code for plotting the mean MHW cumulative intensity for each ecoregion, scenario, and time. 
##Figure 2a-b
#Nur Arafeh Dalmau, nadalmau@stanford.edu_ Updated January 2025.
# Manuscript Arafeh-Dalmau et al. 2025. Intensifying marine heatwaves and limited protection threaten floating kelp forests globally. Nature Communications

library(ggplot2)
library(tidyverse)
library(sf)
library(maps)
library(viridis)
library(cowplot)
library(gridExtra)


##Load data

Ecoregions_MHWs <- read.csv("Data/Outputs/Ecoregions_mean_MHWs.csv")

# Download marine ecoregions of the world here: https://databasin.org/datasets/3b6b12e7bcca419990c9081c0af254a2/
shapefile_data <- st_read("Data/Spalding Ecoregions/meow_ecos.shp")

#Rename column to match with the csv. file
shapefile_data <- shapefile_data %>%
  rename(Ecoregion = ECOREGION) 

# Perform the join

ecoregions <- shapefile_data %>%
  left_join(Ecoregions_MHWs, by = "Ecoregion") %>% 
  drop_na %>%
  dplyr::select(-contains("ci")) 


# Load world map for the plot

world_map <- map_data("world")

# Function for plotting
create_ecoregion_plot <- function(ecoregions, world_map, fill_column, limits = c(70, 1935)) {
  ggplot() +
    geom_sf(data = ecoregions, aes_string(fill = fill_column), show.legend = FALSE, color = NA) +
    scale_fill_viridis_c(limits = limits) +   # Set 'viridis' color scale
    # Plot world map
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                 fill = "gray45") +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}

# List of column names to plot
columns_to_plot <- c("Sho_126_raster", "Sho_245_raster", "Sho_585_raster", 
                     "Mid_126_raster", "Mid_245_raster", "Mid_585_raster", 
                     "Long_126_raster", "Long_245_raster", "Long_585_raster")

# Loop through columns and generate plots
plots <- list()

for (col in columns_to_plot) {
  plot <- create_ecoregion_plot(ecoregions, world_map, col)
  plots[[col]] <- plot
}



# Arrange the 9 plots in a 3x3 grid

final_plot <- plot_grid(
  plots[[1]], plots[[2]], plots[[3]],
  plots[[4]], plots[[5]], plots[[6]],
  plots[[7]], plots[[8]], plots[[9]],
  ncol = 3, align = "v", 
  rel_widths = rep(1, 1),  # Adjust relative widths for columns
  rel_heights = rep(1, 1)  # Adjust relative heights for rows
) + theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

# Show final plot
final_plot

################Figure 2b, estimating mean cumulative MHW intensity for floating kelp at a 1 degree latitude

##Load data
Kelp_MHWs <- read.csv ("Data/Outputs/Mean_MHWs.csv")

### To avoid connected no-data, need to create a Lat column and a join to have NAs

Lat <- seq(-55875, 61375, by= 250) %>% data_frame()

colnames(Lat) <- "Lat"

Lat <- Lat %>% summarise(y = Lat/1000)

#Estimate means

Kelp_MHWs_M <- Kelp_MHWs %>% 
  group_by (y) %>% summarise(m_O = mean(Ob),
                             m_Sho_126 =mean(Sho_126), m_Mid_126 =mean(Mid_126), 
                             m_Long_126 =mean(Long_126), 
                             m_Sho_245 =mean(Sho_245), m_Mid_245 =mean(Mid_245), 
                             m_Long_245 =mean(Long_245), 
                             m_Sho_585 =mean(Sho_585), m_Mid_585 =mean(Mid_585), 
                             m_Long_585 =mean(Long_585))


Kelp_MHWs2 <- full_join(Lat, Kelp_MHWs_M, by = "y", multiple = "all")



#################Plots

###Short term

# Create the plots 
p1 <- ggplot(Kelp_MHWs2, aes(x = y)) +
  geom_line(aes(y = m_O, color = "Contemporany"), size = 0.7) +
  geom_line(aes(y = m_Sho_126, color = "SSP1-2.6"), size = 0.7) +
  geom_line(aes(y = m_Sho_245, color = "SSP2-4.5"), size = 0.7) +
  geom_line(aes(y = m_Sho_585, color = "SSP5-8.5"), size = 0.7) +
  xlab("Latitude") + 
  ylab("Cumm_intensity") +
  theme_bw() + 
  coord_flip() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.line.x.top = element_blank(),
        axis.line.y.right = element_blank(),
        axis.title.x = element_blank()) +
  ylim(0, 1900) +
  scale_color_manual(name = "", 
                     values = c("Contemporany" = "black", 
                                "SSP1-2.6" = "#173C66", 
                                "SSP2-4.5" = "#F79420", 
                                "SSP5-8.5" = "#951B1E")) +
  theme(legend.position = "none")  # Remove legend

p1

p2 <- ggplot(Kelp_MHWs2, aes(x = y)) +
  geom_line(aes(y = m_O, color = "Contemporany"), size = 0.7) +
  geom_line(aes(y = m_Mid_126, color = "SSP1-2.6"), size = 0.7) +
  geom_line(aes(y = m_Mid_245, color = "SSP2-4.5"), size = 0.7) +
  geom_line(aes(y = m_Mid_585, color = "SSP5-8.5"), size = 0.7) +
  theme_bw() + 
  coord_flip() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.line.x.top = element_blank(),
        axis.line.y.right = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 9),
        legend.key.size = unit(0.5, "lines"),
        legend.text = element_text(size = 8)) +
  ylim(0, 1900) +
  labs(x = "Latitude") +
  scale_color_manual(name = "", 
                     values = c("Contemporany" = "black", 
                                "SSP1-2.6" = "#173C66", 
                                "SSP2-4.5" = "#F79420", 
                                "SSP5-8.5" = "#951B1E")) +
  theme(legend.position = "none")  # Remove legend

p2

p3 <- ggplot(Kelp_MHWs2, aes(x = y)) +
  geom_line(aes(y = m_O, color = "Contemporany"), size = 0.7) +
  geom_line(aes(y = m_Long_126, color = "SSP1-2.6"), size = 0.7) +
  geom_line(aes(y = m_Long_245, color = "SSP2-4.5"), size = 0.7) +
  geom_line(aes(y = m_Long_585, color = "SSP5-8.5"), size = 0.7) +
  theme_bw() + 
  coord_flip() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.line.x.top = element_blank(),
        axis.line.y.right = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 9),
        legend.key.size = unit(0.5, "lines"),
        legend.text = element_text(size = 8)) +
  ylim(0, 1900) +
  labs(x = "Latitude") +
  scale_color_manual(name = "", 
                     values = c("Contemporany" = "black", 
                                "SSP1-2.6" = "#173C66", 
                                "SSP2-4.5" = "#F79420", 
                                "SSP5-8.5" = "#951B1E")) +
  theme(legend.position = "none")  # Remove legend

p3

##Plot

final_plot2 <- plot_grid(p1, p2, p3, ncol = 3, align = "v")

# Show final plot
final_plot2




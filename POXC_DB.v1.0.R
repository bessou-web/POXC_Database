#A global database on land use and land management change effects on soil KMnO4-oxidisable organic carbon
#Authors: Cécile Chéron-Bessou, Damien Beillouin, Alexis Thoumazeau, Lydie Chapuis-Lardy, Tiphaine Chevallier, Julien Demenois, Paul N Nelson
#v1.0, 2025-2-10

####################################### 1. Load DATA


#libraries
library(tidyverse)      # Wrangling with data
library(tidyr)          # To clean data part of tidyverse?
library(tibble)
library(lubridate)      # For dates, in case
library(ggplot2)
library(ggExtra)
library(ggpointdensity) # To visualise data
library(ggridges)
library(maps)
library(mapdata)
library(ggnewscale)
library(raster)
library(plotbiomes)
library(sp)
library(cowplot)
library(ggpubr)         # To add statistics easily to ggplot2
library(hrbrthemes)
library(viridis)        # To have nice colours and robust to colourblindness
library(extrafont)
library(ragg)
library(janitor)
library(forcats)
library(dplyr)          # To select columns
library(stringr)        # To modify character strings in columns
library(ggtext)
library(writexl)        # To export dataframe to Excel
library(openxlsx)
library(rstatix)        # To do ANOVA
library(esc)
library(broom)          # To use tidy()
library(magrittr)       # To use %>% [ CTRL + MAJ + M]


setwd(dir="REPOSITORY-NAME")


# 1. Data files for the data paper----

## 1.1 A_Studies, n= 723----
# Table of all studies extracted from search motors and filtered for errors

A_data <- read.delim("~A_LIST.OF.STUDIES_POXC_DB_v1.0.csv")


## 1.2 B_REJECTED_Studies, n= 454 ----
# Table of rejected studies according to the selection criteria

B_data <- read.delim("~B_REJECTED.STUDIES_POXC_DB_v1.0.csv")



## 1.3 C_RETAINED_Studies, , n= 372 ----
# Table of retained studies according to the selection criteria, including the primary studies added from meta-analyses or syntheses
# This table contains the geographical information of each experimental site, including extra lines for studies covering more than one site

C_data <- read.delim("~C_RETAINED.STUDIES_POXC_DB_v1.0.csv")


## 1.4 D_EFFECT.SIZES, , n= 13,636----

D_data <- read.delim("~D_EFFECT.SIZES_POXC_DB_v1.0.csv")


## 1.5 E_QUALITY----
# Table containing the quality scores for each retained study

E_data <- read.delim("~/E_QUALITY_POXC_DB_v1.0.csv")


####################################### 2. Mapping DATA
# 2. Data mapping ----
## 2.1 POXC points for the mapping and Whittaker plot----

POXC_map <- C_data
POXC_map <- POXC_map[,-c(24:28)]

unique(POXC_map$Country)

POXC_whit<- POXC_map


## 2.2 Creation of the world map with the number of experiments per country translated into colour classes----

# World map with country names and coordinates

world_map <- map_data("world")
setdiff(world_map$region,POXC_map$Country) #207 country not covered; 45 represented
setdiff(POXC_map$Country,world_map$region)
summary(POXC_map, summax=80)

# Adding the number of experiments per country based on their coordinates

# Counting the number of experiments per combination of x-y coordinates (more than one experiment is possible per combination)

NB_Points<-POXC_map %>% group_by(Lat_y,Long_x) %>% tally()

# Counting the number of experiments per country

NB_Country<-POXC_map %>% group_by(Country) %>% tally()
names(NB_Country)[1]<- 'region'

glimpse(NB_Country)
summary(NB_Country)

# Merging both localised data

world_map2<- left_join(world_map,NB_Country)


# Create a discrete variable and plot

world_map2$cut_n<- cut (world_map2$n, breaks= c(1, 5, 20, 60, 80, 110), include.lowest = TRUE)

ggplot() +
  geom_map(data = world_map2, map = world_map,
           aes(x = long, y = lat, map_id = region,
               fill = cut_n), color = "grey75", linewidth = 0.2)+
  labs(fill = 'Number of experiments'
       ,color = '.'
       ,title = ''
       ,x = NULL
       ,y = NULL) +
  labs(title = "",
       x = "Longitude", y = "Latitude")+
  theme_bw()+
  theme(panel.grid.major = element_line(colour = 'grey75', size = 0.3, linetype = 3))+
  theme(axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.3)),
        legend.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.3)),
        legend.position ="bottom")+
  scale_fill_manual(values = c('#FFD700','#B5CF68','#90CC9C','#6BC8D0','#446DA8'), na.value= "grey92")+
  geom_point(data = POXC_map,
             inherit.aes = FALSE,
             mapping = aes(x = Long_x, y = Lat_y),
             size = 1.8,
             shape  = 23,
             colour = "white",
             fill   = "grey10",
             alpha = 0.5)


## 2.3 Whittetaker plot----

# Uploading temp and rainfall data from Worldclim (free)
## https://www.worldclim.org/data/worldclim21.html (19.tiff) WorldClim Bioclimatic variables for WorldClim version 2., 30-seconde resolution
## They are the average for the years 1970-2000.BIO1 = Annual Mean Temperature + BIO12 = Annual Mean Precipitation

P1 <- raster("~wc2.1_30s_bio_12.tif")
T1 <- raster("~wc2.1_30s_bio_1.tif")

# Adding averaged precipitations and temperature based on the coordinates

POXC_W<-POXC_whit %>% dplyr::filter(!is.na(Long_x),!is.na(Lat_y))
TEST1<-sf::st_as_sf(POXC_W, coords = c( "Long_x", "Lat_y" ) )
values <- raster::extract(P1,TEST1)
DATA_P <- cbind.data.frame(POXC_W,values)
names(DATA_P)[24]<-"Precipitations"

values <- raster::extract(T1,TEST1)
DATA_T <- cbind.data.frame(POXC_W,values)
names(DATA_T)[24]<-"Temperature"


DATA<-left_join(DATA_T,DATA_P)

# Needed package for the Whittaker plot

install.packages("remotes")
remotes::install_github("valentinitnelav/plotbiomes")
library(plotbiomes)


whittaker_base_plot() +

  geom_point(data = DATA,         # Adding the temperature - precipitation data points
             aes(x = Temperature,
                 y = Precipitations/10),
             size   = 3,
             shape  = 23,
             colour = "white",
             fill   = "gray35",
             stroke = 0.5,
             alpha  = 0.8) +
  theme_bw()+
  theme(panel.grid.major = element_line(colour = 'grey50', size = 0.3, linetype = 3),
        axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)),
        legend.position = "bottom")


####################################### 3. Figures

# 3. Article figures ----
### 3.1 = Fig. 1 A&B Cumulative evolution of publication numbers per year and Quality score ----

names(E_data)[c(5,6,7)]<-c("Crit_Misclassification","Crit_Exposure issue","Crit_Reporting issue")

head(E_data)


QUAL_long <- E_data %>%
  pivot_longer(cols = starts_with("Crit"),
               names_to = "Criterion",
               values_to = "Risk_Level")

VERIF<- QUAL_long %>% filter(is.na(Risk_Level))
QUAL_long <- QUAL_long %>%
  mutate(Risk_Level = factor(Risk_Level,
                             levels = c("High risk of issues",
                                        "Medium risk of issues",
                                        "Low risk of issues"))) %>%
  mutate(Risk_Level = fct_recode(Risk_Level,
                                 "High" = "High risk of issues",
                                 "Medium" = "Medium risk of issues",
                                 "Low" = "Low risk of issues"
  )) %>%
  mutate(Criterion = fct_recode(Criterion,
                                "Exposure" = "Crit_Exposure issue",
                                "Misclassification" = "Crit_Misclassification",
                                "Reporting" = "Crit_Reporting issue"
  ))

# Step 2: Create a stacked bar chart for quality appraisals
quality_plot <- ggplot(QUAL_long, aes(x = Criterion, fill = Risk_Level)) +
  geom_bar(position = "fill", color = "black") +  # 'fill' to create stacked bars
  labs(x = "Quality Criteria",
       y = "Proportion of Studies",
       title = "A",
       fill = "Risk of issues") +
  scale_fill_manual(values = c("Low" = "#eed5b7",
                               "Medium" = "#fdbb84",
                               "High" = "#cd3333")) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


print(quality_plot)


### Publication counts

QUAL <- E_data
QUAL<-QUAL[,1:8]
names(QUAL)[8]<-"SCORE"

A_data <-A_data %>% filter(Studies %in% QUAL$Studies)
LIST<-left_join(A_data,QUAL)


# Grouped Graph by Crop
grouped_data <- LIST %>%
  group_by(Publication_Year, SCORE) %>%
  summarise(Count = n(),.groups = "drop") %>%
  arrange(Publication_Year) %>%
  group_by(SCORE) %>%
  mutate(Cumulative = cumsum(Count)) %>%
  ungroup() %>%
  mutate(SCORE = fct_recode(SCORE,
                            "High" = "High risk of issues",
                            "Medium" = "Medium risk of issues",
                            "Low" = "Low risk of issues"))


# Plot the grouped cumulative count graph

Plot_n_score_yr <- ggplot(grouped_data, aes(x = Publication_Year, y = Cumulative, color = SCORE, group = SCORE)) +
  geom_line(size = 1.5)+
  scale_color_manual(values = c("#cd3333","#eed5b7","#fdbb84")) +
  geom_point(size = 2.5) +  #, pch = 23, color = "black"
  scale_fill_manual(values = c("#cd3333","#eed5b7","#fdbb84")) +
  ylim(0,200)+
  labs(title = "B",
       x = "Year",
       y = "Cumulative Count of Studies",
       color = "Risk of issues") +

  theme_minimal(base_size = 13)

print(Plot_n_score_yr)

### 3.2 = Fig. 1 C&D Scatter plot with marginal densities by final land use, Fig. 1 in the article  ----


scatter_plot <- ggplot(POXC_metadata, aes(x = log(POXC_Mean_T_g.kg),
                                          y = log(POXC_Mean_C_g.kg))) +
  geom_hline(aes(yintercept=0),linetype= "dashed", color="gray50")+     # To add reference lines;
  geom_vline(aes(xintercept=0),linetype="dashed", color="gray50")+      # To remove unnecessary aes() inside geom_hline(), geom_vline(), and geom_abline()These are static values and don’t require aes(), making the code cleaner and slightly faster.


  geom_pointdensity(alpha = 0.7) + scale_color_viridis_c()+             # Density-based scatter plot

  theme_minimal(base_size = 13) +

  # Labels and title
  labs(x = "log(POXC Treatment) (g/kg)", y = "log(POXC Control) (g/kg)",
       title = "C",legend.position = "right", color = "Density") +   # 'color' instead of 'legend.title' for consistency

  # Identity line (y = x)
  geom_abline(aes(intercept=0, slope=1), linetype=2, color="gray")

print(scatter_plot)

dens<-ggplot(POXC_metadata, aes(x = log(POXC_Mean_T_g.kg),
                                y = LU_to, fill = LU_to)) +
  theme_minimal(base_size = 13)+

  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right")+

  geom_density_ridges(
    aes(point_color = LU_to, point_fill = LU_to),
    alpha = 0.2, point_alpha = 0.7, jittered_points = TRUE) +
  scale_point_color_hue(l = 40) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(19:25))+

  guides(point_color = "none", point_fill = "none") +

  labs(x = "log(POXC Treatment) (g/kg)",
       y = "Land Uses",
       title = "D",
       fill = "Land uses")+
  coord_flip()

print(dens)

plot_grid(quality_plot, Plot_n_score_yr, scatter_plot,dens, align = 'hv', nrow = 2, ncol=2, rel_widths = c(1,1.1), rel_heights = c(1,1.1))

### 3.3 = Fig. 5 Heat map by land use categories LU_to/LU_from ----

heatmap_data1 <- POXC_metadata %>%
  group_by(LU_to, LU_from) %>%
  tally() %>%                      # Same as summarise(n = n())
  mutate(n_category = cut(n,
                          breaks = c(0, 10, 50, 100, 250, 500, Inf),
                          include.lowest = FALSE
  ))

ORDER_Xa <- heatmap_data1 %>%
  filter(n_category != "NA",
  ) %>%
  group_by(LU_to) %>%             # To group by LU_to
  summarise(order_x = n())        # To count the number of rows in each group


ORDER_Ya <- heatmap_data1 %>%
  filter(n_category != "NA",
  ) %>%
  group_by(LU_from) %>%           # To group by LU_from
  summarise(order_y = n())        # To count the number of rows in each group

heatmap_data1 <-left_join(heatmap_data1,ORDER_Xa)
heatmap_data1 <-left_join(heatmap_data1,ORDER_Ya)

# Heatmap with user-defined colours based on Viridis theme
heatmap_plot1 <- ggplot(heatmap_data1,
                        aes(x = reorder(LU_to,order_x),
                            y = reorder(LU_from,order_y), fill = n_category)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = n), color = "black", size = 6) +  # Displaying the values
  scale_fill_manual(
    values = c(
            "#33638dff",
            "#287D8EFF",
            "#20a387ff",
            "#55c667ff",
            "#95d840ff",
            "#fde725ff"
            ),
    name = "POXC measurements"
  ) +
  labs(
    x = "Final Land Use",
    y = "Initial Land Use") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic", color = "gray40"),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

print(heatmap_plot1)

### 3.4 = Fig. 6 Heatmap by land use managements within land use categories LU_to_mngt/LU_from_mngt ----

Arable_grouped <- POXC_metadata %>%
  filter(LU_to == "Arable land",
         LU_from == "Arable land") %>%
  mutate(LU_to_mngt = trimws(LU_to_mngt)) %>%
  mutate(LU_from_mngt = trimws(LU_from_mngt))


Arable_grouped <- Arable_grouped %>%
  filter(LU_from_mngt != "unspecified" & LU_from_mngt != "perennial crop unspecified") %>%
  mutate(int1_grouped = case_when(
    LU_from_mngt %in% c("no input", "perennial crop no input") ~ "Crop without any input",
    LU_from_mngt %in% c("perennial crop high organic inputs", "perennial crop low organic inputs", "perennial crop medium organic inputs", "perennial crop mineral and organic inputs") ~ "Perennial organic inputs",
    LU_from_mngt %in% c("perennial crop medium mineral inputs", "perennial crop high mineral inputs", "perennial crop low mineral inputs") ~ "Perennial mineral inputs",
    LU_from_mngt %in% c("high organic inputs", "low organic inputs", "medium organic inputs", "mineral and organic inputs") ~ "Annual crop organic inputs",
    LU_from_mngt %in% c("high mineral inputs", "low mineral inputs", "medium mineral inputs") ~ "Annual crop mineral inputs",
    LU_from_mngt %in% c("irrigated", "flooded") ~ "Irrigated or flooded crop",
    LU_from_mngt %in% c("non-irrigated", "non/alternatively-flooded") ~ "Non-irrigated/alternatively-flooded crop",
    LU_from_mngt %in% c("w legumes/cover crop") ~ "Rotation with cover crop",
    LU_from_mngt %in% c("w/o legumes/cover crop") ~ "Rotation without cover crop",
    LU_from_mngt %in% c("high intensity tillage") ~ "High intensity tillage",
    LU_from_mngt %in% c("intermediate intensity tillage") ~ "Intermediate intensity tillage",
    LU_from_mngt %in% c("no tillage") ~ "No tillage",
    LU_from_mngt %in% c("intercropping") ~ "Intercropping",
    LU_from_mngt %in% c("no intercropping") ~ "No intercropping",
    LU_from_mngt %in% c("Organic") ~ "Organic system",
    LU_from_mngt %in% c("conventional") ~ "Conventional system",
    .default = "Others"  # Default category for anything else
  ))

Arable_grouped <- Arable_grouped %>%
  filter(LU_to_mngt != "unspecified" & LU_to_mngt != "perennial crop unspecified") %>%
  mutate(int2_grouped = case_when(
    LU_to_mngt %in% c("no input", "perennial crop no input") ~ "Crop without any input",
    LU_to_mngt %in% c("perennial crop high organic inputs", "perennial crop low organic inputs", "perennial crop medium organic inputs", "perennial crop mineral and organic inputs") ~ "Perennial organic inputs",
    LU_to_mngt %in% c("perennial crop medium mineral inputs", "perennial crop high mineral inputs", "perennial crop low mineral inputs") ~ "Perennial mineral inputs",
    LU_to_mngt %in% c("high organic inputs", "low organic inputs", "medium organic inputs", "mineral and organic inputs") ~ "Annual crop organic inputs",
    LU_to_mngt %in% c("high mineral inputs", "low mineral inputs", "medium mineral inputs") ~ "Annual crop mineral inputs",
    LU_to_mngt %in% c("irrigated", "flooded") ~ "Irrigated or flooded crop",
    LU_to_mngt %in% c("non-irrigated", "non/alternatively-flooded") ~ "Non-irrigated/alternatively-flooded crop",
    LU_to_mngt %in% c("w legumes/cover crop") ~ "Rotation with cover crop",
    LU_to_mngt %in% c("w/o legumes/cover crop") ~ "Rotation without cover crop",
    LU_to_mngt %in% c("high intensity tillage") ~ "High intensity tillage",
    LU_to_mngt %in% c("intermediate intensity tillage") ~ "Intermediate intensity tillage",
    LU_to_mngt %in% c("no tillage") ~ "No tillage",
    LU_to_mngt %in% c("intercropping") ~ "Intercropping",
    LU_to_mngt %in% c("no intercropping") ~ "No intercropping",
    LU_to_mngt %in% c("Organic") ~ "Organic system",
    LU_to_mngt %in% c("conventional") ~ "Conventional system",
    TRUE ~ "Others"  # Default category for anything else
  ))



heatmap_data_arable <- Arable_grouped %>%
  group_by(int1_grouped, int2_grouped) %>%
  tally() %>%
  mutate(n_category = cut(n,
                          breaks = c( 0,10,50, 100, 250, 500, 1000, Inf),
                          right = TRUE))

ORDER_X <- heatmap_data_arable %>%
  filter(n_category != "NA") %>%
  group_by(int2_grouped) %>%          # To group by int1 and int2
  summarise(order_x = n())            # To count the number of rows in each group


ORDER_Y <- heatmap_data_arable %>%
  filter(n_category != "NA") %>%
  group_by(int1_grouped) %>%          # To group by int1 and int2
  summarise(order_y = n())            # To count the number of rows in each group

heatmap_data_arable<-left_join(heatmap_data_arable,ORDER_X)
heatmap_data_arable<-left_join(heatmap_data_arable,ORDER_Y)


# Heatmap with user-defined colours based on Viridis theme

heatmap_plot2 <- ggplot(heatmap_data_arable,
                        aes(x = reorder(int2_grouped,order_x),
                            y = reorder(int1_grouped,order_y), fill = n_category)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = n), color = "black", size = 5) +
  scale_fill_manual(
    values = c(
      "#33638dff",
      "#287D8EFF",
      "#1f968bff",
      "#29af7fff",
      "#73d055ff",
      "#b8de29ff",
      "#fde725ff"
    ),
    name = "POXC measurements") +
  labs(
    x = "Final Land-Use",
    y = "Initial Land-Use") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic", color = "gray40"),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

print(heatmap_plot2)



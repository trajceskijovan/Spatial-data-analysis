# -----------------------------------------------------------------
# Visualize spatial data
# Jovan Trajceski
# -----------------------------------------------------------------

# Clear up data in global environment
rm(list=ls())

# Install libraries only if required
install.packages('devtools')
devtools::install_github("hrbrmstr/albersusa")
install.packages("sf")
devtools::install_github("yutannihilation/ggsflabel")
install.packages('dplyr')
install.packages("ggplot2",dependencies = TRUE)
install.packages("tidyverse")
install.packages("rio")
install.packages("plotly")
install.packages("janitor")
devtools::install_github("sfirke/janitor")
devtools::install_github('thomasp85/gganimate')
install.packages("viridis")
install.packages("cowplot")
install.packages("ggpubr")
install.packages("ggthemes")

# Run and load Libraries
library(albersusa)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(rio)
library(plotly)
library(janitor)
library(gganimate)
library(viridis)
library(ggsflabel)
library(cowplot)
library(ggpubr)
library(ggthemes)

# albersusa includes the usa_sf() function which returns a simple features data frame 
# which contains adjusted coordinates for Alaska and Hawaii to plot them with the mainland
usa_sf()

# Load USA base map (source: census.gov or can be found on github)
usa <- st_read("cb_2017_us_state_20m.shp")

# Data filtering
library(dplyr)
lower_usa_48 <- usa %>%
    filter(!(NAME %in% c("Alaska", "District of Columbia", "Hawaii", "Puerto Rico")))

# Load "obesity dataset" and merge with USA base map data
ObesityRate <- read.csv("Obesity Rate.csv", stringsAsFactors=FALSE)
USAobesity <- merge(ObesityRate, lower_usa_48, by="NAME")
str(USAobesity)

# Load "inactivity dataset" and merge with USA base map data
PhysicalInactivityRate <- read.csv("Physical Inactivity Rate.csv", stringsAsFactors=FALSE)
USAInactivity <- merge(PhysicalInactivityRate, lower_usa_48, by="NAME")
str(USAInactivity)

# Load "diabetes dataset" and marge with USA base map data
DiabetesRate <- read.csv("Diabetes Rate.csv", stringsAsFactors=FALSE)
USADiabetesRate <- merge(DiabetesRate, lower_usa_48, by="NAME")
str(USADiabetesRate)

# Load "hypertension dataset" and merge with USA base map data
HypertensionRate <- read.csv("Hypertension.csv", stringsAsFactors=FALSE)
USHypertensionRate <- merge(HypertensionRate, lower_usa_48, by="NAME")
str(USHypertensionRate)

# Create a clean theme
theme1<- theme(panel.grid.major = element_line(colour = 'transparent'), 
               axis.title.x=element_blank(), 
               axis.text.x=element_blank(), 
               axis.ticks.x=element_blank(), 
               axis.title.y=element_blank(), 
               axis.text.y=element_blank(), 
               axis.ticks.y=element_blank(), 
               panel.background=element_blank(), 
               panel.border=element_blank(), 
               panel.grid.minor=element_blank(), 
               plot.background=element_blank(),
               plot.title = element_text(hjust = 0.5))


# Final Plot: Setup #1

library(ggsflabel)

plot1 <- USAobesity %>%
    ggplot() +
    geom_sf(aes(geometry = geometry,fill = Obesity.Rate)) +
    #geom_sf_label_repel(aes(label = NAME), max.overlaps=Inf,size = 3, force = 50) +
    scale_fill_viridis_c("Obesity Rate", option = "viridis") 

plot2 <- USAInactivity %>%
    ggplot() +
    geom_sf(aes(geometry = geometry,fill = Physical.Inactivity.Rate)) +
    #geom_sf_label_repel(aes(label = NAME), max.overlaps=Inf,size = 3, force = 50) +
    scale_fill_viridis_c("Physical Inactivity Rate", option = "viridis") 

plot3 <- USADiabetesRate %>%
    ggplot() +
    geom_sf(aes(geometry = geometry,fill = Diabetes.Rate)) +
    scale_fill_viridis_c("Diabetes Rate", option = "viridis")

plot4 <- USHypertensionRate %>%
    ggplot() +
    geom_sf(aes(geometry = geometry,fill = Hypertension.Rate)) +
    scale_fill_viridis_c("Hypertension Rate", option = "viridis")

library(cowplot)
library(ggpubr)
library(ggthemes)
figure<-ggarrange(plot1+theme1, plot2+theme1, plot3+theme1, plot4+theme1 + rremove("x.text"),
                  labels = c("Fig 1: Obesity",
                             "Fig 2: Physical Inactivity",
                             "Fig 3: Diabetes",
                             "Fig 4: Hypertension"),
                  font.label = list(size = 11, color = "black", family = NULL),
                  ncol = 2, nrow = 2)
annotate_figure(figure,
                top = text_grob("Obesity Analysis in the United States", 
                                color = "darkblue", face = "bold", size = 18),
                bottom = text_grob("Data source: Center for Disease Control", color = "black",
                                   hjust = 1, x = 0.9, face = "italic", size = 12)
)

# Save image of design 1
ggsave(paste0("Map_ver_1_Jovan_Trajceski.png"), width = 14, height = 6)


# Final Plot: Setup #2

library(ggsflabel)

plot1 <- USAobesity %>%
    ggplot() +
    geom_sf(aes(geometry = geometry,fill = Obesity.Rate)) +
    #geom_sf_label_repel(aes(label = NAME), max.overlaps=Inf,size = 3, force = 50) +
    labs(title = "Figure 1: Obesity") +
    scale_fill_viridis_c("Obesity Rate", option = "inferno") 

plot2 <- USAInactivity %>%
    ggplot() +
    geom_sf(aes(geometry = geometry,fill = Physical.Inactivity.Rate)) +
    #geom_sf_label_repel(aes(label = NAME), max.overlaps=Inf,size = 3, force = 50) +
    labs(title = "Figure 2: Physical Inactivity") +
    scale_fill_viridis_c("Physical Inactivity Rate", option = "inferno") 

plot3 <- USADiabetesRate %>%
    ggplot() +
    geom_sf(aes(geometry = geometry,fill = Diabetes.Rate)) +
    labs(title = "Figure 3: Diabetes") +
    scale_fill_viridis_c("Diabetes Rate", option = "inferno")

plot4 <- USHypertensionRate %>%
    ggplot() +
    geom_sf(aes(geometry = geometry,fill = Hypertension.Rate)) +
    labs(title = "Figure 4: Hypertension") +
    scale_fill_viridis_c("Hypertension Rate", option = "inferno")

library(cowplot)
library(ggpubr)
library(ggthemes)
figure<-ggarrange(plot1+theme1, plot2+theme1, plot3+theme1, plot4+theme1 + rremove("x.text"),
                  #font.label = list(size = 11, color = "black", family = NULL),
                  ncol = 2, nrow = 2)
annotate_figure(figure,
                top = text_grob("Obesity Analysis in the United States", 
                                color = "darkblue", face = "bold", size = 18),
                bottom = text_grob("Data source: Center for Disease Control", color = "black",
                                  hjust = 1, x = 0.9, face = "italic", size = 12)
)

# Save image of design 2
ggsave(paste0("Map_ver_2_Jovan_Trajceski.png"), width = 14, height = 6)


# This code is free to use for academic purposes only, provided that a proper reference is cited. 
# This code comes without technical support of any kind. 
# Under no circumstances will the author be held responsible for any use of this code in any way.



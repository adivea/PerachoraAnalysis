## Double figure for JFA with feature and unit results
## using tmaps

# libraries
library(tidyverse)
library(sf)
library(tmap)

## Load survey unit table and shapefile, combine into sf object
unitdata <- read_csv("C:/Users/Adela/Documents/Professional/Projects/Perachora/ALLSurveyUnits_totals.csv")
units <- st_read("data/AllTeams.shp") 
unit_data <- left_join(units, unitdata, by = c("SUID" = "SurveyUnitID"))

## Extract spatial reference and project features to it
gr <- st_crs(unit_data)
features <- st_as_sf(features, crs = gr)


## Create a basic map
library(tmap)
#bbox = c(662387.7, 664230.4),c(4210503.5, 4211275.7) 
A <- tm_shape(unit_data, bbox = st_bbox(units)) + 
 # tm_polygons(col = "ivory", border.col = "ivory3")+
  tm_polygons(col="cornsilk2", border.alpha = 0) +
  tm_symbols(size = "MostRecentlyComputedAncient", col = "darkblue",
             scale = 4,
             title.size = "Sherd count") +
  # tm_compass(type = "arrow", position = c("right", "top")) +
  # tm_scale_bar(position = c("right", "top"), breaks = c(0,0.1,0.2)) + 
  # tm_credits("Sobotkova 2020", position = c("left", "bottom")) +
  # tm_layout(title = "Artefact count per survey unit")
  tm_layout(title = "A")

A
B <- tm_shape(unit_data, bbox = st_bbox(units), unit = "m") + 
  tm_polygons(col="cornsilk2", border.alpha = 0)+
  tm_shape(features) + 
  tm_symbols(shape = 6, col="#666666", size = 0.2) +
  tm_compass(type = "arrow", position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"), breaks = c(0,100,200), text.size = 0.75) + 
  #tm_credits("Sobotkova 2020", position = c("right", "bottom"), size = 0.8) +
  #tm_layout(title = "Registered standing features") 
  tm_layout(title = "B")
B
unit_data <- unit_data %>% 
  mutate(AncientDensity = MostRecentlyComputedAncient / Area_ha)  
  
C <- tm_shape(unit_data, bbox = st_bbox(units), unit = "m") + 
  # tm_polygons(col = "ivory", border.col = "ivory3")+
  tm_polygons(col="cornsilk2", border.alpha = 0) +
  tm_symbols(size = "AncientDensity", col = "darkblue",
             scale = 4,
             title.size = "Ancient Density per ha") +
  # tm_compass(type = "arrow", position = c("right", "top")) +
  # tm_scale_bar(position = c("right", "top"), breaks = c(0,0.1,0.2)) + 
  # tm_credits("Sobotkova 2020", position = c("left", "bottom")) +
  tm_layout(title = "Density of ancient artefacts")

# Printing
# need 300dpi or vector graphics

?pdf()
  pdf("figures/Figure11a_bw.pdf",
      colormodel = "gray", 
      width= 10, height = 6.5)
  # Arrange the two tmap layouts above one another
  current.mode <- tmap_mode("plot")
  tmap_arrange(a,b)  # add c if you want to see densities
  tmap_mode(current.mode)
  
  dev.off()

# SVG graphics device
svg("figures/figurex7.svg", width = 7.5)

# Arrange the two tmap layouts above one another
current.mode <- tmap_mode("plot")
tmap_arrange(a,b)  # add c if you want to see densities
tmap_mode(current.mode)

# Close the graphics device
dev.off() 


## Visbility and daily progress

library(lubridate)
units$Date <- as_date(units$createdAtGMT)

daily <- tm_shape(units, 
                  bbox = st_bbox(units), 
                  unit = "m") + 
  tm_polygons(col = "Date", 
              border.alpha = 0,
              palette = "Accent",
              title = "Daily coverage",
              n = 5) +
  tm_shape(border_ppap, bbox = st_bbox(units)) +
  tm_borders(col = "grey", 
             lwd = 2)+
  tm_compass(type = "arrow", 
             position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"),
               breaks = c(0,100,200)) 
# tm_credits("Sobotkova 2021",
# position = c("left", "bottom")) +
#tm_layout(title = "PPAP 2020")
# bg.color = "lightblue")

pdf("figures/Figure03.pdf",
    #colormodel = "gray", 
    width= 7.5, height = 10)
# Arrange the two tmap layouts above one another
current.mode <- tmap_mode("plot")
tmap_arrange(visibility,daily)  
tmap_mode(current.mode)

dev.off()


############## Densities

a <- tm_shape(units, bbox = st_bbox(units)) + 
  # tm_polygons(col = "ivory", border.col = "ivory3")+
  tm_polygons(col="cornsilk2", border.alpha = 0) +
  tm_symbols(size = "MostRecentlyComputedAncient",
             col = "darkblue",
             scale = 3,
             title.size = "Sherd count") +
  tm_shape(border_ppap, bbox = st_bbox(units)) +
  tm_borders(col = "grey", 
             lwd = 2)+
  # tm_compass(type = "arrow", position = c("right", "top")) +
  # tm_scale_bar(position = c("right", "top"), breaks = c(0,0.1,0.2)) + 
  # tm_credits("Sobotkova 2020", position = c("left", "bottom")) +
  # tm_layout(title = "Artefact count per survey unit")
  tm_layout(title = "A")
a
b <- tm_shape(units, bbox = st_bbox(units), unit = "m") + 
  tm_polygons(col="cornsilk2", border.alpha = 0)+
  tm_shape(features) + 
  tm_symbols(shape = 6, col="#666666", size = 0.2) +
  tm_shape(border_ppap, bbox = st_bbox(units)) +
  tm_borders(col = "grey", 
             lwd = 2)+
  tm_compass(type = "arrow", 
             position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"),
               breaks = c(0,100,200), 
               text.size = 0.75) + 
  #tm_credits("Sobotkova 2020", position = c("right", "bottom"), size = 0.8) +
  #tm_layout(title = "Registered standing features") 
  tm_layout(title = "B")

current.mode <- tmap_mode("plot")
tmap_arrange(a,b)

#### Artefact-log
# wrangle the data 
units <- units %>% 
  mutate(AncientDensityHa = 
           MostRecentlyComputedAncient / Area_ha,
         AncientDensityLog = case_when(
           MostRecentlyComputedAncient == 0 ~ 0,
           MostRecentlyComputedAncient > 0 ~ log10(MostRecentlyComputedAncient))) 

# plot it
density_log <- tm_shape(units, 
                        bbox = st_bbox(units), 
                        unit = "m") + 
  tm_polygons(col="AncientDensityLog", 
              border.alpha = 0,
              title = "Density of ancient artefacts (log) ") +
  tm_shape(border_ppap, 
           bbox = st_bbox(units)) +
  tm_borders(col = "grey", 
             lwd = 2)+
  tm_shape(features) + 
  tm_symbols(shape = 6, 
             col="#666666",
             size = 0.2) +
  tm_compass(type = "arrow", 
             position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"),
               breaks = c(0,100,200)) # + 
# tm_credits("Sobotkova 2021",
# position = c("left", "bottom")) +
# tm_layout(title = "Density of ancient artefacts")

pdf("figures/Figure04.pdf")
density_log
while (!is.null(dev.list()))  dev.off()

pdf("figures/Figure05.pdf")
features
dev.off()
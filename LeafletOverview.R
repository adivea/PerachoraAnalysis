### LEAFLET PERACHORA OVERVIEW MAP for internal use (low resolution output)


# Libraries
library(sf)
library(leaflet)
library(tidyverse)
library(mapview, quietly = T, warn.conflicts = F)

# load tabular survey data with latlong 
feature_data <- read_csv("C:/Users/Adela/Documents/Professional/Projects/Perachora/ALLFeature_20200129.csv")
feature_data
# library(sp)
# draw extra dot? NOT WORKING
# mapcenter = locator(1,type="p")


# start basemap (note the argument to hide the zoom buttons)
ppapimap <- 
leaflet(data = st_transform(units, crs = 4326)) %>% 
  addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
  addProviderTiles("Stamen.TonerBackground", group="Background") %>% 
  addProviderTiles("Stamen.TonerLite", group = "Lite") %>% 
  addProviderTiles("Stamen.Watercolor", group = "Watercolor") %>%
  addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
  
## add Features
  addCircleMarkers(data=feature_data, group="Features", color = "black",
                   radius = 7,   fillOpacity = 0.75, stroke = FALSE,
                   #clusterOptions = markerClusterOptions(),
             popup = paste0("FeatureID: ", features$identifier,
                            "<br> Type: ", features$Type,
                            "<br> LU Around: ", features$LanduseAro,
                            "<br> Description: ", features$Descriptio)) %>%
## add Major Sites
  addCircleMarkers(lat = c(38.0300201,38.028), lng = c(22.8586321,22.8530),
                   #  color = "#A9A9A9",
                   color = "black",
                   fill = "white",
                   fillOpacity = 0, 
                   stroke = TRUE,
                   popup = c("Fountain House", "Heraion") 
                   # label = c("Fountain House", "Heraion"),
                   # labelOptions = labelOptions(noHide = T, textsize = "20px")
  ) %>% 

 # add inset map
  addMiniMap(
    # tiles = providers$Esri.WorldTopoMap,
    tiles = providers$Stamen.TonerLite,
   # tiles = providers$Esri.OceanBasemap,
    position = 'topright', 
    width = 400, height = 400,
    zoomLevelOffset = -6,
    #centerFixed = c(),
    toggleDisplay = TRUE) %>%
  
addLayersControl(
    baseGroups = c("Lite","Watercolor","Background", "ESRI Aerial", "Topo"),
    options = layersControlOptions(collapsed = T)) %>% 
  
addScaleBar(position = c("bottomleft"), 
            options = scaleBarOptions(maxWidth = 300, metric = TRUE,imperial = FALSE))

# Show the result
ppapimap


# Save map as a html document (optional, replacement of pushing the export button)
# only works in root
library(htmlwidgets)
saveWidget(ppapimap, "ppapimap.html", selfcontained = TRUE)

help(png)
# Print result
# SVG graphics device
png("figures/01again.png", width = 7, units = "in", res = 300)

# Arrange the two tmap layouts above one another
ppapimap

# Close the graphics device
dev.off() 
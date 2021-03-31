# Mapview map with grid survey data from PPAP 2020 season

# libraries
library(tidyverse)
library(sf)
library(mapview)

suid = unit_data %>% st_centroid() %>% mutate(radius = MostRecentlyComputedAncient) %>% arrange(desc(MostRecentlyComputedAncient))
# build a map by providing a map tile provider and some options for the circles.
# with mapview the circle size will stay constant whatever the zoom level is.
mapview(suid, map.types = "Stamen.TonerLite", cex="radius", legend=FALSE,
        col.regions="#217844", lwd=0, alpha=0.4)


##################  GRIDDED MAP

## Create a grid
library(cartography)

st_is_valid(unit_data) # one polygon overlaps, it's number 46
ud <- unit_data[-46,] # eliminate topology error

# create a grid of squares
grid <- getGridLayer(
  x = ud, 
  cellsize = 25*25, 
  type = "regular", 
  var = c('Area_ha', 'MostRecentlyComputedAncient')
)

# Compute the density of artefacts
grid$dens <- grid$MostRecentlyComputedAncient / grid$Area_ha

# Display the map as choropleth layer
par(mar=c(0.2,0.2,1.4,0.2))

# The carto.pal() function give access to various cartographic color palettes
cols <- carto.pal("green.pal", 3,"wine.pal",3)

# The getBreaks() function is used to classify the variable
bks <- getBreaks(v = suid$AncientDensity, method = "quantile", nclass = 6)
?getBreaks() #a classification method; one of "fixed", "sd",  "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "q6", "geom", "arith", "em" or "msd" (see Details

# Plot
plot(st_geometry(unit_data), col = "ivory", border="ivory3", bg="azure", 
     xlim = st_bbox(unit_data)[c(1,3)], ylim =  st_bbox(unit_data)[c(2,4)])
choroLayer(grid, var = "dens", breaks = bks, 
           col = cols, border = "grey80", 
           lwd = 0.4, legend.pos = "topleft", 
           legend.title.txt = "Density of artefacts", add = T)
# plot(st_geometry(river.geom),col="azure",lwd = 3,add=TRUE)
# plot(st_geometry(roads.geom),col="#666666",lwd = 1.2,add=TRUE)
labelLayer(pref,txt="LIBELLE",halo=TRUE,col="white",bg="black",cex=.9) 
layoutLayer(title = "Density of surface artefacts per hectare", 
            sources = "PPAP 2020, OpenStreetMap contributors",
            author = "Sobotkova 2020", 
            theme = "green.pal", 
            col = "darkred", coltitle = "white", 
            tabtitle = TRUE,
            frame = TRUE, scale = 10)
north(pos = "topright")


################# HEXAGONAL GRID

# create a grid of hexagons
gridh <- getGridLayer(
  x = ud, 
  cellsize = 25*25, 
  type = "hexagonal", 
  var = c('Area_ha', 'MostRecentlyComputedAncient')
)


# Compute the density of artefacts
gridh$dens <- gridh$MostRecentlyComputedAncient / gridh$Area_ha

# Display the map as choropleth layer
par(mar=c(0.2,0.2,1.4,0.2))

# The carto.pal() function give access to various cartographic color palettes
cols <- carto.pal("green.pal", 3,"wine.pal",3)

# The getBreaks() function is used to classify the variable
bks <- getBreaks(v = suid$AncientDensity, method = "quantile", nclass = 6)
?getBreaks() #a classification method; one of "fixed", "sd",  "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "q6", "geom", "arith", "em" or "msd" (see Details

# Plot
plot(st_geometry(unit_data), col = "ivory", border="ivory3", bg="azure", 
     xlim = st_bbox(unit_data)[c(1,3)], ylim =  st_bbox(unit_data)[c(2,4)])
choroLayer(gridh, var = "dens", breaks = bks, 
           col = cols, border = "grey80", 
           lwd = 0.4, legend.pos = "topleft", 
           legend.title.txt = "Density of artefacts", add = T)
# plot(st_geometry(river.geom),col="azure",lwd = 3,add=TRUE)
# plot(st_geometry(roads.geom),col="#666666",lwd = 1.2,add=TRUE)
labelLayer(pref,txt="LIBELLE",halo=TRUE,col="white",bg="black",cex=.9) 
layoutLayer(title = "Density of surface artefacts per hectare", 
            sources = "PPAP 2020, OpenStreetMap contributors",
            author = "Sobotkova 2020", 
            theme = "green.pal", 
            col = "darkred", coltitle = "white", 
            tabtitle = TRUE,
            frame = TRUE, scale = 10)
north(pos = "topright")


## Plot the Grid with mapview
mapview(grid, map.types = "Stamen.TonerLite", zcol="MostRecentlyComputedAncient")

gridh$sqrcount <- sqrt(gridh$MostRecentlyComputedAncient)
mapview(gridh, map.types = "Stamen.TonerLite", zcol="sqrcount")

mapview(features, map.types = "Stamen.TonerLite")

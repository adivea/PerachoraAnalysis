## Perachora Overview map -- superseded my TmapOverview.Rmd

library(tidyverse)
library(sf)
library(tmap)
library(leaflet)
library(mapview)
library(raster)
library(googlesheets4)

# Study area polygon

df <- read_sheet("https://docs.google.com/spreadsheets/d/14XGlzReWCke0PbpZL8W3lIfnK4vqh_K84leWD9CS1GY/edit#gid=0", col_types = "nnnc") 
df[8,]
# Trying to figure out what coordinate system the Greeks used!!?
polygon <- df %>%
  st_as_sf(coords = c("X", "Y"), crs = 2100) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
polygon %>% mapview()

st_area(polygon)
st_write(polygon, "data/PPAPstudyarea.shp")
plot(polygon)

# PPAP data
features <- st_read("data/Features20200130.shp")
units <- st_read("data/AllTeams.shp")
SUattributes <- read_csv2("E:/Perachora2020/documentation/ALLSurveyUnits_noannotation.csv")
units_sf <- inner_join(units, SUattributes, by= c("SUID" = "SurveyUnitID")) # only those that have a polygon (all but day 1)


# Greek border
border <- st_read("data/Greece/periphereies/periphereies.shp", options = "ENCODING=WINDOWS-1252")
border <- st_transform(border, crs = 32634)
# Greek coastline from geography.gr
# coast <- st_read("data/Greece/aktogrammh/aktogrammh.shp")
# coast_34N <- st_transform(aktogramme, crs = st_crs(features))

# OSM Greek data
places <- st_read("data/Greece/places.shp", options = "ENCODING=UNICODE")
roads <- st_read("data/Greece/roads.shp")
waterways <- st_read("data/Greece/waterways.shp")
natural <- st_read("data/Greece/natural.shp")
points <- st_read("data/Greece/points.shp")

plot(border$geometry)
e <- drawExtent(show = TRUE, col = "red")

to34N <- function(shape){
      shape <- st_transform(shape, crs = 32634)
      shape <- st_crop(shape, e)
}
p <- to34N(points)
w <- to34N(waterways)
b <- to34N(border)
plot(b$geometry); plot(p$geometry, col = "red", add=TRUE)
plot(w$geometry, col = "blue", add =TRUE)

el <- getData("SRTM", lon = 22.6, lat = 38)
plot(el)
elev <- crop(projectRaster(el, crs = CRS("+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs")),e)
plot(elev)
?writeRaster()
writeRaster(elev, "data/PPAPelev.tif", format = "GTiff", overwrite=TRUE)

ee <- drawExtent(show = TRUE, col = "red")
ppap_e <- crop(elev, ee)

bb <- st_make_grid(units, n = 1)
plot(ppap_e); plot(units$geometry, col = "red", add=TRUE); plot(bb, add=TRUE)

ggplot(units_sf)+
  geom_sf(aes(fill = log(as.numeric(MostRecentlyComputedAncient)))) 
mapview(units_sf, zcol = "MostRecentlyComputedAncient")

units_sf %>% 
  mutate(TotalAncLog = case_when(
    MostRecentlyComputedAncient = 0 ~ NA,
    MostRecentlyComputedAncient > 0 ~ log(MostRecentlyComputedAncient)))

units_sf <- units_sf %>% 
  filter(MostRecentlyComputedAncient!=0) %>% 
  mutate(TotalAncLog = log(MostRecentlyComputedAncient),
         AncPerHa = MostRecentlyComputedAncient/(st_area(units_sf)*10000),
         AncPerHaLog = log(AncPerHa))

mapview(units_sf, zcol = "TotalAncLog")
eee <- drawExtent(show = TRUE, col = "red")
p_e <- crop(elev, eee)
plot(p_e);plot(units$geometry, add=TRUE);
plot(bb, add=TRUE) ;plot(b$geometry, add = TRUE)

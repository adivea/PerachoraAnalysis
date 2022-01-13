# Overview PRINT  JFA and JMA

plot(b_gr_simple$geometry)
gr <- drawExtent(show=TRUE, col = "red")
b_gr_simplecropped <- st_crop(b_gr_simple, gr)
plot(b_gr_simplecropped$geometry)
     
# Small tiffs (use default size)
overview2

# Larger tiff
overview3 <- tm_shape(elev, bbox = b4) +
  tm_raster(palette = "Greys", 
            title = "Elevation (m)", 
            style = "pretty") +
  tm_shape(lakes) +
  tm_polygons(col = "lightblue",border.col = "lightblue",
              lwd = 1) +
  tm_text("name",
          size = 1.1,
          col = "blue",
          ymod = -0.5,
          xmod = 0.5) +
  tm_shape(b_gr) +
  tm_borders(lwd = 3) +
  #col = "darkgrey")+ 
  tm_shape(ppap_area)+
  tm_borders(col = "purple",
             lwd = 3) +
  # tm_shape(bbb) +
  # tm_borders(col = "red",
  #            lwd = 3) +
  tm_shape(localtowns) +
  tm_bubbles(size = 1,
             shape = 21,
             col = "lightgrey")+
  tm_text("name",
          size = 2,
          ymod = -1.5,
          xmod = 0.5) +
  tm_shape(heraion_sf) +
  tm_bubbles(size = 1,
             col = "black")+
  tm_text("name",
          size = 2,
          ymod = -1.5,
          xmod = 0.5) +
  tm_shape(archsites) +
  tm_bubbles(size = 1,  # update sizes to suit print
             shape = 21,
             col = "lightgrey")+
  tm_text("name",
          size = 2,
          ymod = -1.5,
          xmod = 0.5) +
  tm_scale_bar(breaks = c(0, 2.5, 5),
               position = c("RIGHT", "bottom"),
               text.size = 1.75) +
  tm_compass(position = c("RIGHT", "top"),
             type = "rose", 
             size = 5) +
# tm_credits(text = "A. Sobotkova, 2021") +
# tm_layout(main.title = "Perachora Study Area 2020",
  tm_layout(frame = TRUE,
             legend.text.size = 1.75,
            legend.title.size = 2.5)
#         #  bg.color = "lightblue",
#           inner.margins = c(0, 0, 0, 0))
overview3

### Test print basemap Figure 01
tiff("figures/Fig01.tiff", width = 1300, height=877, units = "px",
     compression = "none")
overview3
dev.off()

### Figure 01 PRINT BASEMAP WITH Inset LOWRES
library(tmaptools)
library(grid)

tiff("figures/Fig01.tiff", width = 1300, height=8770, units = "px",
     compression = "none")
overview3

# print insets
print(greece, vp=viewport(x= 0.32, y= 0.17, width= 0.3, height= 0.3))

dev.off()




### Figure 01 PRINT BASEMAP WITH Inset HIRES
library(tmaptools)
library(grid)

tiff("figures/Fig01.tiff", width = 1000, height=1200, units = "px",
     compression = "none")
overview3

# print insets
print(greece, vp=viewport(x= 0.17, y= 0.165, width= 0.3, height= 0.3))

dev.off()


####################################### Fig 05  -- JMA

############ Figure 03  -- JMA
daily1 <- tm_shape(units, 
                  bbox = st_bbox(units), 
                  unit = "m") + 
  tm_polygons(col = "Date", 
              #border.alpha = 0,
              palette = "Accent",
              title = "Daily coverage",
              n = 5) +
  tm_shape(border_ppap, bbox = st_bbox(units)) +
  tm_borders(col = "grey", 
             lwd = 2)+
  tm_shape(rbind(heraion_sf, houseA)) +
  tm_bubbles(size = 0.01,
             col = "white")+
  tm_text("name",
          size = 2,
          ymod = -0.7,
          xmod = 0.5) + 
  tm_compass(type = "arrow", 
             position = c("right", "top"),
             size = 3) +
  tm_scale_bar(position = c("right", "bottom"),
               breaks = c(0,100,200),
               text.size = 2) +
  tm_layout(frame = TRUE,
            legend.text.size = 1.75,
            legend.title.size = 2.5)

tiff("figures/JMA/Fig03.tiff", width = 1100, height=1100, units = "px",
     pointsize = 12,
     compression = "none")
daily1
dev.off()

# Print
tiff("figures/JMA/Fig03.tiff", width = 1100, height=550, units = "px",
     pointsize = 12,
     compression = "none")
daily1
dev.off()

### Figure 3 and 5 VISIBILITY AND DAILY COVERAGE -- JMA

visibility1 <- tm_shape(units, 
                        bbox = st_bbox(units), 
                        unit = "m") + 
  tm_polygons(col = "Visibility", 
              #border.alpha = 0,
              palette = "Spectral",
              title = "Ground visibility",
              n = 5) +
  # tm_shape(aktog_ppap) +
  # tm_lines(col = "blue")+
  tm_shape(border_ppap, bbox = st_bbox(units)) +
  tm_borders(col = "grey", 
             lwd = 2)+
  tm_shape(rbind(heraion_sf, houseA)) +
  tm_bubbles(size = 0.01,
             col = "white")+
  tm_text("name",
          size = 2,
          ymod = -0.7,
          xmod = 0.5) + 
  tm_compass(type = "arrow",
             position = c("right", "top"),
             size = 3) +
  tm_scale_bar(position = c("right", "bottom"),
               breaks = c(0,100,200),
               text.size = 2) +
  tm_layout(frame = TRUE,
            legend.text.size = 1.75,
            legend.title.size = 2.5)

# Print

tiff("figures/JMA/Fig05.tiff", width = 1100, height=550, units = "px",
     pointsize = 12,
     compression = "none")
visibility1
dev.off()


### OPTIONAL COMBINED Figure 3 and 5  == JMA

visibility <- tm_shape(units, 
                       bbox = st_bbox(units), 
                       unit = "m") + 
  tm_polygons(col = "Visibility", 
              #border.alpha = 0,
              palette = "Spectral",
              title = "Ground visibility",
              n = 5) +
  # tm_shape(aktog_ppap) +
  # tm_lines(col = "blue")+
  tm_shape(border_ppap, bbox = st_bbox(units)) +
  tm_borders(col = "grey", 
             lwd = 2)+
  tm_shape(rbind(heraion_sf, houseA)) +
  tm_bubbles(size = 0.01,
             col = "white")+
  tm_text("name",
          size = 2,
          ymod = -0.7,
          xmod = 0.5) + 
  # tm_compass(type = "arrow", 
  #            position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"),
               breaks = c(0,100,200),
               text.size = 2) +
  tm_layout(frame = TRUE,
            legend.text.size = 1.75,
            legend.title.size = 2.5)

daily <- tm_shape(units, 
                  bbox = st_bbox(units), 
                  unit = "m") + 
  tm_polygons(col = "Date", 
              #border.alpha = 0,
              palette = "Accent",
              title = "Daily coverage",
              n = 5) +
  tm_shape(border_ppap, bbox = st_bbox(units)) +
  tm_borders(col = "grey", 
             lwd = 2)+
  tm_shape(rbind(heraion_sf, houseA)) +
  tm_bubbles(size = 0.01,
             col = "white")+
  tm_text("name",
          size = 2,
          ymod = -0.7,
          xmod = 0.5) + 
  tm_compass(type = "arrow", 
             position = c("right", "top"),
             size = 3) +
  # tm_scale_bar(position = c("right", "bottom"),
  #              breaks = c(0,100,200)) +
  tm_layout(frame = TRUE,
            legend.text.size = 1.75,
            legend.title.size = 2.5)

# Print combined
tiff("figures/JMA/Fig08.tiff", width = 1100, height=1100, units = "px",
     pointsize = 12,
     compression = "none")
tmap_arrange(daily, visibility) 
dev.off()

#####################################  Fig 06  -- JMA

### Figure 06 

density_log <- tm_shape(units, 
                        bbox = st_bbox(units), 
                        unit = "m") + 
  tm_polygons(col="AncientDensityLog", 
              #border.alpha = 0,
              title = "Density of artefacts (log10) ") +
  tm_shape(border_ppap, 
           bbox = st_bbox(units)) +
  tm_borders(col = "grey", 
             lwd = 2)+
  tm_shape(features) + 
  tm_symbols(shape = 6, 
             col="black",
             size = 0.65) +
  tm_shape(rbind(heraion_sf, houseA)) +
  tm_bubbles(size = 0.01,
             col = "white")+
  tm_text("name",
          size = 2,
          ymod = -0.7,
          xmod = 0.5) + 
  tm_compass(type = "arrow", 
             position = c("right", "top"),
             size = 3) +
  tm_scale_bar(position = c("right", "bottom"),
               breaks = c(0,100,200), 
               text.size = 2)+
  tm_layout(frame = TRUE,
            legend.text.size = 1.75,
            legend.title.size = 2.5)


tiff("figures/JMA/Fig06.tiff", width = 1100, height=550, units = "px",
     pointsize = 12,
     compression = "none")
density_log
dev.off()

#####################################  JFA
### Figure 11 Adjust for tiff

a1 <- tm_shape(units, bbox = st_bbox(units)) + 
  tm_polygons(col="cornsilk2", border.alpha = 0) +
  tm_symbols(size = "MostRecentlyComputedAncient",
             col = "darkblue",
             scale = 5,
             title.size = "Sherd count per unit") +
  tm_shape(border_ppap, bbox = st_bbox(units)) +
  tm_borders(col = "grey", 
             lwd = 4)+
  tm_shape(st_as_sf(contours))+
  tm_lines(col = "lightgrey")+
  tm_shape(lakes) +
  tm_polygons(col = "lightblue",border.col = "lightblue",
              lwd = 1) +
  tm_layout(title = "A", 
            legend.text.size = 1.5)
a1
b1 <- tm_shape(units, bbox = st_bbox(units), unit = "m") + 
  tm_polygons(col="cornsilk2", border.alpha = 0)+
  tm_shape(features) + 
  tm_symbols(shape = 6, col="#666666", size = 1) +
  tm_shape(border_ppap, bbox = st_bbox(units)) +
  tm_borders(col = "grey", 
             lwd = 4)+
  tm_shape(contours)+
  tm_lines(col = "lightgrey")+
  tm_shape(lakes) +
  tm_polygons(col = "lightblue",border.col = "lightblue",
              lwd = 1) +
  tm_compass(type = "arrow", 
             position = c("left", "bottom"),
             size = 6) +
  tm_scale_bar(position = c("left", "bottom"),
               breaks = c(0,100,200), 
               text.size = 1.75) + 
  tm_layout(title = "B", 
            title.size = 4,
            legend.text.size = 2)

current.mode <- tmap_mode("plot")
tmap_arrange(a1,b1)


### Figure 11 PRINT - actually I ended up converting the PDF to TIFF in Acrobat

tiff("figures/Figure11.tiff",
 #   colormodel = "gray",
    width = 1300, height=1000, units = "px",
    compression = "none")

# Arrange the two tmap layouts above one another
#current.mode <- tmap_mode("plot")
tmap_arrange(a1,b1)  # add c if you want to see densities
#tmap_mode(current.mode)

dev.off()


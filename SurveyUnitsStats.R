# Processing grid survey data from PPAP 2020 season


# packages
#install.packages("tidyverse")

# libraries
library(tidyverse)
library(sf)

# get data
units <- st_read("data/AllTeams.shp")
plot(units)
attributes(units)
SUattributes <- read_csv2("D:/Perachora2020/documentation/ALLSurveyUnits_noannotation.csv")
dim(SUattributes)

features <- st_read("D:/Perachora2020/geospatial/Vector/Features20200130.shp")

# join tables to spatial data in Survey Units
# check for consistency of shared key column int he two tables *needs to be numeric
class(units$SUID)
class(SUattributes$SurveyUnitID)


# join tables
units_spatial <- inner_join(units, SUattributes, by= c("SUID" = "SurveyUnitID")) # only those that have a polygon (all but day 1)
units_all <- right_join(units, SUattributes, by= c("SUID" = "SurveyUnitID"))

# some descriptive stats
units_spatial # 273 features

head(units_spatial)
attributes(units_all)
summary(units_spatial$MostRecentlyComputedTotal)
sum(units_spatial$Area_ha)
sum(units_spatial$MostRecentlyComputedTotal)
summary(units_spatial$Area_ha)

273*0.25*0.25 # check that the total area is 273 units multiplied by a 16th of hectare

# coerce back to dataframe
units <- st_set_geometry(units_spatial, NULL)  # I need to extract dataframe to do summaries
dim(units)

# summaries and stats ofarea by landuse
SUattributes %>% group_by(LandUse) %>% tally()

unitLU_summary <- units %>% 
  group_by(LandUse) %>%
  summarize(mean_area = mean(Area_ha, na.omit = TRUE),
            max_area = max(Area_ha),
            min_area = min(Area_ha),
            total_area = sum(Area_ha),
            count=n(),
            percent_of_totaln = count/273*100,
            percent_of_totalarea = total_area/16.67*100) %>% # count can be done with cumsum(LandUse)
  arrange(desc(count))


#https://stackoverflow.com/questions/43417530/how-to-compute-rowsums-using-tidyverse
unitLU_summary
unitLU_summary %>% mutate(total = rowSums(count))
cumsum(unitLU_summary$count)

write_csv(unitLU_summary, "LUsummary.csv")

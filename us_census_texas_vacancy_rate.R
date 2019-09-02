# Packages
library(tigris)
library(acs)
library(stringr) # to pad fips codes
library(rgdal)    # for readOGR and others
library(sp)       # for spatial objects
library(leaflet)  # for interactive maps (NOT leafletR here)
library(dplyr)    # for working with data frames
library(ggplot2)  # for plotting
library(RColorBrewer) # color pallete 
library(data.table)

api.key.install(key="402a5df4b45c37783f5a911e48b9e51bfac83992")


# B25004 VACANCY STATUS
# Vacant year - round units for rent: B25004 - Vacancy Status (For rent)
# renter occupied units: B25003 - Tenure (Renter occupied)
# vacant year round units rented but awaiting occupancy: B25004 - Vacancy Status (Rented, not occupied) 


geo<-geo.make(state=c("TX"),
              county="*", tract="*")
#geo<-geo.make(state=c("NY"),
#              county=c(5, 47, 61, 81, 85), tract="*")

# !!!! important note -- the package has not been updated to 2013
# data so I'm using the five year span that ends in 2012

# create a geographic set to grab tabular data (acs)
geo<-geo.make(state=c("TX"),
              county="*", tract="*")
#geo<-geo.make(state=c("NY"),
#              county=c(5, 47, 61, 81, 85), tract="*")

# !!!! important note -- the package has not been updated to 2013
# data so I'm using the five year span that ends in 2012

population_by_age_2017 <-acs.fetch(endyear = 2017, span = 5, geography = geo,
                                   table.number = "B01001", col.names = "pretty")

#Vacant year - round units for rent: B25004 - Vacancy Status (For rent)
#renter occupied units: B25003 - Tenure (Renter occupied)
vacant_year_round_units_2017 <- acs.fetch(geography=geo, table.number="B25004", endyear=2017, span=5, col.names="pretty")
vacant_year_round_units_2016 <- acs.fetch(geography=geo, table.number="B25004", endyear=2016, span=5, col.names="pretty")
vacant_year_round_units_2015 <- acs.fetch(geography=geo, table.number="B25004", endyear=2015, span=1, col.names="pretty")
vacant_year_round_units_2014 <- acs.fetch(geography=geo, table.number="B25004", endyear=2014, span=1, col.names="pretty")
vacant_year_round_units_2013 <- acs.fetch(geography=geo, table.number="B25004", endyear=2013, span=1, col.names="pretty")
vacant_year_round_units_2012 <- acs.fetch(geography=geo, table.number="B25004", endyear=2012, span=5, col.names="pretty")

renter_occupied_units_2017 <- acs.fetch(geography=geo, table.number="B25003", endyear=2017, span=5, col.names="pretty")
renter_occupied_units_2016 <- acs.fetch(geography=geo, table.number="B25003", endyear=2016, span=5, col.names="pretty")
renter_occupied_units_2015 <- acs.fetch(geography=geo, table.number="B25003", endyear=2015, span=1, col.names="pretty")
renter_occupied_units_2014 <- acs.fetch(geography=geo, table.number="B25003", endyear=2014, span=1, col.names="pretty")
renter_occupied_units_2013 <- acs.fetch(geography=geo, table.number="B25003", endyear=2013, span=1, col.names="pretty")
renter_occupied_units_2012 <- acs.fetch(geography=geo, table.number="B25003", endyear=2012, span=5, col.names="pretty")

names(attributes(renter_occupied_units_2017))
attr(renter_occupied_units_2017, "acs.colnames")
#[1] "Tenure: Total:"          "Tenure: Owner occupied"  "Tenure: Renter occupied"

names(attributes(vacant_year_round_units_2017))
attr(vacant_year_round_units_2017, "acs.colnames")
## "Vacancy Status: For rent"
## "Vacancy Status: Rented, not occupied"

# convert to a data.frame for merging

vacant_year_round_units_df_2017 <- data.frame(paste0(str_pad(vacant_year_round_units_2017@geography$state, 2, "left", pad="0"), 
                                        str_pad(vacant_year_round_units_2017@geography$county, 3, "left", pad="0"), 
                                        str_pad(vacant_year_round_units_2017@geography$tract, 6, "left", pad="0")), 
                                        vacant_year_round_units_2017@estimate[,c("Vacancy Status: For rent",
                                                             "Vacancy Status: Rented, not occupied")], 
                                 stringsAsFactors = FALSE)


rownames(vacant_year_round_units_df_2017)<-1:nrow(vacant_year_round_units_df_2017)
names(vacant_year_round_units_df_2017)<-c("GEOID", "total_vacant_year_round_units_2017","vacancy_status_rented_not_occupied_2017")

vacant_year_round_units_df_2012 <- data.frame(paste0(str_pad(vacant_year_round_units_2012@geography$state, 2, "left", pad="0"), 
                                                     str_pad(vacant_year_round_units_2012@geography$county, 3, "left", pad="0"), 
                                                     str_pad(vacant_year_round_units_2012@geography$tract, 6, "left", pad="0")), 
                                              vacant_year_round_units_2012@estimate[,c("Vacancy Status: For rent",
                                                                                       "Vacancy Status: Rented, not occupied")], 
                                              stringsAsFactors = FALSE)
rownames(vacant_year_round_units_df_2012)<-1:nrow(vacant_year_round_units_df_2012)
names(vacant_year_round_units_df_2012)<-c("GEOID", "total_vacant_year_round_units_2012","vacancy_status_rented_not_occupied_2012")

# Renter Occupied 
renter_occupied_units_df_2017 <- data.frame(paste0(str_pad(renter_occupied_units_2017@geography$state, 2, "left", pad="0"), 
                                                   str_pad(renter_occupied_units_2017@geography$county, 3, "left", pad="0"), 
                                                   str_pad(renter_occupied_units_2017@geography$tract, 6, "left", pad="0")), 
                                            renter_occupied_units_2017@estimate[,c("Tenure: Renter occupied")], 
                                            stringsAsFactors = FALSE)

rownames(renter_occupied_units_df_2017)<-1:nrow(renter_occupied_units_df_2017)
names(renter_occupied_units_df_2017)<-c("GEOID", "total_renter_occupied_units_2017")

renter_occupied_units_df_2012 <- data.frame(paste0(str_pad(renter_occupied_units_2012@geography$state, 2, "left", pad="0"), 
                                                   str_pad(renter_occupied_units_2012@geography$county, 3, "left", pad="0"), 
                                                   str_pad(renter_occupied_units_2012@geography$tract, 6, "left", pad="0")), 
                                            renter_occupied_units_2012@estimate[,c("Tenure: Renter occupied")], 
                                            stringsAsFactors = FALSE)

rownames(renter_occupied_units_df_2012)<-1:nrow(renter_occupied_units_df_2012)
names(renter_occupied_units_df_2012)<-c("GEOID", "total_renter_occupied_units_2012")

# Join data prior to statistics 
dt1 <- data.table(vacant_year_round_units_df_2017, key = "GEOID") 
dt2 <- data.table(vacant_year_round_units_df_2012, key = "GEOID")
joined.dt1.dt.2_vacant_year_round_units <- dt1[dt2]

dt1 <- data.table(renter_occupied_units_df_2017, key = "GEOID") 
dt2 <- data.table(renter_occupied_units_df_2012, key = "GEOID")
joined.dt1.dt.2_renter_occupied_units <- dt1[dt2]

rental_vacancy = joined.dt1.dt.2_vacant_year_round_units[joined.dt1.dt.2_renter_occupied_units]

#rental_vacancy = subset(rental_vacancy, vacancy_status_rented_not_occupied_2017 >0)

# Statistics
# Calculate 2012 to 2016 % change
# Vacancy Rate Calculation = vacant year round units for rent / (renter occupied units + vacant year round rented but awaiting occupancy + vacant year round units for rent)
rental_vacancy$vacancy_rate_2017 = (rental_vacancy$total_vacant_year_round_units_2017 / (rental_vacancy$total_renter_occupied_units_2017+rental_vacancy$vacancy_status_rented_not_occupied_2017+rental_vacancy$total_vacant_year_round_units_2017))*100
rental_vacancy$vacancy_rate_2012 = (rental_vacancy$total_vacant_year_round_units_2012 / (rental_vacancy$total_renter_occupied_units_2012+rental_vacancy$vacancy_status_rented_not_occupied_2012+rental_vacancy$total_vacant_year_round_units_2012))*100

rental_vacancy$percent = (rental_vacancy$vacancy_rate_2017 - rental_vacancy$vacancy_rate_2012) / rental_vacancy$vacancy_rate_2012*100
rental_vacancy$delta = (rental_vacancy$vacancy_rate_2017 - rental_vacancy$vacancy_rate_2012)

# Convert Inf and NA to 0 
rental_vacancy[is.na(rental_vacancy)] <- 0
rental_vacancy$percent[!is.finite(rental_vacancy$percent)] <- 0

# Subset data to remove all 0
sorted_rental_vacancy <- subset(rental_vacancy, percent != -100)

# Sort Data 
sorted_rental_vacancy = sorted_rental_vacancy[order(percent)]
head(sorted_rental_vacancy,100)

# Link for Cartographic Boundary Shapefiles - Census Tracts
# https://www.census.gov/geo/maps-data/data/cbf/cbf_tracts.html
#Tracts the hard way 
tract <- readOGR(dsn="C:/MultiFamily_investing/cb_2017_48_tract_500k", layer = "cb_2017_48_tract_500k")
## OGR data source with driver: ESRI Shapefile 
## Source: "x:/junk/claire/leaflet_plot", layer: "cb_2014_36_tract_500k"
## with 4906 features
## It has 9 fields


# GEOID tracts https://tigerweb.geo.census.gov/tigerwebmain/Files/bas17/tigerweb_bas17_tract_2016_tx.html
# convert the GEOID to a character
tract@data$GEOID<-as.character(tract@data$GEOID)

sorted_rental_vacancy_merged<- geo_join(tract, sorted_rental_vacancy, "GEOID", "GEOID")
# there are some tracts with no land that we should exclude
sorted_rental_vacancy_merged <- sorted_rental_vacancy_merged[sorted_rental_vacancy_merged$ALAND>0,]

## Make vector of colors for values smaller than 0 (20 colors)
rc1 <- colorRampPalette(colors = c("#ffffcc", "#fecc5c","#fd8d3c","#f03b20","#bd0026"), space = "Lab")(20)

## Make vector of colors for values larger than 0 (180 colors)
#rc2 <- colorRampPalette(colors = c("white", "#d7b5d8","#df65b0","#dd1c77","#980043"), space = "Lab")(180)
rc2 <- colorRampPalette(colors = c("#ffffcc", "#a1dab4","#41b6c4","#2c7fb8","#253494"), space = "Lab")(180)


## Combine the two color palettes
rampcols <- c(rc1, rc2)

## If you want to preview the color range, run the following code
#previewColors(colorNumeric(palette = rampcols, domain = NULL), values = -20:180)

popup <- paste0("GEOID: ", sorted_rental_vacancy_merged$GEOID, "<br>", "Vacancy Rate<br>", sorted_rental_vacancy_merged$vacancy_rate_2017)
pal <- colorNumeric(
  palette = "YlGnBu",
  #palette = rampcols,
  domain = sorted_rental_vacancy_merged$vacancy_rate_2017
)

map3<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = sorted_rental_vacancy_merged, 
              fillColor = ~pal(vacancy_rate_2017), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = sorted_rental_vacancy_merged$vacancy_rate_2017, 
            position = "bottomright", 
            #title = "Percent of Households<br>above $200k",
            title = "Vacancy Rate 2017",
            labFormat = labelFormat(suffix = "")) 
map3

# SAve Widgets
library(htmlwidgets)
saveWidget(map3, file="texas_2017_rental_vacancy_rate.html", selfcontained=TRUE)
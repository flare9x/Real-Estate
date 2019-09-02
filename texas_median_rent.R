# Visualise US census median rent

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


# #B25064 - MEDIAN GROSS RENT (DOLLARS)
# create a geographic set to grab tabular data (acs)
geo<-geo.make(state=c("TX"),
              county="*", tract="*")
#geo<-geo.make(state=c("NY"),
#              county=c(5, 47, 61, 81, 85), tract="*")

# !!!! important note -- the package has not been updated to 2013
# data so I'm using the five year span that ends in 2012

rent_median_2017 <-acs.fetch(endyear = 2017, span = 5, geography = geo,
                             table.number = "B25064", col.names = "pretty")


# use of col.names = "pretty" above gives the full column definitions
# if you want Census variable IDs use col.names="auto".

# table.number = https://censusreporter.org/topics/table-codes/
# find tables = https://censusreporter.org/tables/B02003/

names(attributes(rent_median_2017))
## [1] "endyear"        "span"           "acs.units"      "currency.year"  "modified"       "geography"      "acs.colnames"  
## [8] "estimate"       "standard.error" "class"  
attr(rent_median_2017, "acs.colnames")
##  [1] "Median Gross Rent (Dollars): Median gross rent"            

# convert to a data.frame for merging
median_rent_df_2017 <- data.frame(paste0(str_pad(rent_median_2017@geography$state, 2, "left", pad="0"), 
                                         str_pad(rent_median_2017@geography$county, 3, "left", pad="0"), 
                                         str_pad(rent_median_2017@geography$tract, 6, "left", pad="0")), 
                                  rent_median_2017@estimate[,c("Median Gross Rent (Dollars): Median gross rent")], 
                                  stringsAsFactors = FALSE)



median_rent_df_2017 <- select(median_rent_df_2017, 1:2)
rownames(median_rent_df_2017)<-1:nrow(median_rent_df_2017)
names(median_rent_df_2017)<-c("GEOID", "rent_median_2017")

# Replace -66666 vaalues 
median_rent_df_2017$rent_median_2017[median_rent_df_2017$rent_median_2017 == -666666666] <- 0

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

median_rent_merged <- geo_join(tract, median_rent_df_2017, "GEOID", "GEOID")
# there are some tracts with no land that we should exclude
median_rent_merged <- median_rent_merged[median_rent_merged$ALAND>0,]

popup <- paste0("GEOID: ", median_rent_merged$GEOID, "<br>", "Median Rent<br>", median_rent_merged$rent_median_2017)
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = median_rent_merged$rent_median_2017
)

map3<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = median_rent_merged, 
              fillColor = ~pal(rent_median_2017), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = median_rent_merged$rent_median_2017, 
            position = "bottomright", 
            #title = "Percent of Households<br>above $200k",
            title = "2017<br>Median Rent",
            labFormat = labelFormat(suffix = "")) 
map3


# SAve Widgets
library(htmlwidgets)
saveWidget(map3, file="texas_2017_median_rent.html", selfcontained=FALSE)

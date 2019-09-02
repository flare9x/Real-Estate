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


# #B25077 MEDIAN VALUE (DOLLARS)
# create a geographic set to grab tabular data (acs)
geo<-geo.make(state=c("TX"),
              county="*", tract="*")
#geo<-geo.make(state=c("NY"),
#              county=c(5, 47, 61, 81, 85), tract="*")

# !!!! important note -- the package has not been updated to 2013
# data so I'm using the five year span that ends in 2012

value_median_2017 <-acs.fetch(endyear = 2017, span = 5, geography = geo,
                             table.number = "B25077", col.names = "pretty")

value_median_2011 <-acs.fetch(endyear = 2011, span = 5, geography = geo,
                             #table.name = "Housing Characteristics",
                             table.number = "B25077", 
                             col.names = "pretty")

# use of col.names = "pretty" above gives the full column definitions
# if you want Census variable IDs use col.names="auto". Here are the
# variables we want with pretty and auto results.
#"Household Income: Total:" ("B19001_001")
#"Household Income: $200,000 or more" ("B19001_017")

# table.number = https://censusreporter.org/topics/table-codes/
# find tables = https://censusreporter.org/tables/B02003/


# the resulting "income" object is not a data.frame it's a list
# to see what's available

names(attributes(value_median_2017))
##  [1] "endyear"        "span"           "acs.units"      "currency.year" 
##  [5] "modified"       "geography"      "acs.colnames"   "estimate"      
##  [9] "standard.error" "class"
attr(value_median_2017, "acs.colnames")
##  [1] "Household Income: Total:"              
##  [2] "Household Income: Less than $10,000"   
##  [3] "Household Income: $10,000 to $14,999"  
##  [4] "Household Income: $15,000 to $19,999"  
##  [5] "Household Income: $20,000 to $24,999"  
##  [6] "Household Income: $25,000 to $29,999"  
##  [7] "Household Income: $30,000 to $34,999"  
##  [8] "Household Income: $35,000 to $39,999"  
##  [9] "Household Income: $40,000 to $44,999"  
## [10] "Household Income: $45,000 to $49,999"  
## [11] "Household Income: $50,000 to $59,999"  
## [12] "Household Income: $60,000 to $74,999"  
## [13] "Household Income: $75,000 to $99,999"  
## [14] "Household Income: $100,000 to $124,999"
## [15] "Household Income: $125,000 to $149,999"
## [16] "Household Income: $150,000 to $199,999"
## [17] "Household Income: $200,000 or more"

# convert to a data.frame for merging
median_value_df_2017 <- data.frame(paste0(str_pad(value_median_2017@geography$state, 2, "left", pad="0"), 
                                         str_pad(value_median_2017@geography$county, 3, "left", pad="0"), 
                                         str_pad(value_median_2017@geography$tract, 6, "left", pad="0")), 
                                  value_median_2017@estimate[,c("Median Value (Dollars) for Owner-Occupied Housing Units: Median value (dollars)")], 
                                  stringsAsFactors = FALSE)



median_value_df_2017 <- select(median_value_df_2017, 1:2)
rownames(median_value_df_2017)<-1:nrow(median_value_df_2017)
names(median_value_df_2017)<-c("GEOID", "value_median_2017")

# 2011
median_value_df_2011 <- data.frame(paste0(str_pad(value_median_2011@geography$state, 2, "left", pad="0"), 
                                         str_pad(value_median_2011@geography$county, 3, "left", pad="0"), 
                                         str_pad(value_median_2011@geography$tract, 6, "left", pad="0")), 
                                  value_median_2011@estimate[,c("Median Value (Dollars) for Owner-Occupied Housing Units: Median value (dollars)")], 
                                  stringsAsFactors = FALSE)



median_value_df_2011 <- select(median_value_df_2011, 1:2)
rownames(median_value_df_2011)<-1:nrow(median_value_df_2011)
names(median_value_df_2011)<-c("GEOID", "value_median_2011")

# Join data prior to statistics 
dt1 <- data.table(median_value_df_2017, key = "GEOID") 
dt2 <- data.table(median_value_df_2011, key = "GEOID")

joined.dt1.dt.2 <- dt1[dt2]

# Statistics

# Replace -66666 vaalues 
joined.dt1.dt.2$value_median_2017[joined.dt1.dt.2$value_median_2017 == -666666666] <- 0
#joined.dt1.dt.2 = joined.dt1.dt.2$percent[joined.dt1.dt.2$percent > 0]
#all = unique(joined.dt1.dt.2$percent)
#all[all > 0]

#income_df$percent <- 100*(income_df$Unemployed/income_df$total)
joined.dt1.dt.2$percent = ((joined.dt1.dt.2$value_median_2017 - joined.dt1.dt.2$value_median_2011)/joined.dt1.dt.2$value_median_2011)*100
joined.dt1.dt.2$percent = joined.dt1.dt.2$value_median_2011 #- joined.dt1.dt.2$value_median_2011
joined.dt1.dt.2$percent = joined.dt1.dt.2$value_median_2017 #- joined.dt1.dt.2$value_median_2011

# Convert Inf and NA to 0 
joined.dt1.dt.2[is.na(joined.dt1.dt.2)] <- 0
joined.dt1.dt.2$percent[!is.finite(joined.dt1.dt.2$percent)] <- 0
# Link for Cartographic Boundary Shapefiles - Census Tracts
# https://www.census.gov/geo/maps-data/data/cbf/cbf_tracts.html
#Tracts the hard way 
tract <- readOGR(dsn="C:/MultiFamily_investing/cb_2017_48_tract_500k", layer = "cb_2017_48_tract_500k")
## OGR data source with driver: ESRI Shapefile 
## Source: "x:/junk/claire/leaflet_plot", layer: "cb_2014_36_tract_500k"
## with 4906 features
## It has 9 fields


# GEOID tracts https://tigerweb.geo.census.gov/tigerwebmain/Files/bas17/tigerweb_bas17_tract_2017_tx.html
# convert the GEOID to a character
tract@data$GEOID<-as.character(tract@data$GEOID)

median_value_merged<- geo_join(tract, joined.dt1.dt.2, "GEOID", "GEOID")
# there are some tracts with no land that we should exclude
median_value_merged <- median_value_merged[median_value_merged$ALAND>0,]

## Make vector of colors for values smaller than 0 (20 colors)
rc1 <- colorRampPalette(colors = c("#ffffcc", "#fecc5c","#fd8d3c","#f03b20","#bd0026"), space = "Lab")(20)

## Make vector of colors for values larger than 0 (180 colors)
#rc2 <- colorRampPalette(colors = c("white", "#d7b5d8","#df65b0","#dd1c77","#980043"), space = "Lab")(180)
rc2 <- colorRampPalette(colors = c("#ffffcc", "#a1dab4","#41b6c4","#2c7fb8","#253494"), space = "Lab")(180)


## Combine the two color palettes
rampcols <- c(rc1, rc2)

## If you want to preview the color range, run the following code
#previewColors(colorNumeric(palette = rampcols, domain = NULL), values = -20:180)

popup <- paste0("GEOID: ", median_value_merged$GEOID, "<br>", "Median home value<br>", median_value_merged$value_median_2017)
pal <- colorNumeric(
  palette = "YlGnBu",
  #palette = rampcols,
  domain = median_value_merged$value_median_2017
)

map3<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = median_value_merged, 
              fillColor = ~pal(value_median_2017), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = median_value_merged$value_median_2017, 
            position = "bottomright", 
            #title = "Percent of Households<br>above $200k",
            title = "2017<br>Median Home Value",
            labFormat = labelFormat(suffix = "$")) 
map3


# SAve Widgets
library(htmlwidgets)
saveWidget(map1, file="map1.html", selfcontained=TRUE)
saveWidget(map2, file="map2.html", selfcontained=TRUE)
saveWidget(map3, file="texas_2017_median_home_value.html", selfcontained=TRUE)
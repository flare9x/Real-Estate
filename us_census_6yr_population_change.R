# US Census Population 6 yr change

# create a geographic set to grab tabular data (acs)
geo<-geo.make(state=c("TX"),
              county="*", tract="*")
#geo<-geo.make(state=c("NY"),
#              county=c(5, 47, 61, 81, 85), tract="*")

# !!!! important note -- the package has not been updated to 2013
# data so I'm using the five year span that ends in 2012

population_2017 <-acs.fetch(endyear = 2017, span = 5, geography = geo,
                              table.number = "B01003", col.names = "pretty")

population_2011 <-acs.fetch(endyear = 2011, span = 5, geography = geo,
                              table.number = "B01003", col.names = "pretty")

# use of col.names = "pretty" above gives the full column definitions
# if you want Census variable IDs use col.names="auto". Here are the
# variables we want with pretty and auto results.
#"Household Income: Total:" ("B19001_001")
#"Household Income: $200,000 or more" ("B19001_017")

# table.number = https://censusreporter.org/topics/table-codes/
# find tables = https://censusreporter.org/tables/B02003/


# the resulting "income" object is not a data.frame it's a list
# to see what's available

names(attributes(population_2017))
attr(population_2017, "acs.colnames")

# convert to a data.frame for merging
population_df_2017 <- data.frame(paste0(str_pad(population_2017@geography$state, 2, "left", pad="0"), 
                                          str_pad(population_2017@geography$county, 3, "left", pad="0"), 
                                          str_pad(population_2017@geography$tract, 6, "left", pad="0")), 
                                   population_2017@estimate[,c("Total Population: Total")], 
                                   stringsAsFactors = FALSE)



population_df_2017 <- select(population_df_2017, 1:2)
rownames(population_df_2017)<-1:nrow(population_df_2017)
names(population_df_2017)<-c("GEOID", "total_population_2017")

# 2011
population_df_2011 <- data.frame(paste0(str_pad(population_2011@geography$state, 2, "left", pad="0"), 
                                          str_pad(population_2011@geography$county, 3, "left", pad="0"), 
                                          str_pad(population_2011@geography$tract, 6, "left", pad="0")), 
                                   population_2011@estimate[,c("Total Population: Total")], 
                                   stringsAsFactors = FALSE)



population_df_2011 <- select(population_df_2011, 1:2)
rownames(population_df_2011)<-1:nrow(population_df_2011)
names(population_df_2011)<-c("GEOID", "total_population_2011")

# Join data prior to statistics 
dt1 <- data.table(population_df_2017, key = "GEOID") 
dt2 <- data.table(population_df_2011, key = "GEOID")

joined.dt1.dt.2 <- dt1[dt2]

# Statistics

#income_df$percent <- 100*(income_df$Unemployed/income_df$total)
joined.dt1.dt.2$percent = ((joined.dt1.dt.2$total_population_2017 - joined.dt1.dt.2$total_population_2011)/joined.dt1.dt.2$total_population_2011)*100
#joined.dt1.dt.2$percent = (joined.dt1.dt.2$total_population_2017 - joined.dt1.dt.2$total_population_2011)


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

population_merged<- geo_join(tract, joined.dt1.dt.2, "GEOID", "GEOID")
# there are some tracts with no land that we should exclude
population_merged <- population_merged[population_merged$ALAND>0,]

## Make vector of colors for values smaller than 0 (20 colors)
rc1 <- colorRampPalette(colors = c("#ffffcc", "#fecc5c","#fd8d3c","#f03b20","#bd0026"), space = "Lab")(20)

## Make vector of colors for values larger than 0 (180 colors)
#rc2 <- colorRampPalette(colors = c("white", "#d7b5d8","#df65b0","#dd1c77","#980043"), space = "Lab")(180)
rc2 <- colorRampPalette(colors = c("#ffffcc", "#a1dab4","#41b6c4","#2c7fb8","#253494"), space = "Lab")(180)


## Combine the two color palettes
rampcols <- c(rc1, rc2)

## If you want to preview the color range, run the following code
#previewColors(colorNumeric(palette = rampcols, domain = NULL), values = -20:180)

popup <- paste0("GEOID: ", population_merged$GEOID, "<br>", "Population Change<br>", round(population_merged$percent,2))
pal <- colorNumeric(
  #palette = "YlGnBu",
  palette = rampcols,
  domain = population_merged$percent
)

map3<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = population_merged, 
              fillColor = ~pal(percent), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = population_merged$percent, 
            position = "bottomright", 
            #title = "Percent of Households<br>above $200k",
            title = "2017-2011<br>Population 6 Year % Change",
            labFormat = labelFormat(suffix = "%")) 
map3


# SAve Widgets
library(htmlwidgets)
saveWidget(map1, file="map1.html", selfcontained=TRUE)
saveWidget(map2, file="map2.html", selfcontained=TRUE)
saveWidget(map3, file="texas_2011_to_2017_population_5_yr_change.html", selfcontained=TRUE)
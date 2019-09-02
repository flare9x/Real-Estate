library(rvest)
library(stringr)

# Add selector gadget for CSS
# https://chrome.google.com/webstore/detail/selectorgadget/mhjhnkcfbdhnjickkkdbjoemdmbfginb/related?hl=en

# email results 
#https://medium.com/airbnb-engineering/using-googlesheets-and-mailr-packages-in-r-to-automate-reporting-c09579e0377f

i = 9

page_nums= 0:22
links_all_out = list()
rent_month_all_out = list()
unit_mix_all_out = list()
bathrooms_all_out = list()
bedrooms_all_out = list()

# Galveston, TX https://www.gosection8.com/Section-8-housing-in-Galveston-Galveston-TX/pg
# Harris County https://www.gosection8.com/Section-8-housing-in-Houston-Harris-TX/pg=1/


for (i in 1:length(page_nums)) {
  
  # Reading the HTML code from the website
  section_8_pages = read_html(paste0("https://www.gosection8.com/Section-8-housing-in-Houston-Harris-TX/pg=",page_nums[i],"/"))
  
  # Grab the address and links
  address_link <- html_nodes(section_8_pages,'.address')
  html_links = html_attr(address_link,"href")
  # loop to paste www.gosection8.com to link 
  loop_links = list()
  for (j in 1:length(html_links)) {
    loop_links[[j]] = paste0("https://www.gosection8.com",html_links[j])
  }
  html_link_output = do.call(rbind,loop_links)
  # Extract rent per month
  rent_mo_data_html <- html_nodes(section_8_pages,'.rent')
  rent_mo_data <- html_text(rent_mo_data_html)
  unit_mix_html = html_nodes(section_8_pages,'.subbold')
  unit_mix_data = html_text(unit_mix_html)
  
  # subset to remove telephone numbers (unit mix)
  unit_mix_subset = list()
  for (j in 1:length(unit_mix_data)) {
    if (nchar(unit_mix_data[[j]]) < 50) {
      unit_mix_subset[[j]] = NA
    } else
      unit_mix_subset[[j]] = unit_mix_data[[j]]
  }
  
  # data frame and then subset data frame to remove all NA
  unit_mix_data = do.call(rbind,unit_mix_subset)
  unit_mix_data <- unit_mix_data[!is.na(unit_mix_data),]
  
  # Print non matching
  if (length(unit_mix_data) == length(rent_mo_data)) {
    print("Matches")
  } else {
    cat("Non matching, check code on iteration",i)
  }
  
  # loop to extract numbers from string 
  rent_mo_extract = list()
  unit_mix_extract = list()
  bedrooms = list()
  bathrooms = list()

  for (j in 1:length(rent_mo_data)) {
    temp_rent_mo = str_extract_all(rent_mo_data[[j]], "\\d+")[[1]]
    rent_mo_extract[[j]] = as.numeric(paste(temp_rent_mo, collapse=""))
    #if (nchar(temp_rent_mo) == 2) {
     # rent_mo_extract[[j]] = as.numeric(paste0(as.numeric(paste(temp_rent_mo, collapse="")),"00"))
    #} else {
     # rent_mo_extract[[j]] = as.numeric(paste(temp_rent_mo, collapse=""))
    #}
    temp_unit_mix = as.numeric(str_extract_all(unit_mix_data[j], "[0-9]+")[[1]])
    unit_mix_extract[[j]] = as.numeric(paste(temp_unit_mix, collapse=""))
  }
  
  # make data frame 
  rent_mo_output = as.data.frame(do.call(rbind,rent_mo_extract))
  unit_mix_output = as.data.frame(do.call(rbind,unit_mix_extract))
  
  # Extract bedrooms and bathrooms 
  bedrooms = list()
  bathrooms = list()
  for (j in 1:nrow(unit_mix_output)) { 
    if (nchar(unit_mix_output[j,1]) == 3) {
      strings = substring(unit_mix_output[j,1], 1:3, 1:3)
      bathrooms[[j]] = paste0(strings[2],strings[3])
      bedrooms[[j]] = strings[1]
    } else if (nchar(unit_mix_output[j,1]) == 2) {
      strings = substring(unit_mix_output[j,1], 1:2, 1:2)
      bathrooms[[j]] = strings[2]
      bedrooms[[j]] = strings[1]
    } else if (nchar(unit_mix_output[j,1]) == 1) {
      strings = substring(unit_mix_output[j,1], 1:1, 1:1)
      bathrooms[[j]] = strings[1]
      bedrooms[[j]] = 0 #efficiency / studio
    } else if (nchar(unit_mix_output[j,1]) == 4) {
      strings = substring(unit_mix_output[j,1], 1:3, 1:3)
      bathrooms[[j]] = paste0(strings[2],strings[3])
      bedrooms[[j]] = strings[1] #efficiency / studio
    }
  } # end j loop
  
  # make data frame 
  bathroom_output = as.data.frame(do.call(rbind,bathrooms))
  bedroom_output = as.data.frame(do.call(rbind,bedrooms))
  
  # Save outputs 
  links_all_out[[i]] = html_link_output
  rent_month_all_out[[i]] = cbind(rent_mo_output)
  unit_mix_all_out[[i]] = cbind(unit_mix_output)
  bathrooms_all_out[[i]] = cbind(bathroom_output)
  bedrooms_all_out[[i]] = cbind(bedroom_output)
  
  length(unit_mix_all_out)
  
} # end loop 

# Export all 
all_links_final = as.data.frame(do.call(rbind,links_all_out))
all_rent_mo = as.data.frame(do.call(rbind,rent_month_all_out))
all_bathrooms = as.data.frame(do.call(rbind,bathrooms_all_out))
all_bedrooms = as.data.frame(do.call(rbind,bedrooms_all_out))

# final 
all_df = data.frame(all_links_final,all_rent_mo,all_bathrooms,all_bedrooms)

names = c("links","rent_per_mo","bathroom_no","bedroom_no")
colnames(all_df) = names

# Average price per bedroom
bedrooms = 1:6
average_rent_per_bdrm = list()
min_rent_per_bdrm = list()
max_rent_per_bdrm = list()
sd_rent_per_bdrm = list()

for (i in 1:6) { 
  average_rent_per_bdrm[[i]] = mean(all_df[all_df$bedroom_no == i, "rent_per_mo"], na.rm = TRUE)
  min_rent_per_bdrm[[i]] = min(all_df[all_df$bedroom_no == i, "rent_per_mo"], na.rm = TRUE)
  max_rent_per_bdrm[[i]] = max(all_df[all_df$bedroom_no == i, "rent_per_mo"], na.rm = TRUE)
  sd_rent_per_bdrm[[i]] = sd(all_df[all_df$bedroom_no == i, "rent_per_mo"], na.rm = TRUE)
}

# make df
bedroom_mean_price_mo = as.data.frame(do.call(rbind,average_rent_per_bdrm))
bedroom_min_price_mo = as.data.frame(do.call(rbind,min_rent_per_bdrm))
bedroom_max_price_mo = as.data.frame(do.call(rbind,max_rent_per_bdrm))
sd_price_mo = as.data.frame(do.call(rbind,sd_rent_per_bdrm))

# obs from mean = z = x - mean / stdev
your_listing_price = 500
bedroom_select = 2
z = (your_listing_price - bedroom_mean_price_mo[bedroom_select,1]) / sd_price_mo[bedroom_select,1]
z

# Plot 1
city_name = "Houston"

barplot(bedroom_mean_price_mo$V1, names.arg = bedrooms,col = "grey", xlab="Bedroom Number", ylab="Rent Per Month", yaxt = "n", main=paste("All gosection 8 litings ",city_name,", TX - Per Bedroom Number - Average Rent Per Month"), cex.main = 1)
axis(2, at=seq(0,max(bedroom_mean_price_mo$V1, na.rm=T),100))

# count total available stock 
bedrooms = 1:6
total_stock_per_bedroom = list()
for (i in 1:6) { 
  total_stock_per_bedroom[[i]] = sum(all_df$bedroom_no == i)
}

# make df
total_unit_stock = as.data.frame(do.call(rbind,total_stock_per_bedroom))

# Plot 2
barplot(total_unit_stock$V1, names.arg = bedrooms,col = "grey", xlab="Total Units - Bedroom Size", ylab="Total Stock", yaxt = "n", main=paste("All gosection 8 litings ",city_name,", TX - Total Unit Stock Per Bedroom Size"), cex.main = 1)
axis(2, at=seq(0,max(total_unit_stock$V1, na.rm=T),1))

# Final Stats data frame 
stats_all = data.frame(bedroom_mean_price_mo, sd_price_mo, bedroom_min_price_mo, bedroom_max_price_mo,total_unit_stock)
names = c("Mean Rent Per Month","Standard Deviation","Min Price","Max Price","Total Unit Stock")
colnames(stats_all) = names
stats_all = round(stats_all, digits = 2)

# Plot 3
library(gridExtra)
grid.table(stats_all)
graphics.off() 

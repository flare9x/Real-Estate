# gosection8.com Prospects 
# Andrew Bannerman - 8/13/2019

# Read files 
data_dir <- "C:/Users/Andrew.Bannerman/Desktop/Real Estate/Real Estate Portfolio/Galveston section8 prospects/p"
data_list = list()
i=1
for (i in 1:5) {
  temp = read.csv(paste0(data_dir,i,".csv"),stringsAsFactors = FALSE, header=F)
  str(temp)
  col_nums = length(colnames(temp))
  if (col_nums == 2) {
    temp_df = data.frame(paste(temp$V1,temp$V2),stringsAsFactors = FALSE)
    names = "one"
    colnames(temp_df) = names
  } else if (col_nums == 3) {
    temp_df = data.frame(paste(temp$V1,temp$V2),stringsAsFactors = FALSE)
    names = "one"
    colnames(temp_df) = names
  } else if (col_nums == 1) {
    temp_df = data.frame(temp,stringsAsFactors = FALSE)
    names = "one"
    colnames(temp_df) = names
  }
  data_list[[i]] = temp_df
  cat("\n this is iteration",i)
}


# make one data frame 
all_data = as.data.frame(do.call(rbind,data_list))
all_data = data.frame(all_data[!apply(all_data == "  ", 1, all),])
city_index = seq(4,nrow(all_data),10)
voucher_price_index = seq(6,nrow(all_data),10)
bedrooms_index = seq(5,nrow(all_data),10)

# Extract City Names
city_extract = list()
i=1
for(i in 1:length(city_index)) {
  city_extract[[i]] = paste(all_data[city_index[i],1])
}

city = do.call(rbind,city_extract)

# Extract voucher prices 
voucher_extract = list()
i=1
for(i in 1:length(voucher_price_index)) {
  temp = str_extract_all(all_data[voucher_price_index[i],1], "\\d+")[[1]]
  voucher_extract[[i]] = as.numeric(paste(temp, collapse=""))
}

voucher = do.call(rbind,voucher_extract)

# Extract bedrooms  
bedroom_extract = list()
j=1
for(j in 1:length(bedrooms_index)) {
  temp = paste(all_data[bedrooms_index[j],1])
  temp = str_extract_all(temp, "\\d+")[[1]]
  temp = as.numeric(paste(temp, collapse=""))
  if (nchar(temp) == 2) {
    strings = substring(temp, 1:2, 1:2)
    bedroom_extract[[j]] = as.numeric(strings[1]) # pick lower bedroom limit
  } else if (nchar(temp) == 1) {
    strings = substring(temp, 1:1, 1:1)
    bedroom_extract[[j]] = as.numeric(strings[1])
  }
   # strings = substring(temp, 1:1, 1:1)
  #bedroom_extract[[i]] = paste(all_data[bedrooms_index[i],1])
}

bedrooms = do.call(rbind,bedroom_extract)

# all data 
all_df = data.frame(city,voucher,bedrooms)
names = c("city","voucher_amount","bedrooms")
colnames(all_df) = names

# Average voucher price per bedroom
bedrooms = 1:6
average_voucher_per_bdrm = list()
min_rent_per_bdrm = list()
max_rent_per_bdrm = list()
sd_rent_per_bdrm = list()

for (i in 1:6) { 
  average_voucher_per_bdrm[[i]] = mean(all_df[all_df$bedrooms == i, "voucher_amount"], na.rm = TRUE)
  min_rent_per_bdrm[[i]] = min(all_df[all_df$bedrooms == i, "voucher_amount"], na.rm = TRUE)
  max_rent_per_bdrm[[i]] = max(all_df[all_df$bedrooms == i, "voucher_amount"], na.rm = TRUE)
  sd_rent_per_bdrm[[i]] = sd(all_df[all_df$bedrooms == i, "voucher_amount"], na.rm = TRUE)
}

# make df
bedroom_mean_price_mo = as.data.frame(round(do.call(rbind,average_voucher_per_bdrm),digits=2))
bedroom_min_price_mo = as.data.frame(round(do.call(rbind,min_rent_per_bdrm),digits=2))
bedroom_max_price_mo = as.data.frame(round(do.call(rbind,max_rent_per_bdrm),digits=2))
sd_price_mo = as.data.frame(round(do.call(rbind,sd_rent_per_bdrm),digits=2))
bedroom_row_names = c("1 Bedroom","2 Bedroom","3 Bedroom","4 Bedroom","5 Bedroom","6 Bedroom")
all_stats = data.frame(bedroom_row_names,bedroom_mean_price_mo, sd_price_mo, bedroom_min_price_mo, bedroom_max_price_mo)
names = c("Bedroom Number","Mean Voucher Per Month","Standard Deviation","Min Price","Max Price")
colnames(all_stats) = names
# Plot 
library(gridExtra)
grid.table(all_stats)
graphics.off() 

# Count total number bedrooms voucher holder approved for
bedrooms = 1:6
total_approved_bedroom = list()
for (i in 1:6) { 
  total_approved_bedroom[[i]] = sum(all_df$bedrooms == i)
}

# make df
total_approved = as.data.frame(do.call(rbind,total_approved_bedroom))

# Plots
city_name = "Galveston"

barplot(bedroom_mean_price_mo$V1, names.arg = bedrooms,col = "grey", xlab="Bedroom Number", ylab="Rent Per Month", yaxt = "n", main=paste("All gosection 8 prospects ",city_name,", TX - Per Bedroom Number - Average Voucher Price Per Month"), cex.main = 1)
axis(2, at=seq(0,max(bedroom_mean_price_mo$V1, na.rm=T),100))

barplot(total_approved$V1, names.arg = bedrooms,col = "grey", xlab="Total Units - Bedroom Size", ylab="Total Stock", yaxt = "n", main=paste("All gosection 8 prospects ",city_name,", TX - Total Approved Voucher Per Bedroom Size"), cex.main = 1)
axis(2, at=seq(0,max(total_approved$V1, na.rm=T),1))


# Export csv
write.csv(all_data,"C:/Users/Andrew.Bannerman/Desktop/Real Estate/Real Estate Portfolio/Galveston section8 prospects/out.csv")


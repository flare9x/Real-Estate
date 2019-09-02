# Fair market rents - HUD 
# Source: https://www.huduser.gov/portal/datasets/fmr.html#history

library(fredr)
library(lubridate)
library(scales)
library(ggplot2)

data = read.csv("C:/Users/Andrew.Bannerman/Desktop/Real Estate/Rental Property Research and Deals/FMR_All_1983_2019_rev.csv",stringsAsFactors = FALSE, header=T)

# Find index position of county data
select_county = "Galveston County"
index_position = which(data == select_county, arr.ind = TRUE) # row,col position in data frame
index_position[1,1]

#subset fair market rent county data 
sub_county_FMR = data[index_position[1,1],]

# custom index positions 
beds_0 = seq(4,length(sub_county_FMR)-16,7)
beds_1 = seq(5,length(sub_county_FMR)-16,7)
beds_2 = seq(6,length(sub_county_FMR)-16,7)
beds_3 = seq(7,length(sub_county_FMR)-16,7)
beds_4 = seq(8,length(sub_county_FMR)-16,7)

start = 1983
end = 2019
years = end - start

bedrooms = 0:4
beds_0_data_extract = list()
beds_1_data_extract = list()
beds_2_data_extract = list()
beds_3_data_extract = list()
beds_4_data_extract = list()
for (i in 1:years) {
  cat("this is iteration",i)
  beds_0_data_extract[[i]] = sub_county_FMR[,beds_0[i]]
  beds_1_data_extract[[i]] = sub_county_FMR[,beds_1[i]]
  beds_2_data_extract[[i]] = sub_county_FMR[,beds_2[i]]
  beds_3_data_extract[[i]] = sub_county_FMR[,beds_3[i]]
  beds_4_data_extract[[i]] = sub_county_FMR[,beds_4[i]]
}

# make data frames 
beds_0_data_df = rev(do.call(rbind,beds_0_data_extract))
beds_1_data_df = rev(do.call(rbind,beds_1_data_extract))
beds_2_data_df = rev(do.call(rbind,beds_2_data_extract))
beds_3_data_df = rev(do.call(rbind,beds_3_data_extract))
beds_4_data_df = rev(do.call(rbind,beds_4_data_extract))

all_df = data.frame(beds_0_data_df,beds_1_data_df,beds_2_data_df,beds_3_data_df,beds_4_data_df)

# Yearly Changes 
beds_0_delta = seq(0,nrow(all_df)-1,1)
beds_1_delta = seq(0,nrow(all_df)-1,1)
beds_2_delta = seq(0,nrow(all_df)-1,1)
beds_3_delta = seq(0,nrow(all_df)-1,1)
beds_4_delta = seq(0,nrow(all_df)-1,1)
beds_0_perc = seq(0,nrow(all_df)-1,1)
beds_1_perc  = seq(0,nrow(all_df)-1,1)
beds_2_perc  = seq(0,nrow(all_df)-1,1)
beds_3_perc = seq(0,nrow(all_df)-1,1)
beds_4_perc  = seq(0,nrow(all_df)-1,1)
for (i in 2:nrow(all_df)) {
  beds_0_delta[i] = all_df$beds_0_data_df[i] -all_df$beds_0_data_df[i-1]
  beds_1_delta[i] = all_df$beds_1_data_df[i] -all_df$beds_1_data_df[i-1]
  beds_2_delta[i] = all_df$beds_2_data_df[i] -all_df$beds_2_data_df[i-1]
  beds_3_delta[i] = all_df$beds_3_data_df[i] -all_df$beds_3_data_df[i-1]
  beds_4_delta[i] = all_df$beds_4_data_df[i] -all_df$beds_4_data_df[i-1]
  
  beds_0_perc[i] = (all_df$beds_0_data_df[i] - all_df$beds_0_data_df[i-1])/ all_df$beds_0_data_df[i]
  beds_1_perc[i] = (all_df$beds_1_data_df[i] - all_df$beds_1_data_df[i-1])/ all_df$beds_1_data_df[i]
  beds_2_perc[i] = (all_df$beds_2_data_df[i] - all_df$beds_2_data_df[i-1])/ all_df$beds_2_data_df[i]
  beds_3_perc[i] = (all_df$beds_3_data_df[i] - all_df$beds_3_data_df[i-1])/ all_df$beds_3_data_df[i]
  beds_4_perc[i] = (all_df$beds_4_data_df[i] - all_df$beds_4_data_df[i-1])/ all_df$beds_4_data_df[i]
}

# Average change 
mean_yearly_beds_0 = mean(beds_0_delta)
mean_yearly_beds_1 = mean(beds_1_delta)
mean_yearly_beds_2 = mean(beds_2_delta)
mean_yearly_beds_3 = mean(beds_3_delta)
mean_yearly_beds_4 = mean(beds_4_delta)

mean_perc_yearly_beds_0 = mean(beds_0_perc) *100
mean_perc_yearly_beds_1 = mean(beds_1_perc) *100
mean_perc_yearly_beds_2 = mean(beds_2_perc) *100
mean_perc_yearly_beds_3 = mean(beds_3_perc) *100
mean_perc_yearly_beds_4 = mean(beds_4_perc) *100

all_change = c(mean_yearly_beds_0,mean_yearly_beds_1,mean_yearly_beds_2,mean_yearly_beds_3,mean_yearly_beds_4)
all_change = as.data.frame(all_change)

all_perc_change = c(mean_perc_yearly_beds_0,mean_perc_yearly_beds_1,mean_perc_yearly_beds_2,mean_perc_yearly_beds_3,mean_perc_yearly_beds_4)
all_perc_change = as.data.frame(all_perc_change)

# Set colour columns 
all_change$cols = c("bed_0","bed_1","bed_2","bed_3","bed_4")
all_perc_change$cols = c("bed_0","bed_1","bed_2","bed_3","bed_4")

# x axis 
index = c(0,1,2,3,4)

library(ggplot2)
ggplot(all_change,aes(x=index,y=all_change,fill=cols)) +
  geom_bar(stat = "identity") +
  geom_text(label = round(all_change$all_change,digits =2),size = 3, 
  position = position_stack(vjust = 0.5))+
  theme_bw()+
  ggtitle("Mean Annual $ Change - HUD Fair Market Rents")+
  ylab("Annual Mean Change $")+
  xlab("Bedroom")+
  scale_y_continuous(breaks = seq(0, max(all_change$all_change)+5, by = 5))+
  labs(colour="Bedroom Size")

ggplot(all_perc_change,aes(x=index,y=all_perc_change,fill=cols)) +
  geom_bar(stat = "identity") +
  geom_text(label = round(all_perc_change$all_perc_change,digits =2),size = 3, 
            position = position_stack(vjust = 0.5))+
  theme_bw()+
  ggtitle("Mean Annual % Change - HUD Fair Market Rents")+
  ylab("Annual % Mean Change")+
  xlab("Bedroom")+
  scale_y_continuous(breaks = seq(0, max(all_perc_change$all_perc_change)+.1, by = .2))+
  labs(colour="Bedroom Size")

# Fred Data 
# FRED API key 833af24cef7a3b0108fb6687ac12c721


# Set key 
fredr_set_key("833af24cef7a3b0108fb6687ac12c721")

require(fredr)
# request data 
all_cpi_data = fredr(
  series_id = "CPALTT01USA661S",
  observation_start = as.Date("1984-01-01")
)

# percent change 
cpi_perc_changes = seq(1,nrow(all_cpi_data),1)
for (i in 2:length(cpi_perc_changes)) {
  cpi_perc_changes[i] = (all_cpi_data$value[i] -all_cpi_data$value[i-1]) / all_cpi_data$value[i]
}

mean(cpi_perc_changes)*100

all_cpi_data = data.frame(all_cpi_data,cumsum(cpi_perc_changes))

ggplot() +
  theme_bw()+
  geom_point(data=all_cpi_data,aes(x = date, y = cumsum.cpi_perc_changes.,colour="#FF9999")) +
  geom_line(data=all_cpi_data, aes(x = date, y = cumsum.cpi_perc_changes.,colour="#FF9999")) +
  theme(axis.text.x = element_text(size=10,angle = 90),legend.position="right")+
  ggtitle("Consumer Price Index: Total All Items for the United States - Cumulative % Change")+
  ylab("(%)")+
  xlab("Year")+
  labs(subtitle="Date Range - 1984 to 2019")+
  #labs(colour="Net Absorption")+
  scale_y_continuous(breaks = seq(0, max(all_cpi_data$cumsum.cpi_perc_changes.), by = .2))+
  #scale_x_continuous(breaks = seq(min(homeownership_data$DATE), max(homeownership_data), by = 2))+
  theme(legend.position="none")+
  scale_x_date(breaks = date_breaks("24 months"), 
               labels=date_format(format = "%Y-%m-%d", tz = "UTC"))

# plots 
date_index = seq(1984,end,1)
date_index = paste0("1","-","1","-",date_index)
date_index = mdy(date_index)

# plots and prepare NBER recession data 

recession_dates = read.csv("C:/Users/Andrew.Bannerman/Desktop/Real Estate/Data/Macro/NBER_recession_dates.csv",header=TRUE, sep=",",skip=0,stringsAsFactors=FALSE)

# recession data
# Convert all recession dates to a readable format 
dates_peak = paste0(recession_dates$month_peak,"-",recession_dates$day_peak,"-",recession_dates$year_peak)
#dates_peak[1] = ""
dates_peak = as.Date(dates_peak, format="%B-%d-%Y")

dates_trough = paste0(recession_dates$month_trough,"-",recession_dates$day_trough,"-",recession_dates$year_trough)
dates_trough = as.Date(dates_trough, format="%B-%d-%Y")

# Recession Dates
# Recession data frame
recession_df = data.frame(dates_peak,dates_trough)
colnames = c("Peak","Trough")
colnames(recession_df) = colnames
recessions_trim = subset(recession_df, Peak >= min(date_index))

# Set colour columns 
all_df$bed_0 = "bed_0"
all_df$bed_1 = "bed_1"
all_df$bed_2 = "bed_2"
all_df$bed_3 = "bed_3"
all_df$bed_4 = "bed_4"

# Plot Data 
ggplot() +
  geom_line(data=all_df, aes(x = date_index, y = beds_0_data_df,group=bed_0,colour=bed_0)) +
  geom_line(data=all_df, aes(x = date_index, y = beds_1_data_df,group=bed_1,colour=bed_1)) +
  geom_line(data=all_df, aes(x = date_index, y = beds_2_data_df,group=bed_2,colour=bed_2)) +
  geom_line(data=all_df, aes(x = date_index, y = beds_3_data_df,group=bed_3,colour=bed_3)) +
  geom_line(data=all_df, aes(x = date_index, y = beds_4_data_df,group=bed_4,colour=bed_4)) +
  theme(axis.text.x = element_text(size=6,angle = 90),legend.position="right")+
  ggtitle("Galveston County Historical Fair Market Rent Value")+
  ylab("HUD FMR")+
  xlab("Date")+
  labs(colour="Bedroom Size")+
  scale_y_continuous(breaks = seq(0, max(all_df$beds_4_data_df)+200, by = 100))+
  geom_rect(data=recessions_trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.2)+
  scale_x_date(breaks = date_breaks("12 months"), 
               labels=date_format(format = "%Y-%m-%d", tz = "UTC"))+
  annotate("text", x = as.Date("2009-03-06"), y = 1350, label = "Recession",colour = "grey")+
  theme_classic(base_size = 15)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.minor =   element_blank(),
        panel.grid.major =   element_line(colour = "grey",size=0.1))+
  labs(caption=("Recession Data Source: National Bureau of Economic Research"))+
  theme(plot.caption = element_text(color = "black", face = "italic",size = 10))





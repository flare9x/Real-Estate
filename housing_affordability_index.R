# Housing Affordability Index 
# Example Calculation
IR = 0.0486
MEDPRICE =113000
MEDINC = 44073

PMT = MEDPRICE*0.8*(IR/12)/(1-(1/(1+IR/12)^360))

# Necessary_monthly_income 
MINC  = ((PMT*12)/MEDINC)*100

# qualifying_income 
QINC = PMT * 4 * 12

# Affordability Index
affordability_index = (MEDINC/QINC)*100

# Key
# IR = Interest Rate
# MEDPRICE = Median price of existing single-family home sale
# PMT= Monthly payment
# MEDINC = Median Family Income
# MINC = Necessary Monthly Income
# QINC = Qualifying Income

# Plot data for new median sales / median family income and 30 year fixed rate

# Census Data from FREDs
library(fredr)
library(data.table)

fredr_set_key("833af24cef7a3b0108fb6687ac12c721")

# Median family Income in the United States (MEFAINUSA646N)
median_family_income_USA = fredr(
  series_id = "MEFAINUSA646N",
  frequency = "a",  # annualy
  observation_start = as.Date("1984-01-01")
)

# Median Sales Price of Houses Sold for the United States (MSPUS)
median_sales_price_houses_USA = fredr(
  series_id = "MSPUS",
  frequency = "a",  # annualy
  observation_start = as.Date("1984-01-01")
)

# Mortgage data 
fixed_30 = fredr(
  series_id = "MORTGAGE30US",
  frequency = "a",  # annualy
  observation_start = as.Date("1984-01-01")
)


# Adjust monthly interest data to quarterly 
#data = read.csv(file="C:/MultiFamily_investing/Affordability House Index/historical_interest_rate_data_monthly.csv", header=TRUE, sep=",",stringsAsFactors = FALSE)

# Extract months from index month 
# Extract Words from string
#extract_words = list()
#i = 1
#for (i in 1:nrow(data)){
#  extract_words[[i]] = strsplit(data$Index.Month, "-")[[i]]  # Split sentance into individual words
#  if(any(extract_words[[i]] =="Jan")) {
#    data$month_extract[i] = "January" # Save metro id to data frame
#  }
#  else if(any(extract_words[[i]] !="Jan")) { 
#    data$month_extract[i] = "" # Save micro id to data frame
#  }
#}

# subset all december data 
#dec_data = subset(data, month_extract == "January")
#colnames(dec_data)

## Convert Date to yyyy-mm-dd
#require(lubridate)
#dec_data$date = dmy(paste("01-", dec_data$Index.Month , sep =""))

# Join data 
dt1 <- data.table(fixed_30, key = "date")
dt2 <- data.table(median_family_income_USA, key = "date") 
dt3 <- data.table(median_sales_price_houses_USA, key = "date") 
joined.dt1.dt2 <- dt1[dt2]
final = joined.dt1.dt2[dt3]

# Create Housing Affordability Index
for(i in 1:nrow(final)) {
  IR = final$value[i]/100
  final$PMT[i] =final$i.value.1[i]*0.8*(IR/12)/(1-(1/(1+IR/12)^360))
  
  # Necessary_monthly_income 
  final$MINC[i] = ((final$PMT[i]*12)/final$i.value[i])*100
  
  # qualifying_income 
  final$QINC[i] = final$PMT[i]*4*12
  
  # Affordability Index
  final$affordability_index[i] = (final$i.value[i]/final$QINC[i])*100
}

# Plot Affordability Index
plot(final$date,final$affordability_index,type="b",col="red",xaxt='n',main="Housing Affordability Index",xlab="Date",ylab="Housing Affordability Index")
axis.Date(1, at = seq(final$date[1], final$date[length(final$date)], by = "year"),
          format = "%Y", las = .5)
abline(h=100, col="blue",lty=2)

# Plot Principal And Interest Payments
plot(final$date,final$PMT,type="b",col="red",xaxt='n',main="Principal & Interest Rate Payments",xlab="Date",ylab="PMT")
axis.Date(1, at = seq(final$date[1], final$date[length(final$date)], by = "year"),
          format = "%Y", las = .5)

# Plot Qualifying Income
plot(final$date,final$QINC,type="b",col="red",xaxt='n',main="Qualifying Income",xlab="Date",ylab="QINC")
axis.Date(1, at = seq(final$date[1], final$date[length(final$date)], by = "year"),
          format = "%Y", las = .5)

# Plot Necessary Income
plot(final$date,final$MINC,type="b",col="red",xaxt='n',main="Necessary Income",xlab="Date",ylab="MINC")
axis.Date(1, at = seq(final$date[1], final$date[length(final$date)], by = "year"),
          format = "%Y", las = .5)

# To interpret the indices, a value of 100 means that a family with the median income has exactly enough income to qualify for a mortgage on a median-priced home.
# An index above 100 signifies that family earning the median income has more than enough income to qualify for a mortgage loan on a median-priced home, assuming a 20 percent down payment. For example, a composite HAI of 120.0 means a family earning the median family income has 120% of the income necessary to qualify for a conventional loan covering 80 percent of a median-priced existing single-family home. An increase in the HAI, then, shows that this family is more able to afford the median priced home.


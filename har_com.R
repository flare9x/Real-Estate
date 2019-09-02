library(rvest)
library(mailR)
library(stringr)

# Add selector gadget for CSS
# https://chrome.google.com/webstore/detail/selectorgadget/mhjhnkcfbdhnjickkkdbjoemdmbfginb/related?hl=en

# email results 
#https://medium.com/airbnb-engineering/using-googlesheets-and-mailr-packages-in-r-to-automate-reporting-c09579e0377f

filter_max_price = 350000
# Houston link = paste0("https://www.har.com/search/dosearch/?page=",i,",&for_sale=1&property_class_id=4&city=Houston")

# Reading the HTML code from the website
i=25
links_all_out = list()
day_nums_all_out = list()
for (i in 1:19) {
  har_com_houston_pages <- read_html(paste0("https://www.har.com/search/dosearch/?page=",i,",&for_sale=1&property_class_id=4&city=Houston"))
  
  
  # Grab the address and links
  address_link <- html_nodes(har_com_houston_pages,'.address')
  html_links = html_attr(address_link,"href")
  
  # loop to paste www.har.com to link 
  loop_links = list()
  for (j in 1:length(html_links)) {
    loop_links[[j]] = paste0("https://www.har.com",html_links[j])
  }
  
  # make data frame 
  html_link_output = do.call(rbind,loop_links)
  
  # Grab the number of days on the market
  days_data_html <- html_nodes(har_com_houston_pages,'.pull-right')
  days_no_data <- html_text(days_data_html)
  
  # Subset to remove cancel and blank space
  days_no_data = days_no_data[3:length(days_no_data)]  # clean to remove cancel and blank space
  
  # loop to extract numbers from string 
  market_days = list()
  for (j in 1:length(days_no_data)) {
    market_days[[j]] = as.numeric(str_extract_all(days_no_data[j], "[0-9]+")[[1]])
  }
  
  # make data frame 
  market_days_output = as.data.frame(do.call(rbind,market_days))
  
  # Print non matching
  if (nrow(market_days_output) == nrow(html_link_output)) {
    print("Matches")
  } else {
    cat("Non matching, check code on iteration",i,"\n")
    cat(paste0("https://www.har.com/search/dosearch/?page=",i,",&for_sale=1&property_class_id=4&city=Houston"),"\n")
  }
  
  # Loop to look through the market days and if == 1 send an email with corresponding link
  for (j in 1:nrow(market_days_output)) { 
    if (market_days_output[j,1] == 1) {
      cat("page",i,market_days_output[j,1],"days \n")
      print(html_link_output[j,1])}  #### add email function here to email out when days on market == 1 and email corresponding email link
  }
  
  # Save outputs 
  links_all_out[[i]] = html_link_output
  day_nums_all_out[[i]] = cbind(market_days_output)
  
} # end


# Export all 
all_links_final = do.call(rbind,links_all_out)
all_days_mkt = do.call(rbind,day_nums_all_out)

min(all_days_mkt)

# final 
all_df = data.frame(all_links_final,all_days_mkt)


####+#++#+#+#+#+#+#+ 
# email code 


# Write the content of your email
msg <- paste("Hi team,","","The mpg car dashboard is up-to-date as of",as.character(date()),"and can be accessed here: https://docs.google.com/spreadsheets/d/1LYxV8Z324o-OALSwO2h99fUecBdb_t-8BYgBbe0G1KU/edit#gid=0","","Best,","Your name")
# Define who the sender is
sender <- "firstname.lastname@email.com"
# Define who should get your email
recipients <- c("email_of_recipient1",
                "email_of_recipient2",
                "email_of_recipient3")
# Send your email with the send.mail function
send.mail(from = sender,
          to = recipients,
          subject = "Top 10 cars dashboard",
          body = msg,
          smtp = list(host.name = "smtp.gmail.com", port = 587,
                      user.name = "firstname.lastname@email.com",
                      passwd = "your_app_specific_password", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)
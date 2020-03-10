## Import data

library(readr)
sales_activity_report <- read_csv("C:/Users/Christian Santizo/Google Drive/CSULB/Courses/STAT 560/Project/sales_activity_report.csv", 
                                  skip = 12)

## Extracting variables from the original data 

data.full = head(sales_activity_report,-2) # Eliminate totals

dates = unlist(data.full[,c("Order Date")], use.names = FALSE)

price = unlist(data.full[,c("Net Earnings")], use.names = FALSE)

states= unlist(data.full[,c("Buyer State")], use.names = FALSE)



## Formats order dates into Date format 

dates = as.Date(dates,"%m/%d/%Y")

## Removes dollar signs from Order Price

# Function that takes out dollar signs

strip_dollars = function(x) { 
  as.numeric(gsub('\\$','',x))
}

#

price = as.vector(unlist(lapply(price, strip_dollars)))

## Convert lowercase states abbreviations to uppercase

# Function that converts lowercase characters to uppercase characters

lower.to.upper = function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}

#

states =as.factor(unlist(lapply(states, lower.to.upper)))

## Creates data set of order date (by month) and aggregate net earnings 

dd <- data.frame(dates, price)

library(dplyr)
library(lubridate)

dd.m = dd %>% group_by(month=floor_date(dates, "month")) %>% summarize(m.net.earnings=sum(price))

## Write out text datafile and a SAS program to read it

# Add months since Dec 2015

N = c(1:dim(dd.m)[1])
dd.m = cbind(dd.m, N)

#

library(foreign)
write.foreign(dd.m,
              "C:/Users/Christian Santizo/Google Drive/CSULB/Courses/STAT 560/Project/monthly_net_earnings.txt",
              "C:/Users/Christian Santizo/Google Drive/CSULB/Courses/STAT 560/Project/monthly_net_earnings.sas",
              package="SAS")








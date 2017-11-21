########################################################################
# Script to dabble with data exploration before playoing with knitr.
########################################################################
library(dplyr)

if(!dir.exists("data")) {
  dir.create("data")
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "data/dataset.csv.bz2")
}

if(!exists("data_init")) {
  data_init <- read.csv2("data/dataset.csv.bz2", sep = ",")
}

data <- data_init

data$year <- as.numeric(format(as.Date(data$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))

findBreakingPoint <- data[c("year")]
findBreakingPoint <- findBreakingPoint %>%
  group_by(year) %>%
  summarise(count = n())

minyear <- min(findBreakingPoint[findBreakingPoint$count > mean(findBreakingPoint$count),]$year)

data <- subset(data, minyear <= year)

finvars <- names(data) %in% c("year", "EVTYPE", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
financial_data <- data[finvars]
financial_data$CROPDMG <- as.numeric(as.character(financial_data$CROPDMG))
financial_data$PROPDMG <- as.numeric(as.character(financial_data$PROPDMG))
financial_data <- transform(financial_data, PROPDMG = ifelse(tolower(PROPDMGEXP) == "k", PROPDMG * 1e3, 
                                                             ifelse(tolower(PROPDMGEXP) == "m", PROPDMG * 1e6, 
                                                                    ifelse(tolower(PROPDMGEXP) == "b", PROPDMG * 1e9, PROPDMG))))
financial_data <- transform(financial_data, CROPDMG = ifelse(tolower(CROPDMGEXP) == "k", CROPDMG * 1e3, 
                                                             ifelse(tolower(CROPDMGEXP) == "m", CROPDMG * 1e6, 
                                                                    ifelse(tolower(CROPDMGEXP) == "b", CROPDMG * 1e9, CROPDMG))))

financial_data <- financial_data %>%
  group_by(year, EVTYPE) %>%
  summarise(totalcropdmg=sum(CROPDMG), totalpropdmg=sum(PROPDMG))
  
financial_data <- financial_data %>%
  group_by(EVTYPE) %>%
  summarise(meancropperyear=sum(totalcropdmg), meanpropperyear=sum(totalpropdmg)) %>%
  order_by(meanpropperyear)

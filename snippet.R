########################################################################
# Script to dabble with data exploration before playoing with knitr.
########################################################################
library(knitr)
library(ggplot2)
library(tidyr)
library(dplyr)

# load data
if(!dir.exists("data")) {
  dir.create("data")
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "data/dataset.csv.bz2")
}
data <- read.csv2("data/dataset.csv.bz2", sep = ",")

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

eventType_count <- data[c("EVTYPE")]
eventType_count$EVTYPE <- as.character(eventType_count$EVTYPE)
eventType_count <- eventType_count %>%
  group_by(EVTYPE) %>%
  summarise(count=n()) %>%
  top_n(n = 20, wt=count)

health_data <- data[c("year", "EVTYPE", "INJURIES", "FATALITIES")]
health_data$INJURIES <- as.numeric(as.character(health_data$INJURIES))
health_data$FATALITIES <- as.numeric(as.character(health_data$FATALITIES))

health_data_abs <- health_data %>%
  group_by(EVTYPE) %>%
  summarise(injuries=sum(INJURIES), fatalities=sum(FATALITIES)) %>%
  mutate(total_health_consequences=injuries + fatalities)

health_data_rel <- health_data %>%
  group_by(EVTYPE) %>%
  summarise(injuries=mean(INJURIES), fatalities=mean(FATALITIES)) %>%
  mutate(relative_health_consequences=injuries + fatalities)

health_data_abs_injuries <- health_data_abs %>%
  top_n(n =10, wt=injuries)

health_data_abs_fatalities <- health_data_abs %>%
  top_n(n =10, wt=fatalities)

health_data_abs_total <- health_data_abs %>%
  top_n(n =10, wt=total_health_consequences)

health_data_rel_injuries <- health_data_rel %>%
  top_n(n =10, wt=injuries)

health_data_rel_fatalities <- health_data_rel %>%
  top_n(n =10, wt=fatalities)

health_data_rel_total <- health_data_rel %>%
  top_n(n =10, wt=relative_health_consequences)

health_data_abs <- health_data_abs_total %>%
  gather("CONSEQUENCE_TYPE", "VALUE", c("injuries","fatalities")) %>%
  mutate(TYPE_ANALYSIS="Absolute from 1994 to 2011") %>%
  select(-one_of("total_health_consequences"))

health_data_rel <- health_data_rel_total %>%
  gather("CONSEQUENCE_TYPE", "VALUE", c("injuries","fatalities")) %>%
  mutate(TYPE_ANALYSIS="Relative Per Event") %>%
  select(-one_of("relative_health_consequences"))

health_data_plot <- rbind(health_data_abs, health_data_rel)

financial_plot <- ggplot(health_data_plot, aes(EVTYPE, y=VALUE, fill=CONSEQUENCE_TYPE))
financial_plot +  geom_bar(stat = "identity") + 
  geom_bar(stat = "identity") + facet_wrap(facets = ~TYPE_ANALYSIS, scales = "free", nrow=2, ncol=1) +
ggtitle("10 top most events in relation to health") + ylab("number of occurences") + xlab("Events")
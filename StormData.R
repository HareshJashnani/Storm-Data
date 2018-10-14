if(!file.exists("StormData.csv.bz2")) {
  Original_Data_URL <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(Original_Data_URL, destfile="StormData.csv.bz2")
}

data <- read.csv("StormData.csv.bz2", stringsAsFactors=F)

str(data)
# ==> 902297 obs. of  37 variables
head(data)
tail(data)
summary(data)
summary(data$EVTYPE)
# EVTYPE
# Length:902297
# Class :character
# Mode  :character
table(data$EVTYPE)
eventtype_unique <- unique(data$EVTYPE)
summary(eventtype_unique)
# Length     Class      Mode 
# 985      character  character 
data$EVTYPE <- tolower(data$EVTYPE)
eventtype_unique <- unique(data$EVTYPE)
summary(eventtype_unique)
# Length     Class      Mode 
# 898      character character 
head(data$INJURIES)
summary(data$INJURIES)
head(data$FATALITIES)
summary(data$FATALITIES)

# Analysis

# install.packages("ggplot2", dependencies = T)
library(ggplot2)
library(lattice) 

# For "1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?"" question,
# I will calculate how many injuries+fatalities an event caused using aggregate function.
casualties <- with(data, aggregate(INJURIES + FATALITIES ~ EVTYPE, data=data, FUN = "sum"))

# Change the name
names(casualties)[2] <- "Totalcasualties"

# Order the number of casualties by decending method
ordered_casualties <- casualties[order(-casualties$Totalcasualties),]

# Just see top6 using head()
Top6 <- head(ordered_casualties)

# Report Out
# Draw a barplot
barplot(Top6$Totalcasualties, main = "Which event caused the most harmful with respect to population health", 
        xlab = "Total No. of casualties", names.arg=Top6$EVTYPE)

# "The most harmful event is"

Top6$EVTYPE[1]

### Question 2 - Econimic Damage

str(data)
str(data$CROPDMGEXP)
unique(data$CROPDMGEXP)
unique(data$PROPDMGEXP)
str(data$PROPDMGEXP)
table(data$CROPDMGEXP)
table(data$PROPDMGEXP)

# Variable 'CROPDMGEXP' and 'PROPDMGEXP' will be subsetted values with "M", "K", "B" since these are values mentioned in pdf "NATIONAL WEATHER SERVICE INSTRUCTION" p12.
# First, 'CROPDMGEXP' subsetting
data2 <- subset(data, (data$CROPDMGEXP == "M" | data$CROPDMGEXP == "K" | data$CROPDMGEXP == "B") | 
                  (data$PROPDMGEXP =="M" | data$PROPDMGEXP =="K" | data$PROPDMGEXP =="B") )

str(data2)
# CROPDMG value will be multipied accordingly with,
# "M", "K", "B" which mean 1000000, 1000, 1000000000 respectively.
summary(data2$CROPDMG)
table(data2$CROPDMGEXP)
# Character with 'B', 'K', 'M' are assigned values accordingly as follows. Other than these three charaacter, 
# the multiplier will be 1.

for(i in 1:length(data2$CROPDMGEXP)) {
  ifelse(data2$CROPDMGEXP[i] == "M", data2$CROPDMG[i] <- data2$CROPDMG[i] * 1000000, 
         ifelse(data2$CROPDMGEXP[i] == "K", data2$CROPDMG[i] <- data2$CROPDMG[i] * 1000,
                ifelse(data2$CROPDMGEXP[i] == "B", data2$CROPDMG[i] <- data2$CROPDMG[i] * 1000000000, data2$CROPDMG[i] <- data2$CROPDMG[i] * 1)))
}
summary(data2$CROPDMG)

summary(data2$PROPDMG)

table(data2$PROPDMGEXP)

for(i in 1:length(data2$PROPDMGEXP)) {
  ifelse(data2$PROPDMGEXP[i] == "M", data2$PROPDMG[i] <- data2$PROPDMG[i] * 1000000,
         ifelse(data2$PROPDMGEXP[i] == "K", data2$PROPDMG[i] <- data2$PROPDMG[i] * 1000,
                ifelse(data2$PROPDMGEXP[i] == "B", data2$PROPDMG[i] <- data2$PROPDMG[i] * 1000000000, data2$PROPDMG[i] <- data2$PROPDMG[i] * 1)))
}
summary(data2$PROPDMG)

## Analysis

library(ggplot2)
library(lattice) 

# I will calculate how much economic values are lost using aggregate function.
Eco_dmg <- with(data2, aggregate(CROPDMG + PROPDMG ~ EVTYPE, data=data, FUN = "sum"))

# Change the name
names(Eco_dmg)[2] <- "Total_Economic_Damage"

# Order the economic values by decending method
ordered_eco_dmg <- Eco_dmg[order(-Eco_dmg$Total_Economic_Damage),]

# Just see top6 using head()
Top6_Eco <- head(ordered_eco_dmg)

## Report

# Draw a barplot
barplot(Top6_Eco$Total_Economic_Damage, main = "Which event caused the greatest economic consequence", 
        xlab = "Total Economic Consequences", names.arg=Top6$EVTYPE)

# The greatest economic consequence event is

Top6_Eco$EVTYPE[1]



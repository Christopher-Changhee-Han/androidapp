library(dplyr)
library(lubridate)

setwd("C:/SDS labs")
googleData <- read.csv("googleplaystore.csv", stringsAsFactors = FALSE)

#clean the variable names
names(googleData) <- tolower(names(googleData))
names(googleData) <- sub(".", "", names(googleData), fixed = TRUE)

#remove all rows with NaN in rating as they are not applicable for this analysis
googleData <- googleData[complete.cases(googleData$rating),]

#clean the times
googleData$lastupdated <- mdy(googleData$lastupdated)

#remove one bad row of data (the values were shifted by one column)
bad_row <- which(is.na(googleData$lastupdated))
googleData <- googleData[-bad_row,]

#clean the variable 'size' to remove M (e.g 19M), convert to numeric from chr
## note: varies with device is coerced to NA which serves similar meaning
googleData$size <- as.numeric(sub("M", "", googleData$size, fixed = TRUE))

#clean the variable 'price' to remove $ (e.g $2.99), convert to numeric from chr
googleData$price <- as.numeric(sub("$", "", googleData$price, fixed = TRUE))

#re-format the variables 'category', 'reviews', 'type', 'installs'
googleData$category <- factor(googleData$category)
googleData$reviews <- as.numeric(googleData$reviews)
googleData$type <- factor(googleData$type)
googleData$installs <- factor(googleData$installs)

#clean the variable 'installs' maybe? run an initial model first to determine if
#the lower installs can be merged

#remove the column 'genres' as most of it overlaps with category
#remove the column 'currentver' as the format varies vastly and is not relevant
#remove the column 'app' as the names do not matter

googleData <- googleData[, !names(googleData) 
                         %in% c("genres", "currentver", "app")]

#clean up the variable 'androidver'
##remove the 'and up' from '4.2 and up', convert to numeric

googleData$androidver <- as.numeric(
        sub(" and up", "", googleData$androidver, fixed = TRUE))


#run initial linear model

lmgoogle <- lm(rating~ ., data = googleData)
summary(lmgoogle)

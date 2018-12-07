#this function cleans the Google Play Store data from Kaggle
#the data that results should be ready for analysis
#some variables are modified/removed so read through the code

library(dplyr)
cleandata <- function(googleData){
#for convention, make every variable name lower case and without any symbols
        names(googleData) <- tolower(names(googleData))
        names(googleData) <- sub(".", "", names(googleData), fixed = TRUE)

#get rid of duplicate rows
        googleData <- googleData %>% distinct(app, .keep_all = TRUE)

#remove all rows with missing values of rating
        googleData <- googleData[complete.cases(googleData$rating),]

#clean the variable 'times' with lubridate package
        googleData$lastupdated <- mdy(googleData$lastupdated)

#remove one bad row of data (the values were shifted by one column)
        bad_row <- which(is.na(googleData$lastupdated))
        googleData <- googleData[-bad_row,]

#clean the variable 'size' to remove M (e.g 19M), convert to numeric from chr
## note: varies with device is coerced to NA which serves similar meaning
## for the purpose of analysis, rows with NA's were replaced with the average
        googleData$size <- as.numeric(sub("M", "", googleData$size, fixed = TRUE))
        sizemean <- round(mean(googleData$size, na.rm = TRUE), digits = 1)
        googleData$size[is.na(googleData$size)] <- sizemean

#clean the variable 'price' to remove $ (e.g $2.99), convert to numeric from chr
        googleData$price <- as.numeric(sub("$", "", googleData$price, fixed = TRUE))
        sum(is.na(googleData$price))

#re-format the variables 'category', 'reviews', 'type', 'installs'
        googleData$category <- factor(googleData$category)
        googleData$reviews <- as.numeric(googleData$reviews)
        googleData$type <- factor(googleData$type)
        googleData$installs <- factor(googleData$installs)

#remove the column 'genres' as most of it overlaps with category
#remove the column 'currentver' as the format varies vastly and is not relevant
#remove the column 'app' as the names do not matter

        googleData <- googleData[, !names(googleData) 
                         %in% c("genres", "currentver", "app")]

#clean up the variable 'androidver'
##remove the 'and up' from '4.2 and up', convert to numeric
##replace the NA's with the average
        googleData$androidver <- as.numeric(
                sub(" and up", "", googleData$androidver, fixed = TRUE))
        versionmean <- round(mean(googleData$androidver, na.rm = TRUE), digits = 1)
        googleData$androidver[is.na(googleData$androidver)] <- versionmean

#lastly, remove one row with content rating value 'Unrated', it is the only one with
#the unspecified content rating. It could be thought of as NA and we remove it
#for the purpose of this project
        googleData <- googleData[-which(googleData$contentrating == "Unrated"),]

        googleData
}
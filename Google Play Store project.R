#download csv file from https://www.kaggle.com/lava18/google-play-store-apps#googleplaystore.csv

install.packages("devtools") #if needed
devtools::install_github("MichaelJMahometa/SDSRegressionR", force = TRUE)

library(dplyr)
library(lubridate)
library(car)
library(SDSRegressionR)

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

#clean the variable 'installs' maybe? run an initial model first to determine if
#the lower installs can be merged

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
#test few different models, and see which fits best

##run initial linear model
fullmodel <- lm(rating~ ., data = googleData)
summary(fullmodel)

##check some conditions
residFitted(fullmodel)
vif(fullmodel)

Anova(fullmodel, type = "III")
#get the test error
#use 2/3 data for training data, 1/3 for test
set.seed(201)
twothirds = sample(1:nrow(googleData), 6244)
train <- googleData[twothirds,]
test <- googleData[-twothirds,]

lmgoogle <- lm(rating~ ., data = train)
lm.pred = predict(lmgoogle, test, type = "response")



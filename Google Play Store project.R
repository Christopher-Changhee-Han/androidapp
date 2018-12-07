#download csv file from https://www.kaggle.com/lava18/google-play-store-apps#googleplaystore.csv

install.packages("devtools") #if needed
devtools::install_github("MichaelJMahometa/SDSRegressionR", force = TRUE)

library(dplyr)
library(lubridate)
library(car)
library(SDSRegressionR)
library(splines)
library(boot)
library(gvlma)

source("cleandata.R")

googleData <- read.csv("googleplaystore.csv", stringsAsFactors = FALSE)
googleData <- cleandata(googleData) #cleans, and formats data, reference the source file for details

#run initial linear model
fullmodel <- lm(rating~ ., data = googleData)
summary(fullmodel)

#check some conditions
residFitted(fullmodel)
vif(fullmodel)

#check results
Anova(fullmodel, type = "III")

#check the distribution of rating
hist(googleData$rating)

#calculate MSE

set.seed(201)
results <- setNames(c(0, 0), c("MSE", "c.v MSE"))
twothirds = sample(1:nrow(googleData), 2*(nrow(googleData)/3)) #use 2/3 data for training data, 1/3 for test
train <- googleData[twothirds,]
test <- googleData[-twothirds,]

lmdata <- lm(rating ~., data = train)
lm.pred = predict(lmdata, test, type = "response")
results[1] <- mean((test$rating- lm.pred)^2) #MSE

#average MSE calculated using 10-fold cross validation with 5 iterations
cv.error10=rep(0,5)
degree= 1:5
d=1
for(d in degree){
        glm.fit=glm(rating~ ., data=googleData)
        cv.error10[d]=cv.glm(googleData,glm.fit,K=10)$delta[1]
}
results[2] <- mean(cv.error10)
results


#initial look at the variables
par(mfrow=c(3,3), oma = c(0,0,2,0), col= "darkgrey", cex = 0.5)
boxplot(rating~category, data = googleData, main = "Category")
with(googleData[googleData$reviews < summary(googleData$reviews)[[5]],], plot(rating ~ reviews, main = "Reviews"))
plot(rating~size, data = googleData, main = "Size")
boxplot(rating~installs, data = googleData, main = "Installs")
boxplot(rating~type, data = googleData, main = "Type")
plot(rating~price, data = googleData, main = "Price")
boxplot(rating~contentrating, data = googleData, main = "Content Rating")
plot(rating~lastupdated, data = googleData, main = "Last Updated")
plot(rating~androidver, data = googleData, main = "Android Ver.")
title("Predictors vs. Rating", outer=TRUE, cex.main = 2) 

#test the relationship between installs and reviews

summary(lm(reviews~ installs, data= googleData))
Anova(lm(reviews~ installs, data= googleData), type = "III")

#test the relationship between price and reviews

summary(lm(price~ type, data= googleData))
Anova(lm(price~ type, data= googleData), type = "III")

#fit polynomial, splines, local regression on price for practice

onlypaid <- googleData[googleData$price != 0,] #subset data into only paid apps
plot(rating~ price, data = onlypaid)

onlypaid <- onlypaid[onlypaid$price < 100,]
plot(rating~ price, data = onlypaid) #examine the data visually

#polynomials
lmonlypaid <- lm(rating~ price, data = onlypaid) #linear
abline(lmonlypaid)
quadonlypaid <- lm(rating~ poly(price, 2, raw = TRUE), data = onlypaid) #quadratic polynomial
abline(quadonlypaid, col= "blue") #looks worse lmao

cubiconlypaid <- lm(rating~ poly(price, 3, raw = TRUE), data = onlypaid) #cubic polynomial
abline(cubiconlypaid, col= "red") #similar to linear

highdegreeonlypaid <- lm(rating~ poly(price, 10, raw = TRUE), data = onlypaid) #10th degree polynomial, just to prove a point
abline(highdegreeonlypaid, col= "green")
 
anova(lmonlypaid, quadonlypaid, cubiconlypaid, highdegreeonlypaid) #compare the different fits

#Splines

fit=lm(rating~bs(price, knots=c(10,20,30)),data=onlypaid) #cubic spline
plot(rating ~ price,col="darkgrey", data= onlypaid)
lines(onlypaid$price,predict(fit,list(price = onlypaid$price)),col="darkgreen",lwd=2)
abline(v=c(10,20,30),lty=2,col="darkgreen")

fitsmooth=smooth.spline(onlypaid$rating,onlypaid$price, cv =TRUE) #C.V smoothing spline
lines(fitsmooth,col="red",lwd=2)

fit=loess(rating~price,span=.5,data=onlypaid) #local regression
lines(onlypaid$price, predict(fit, onlypaid$price),col="red",lwd=2)

#Backward subset selection

#remove price and run anova test
noprice <- lm(rating~.-price, data = googleData)
summary(noprice)
anova(fullmodel, noprice)

#remove reviews and run anova teset
noreviews <- lm(rating~. -reviews, data = googleData)
summary(noreviews)
anova(fullmodel, noreviews)

#remove installs and run anova test
noinstalls <-lm(rating~. -installs, data = googleData)
summary(noinstalls)
anova(fullmodel, noinstalls)

#let's set our alpha to 0.001 and remove price and reviews from our model

updatedmodel <- lm(rating~. - price - reviews, data = googleData) #removes price, reviews
summary(updatedmodel)
Anova(updatedmodel, type = "III")

updatedmodel2 <- lm(rating~. - price - reviews - size - contentrating, data = googleData) #removes size, content rating
summary(updatedmodel2)
Anova(updatedmodel2)

anova(fullmodel, updatedmodel, updatedmodel2) #compare all the different versions of models

newData <- select(googleData, c("category", "rating", "installs", "type", "lastupdated"))
dim(newData) #data frame with the updated model

#calculate the updated test error, see if it's any different

set.seed(201)
twothirds2 = sample(1:nrow(newData), 2*nrow(newData)/3) #use 2/3 data for training data, 1/3 for test
train2 <- newData[twothirds2,]
test2 <- newData[-twothirds2,]

lmgoogle2 <- lm(rating~ ., data = train2)
lm.pred2 = predict(lmgoogle2, test2, type = "response")

results[1] <- mean((test2$rating - lm.pred2)^2) 

#cross validation test error
cv.error10=rep(0,5)
degree= 1:5
d =1
for(d in degree){
        glm.fit=glm(rating~., data=newData)
        cv.error10[d]=cv.glm(newData,glm.fit,K=10)$delta[1]
} 
results[2] <- mean(cv.error10)
results


#Testing different transformations on the response variable
par(mfrow=c(2,2))
hist(newData$rating, breaks = 50, main = "Frequency Distribution of Rating", xlab = "Rating") #oh crap, it's left skewed
hist((newData$rating)^2, breaks = 50, main = "Frequency Distribution of Rating^2", xlab = "Rating^2") #squaring it doesn't make too much difference
hist(log(newData$rating), breaks = 50, main = "Frequency Distribution of log(Rating)", xlab = "log(Rating)") #log doesn't change it either
boxplot(newData$rating, main = "Distribution of Rating")
summary(newData$rating) #25% of the data is 1-4, 75% of the data is 4.0 - 5.0

#Subset the data into two parts
lowrating <- filter(newData, rating < 4) #Rating less than 4.0
highrating <- filter(newData, rating >=4) #Rating greater than equal to 4.0

#let's run linear models on these two
lmltf <- lm(rating~., data = lowrating) #Rating less than 4.0
summary(lmltf)
Anova(lmltf, type = "III")

lmmtf <- lm(rating~., data = highrating) #Rating greater than equal to 4.0
summary(lmmtf)
Anova(lmmtf, type = "III")

#plot the relationships with par, plot

par(mfrow=c(2,3))
plot(rating~category, data = lowrating)
plot(rating~installs, data = lowrating)
plot(rating~type, data = lowrating)
plot(rating~lastupdated, data = lowrating)
plot(rating~androidver, data = lowrating)
residualPlot(lmltf)
title("Rating < 4.0", outer=TRUE, line = -2) 

#do same for more than four

par(mfrow=c(2,3))
plot(rating~category, data = highrating)
plot(rating~installs, data = highrating)
plot(rating~type, data = highrating)
plot(rating~lastupdated, data = highrating)
plot(rating~androidver, data = highrating)
residualPlot(lmmtf)
title("Rating >= 4.0", outer=TRUE, line = -2) 

#rank category by average rating
a <- unique(newData$category) #get unique values of category
b <- vector(length = length(a), mode = "numeric") #create a vector of same length as the unique values
c = 0 #set counter
for (i in a){
    b[c] <- mean(newData[newData$category == i, 2]) #calculate mean of each unique value of installs and store it
    c = c + 1 #increase counter
}
names(b) <- a #assign names to each value
head(sort(b, decreasing = TRUE)) #get the top values

#rank installs by average rating

d <- unique(newData$installs) #get unique values of installs
e <- vector(length = length(d), mode = "numeric") #create a vector of same length as the unique values
f = 0 #set counter  
for (i in d){
  e[f] <- mean(newData[newData$installs == i, 2]) #calculate mean of each unique value of installs and store it
  f = f + 1 #increase counter
}
names(e) <- d #assign names to each value
head(sort(e, decreasing = TRUE)) #get the top values
tail(sort(e, decreasing = TRUE))
#free vs paid

typemean <- vector(length =2, mode= "numeric")
typemean[1] <- mean(newData[newData$type == "Free", 2])
typemean[2] <- mean(newData[newData$type == "Paid", 2])

plot(rating ~ lastupdated, data = newData)
summary(lm(rating~lastupdated, data= newData))

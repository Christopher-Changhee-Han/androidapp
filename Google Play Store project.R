#download csv file from https://www.kaggle.com/lava18/google-play-store-apps#googleplaystore.csv

install.packages("devtools") #if needed
devtools::install_github("MichaelJMahometa/SDSRegressionR", force = TRUE)

library(dplyr)
library(lubridate)
library(car)
library(SDSRegressionR)
library(splines)
library(boot)

source("cleandata.R")

#read in and clean the data to be ready for analysis
googleData <- read.csv("googleplaystore.csv", stringsAsFactors = FALSE)
sum(googleData$Current.Ver == "Varies with device")
googleData <- cleandata(googleData)
#test few different models, and see which fits best
##run initial linear model
fullmodel <- lm(rating~ ., data = googleData)
summary(fullmodel)

##check some conditions
residFitted(fullmodel)
vif(fullmodel)

##check results
Anova(fullmodel, type = "III")

#calculate MSE
#calculate the test error of a linear model
#use 2/3 data for training data, 1/3 for test
#returns a vector of length two, 
#[1] is MSE using 1/3 of the data as test
#[2] is average MSE calculated using 10-fold cross validation with 5 iterations

set.seed(201)
results <- setNames(c(0, 0), c("MSE", "c.v MSE"))
twothirds = sample(1:nrow(googleData), 2*(nrow(googleData)/3))
train <- googleData[twothirds,]
test <- googleData[-twothirds,]

#set unused levels of installs to NA
#id <- which(!(test$installs %in% levels(train$installs)))
#test$installs[id] <- NA
lmdata <- lm(rating ~., data = train)
lm.pred = predict(lmdata, test, type = "response")
results[1] <- mean((test$rating- lm.pred)^2) #MSE

#what about cross validation error?
cv.error10=rep(0,5)
degree= 1:5
d=1
for(d in degree){
        glm.fit=glm(rating~ ., data=googleData)
        cv.error10[d]=cv.glm(googleData,glm.fit,K=10)$delta[1]
}
results[2] <- mean(cv.error10)
results #0.261, 0.242

#category seems to be significant
par(mfrow=c(2,3))
plot(rating~category, data = googleData)

#so does the number of installs
plot(rating~installs, data = googleData)

#let's take a look at the number of reviews

plot(rating~ reviews, data = googleData) # hard to see, try using only upto 3rd quartile
with(googleData[googleData$reviews < summary(googleData$reviews)[[5]],], plot(rating ~ reviews))
#lets try running a linear model just with reviews
lmreview <- lm(rating~ reviews, data = googleData[googleData$reviews < summary(googleData$reviews)[[5]],])
residFitted(lmreview)
summary(lmreview) #very small R squared

#what about type?

plot(rating~ type, data = googleData)

#category, reviews, installs, type, price, lastupdated, androidver
#price

plot(rating~ price, data = googleData)

#last updated

plot(rating~ lastupdated, data = googleData)

#android ver

plot(rating~ androidver, data = googleData)

#perhaps reviews and installs are correlated? let's test it out

summary(lm(reviews~ installs, data= googleData))
Anova(lm(reviews~ installs, data= googleData), type = "III")
##seems like they do have a significant relationship

#we also suspect that type and price carry similar information
#type and price is also inherently related

onlypaid <- googleData[googleData$price != 0,]
##holy crap, there are only 647 observations out of 9365 that are not $0
##that is only 7% of the data!

plot(rating~ price, data = onlypaid)
#let's get rid of obvious outliers and plot again

onlypaid <- onlypaid[onlypaid$price < 100,]
plot(rating~ price, data = onlypaid)
#highly right skewed, data heavily concentrated to the left
#lets try to fit a linear line

lmonlypaid <- lm(rating~ price, data = onlypaid)
abline(lmonlypaid) #doesn't look that good, lets try polynomial
quadonlypaid <- lm(rating~ poly(price, 2, raw = TRUE), data = onlypaid)
abline(quadonlypaid, col= "blue") #looks worse lmao

cubiconlypaid <- lm(rating~ poly(price, 3, raw = TRUE), data = onlypaid)
abline(cubiconlypaid, col= "red") #similar to linear

#just for fun
highdegreeonlypaid <- lm(rating~ poly(price, 10, raw = TRUE), data = onlypaid)
abline(highdegreeonlypaid, col= "green") #similar to linear

#compare the different fits
anova(lmonlypaid, quadonlypaid, cubiconlypaid, highdegreeonlypaid)
#polynomials are not helping with the fit
#let's try splines

fit=lm(rating~bs(price, knots=c(10,20,30)),data=onlypaid)
plot(rating ~ price,col="darkgrey", data= onlypaid)
lines(onlypaid$price,predict(fit,list(price = onlypaid$price)),col="darkgreen",lwd=2)
abline(v=c(10,20,30),lty=2,col="darkgreen")
#doesn't look good

fitsmooth=smooth.spline(onlypaid$rating,onlypaid$price, cv =TRUE)
lines(fitsmooth,col="red",lwd=2)

fit=loess(rating~price,span=.5,data=onlypaid)
lines(onlypaid$price, predict(fit, onlypaid$price),col="red",lwd=2)

#all seem to be doing terrible
#let's leave it as linear for now, but do we really need the variable price?
#it only gives us additional info of 7% of the observations (those that are Paid apps)
#let's calculate the relationship between the two to prepare us for subset selection

summary(lm(price~ type, data= googleData))
Anova(lm(price~ type, data= googleData), type = "III") #as expected, there is a significant relationship

#let's try backward selection manually
#here's the full linear model code again
fullmodel <- lm(rating~ ., data = googleData)
summary(fullmodel)

#now let's take out price and see if that significantly changes the prediction accuracy
noprice <- lm(rating~.-price, data = googleData)
summary(noprice)
anova(fullmodel, noprice) #the loss is maybe significant? depends on our alpha, let's look at more predictors

#what about reviews and installs
noreviews <- lm(rating~. -reviews, data = googleData)
summary(noreviews)
anova(fullmodel, noreviews)

noinstalls <-lm(rating~. -installs, data = googleData)
summary(noinstalls)
anova(fullmodel, noinstalls) #ok, here we see what removing significant predictor does,

#let's set our alpha to 0.001 and remove price and reviews from our model

updatedmodel <- lm(rating~. - price - reviews, data = googleData)
summary(updatedmodel)
Anova(updatedmodel, type = "III")

#let's also remove size and content rating
updatedmodel2 <- lm(rating~. - price - reviews - size - contentrating, data = googleData)
summary(updatedmodel2)
Anova(updatedmodel2)

#compare all the different versions of models
anova(fullmodel, updatedmodel, updatedmodel2) 

#although our alpha level is 0.001, we keep the variable androidver because it's worth mentioning
#let's make a new dataframe with only those columns we need

newData <- select(googleData, c("category", "rating", "installs", "type", "lastupdated", "androidver"))
dim(newData)

#calculate the updated test error, see if it's any different

#calculate the test error
#use 2/3 data for training data, 1/3 for test
set.seed(201)
twothirds2 = sample(1:nrow(newData), 6244)
train2 <- newData[twothirds,]
test2 <- newData[-twothirds,]

lmgoogle2 <- lm(rating~ ., data = train2)
lm.pred2 = predict(lmgoogle2, test2, type = "response")

mean((test2$rating - lm.pred2)^2) #0.261 essentially same

#what about cross validation error?
cv.error10=rep(0,5)
degree= 1:5
d =1
for(d in degree){
        glm.fit=glm(rating~., data=newData)
        cv.error10[d]=cv.glm(newData,glm.fit,K=10)$delta[1]
} ##0.242 essentially same, removing those variables has no significant effect so we are good


#create graphics for analysis

plot(rating~ lastupdated, data = newData)
abline(lm(rating~lastupdated, data = newData))
hist(newData$lastupdated, breaks = 100)

#plot installs
hist(newData$androidver, breaks = 10)

#look at response variable
hist(newData$rating, breaks = 100) #oh crap, it's left skewed
hist((newData$rating)^2, breaks = 100) #squaring it doesn't make too much difference
#lets plot the rating variable to see if we can subset it

boxplot(newData$rating)
summary(newData$rating) #25% of the data is 1-4, 75% of the data is 4.0 - 5.0

#maybe try dividing data into two and testing the accuracy?
#let's also write a separate file that does the analysis so that we don't write the code everytime

#divde data frame into two separate parts
lessthanfour <- filter(newData, rating < 4)
morethanfour <- filter(newData, rating >=4)

#let's run linear models on these two
#linear model less than four
lmltf <- lm(rating~., data = lessthanfour)
summary(lmltf)
Anova(lmltf, type = "III")

#linear model more than four
lmmtf <- lm(rating~., data = morethanfour)
summary(lmmtf)
Anova(lmmtf, type = "III")

#plot the relationships with par, plot

par(mfrow=c(2,3))
plot(rating~category, data = lessthanfour)
plot(rating~installs, data = lessthanfour)
plot(rating~type, data = lessthanfour)
plot(rating~lastupdated, data = lessthanfour)
plot(rating~androidver, data = lessthanfour)
residualPlot(lmltf)
title("Rating < 4.0", outer=TRUE, line = -2) 

#do same for more than four

par(mfrow=c(2,3))
plot(rating~category, data = morethanfour)
plot(rating~installs, data = morethanfour)
plot(rating~type, data = morethanfour)
plot(rating~lastupdated, data = morethanfour)
plot(rating~androidver, data = morethanfour)
residualPlot(lmmtf)
title("Rating >= 4.0", outer=TRUE, line = -2) 

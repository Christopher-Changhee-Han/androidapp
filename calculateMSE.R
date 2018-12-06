#input the dataset, response, features, and the random seed
#calculate the test error of a linear model
#use 2/3 data for training data, 1/3 for test
#returns a vector of length two, 
#[1] is MSE using 1/3 of the data as test
#[2] is average MSE calculated using 10-fold cross validation with 5 iterations

calculateMSE <- function(dataset, response = "", features = ".", seed = 201){
        set.seed(seed)
        results <- vector(length = 2, mode = "numeric")
        twothirds = sample(1:nrow(dataset), nrow(dataset)/3)
        train <- dataset[twothirds,]
        test <- dataset[-twothirds,]
        
        lmdata <- lm(response~., data= train)
        lm.pred = predict(lmdata, test, type = "response")
        
        results[1] <- mean((test$response - lm.pred)^2) #MSE

        #what about cross validation error?
        cv.error10=rep(0,5)
        degree= 1:5
        d=1
        for(d in degree){
                glm.fit=glm(response~ features, data=dataset)
                cv.error10[d]=cv.glm(dataset,glm.fit,K=10)$delta[1]
        }
        results[2] <- cv.error10/5
        results
}


library(MASS)
library(caret)
library(boot)
creatdataset <- function(n){
 
  theta = NULL
  for(j in 1:5){
    theta[j] = j
  }
  
  beta <- 6
  x <- runif(n,-1,1)
  error <- 1
  
  tau1 <- plogis(theta[1] - beta * x)
  tau2 <- plogis(theta[2] - beta * x)
  tau3 <- plogis(theta[3] - beta * x)
  tau4 <- plogis(theta[4] - beta * x)
  tau5 <- 1
  
  tau <- data.frame(tau1,tau2,tau3,tau4,tau5)
  #tau5 <- plogis(theta[5] - beta * x)
  
  pi1 <- tau1
  pi2 <- tau2-pi1
  pi3 <- tau3-pi2-pi1
  pi4 <- tau4-pi3-pi2-pi1
  pi5 <- tau5-pi4-pi3-pi2-pi1
  
  pi <- data.frame(pi1,pi2,pi3,pi4,pi5)
  y <- array(dim = dim(pi))
  
  for(i in 1:dim(pi)[1]){
    y[i,] = rmultinom(1,1,pi[i,])
  }
  
  
  for(j in 1:5){
    y[,j] = j*y[,j]
  }
  y_order = as.factor(apply(y,1,"sum"))
  mydata <- cbind(y_order,x)
  mydata <- as.data.frame(mydata)
  mydata$y_order <- as.factor(mydata$y_order)
  mydata$x <- as.numeric(mydata$x)
  return(mydata)
}


model <- polr(y_order~x,Hess = T,data = mydata)
summary(model)

MRI1 <- creatdataset(1000)
MRI2 <- creatdataset(1000)

traning <- MRI1[1:500,]
testing <- MRI1[501:length(mydata),]
traning2 <- MRI2[1:500,]
testing2 <- MRI2[501:length(mydata),]


modelt <- train(y_order ~ x, method = "polr", data = traning, trControl = trainControl(method = "LOOCV"))
modelt2 <- train(y_order ~ x, method = "polr", data = traning2, trControl = trainControl(method = "LOOCV"))
modelt

pred1 <- predict(modelt, newdata = testing)
pred2 <- predict(modelt, newdata = testing2)
conm1 <- confusionMatrix(testing$y_order,pred1)
conm1 <- confusionMatrix(testing2$y_order,pred2)
conm1
conm2







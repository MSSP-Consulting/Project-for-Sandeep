library(MASS)
library(caret)
library(boot)
library(ggplot2)

creatdataset <- function(n){
  
  theta = NULL
  for(j in 1:5){
    theta[j] = j
  }
  
  beta <- 6
  x <- runif(n,-1,1)
  x2 <- x + rnorm(n,0,0.01)
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
  mydata <- cbind(y_order,x,x2)
  mydata <- as.data.frame(mydata)
  mydata$y_order <- as.factor(mydata$y_order)
  mydata$x <- as.numeric(mydata$x)
  mydata$x2 <- as.numeric(mydata$x2)
  return(mydata)
}

mydata <- creatdataset(1000)
devs1 = c()
devs2 = c()
for (i in 1:1000){
  model1 <- polr(y_order~x, Hess = T,data = mydata[-i,])
  model2 <- polr(y_order~x2,Hess = T,data = mydata[-i,])
  devs1  = c(devs1, model1$deviance)
  devs2 = c(devs2,model2$deviance)
}


boxplot(devs1,devs2,
        xlab = "MRI groups",
        ylab = "deviance",
        col = c("green","red"),
        names = c("MRI1","MRI2"))


testresult <- ks.test(devs1,devs2)  
testresult


# Compare the means of devs and devs2, which is larger?
# Create two box plots, side by side, of devs and devs2 
# Do a ks test on devs and devs2, is it significantly different?


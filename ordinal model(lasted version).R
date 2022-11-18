library(MASS)
set.seed(1000)
reps <- 1000
n <- 1000
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


model <- polr(y_order~x,Hess = T)

summary(model)
  





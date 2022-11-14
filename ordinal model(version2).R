library(MASS)
set.seed(1000)
reps <- 100
n <- 100
par.est.oprobit <- matrix(NA, nrow = reps, ncol = 2) 
taus.oprobit <- matrix(NA, nrow = reps, ncol = 3)
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
tau5 <- plogis(theta[5] - beta * x)

tau <- data.frame(tau1,tau2,tau3,tau4,tau5)


pi1 <- tau1
pi2 <- tau2+tau1
pi3 <- tau3+tau2+tau1
pi4 <- tau4+tau3+tau2+tau1
pi5 <- tau5+tau4+tau3+tau2+tau1

pi <- data.frame(pi1,pi2,pi3,pi4,pi5)





#for(i in 1:reps){
# Y <- rnorm(n, y, error) 
# r <- rep(NA, n) 
#  r[Y < tau1] <- 0 
#  r[Y >= tau1 & Y < tau2] <- 1
#  r[Y >= tau2 & Y < tau3] <- 2
#  r[Y >= tau3 & Y < tau4] <- 3
#  r[Y >= tau4 & Y < tau5] <- 4
#  r[Y >= tau5] <- 5
#}
#R <- as.ordered(r)
#model <- polr(R ~ x, Hess = TRUE)
#summary(model)




#tau1 <- qnorm(.1, mean = mean(y), sd = sqrt(var(y) + error^2))
#tau2 <- qnorm(.3, mean = mean(y), sd = sqrt(var(y) + error^2))
#tau3 <- qnorm(.5, mean = mean(y), sd = sqrt(var(y) + error^2))
#tau4 <- qnorm(.7, mean = mean(y), sd = sqrt(var(y) + error^2))
#tau5 <- qnorm(.9, mean = mean(y), sd = sqrt(var(y) + error^2))


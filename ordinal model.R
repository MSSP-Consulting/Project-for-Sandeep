library(MASS)
set.seed(1000)
reps <- 1000
n <- 1000
par.est.oprobit <- matrix(NA, nrow = reps, ncol = 2) 
taus.oprobit <- matrix(NA, nrow = reps, ncol = 3)
b0 <- 2
b1 <- 6
x <- runif(n,-1,1)
y <- b0 + b1*x
error <- 1
tau1 <- qnorm(.1, mean = mean(y), sd = sqrt(var(y) + error^2))
tau2 <- qnorm(.3, mean = mean(y), sd = sqrt(var(y) + error^2))
tau3 <- qnorm(.5, mean = mean(y), sd = sqrt(var(y) + error^2))
tau4 <- qnorm(.7, mean = mean(y), sd = sqrt(var(y) + error^2))
tau5 <- qnorm(.9, mean = mean(y), sd = sqrt(var(y) + error^2))

for(i in 1:reps){
  Y <- rnorm(n, y, error) 
  r <- rep(NA, n) 
  r[Y < tau1] <- 0 
  r[Y >= tau1 & Y < tau2] <- 1
  r[Y >= tau2 & Y < tau3] <- 2
  r[Y >= tau3 & Y < tau4] <- 3
  r[Y >= tau4 & Y < tau5] <- 4
  r[Y >= tau5] <- 5
}
R <- as.ordered(r)
model <- polr(R ~ x, Hess = TRUE)
summary(model)

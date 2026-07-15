# genericHorseRace
# does the worse-measured of 2-highly correlated variables lose?
# Tom Davidoff 
# 06/29/26

N <- 1000 # obs
T <- 1000 # simulations
rho  <- .9 # correlation between the two variables
library(MASS)
library(data.table)

betaHat <- data.table(xw=numeric(),xB=numeric())
for (i in 1:T) {
	# create bivariate normal with high correlation for the two 
	x <- mvrnorm(N, mu = c(0, 0), Sigma = matrix(c(1, rho, rho, 1), nrow = 2))
	xw <- x[,1] # well-measured variable
	xb <- x[,2] # badly-measured variable
	xB <- xb+rnorm(N, mean = 0, sd = 1) # badly-measured variable with noise
	y <- xw + xb + rnorm(N, mean = 0, sd = 1) # outcome variable
	reg <- summary(lm(y ~ xw + xB))
	betaHat <- rbind(betaHat, data.table(xw=reg$coefficients[2,1],xB=reg$coefficients[3,1]))
}
print(summary(betaHat))


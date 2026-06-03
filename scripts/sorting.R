# sorting.R
# do a numerical example
# Tom Davidoff 
# 06/01/26

utilityInside <- function(y,p,size,nUnits,theta){
  return(log(y-p) + log(size) - log(nUnits) + log(theta))
}
groupIncome <- c(50000,200000)
utilityOutside <- log(groupIncome)
lotSize <- 33*122 # standard lot
n0 <- 1
FSR0 <- .7
size0 <- lotSize*FSR0/n0
cost <- 400
r <- .05 # appx mortgage constant
cost0 <- cost*size0
# outer boundary of worse group
pB <- cost0
# satisfies log(y-pBr) + log(size0) - log(n0) + log(theta) = utilityOutside[1]
theta0 <- exp(utilityOutside[1] - log(size0) + log(n0) - log(groupIncome[1] - pB))
# call inner boundary location 1, utility pinned down by high types






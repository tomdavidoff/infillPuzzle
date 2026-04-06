# testMechanical.R
# question if mechanical relationship between average price and price per square foot
# Tom Davidoff
# 04/05/26

library(data.table)
library(fixest)

NperCommunity <- 50
NCommunity <- 100
NSim <- 1000
cors <- NULL
for (i in 1:NSim) {
	sqft <- runif(NperCommunity * NCommunity)
	price <- sqft+rnorm(NperCommunity * NCommunity)
	community <- rep(1:NCommunity, each=NperCommunity)
	dt <- data.table(price, sqft, community)
	meanPrice <- dt[, .(meanPrice=mean(price)), by=community]
	# get regression coefficient for price on sqft by community
	dt[, beta := cov(price, sqft)/var(sqft), by=community]
	beta <- dt[, .(beta=mean(beta)), by=community]
	print(beta)
	print(meanPrice)
	dc <- merge(meanPrice, beta, by="community")
	print(summary(dc))
	cors <- c(cors, as.numeric(cor(dc$meanPrice, dc$beta)))
}
	
print(summary(cors))

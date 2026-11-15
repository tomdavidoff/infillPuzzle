# modelSimple.R
# R to solve for equilibrium stuff
# Tom Davidoff
# 11/13/26

# u = t+h+ ln(y-p)
# outside option t+h = k,p=p0
# outside utility = k + ln(y-p0) 
# inside utility = t+h + ln(y-p)
# hence ln(y-p) = k-t-h + ln(y-p0)
# y-p = exp(k-t-h)*(y-p0)
# p = y - exp(k-t-h)*(y-p0)

library(data.table)


wtp <- function(y,p0,k,t,h) {
	return(y - exp(k-t-h)*(y-p0))
}

for (y in c(3,5,10)) {
	for (p0 in c(2.5)) {
		for (k in c(3)) {
			for (t in c(1,2)) {
				for (h in c(1,2)) {
			print(c(y,p0,k,t,h))
			print(wtp(y,p0,k,t,h))
				}
			}
		}
	}
}

for (y in c(3,5,10)) {
	for (p0 in c(1)) {
		for (k in c(2)) {
			for (t in c(1,2)) {
				for (h in c(1,2)) {
			print(c(y,p0,k,t,h))
			print(wtp(y,p0,k,t,h))
				}
			}
		}
	}
}



# Try different utility
#log(t)+log(s) + log(y-p(t,s))
#outside: 0 + 0 + log(y-p0)


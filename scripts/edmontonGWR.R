# edmontonGWR.R
# spatially weighted price level + local elasticity of price wrt gross area
# fit points = permit locations, estimation sample = pre-2023 RS assessment parcels
# adaptive k-NN bisquare bandwidth, parallelized via mclapply
# Tom Davidoff
# 06/25/26

library(data.table)
library(sf)
library(parallel)

# ---------------------------------------------------------------------
# gwrAtPoints: for each fit point, run a weighted lm on the price sample
# and pull out (a) the local logGrossArea coefficient = elasticity and
# (b) the predicted local price level.
#
#   sfFit    : sf of fit points (permits), projected in metric CRS
#   sfPrice  : sf of estimation sample (assessment), same CRS, with
#              logAssessedValue, logGrossArea, logLotSize, logAge
#   k        : adaptive bandwidth = distance to k-th nearest price point
#   refRow   : optional one-row data.frame of covariates at which to
#              predict the price level. If NULL, predict at the sample
#              mean of logGrossArea/logLotSize/logAge (a clean surface).
# returns a data.table aligned row-for-row with sfFit
# ---------------------------------------------------------------------
gwrAtPoints <- function(sfFit, sfPrice, k=200, refRow=NULL, mc.cores=detectCores()-1) {

  stopifnot(st_crs(sfFit)==st_crs(sfPrice))

  fitXY <- st_coordinates(sfFit)
  priceXY <- st_coordinates(sfPrice)
  dtPrice <- as.data.table(st_drop_geometry(sfPrice))

  gform <- logAssessedValue ~ logGrossArea + logLotSize + logAge

  # if no reference covariates given, evaluate the surface at sample means
  if (is.null(refRow)) {
    refRow <- data.frame(logGrossArea=mean(dtPrice$logGrossArea),
                         logLotSize  =mean(dtPrice$logLotSize),
                         logAge      =mean(dtPrice$logAge))
  }

  n <- nrow(fitXY)
  print(paste("gwr over",n,"fit points,",nrow(priceXY),"price points, k =",k))

  oneFit <- function(i) {
    dx <- priceXY[,1]-fitXY[i,1]
    dy <- priceXY[,2]-fitXY[i,2]
    d <- sqrt(dx*dx+dy*dy)             # metres, CRS is UTM
    bw <- sort(d,partial=k)[k]         # adaptive k-NN bandwidth
    w <- (1-(d/bw)^2)^2
    w[d>bw] <- 0
    use <- w>0
    if (sum(use)<10) return(c(elasticity=NA_real_,priceLevel=NA_real_,bw=bw))

    # local frame as a plain data.frame, weight as a column.
    # (data.frame, not dt subset + := , which trips selfref on a shallow copy)
    dLoc <- as.data.frame(dtPrice[use])
    dLoc$wgt <- w[use]

    # whole fit wrapped: one bad point returns NAs rather than killing the core
    tryCatch({
      m <- lm(gform, data=dLoc, weights=wgt)
      cf <- coef(m)
      pl <- as.numeric(predict(m,newdata=refRow))
      c(elasticity=unname(cf["logGrossArea"]), priceLevel=pl, bw=bw)
    }, error=function(e) c(elasticity=NA_real_,priceLevel=NA_real_,bw=bw))
  }

  res <- mclapply(1:n, oneFit, mc.cores=mc.cores)
  # mclapply returns try-error objects on failure; rbind only the numeric vectors
  bad <- !vapply(res, is.numeric, logical(1))
  if (any(bad)) {
    print(paste(sum(bad),"fit points errored; inspect res[bad] - first message:"))
    print(res[[which(bad)[1]]])
    res[bad] <- list(c(elasticity=NA_real_,priceLevel=NA_real_,bw=NA_real_))
  }
  out <- as.data.table(do.call(rbind,res))
  setnames(out,c("elasticity","priceLevel","bw"))
  print(summary(out))
  return(out[])
}

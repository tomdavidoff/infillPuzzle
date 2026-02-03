# vancouverProfit.R
# estimate profitability of redevelopment choices in Vancouver
# Tom Davidoff
# 01/31/26

library(data.table)
library(ggplot2)
library(fixest)
library(sf)

# Job 1 show that in Vancouver, elasticity and price per square foot are not two different things by showing reg price/sqft on elasticity and meanPPSF from prior regression yields only significant coeff on meanPPSF?

# grab and save folio geometries for Vancouver if not yet saved
fGeo <- "~/OneDrive - UBC/dataProcessed/bca26FolioGeometryVancouver.rds"
if (!file.exists(fGeo)) {
  print("HHH")
  dfG <- st_read(
  "/Volumes/T7Office/bigFiles/bca_folios_spatial_file_20251103/bca_folios.gpkg.gpkg",
  query = "SELECT ROLL_NUMBER, ACTUAL_USE_DESCRIPTION, NEIGHBOURHOOD,
           SHAPE_AREA, SHAPE_LEN, geom, JURISDICTION
           FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
           WHERE JURISDICTION = 'City of Vancouver'"
)
  print(head(dfG))
  print(table(dfG$JURISDICTION))
  fCT <- "~/OneDrive - UBC/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"
  dCT <- st_read(fCT)
  dCT <- dCT[dCT$PRUID=="59",]
  print(head(dCT))
  # swap crs of census tracts to that of merged)
  print(st_crs(dCT))
  print(st_crs(dfG))
  dCT <- st_transform(dCT,st_crs(dfG))

  dfG <- st_join(dfG,dCT,join=st_within)
  saveRDS(dfG,fGeo)
}

dfG <- readRDS(fGeo)

# merge geo with census tract geography
# now get census tracts
dtGeo <- as.data.table(dfG)[,.(ROLL_NUMBER,CTNAME,ACTUAL_USE_DESCRIPTION)]
print(table(dtGeo$ACTUAL_USE_DESCRIPTION))
dtGeo <- dtGeo[ACTUAL_USE_DESCRIPTION %in% c("Single Family Dwelling", "Residential Dwelling with Suite")]
dtSales <- fread("~/OneDrive - UBC/Documents/data/bca/data_advice_REVD24_20240331/bca_folio_sales_20240331_REVD24.csv",select=c("ROLL_NUMBER","CONVEYANCE_DATE","CONVEYANCE_PRICE"))
print(head(dtSales))
dtInventory <- fread("~/OneDrive - UBC/Documents/data/bca/Residential_inventory_202501/20250101_A09_Residential_Inventory_Extract.txt",select=c("Roll_Number","MB_Effective_Year","MB_Total_Finished_Area","Land_Width_Width","Land_Depth_Depth"),colClasses=list(character=c("Roll_Number"),numeric=c("MB_Effective_Year","MB_Total_Finished_Area","Land_Width_Width","Land_Depth_Depth")))
print(head(dtInventory))

dtSales <- merge(dtSales,dtInventory,by.x="ROLL_NUMBER",by.y="Roll_Number")
dtSales <- merge(dtSales,dtGeo,by="ROLL_NUMBER")
print(head(dtSales))
print(summary(dtSales))
dtSales[,year:=as.numeric(substr(CONVEYANCE_DATE,1,4))]
MINYEAR <- 2019
MINPRICE <- 500000
dtSales <- dtSales[year>=MINYEAR & CONVEYANCE_PRICE>=MINPRICE & MB_Total_Finished_Area>0 & Land_Width_Width>0 & Land_Depth_Depth>0]
setnames(dtSales,"CTNAME","tract")

# load data
dtPrice <- fread("~/OneDrive - UBC/dataProcessed/bca19_mean_ppsf_slope_by_tract.csv",colClasses=list(character=c("CTUID")))
dtChoice <- readRDS("~/OneDrive - UBC/dataProcessed/vancouverPermitLotsTracts.rds")

dtMerge <- merge(dtChoice,dtPrice,by="CTUID")
dtMerge[,tract:=substr(CTUID,4,10)]

dtPrice[,tract:=substr(CTUID,4,10)]
dtSales <- merge(dtSales,dtPrice,by="tract")
print(summary(feols(log(CONVEYANCE_PRICE/MB_Total_Finished_Area) ~ meanPPSF + log(MB_Total_Finished_Area)*(meanPPSF+elasticity), data=dtSales)))
print(head(dtMerge))
print(head(dtMerge))
print(table(dtMerge[,use]))

# some major outliers in slope

print(summary(dtMerge))
print(summary(dtMerge[nobs>quantile(nobs,.05)]))
print(summary(dtMerge[nobs>quantile(nobs,.1)]))
print(unique(dtMerge[elasticity>0,.(nobs,neighbourhoodDescription,elasticity)]))
dtMerge <- dtMerge[nobs>4]
# Economic intuition says elasticity can't be less than -1 or else price falls in square feet. Also, as there is no assembly for larger lots, elasticity must be less than 0
dtMerge <- dtMerge[elasticity>0,elasticity:=0]
dtMerge <- dtMerge[elasticity < -1,elasticity:=-1]
CUTDISTANCE <- 20
# make this a switch
dtMerge[use=="single",use:=ifelse(distToLaneway>CUTDISTANCE,"singleOnly","singleLaneway")]
dtMerge[nearestLanewayPermit>CUTDISTANCE & use=="single",use:="singleOnly"]
dtMerge[nearestLanewayPermit<=CUTDISTANCE & use=="single",use:="singleLaneway"]
dtMerge <- dtMerge[use!="laneway"] # drop only laneway

# plot use against lat/long --extract from geo_point_2d
dtMerge[, c("lat", "lon") := tstrsplit(geo_point_2d, ", ", type.convert = TRUE)]
ggplot(dtMerge,aes(x=lon,y=lat,color=use)) + geom_point() + theme_minimal()
ggsave("text/vancouverUseSpatial.png",width=6,height=6)

# also do ppsf against lat/long and then do elasticity
ggplot(dtMerge,aes(x=lon,y=lat,color=meanPPSF)) + geom_point() + theme_minimal() + scale_color_viridis_c()
ggsave("text/vancouverPPSFSpatial.png",width=6,height=6)

ggplot(dtMerge,aes(x=lon,y=lat,color=elasticity)) + geom_point() + theme_minimal() + scale_color_viridis_c()
ggsave("text/vancouverElasticitySpatial.png",width=6,height=6)

# show uses with ppsf on x-axis - bar chart with share of uses by ppsf bin
# make ppsf bins by quintile of meanPPSF
dtMerge[,ppsfBin:=cut(meanPPSF,breaks=quantile(meanPPSF,probs=seq(0,1,by=.1),na.rm=TRUE),include.lowest=TRUE)]
dtUsePPSF <- dtMerge[,.N,by=.(ppsfBin,use)]
dtUsePPSF[,totalN:=sum(N),by=ppsfBin]
dtUsePPSF[,share:=N/totalN]
ggplot(dtUsePPSF,aes(x=ppsfBin,y=share,fill=use)) + geom_bar(stat="identity",position="fill") + theme_minimal() +
  labs(title="Vancouver Residential Permits by Price per Square Foot",
       x="Mean Price per Square Foot Bin",
       y="Share of Permits by Use") +
  scale_fill_viridis_d()
ggsave("text/vancouverUseShareByPPSF.png",width=8,height=6)

print(summary(dtMerge))
dtMerge[,MB_total_finished_area:=as.numeric(MB_total_finished_area)]
# ratio of acres to sqft
ACRESF <- 43560
dtMerge[,lotSizeSqftX:=LANDAREA*ACRESF]
dtMerge[,landWidth:=as.numeric(landWidth)]
dtMerge[,landDepth:=as.numeric(landDepth)]
dtMerge[,lotSizeSqft:=landWidth*landDepth]
FSR_SINGLEDUPLEX <- .7 # appx
FSR_LANEWAY <- .16
COSTPSF <- 500
dtMerge[,buildableLaneway:=pmax(MB_total_finished_area+FSR_LANEWAY,(FSR_SINGLEDUPLEX+FSR_LANEWAY*lotSizeSqft))]
dtMerge[,buildableDuplex:=pmax(MB_total_finished_area,FSR_SINGLEDUPLEX*lotSizeSqft)]
dtMerge[,profitLaneway:=buildableLaneway*(meanPPSF - COSTPSF)]
dtMerge[,profitDuplex:=buildableDuplex*(meanPPSF*exp(elasticity/2)-COSTPSF)]
dtMerge[,profitMulti:=lotSizeSqft*(meanPPSF*exp(elasticity/3)-COSTPSF-150)] # placeholder
dtMerge[,deltaProfit := profitLaneway - profitDuplex]
dtMerge[,single := use %chin% c("singleOnly","singleLaneway")]
dtMerge[,plex := use %chin% c("duplex","multi")]
dtMerge[,laneway := use %chin% c("laneway")]
# use is singleOnly or singleLaneway if nearestLanewayPermit >20/<20
CUTDISTANCE <- 20
dtMerge <- dtMerge[abs(deltaProfit)<1000000]
print(summary(dtMerge))
dtMerge <- dtMerge[!is.na(profitLaneway) & !is.na(profitDuplex)]
print(cor(dtMerge[use!="laneway",.(single,plex,meanPPSF,elasticity,deltaProfit)]))
print(cor(dtMerge[MB_effective_year<2000,.(single,plex,laneway,meanPPSF,elasticity,deltaProfit)]))
print(cor(dtMerge[,.(lotSizeSqft,lotSizeSqftX)]))
print(feols(single ~ meanPPSF + deltaProfit,data=dtMerge[MB_effective_year<1960]))
print(feols(single ~ meanPPSF + elasticity,data=dtMerge[MB_effective_year<1960]))
print(summary(dtMerge[,deltaProfit]))

#perfect correlation among price/elasticity/income
# conjecture -- ordered probit, but 2-dimensional
dtSingle <- dtMerge[use %in% c("single","laneway"),.(laneway=mean(laneway),meanPPSF=mean(meanPPSF),elasticity=mean(elasticity)),by=CTUID]
ggplot(dtSingle,aes(x=meanPPSF,y=elasticity,color=laneway)) + geom_point() + theme_minimal()
ggsave("text/singleMeanElasticity.png",width=6,height=6)



### LATER -- income doesn't really help
dtIncome <- fread("~/OneDrive - UBC/dataRaw/9810005801_databaseLoadingData.csv",select=c("GEO","DGUID","Household income statistics (6)","VALUE"))
print(head(dtIncome))
setnames(dtIncome,c("DGUID","Household income statistics (6)","VALUE"),c("DGUID","incomeStat","medianIncome"))
print(head(dtIncome))
dtIncome <- dtIncome[incomeStat=="Median household total income (2020) (2020 constant dollars)"]
print(head(dtIncome))
dtIncome <- dtIncome[grepl("Vancouver",GEO)==TRUE]
print(head(dtIncome))
dtIncome[,tract:=substr(DGUID,13,19)]
print(head(dtIncome))
print(head(dtMerge))
dtMerge <- merge(dtMerge,dtIncome[,.(tract,medianIncome)],by="tract")
print(cor(dtMerge[MB_effective_year<1960,.(single,plex,laneway,meanPPSF,elasticity,deltaProfit,medianIncome)]))

dtMerge[,permitYear:=as.numeric(substr(permitnumbercreateddate,1,4))]
# do share of multi/duplex for years 2024, 2025 (exclude laneway/single)
# Same bin plot as before, but confined to year in 2024/2025 and only multi/duplex, key var is plex share
dtYear <- dtMerge[permitYear %in% c(2024,2025) & plex==1,.(N=.N),by=.(ppsfBin,use)]
dtYear[,totalN:=sum(N),by=.(ppsfBin)]
dtYear[,share:=N/totalN]
print(dtYear)
dtPlexShare <- dtYear[use=="multi"]
print(head(dtPlexShare))
ggplot(dtPlexShare,aes(x=ppsfBin,y=share)) + geom_bar(stat="identity",position="dodge") + theme_minimal() +
  labs(title="Vancouver Duplex/Multiplex Permits by Price per Square Foot",
       x="Mean Price per Square Foot Bin",
       y="Share of Duplex/Multiplex Permits") +
  scale_fill_viridis_d(name="Permit Year")

ggsave("text/vancouverPlexShareByPPSF.png",width=8,height=6)

# profit from multi vs duplex
dtMerge[,deltaProfitMulti:=profitMulti - profitDuplex]
print(cor(dtMerge[plex==1,.(meanPPSF,elasticity,use=="multi",use=="duplex",deltaProfitMulti)]))
print(cor(dtMerge[laneway==0,.(meanPPSF,elasticity,use=="multi",use=="duplex",deltaProfit)]))

# define a profit function as a function of constant alpha * meanPPSF
# plus beta additive mean by census tract with common standard deviation sigma
# choose alpha, beta, sigma such that laneway profit = FSR laneway * alpha*meanPPSF - beta_tract + epsilon


dtMerge[, lanewayFSR := fifelse(permitYear < 2024, 0.16, 0.25)]

### =========================
### ESTIMATION BLOCK (NEW) — DO NOT DUPLICATE
### =========================

dtSingles <- dtMerge[use %chin% c("singleOnly","singleLaneway")]
dtSingles[, hasLaneway := as.integer(use == "singleLaneway")]

tract_levels <- sort(unique(dtSingles$tract))
Nt <- length(tract_levels)
dtSingles[, tract_id := match(tract, tract_levels)]
stopifnot(all(!is.na(dtSingles$tract_id)))

# SCALE to avoid probit saturation
dtSingles[, x := (lanewayFSR * meanPPSF) / 100 ]

piLanewayNLL_stable <- function(par, dtSingles, Nt, lambda_beta = 1e-2) {
  alpha <- par[1]
  beta  <- par[1 + (1:Nt)]
  beta  <- beta - mean(beta)  # identify level

  mu <- alpha * dtSingles$x - beta[dtSingles$tract_id]
  p  <- pnorm(mu)
  p  <- pmin(pmax(p, 1e-9), 1 - 1e-9)

  y <- dtSingles$hasLaneway
  nll <- -sum(y * log(p) + (1 - y) * log(1 - p))

  nll + lambda_beta * sum(beta^2)
}

par0 <- c(0, rep(0, Nt))

fitLaneway_NEW <- optim(
  par = par0,
  fn  = piLanewayNLL_stable,
  dtSingles = dtSingles,
  Nt = Nt,
  method = "L-BFGS-B",
  control = list(maxit = 2000)
)

alpha_hat_NEW <- fitLaneway_NEW$par[1]
beta_hat_NEW  <- fitLaneway_NEW$par[1 + (1:Nt)]
beta_hat_NEW  <- beta_hat_NEW - mean(beta_hat_NEW)

cat("\n=== NEW stable probit (sigma fixed to 1) ===\n")
cat("convergence code =", fitLaneway_NEW$convergence, "\n")
cat("alpha_hat_NEW =", alpha_hat_NEW, "\n")
cat("objective =", fitLaneway_NEW$value, "\n")

betaTract_hat_NEW <- data.table(tract = tract_levels, betaTract = beta_hat_NEW)

dtSinglesCheck <- merge(dtSingles, betaTract_hat_NEW, by="tract", all.x=TRUE)
dtSinglesCheck[, mu := alpha_hat_NEW * x - betaTract]
dtSinglesCheck[, p  := pnorm(mu)]

cat("Mean predicted p (singles):", mean(dtSinglesCheck$p), "\n")
cat("Observed share laneway:", mean(dtSinglesCheck$hasLaneway), "\n")
print(summary(dtSinglesCheck$mu))

### =========================
### TRACT-LEVEL FIT DIAGNOSTICS
### =========================

# collapse to tract means
dtTractFit <- dtSinglesCheck[, .(
  N = .N,
  actual_laneway = mean(hasLaneway),
  pred_laneway   = mean(p),
  mean_mu        = mean(mu),
  mean_x         = mean(x)
), by = tract]

# simple correlations
cat("\nTract-level correlations:\n")
cat("cor(predicted, actual) =",
    cor(dtTractFit$pred_laneway, dtTractFit$actual_laneway), "\n")

cat("cor(mean_mu, actual) =",
    cor(dtTractFit$mean_mu, dtTractFit$actual_laneway), "\n")

# weighted correlation (by number of observations in tract)
wcor <- function(x, y, w) {
  xbar <- weighted.mean(x, w)
  ybar <- weighted.mean(y, w)
  sum(w * (x - xbar) * (y - ybar)) /
    sqrt(sum(w * (x - xbar)^2) * sum(w * (y - ybar)^2))
}

cat("weighted cor(predicted, actual) =",
    wcor(dtTractFit$pred_laneway,
         dtTractFit$actual_laneway,
         dtTractFit$N), "\n")

### =========================
### OOS: betaTract vs duplex share (among single-ish)
### =========================

# 1) Build tract-level duplex share using permits that are NOT in the single sample
#    (i.e., exclude the two single categories used in the laneway estimation)
dtOOS <- dtMerge[!use %chin% c("singleOnly","singleLaneway") &
                  use %chin% c("duplex","singleOnly","singleLaneway"),
                .(tract, use)]

# In practice the line above will keep only "duplex" because we excluded singles already,
# but we keep the structure explicit in case you widen the definition later.
dtOOS[, isDuplex := as.integer(use == "duplex")]

dtDuplexShare <- dtOOS[, .(
  N_oos = .N,
  duplexShare_oos = mean(isDuplex)
), by = tract]

# 2) Join with betaTract (estimated from singles)
dtBetaDuplex <- merge(betaTract_hat_NEW, dtDuplexShare, by="tract", all.x=TRUE)

# Drop tracts with no OOS duplex observations
dtBetaDuplex <- dtBetaDuplex[!is.na(duplexShare_oos) & N_oos > 0]

cat("\nOOS duplex share vs betaTract:\n")
cat("Tracts with OOS data:", nrow(dtBetaDuplex), "of", nrow(betaTract_hat_NEW), "\n")
cat("cor(betaTract, duplexShare_oos) =",
    cor(dtBetaDuplex$betaTract, dtBetaDuplex$duplexShare_oos), "\n")

# Weighted correlation by number of OOS permits
wcor <- function(x, y, w) {
  xbar <- weighted.mean(x, w)
  ybar <- weighted.mean(y, w)
  sum(w * (x - xbar) * (y - ybar)) /
    sqrt(sum(w * (x - xbar)^2) * sum(w * (y - ybar)^2))
}

cat("weighted cor(betaTract, duplexShare_oos) =",
    wcor(dtBetaDuplex$betaTract, dtBetaDuplex$duplexShare_oos, dtBetaDuplex$N_oos), "\n")

### =========================
### TRACT-LEVEL SINGLE SHARE
### =========================

dtTractSingle <- dtMerge[, .( singleShare = mean(grepl("single", use)), meanPPSF_t  = mean(meanPPSF) ), by = tract]

dtTractSingle <- merge( dtTractSingle, betaTract_hat_NEW, by = "tract" )

print(cor(dtTractSingle$betaTract, dtTractSingle$singleShare))
m_beta <- lm(singleShare ~ betaTract, data = dtTractSingle)
print(summary(m_beta))

### =========================
### TRACT-LEVEL PLEX SHARE (2024–2025)
### =========================

dtPlexTract <- dtMerge[
  permitYear %in% c(2024, 2025) & use %chin% c("duplex","multi"),
  .(
    plexShare = mean(use == "multi"),   # share multi vs duplex
    N = .N,
    meanPPSF_t = mean(meanPPSF),
    medianIncome_t = mean(medianIncome, na.rm = TRUE)
  ),
  by = tract
]

# merge in betaTract
dtPlexTract <- merge(
  dtPlexTract,
  betaTract_hat_NEW,
  by = "tract",
  all.x = TRUE
)

# keep tracts with enough activity to be meaningful
dtPlexTract <- dtPlexTract[N >= 5]
summary(dtPlexTract)
m_beta <- lm(
  plexShare ~ medianIncome_t + meanPPSF_t + betaTract,
  data = dtPlexTract,
  weights = N
)
print(summary(m_beta))

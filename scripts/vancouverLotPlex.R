# vancouverLotPlex.R
# 33 vs 50 price gap, duplex takeup, and spatial averaging
# Tom Davidoff
# 05/05/26
#
# Spec:
#   - FSR 0.5-0.8, age 1-39, year 2014-2018
#   - 3rd-order polynomial in logFSR
#   - Universal sample dtSalesRegSubset for all hedonics
#   - Permits restricted to 33' lots (the duplex-vs-SFH margin)
#   - Permit-to-BCA join is a real point-in-polygon (st_intersects on polygons)

library(DBI)
library(RSQLite)
library(data.table)
library(fixest)
library(ggplot2)
library(sf)

# ============================================================================
# ---- Permits ---------------------------------------------------------------
# ============================================================================
permitFile <- "~/DropboxExternal/dataRaw/issued-building-permits.csv"
dtPermit <- fread(permitFile, select = c("PermitNumber","ProjectValue","TypeOfWork",
                                         "PropertyUse","SpecificUseCategory",
                                         "GeoLocalArea","geo_point_2d","YearMonth"))

dtPermit <- dtPermit[PropertyUse == "Dwelling Uses" &
                     TypeOfWork %in% c("New Building","Addition / Alteration")]
CRITQUANT <- .25
qCrit <- quantile(dtPermit[TypeOfWork == "New Building" &
                           SpecificUseCategory %in% c("Duplex","Single Detached House") &
                           ProjectValue > 0, ProjectValue], CRITQUANT)
dtPermit <- dtPermit[TypeOfWork == "New Building" | ProjectValue >= qCrit]
dtPermit <- dtPermit[grepl("Duplex|Single Detached House|Laneway House|Multiple Dwelling",
                           SpecificUseCategory)]

dtPermit[, useCat := tstrsplit(SpecificUseCategory, " ")[[1]]]
dtPermit[, useCat := tstrsplit(useCat, ",")[[1]]]
dtPermit[, year := as.numeric(substring(YearMonth, 1, 4))]
print(table(dtPermit[, useCat]))

dtPermit[, c("permitLat","permitLon") := tstrsplit(geo_point_2d, ", ")]
dtPermit[, `:=`(permitLat = as.numeric(permitLat),
                permitLon = as.numeric(permitLon))]
dtPermit <- dtPermit[!is.na(permitLat) & !is.na(permitLon)]
dtPermit <- dtPermit[, .(PermitNumber, useCat, year, permitLat, permitLon,TypeOfWork,ProjectValue)]

# ============================================================================
# ---- Spatial filters: R1-1 zoning, census tract, BCA parcel ---------------
# ============================================================================

dZ <- st_read("~/DropboxExternal/dataRaw/vancouver_zoning.geojson")
dZ <- dZ[dZ$zoning_district == "R1-1", "zoning_district"]

stShp <- st_read("~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp")
stShp <- st_make_valid(stShp)
stShp <- stShp[, "CTNAME"]

# BCA parcels stay as polygons until after the permit join
sfBCA <- st_as_sf(readRDS("~/DropboxExternal/dataProcessed/bca_vancouver_residential.rds"))
sfBCA <- st_transform(sfBCA, 3005)
sfBCA <- st_make_valid(sfBCA)
dZ    <- st_transform(dZ,    3005)
stShp <- st_transform(stShp, 3005)

# Add CTNAME to BCA
sfBCA <- st_join(sfBCA, stShp, join = st_within)

# Width/depth fallback from inventory if folioDescription missing
sfBCA$landWidth <- as.numeric(sfBCA$landWidth)
sfBCA$landDepth <- as.numeric(sfBCA$landDepth)
if ("land_width" %in% names(sfBCA))
  sfBCA$landWidth[is.na(sfBCA$landWidth)] <- as.numeric(sfBCA$land_width[is.na(sfBCA$landWidth)])
if ("land_depth" %in% names(sfBCA))
  sfBCA$landDepth[is.na(sfBCA$landDepth)] <- as.numeric(sfBCA$land_depth[is.na(sfBCA$landDepth)])

# ---- Join permits to R1-1, tract, and BCA parcel (real point-in-polygon) ---
sfPermit <- st_as_sf(dtPermit, coords = c("permitLon","permitLat"), crs = 4326)
sfPermit <- st_transform(sfPermit, 3005)
sfPermit <- st_join(sfPermit, dZ,    join = st_intersects)
sfPermit <- sfPermit[!is.na(sfPermit$zoning_district), ]
sfPermit <- st_join(sfPermit, stShp, join = st_within)

# Real point-in-polygon join to BCA parcels.
# st_intersects is equivalent to st_within for points-in-polygons.
sfPermit <- st_join(sfPermit,
                    sfBCA[, c("folioID","landWidth","landDepth",
                              "MB_effective_year","neighbourhoodDescription")],
                    join = st_intersects)

matchRate <- mean(!is.na(sfPermit$folioID))
cat("\nPermits joined to a BCA parcel:", sum(!is.na(sfPermit$folioID)),
    "/", nrow(sfPermit), sprintf(" (%.1f%%)\n", 100 * matchRate))

# Drop permits with no parcel match
sfPermit <- sfPermit[!is.na(sfPermit$folioID), ]

# Permit metric coords + drop geometry
permitCoords <- st_coordinates(st_geometry(sfPermit))
dtPermit2 <- as.data.table(st_drop_geometry(sfPermit))
dtPermit2[, `:=`(permitXM = permitCoords[,1], permitYM = permitCoords[,2])]
dtPermit2[, landWidth := as.numeric(landWidth)]
dtPermit2[, roundWidth := round(landWidth)]
dtPermit2[, MB_effective_year := as.numeric(MB_effective_year)]
dtPermit2[, priorAge := year - MB_effective_year]

cat("Permit table after BCA join: nrow =", nrow(dtPermit2),
    " | with roundWidth =", sum(!is.na(dtPermit2$roundWidth)), "\n")

# ---- Diagnostic: prior-structure age by useCat ----------------------------
cat("\nPrior structure age by useCat (33' lots, 2018-2023):\n")
print(dtPermit2[roundWidth == 33 & useCat %in% c("Single","Duplex") &
                year >= 2018 & year < 2024,
                .(.N,
                  medPriorAge   = median(priorAge, na.rm = TRUE),
                  meanPriorAge  = mean(priorAge,   na.rm = TRUE),
                  shareYoung    = mean(priorAge < 30, na.rm = TRUE),
                  shareVeryYng  = mean(priorAge < 15, na.rm = TRUE)),
                by = useCat])

# Histogram of prior MB_effective_year by useCat
pPriorYear <- ggplot(dtPermit2[roundWidth == 33 & useCat %in% c("Single","Duplex","Laneway") &
                               year >= 2018 & year < 2024 &
                               MB_effective_year >= 1900 & MB_effective_year <= 2023],
                     aes(x = MB_effective_year, fill = useCat)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.5) +
  labs(x = "MB_effective_year of prior structure",
       y = "Count of permits",
       title = "Prior-structure vintage by permit type, 33' lots, 2018-2023") +
  theme_minimal()
ggsave("text/priorYear_by_useCat.png", pPriorYear, w = 7, h = 4)
# print table of same image
cat("\nPrior structure vintage by useCat (33' lots, 2018-2023):\n")
print(dtPermit2[roundWidth == 33 & useCat %in% c("Single","Duplex","Laneway") &
		year >= 2018 & year < 2024 &
		MB_effective_year >= 1900 & MB_effective_year <= 2023,
		.(.N,
		  medMBYear   = median(MB_effective_year, na.rm = TRUE),
		  meanMBYear  = mean(MB_effective_year,   na.rm = TRUE),
		  sharePre1940= mean(MB_effective_year < 1940, na.rm = TRUE)),
		by = useCat])
for (y in 1960:2020) {
	print(y)
	print(table(dtPermit2[roundWidth == 33 & useCat %in% c("Single","Duplex","Laneway") & year >= 2018 & year < 2024 & MB_effective_year == y, .(useCat,TypeOfWork)]))
	print(dtPermit2[roundWidth == 33 & useCat %in% c("Single","Duplex","Laneway") & year >= 2018 & year < 2024 & MB_effective_year == y, median(ProjectValue),by=c("useCat","TypeOfWork")])
}

# Same in priorAge space (probably more useful for inspection)
pPriorAge <- ggplot(dtPermit2[roundWidth == 33 & useCat %in% c("Single","Duplex","Laneway") &
                              year >= 2018 & year < 2024 &
                              priorAge >= 0 & priorAge <= 120],
                    aes(x = priorAge, fill = useCat)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.5) +
  labs(x = "Age of prior structure at permit",
       y = "Count of permits",
       title = "Prior-structure age by permit type, 33' lots, 2018-2023") +
  theme_minimal()
ggsave("text/priorAge_by_useCat.png", pPriorAge, w = 7, h = 4)

# Now build the slim dtBCA (data.table with parcel centroid) for the sales merge
parcelCoords <- st_coordinates(st_centroid(st_geometry(sfBCA)))
dtBCA <- as.data.table(st_drop_geometry(sfBCA))
dtBCA[, `:=`(parcelXM = parcelCoords[,1], parcelYM = parcelCoords[,2])]
dtBCA <- dtBCA[, .(folioID,
                   parcelXM, parcelYM,
                   landWidth, landDepth,
                   MB_total_finished_area, MB_effective_year,
                   actualUseDescription, neighbourhoodDescription,
                   CTNAME)]

# Final permit table: 33' lots only (duplex-vs-SFH margin)
dtPermitClean <- dtPermit2[roundWidth == 33,
                           .(PermitNumber, useCat, year,
                             permitXM, permitYM,
                             CTNAME, neighbourhoodDescription, roundWidth,
                             MB_effective_year, priorAge, folioID)]
cat("\nPermits on 33' lots after spatial filtering:", nrow(dtPermitClean), "\n")
print(table(dtPermitClean[, useCat]))

# ============================================================================
# ---- Sales -----------------------------------------------------------------
# ============================================================================
con <- dbConnect(SQLite(), "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3")
dtSalesRaw <- dbGetQuery(con, "SELECT folioID, conveyanceDate, conveyancePrice,
                               conveyanceTypeDescription FROM sales") |> setDT()
dbDisconnect(con)

dtSalesRaw <- dtSalesRaw[conveyanceTypeDescription == "Improved Single Property Transaction"]
dtSalesRaw <- dtSalesRaw[, .(folioID, conveyanceDate, conveyancePrice)]

dtSales <- merge(dtSalesRaw, dtBCA, by = "folioID")

dtSales[, year      := as.numeric(substring(conveyanceDate, 1, 4))]
dtSales[, logPrice  := log(as.numeric(conveyancePrice))]
dtSales[, roundWidth:= round(landWidth)]
dtSales[, w50       := as.integer(roundWidth == 50)]
dtSales[, w33       := as.integer(roundWidth == 33)]
dtSales[, age       := year - as.numeric(MB_effective_year)]
dtSales[, FSR       := as.numeric(MB_total_finished_area) / (landWidth * landDepth)]
dtSales[, logFSR    := log(FSR)]
dtSales[, logFSR2   := logFSR^2]
dtSales[, logFSR3   := logFSR^3]

dtSales <- dtSales[, .(folioID, year, logPrice,
                       w33, w50, roundWidth, age,
                       MB_effective_year,
                       FSR, logFSR, logFSR2, logFSR3,
                       actualUseDescription, neighbourhoodDescription, CTNAME,
                       parcelXM, parcelYM)]

# ============================================================================
# ---- Universal regression subset ------------------------------------------
# ============================================================================
CRITAGE <- 40
FSR_LO  <- 0.50
FSR_HI  <- 0.80
YR_LO   <- 2013
YR_HI   <- 2019

dtSalesRegSubset <- dtSales[(w33 == 1 | w50 == 1) &
                            year > YR_LO & year < YR_HI &
                            age > 0 & age < CRITAGE &
                            actualUseDescription == "Single Family Dwelling" &
                            !is.na(FSR) & FSR > FSR_LO & FSR < FSR_HI &
                            !is.na(logPrice) & is.finite(logPrice) &
                            !is.na(parcelXM) & !is.na(parcelYM)]

cat("\nUniversal sample:", nrow(dtSalesRegSubset),
    " | w33:", sum(dtSalesRegSubset$w33),
    " | w50:", sum(dtSalesRegSubset$w50),
    " | tracts:", uniqueN(dtSalesRegSubset$CTNAME),
    " | nbhds:", uniqueN(dtSalesRegSubset$neighbourhoodDescription), "\n\n")

FSRctrl <- "logFSR + logFSR2 + logFSR3"

# ============================================================================
# ---- Neighbourhood-level hedonic + plex ratio -----------------------------
# ============================================================================
hedNbhd <- feols(as.formula(paste0("logPrice ~ ", FSRctrl,
                                   " + i(neighbourhoodDescription, w50)",
                                   " + i(neighbourhoodDescription) |",
                                   " MB_effective_year + year")),
                 data = dtSalesRegSubset)

cfN <- coef(hedNbhd)
intN <- grep(":w50$", names(cfN), value = TRUE)
dtHedonicNbhd <- data.table(
  neighbourhoodDescription = gsub("^neighbourhoodDescription::|:w50$", "", intN),
  w50Interact              = cfN[intN]
)

simpleNbhd <- feols(as.formula(paste0("logPrice ~ ", FSRctrl,
                                      " + i(neighbourhoodDescription) |",
                                      " MB_effective_year + year")),
                    data = dtSalesRegSubset[w33 == 1])
cfSN <- coef(simpleNbhd)
feSN <- grep("^neighbourhoodDescription::", names(cfSN), value = TRUE)
dtNbhdFE <- data.table(
  neighbourhoodDescription = gsub("^neighbourhoodDescription::", "", feSN),
  nbhdFE                   = cfSN[feSN]
)
dtHedonicNbhd <- merge(dtHedonicNbhd, dtNbhdFE, by = "neighbourhoodDescription")

dtRatioNbhd <- dtPermitClean[year >= 2018 & year < 2024 & !is.na(neighbourhoodDescription),
                             .(nDuplex  = sum(useCat == "Duplex"),
                               nSingle  = sum(useCat == "Single"),
                               nLaneway = sum(useCat == "Laneway")),
                             by = neighbourhoodDescription]
dtRatioNbhd[, plexRatio := (nDuplex + nLaneway) / pmax(nSingle + nDuplex, 1)]

dtNbhdCompare <- merge(dtHedonicNbhd, dtRatioNbhd, by = "neighbourhoodDescription")
print(dtNbhdCompare)
cat("\nNbhd-level correlations:\n")
print(cor(dtNbhdCompare[, .(w50Interact, plexRatio, nbhdFE)], use = "complete.obs"))

rNbhd1 <- feols(plexRatio ~ nbhdFE,                 data = dtNbhdCompare)
rNbhd2 <- feols(plexRatio ~ w50Interact,            data = dtNbhdCompare)
rNbhd3 <- feols(plexRatio ~ w50Interact + nbhdFE,   data = dtNbhdCompare)
print(etable(rNbhd1, rNbhd2, rNbhd3, digits = 3))

ggplot(dtNbhdCompare, aes(x = nbhdFE, y = w50Interact, color = plexRatio)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(x = "Nbhd FE (w33-only)", y = "w50 interaction (hedonic)",
       title = "Hedonic w50 interaction vs nbhd FE") + theme_minimal()
ggsave("text/FEvw50.png", w = 6, h = 4)

# ============================================================================
# ---- Tract-level hedonic + tract FE (w33-only) + plex ratio ---------------
# ============================================================================
hedTract <- feols(as.formula(paste0("logPrice ~ ", FSRctrl,
                                    " + i(CTNAME, w50) + i(CTNAME) |",
                                    " MB_effective_year + year")),
                  data = dtSalesRegSubset)

cfT <- coef(hedTract)
intT <- grep(":w50$", names(cfT), value = TRUE)
dtHedonicTract <- data.table(
  CTNAME      = gsub("^CTNAME::|:w50$", "", intT),
  w50Interact = cfT[intT]
)

simpleTract <- feols(as.formula(paste0("logPrice ~ ", FSRctrl,
                                       " + i(CTNAME) |",
                                       " MB_effective_year + year")),
                     data = dtSalesRegSubset[w33 == 1])
cfST <- coef(simpleTract)
feST <- grep("^CTNAME::", names(cfST), value = TRUE)
dtTractFE <- data.table(
  CTNAME  = gsub("^CTNAME::", "", feST),
  tractFE = cfST[feST]
)

dtHedonicTract <- merge(dtHedonicTract, dtTractFE, by = "CTNAME")

dtRatioTract <- dtPermitClean[year >= 2018 & year < 2024 & !is.na(CTNAME),
                              .(nDuplex  = sum(useCat == "Duplex"),
                                nSingle  = sum(useCat == "Single"),
                                nLaneway = sum(useCat == "Laneway")),
                              by = CTNAME]
dtRatioTract[, plexRatio := (nDuplex + nLaneway) / pmax(nSingle + nDuplex, 1)]

dtTractCompare <- merge(dtHedonicTract, dtRatioTract, by = "CTNAME")
print(etable(hedTract, simpleTract))
print(dtTractCompare)
cat("\nTract-level correlations:\n")
print(cor(dtTractCompare[, .(w50Interact, plexRatio, tractFE)], use = "complete.obs"))


# ============================================================================
# ---- Spatial averaging: local w50 premium at permit locations -------------
# ============================================================================
gaussKernel <- function(d, h) exp(-0.5 * (d / h)^2)

localFit <- function(xE, yE, h, sales, minN = 40, minPerGroup = 5,
                     refLogFSR = log(0.65), refAge = 20) {
  d <- sqrt((sales$parcelXM - xE)^2 + (sales$parcelYM - yE)^2)
  w <- gaussKernel(d, h)
  keep <- d < 3 * h & w > 1e-6
  if (sum(keep) < minN ||
      sum(sales$w50[keep]) < minPerGroup ||
      sum(sales$w33[keep]) < minPerGroup) {
    return(list(localPremium = NA_real_, localPPSF = NA_real_,
                nSales = sum(keep)))
  }
  sub <- sales[keep]
  sub[, .w := w[keep]]
  fit <- tryCatch(
    feols(logPrice ~ logFSR + logFSR2 + logFSR3 + w50 + age | year,
          data = sub, weights = ~.w),
    error = function(e) NULL
  )
  if (is.null(fit)) return(list(localPremium = NA_real_, localPPSF = NA_real_,
                                nSales = sum(keep)))
  cf <- coef(fit)
  yrFE <- fixef(fit)$year

  meanYrFE <- mean(yrFE, na.rm = TRUE)
  localPPSF <- cf["logFSR"]  * refLogFSR     +
               cf["logFSR2"] * refLogFSR^2   +
               cf["logFSR3"] * refLogFSR^3   +
               cf["age"]     * refAge        +
               meanYrFE

  list(localPremium = unname(cf["w50"]),
       localPPSF    = unname(localPPSF),
       nSales       = sum(keep))
}

cvBandwidth <- function(h, sales, nFolds = 10) {
  set.seed(42)
  folds <- sample(rep(1:nFolds, length.out = nrow(sales)))
  yhat <- rep(NA_real_, nrow(sales))
  for (k in 1:nFolds) {
    train <- sales[folds != k]
    for (i in which(folds == k)) {
      d <- sqrt((train$parcelXM - sales$parcelXM[i])^2 +
                (train$parcelYM - sales$parcelYM[i])^2)
      w <- gaussKernel(d, h)
      keep <- d < 3 * h & w > 1e-6
      if (sum(keep) < 30) next
      sub <- train[keep]
      sub[, .w := w[keep]]
      fit <- tryCatch(
        feols(logPrice ~ logFSR + logFSR2 + logFSR3 + w50 + age | year,
              data = sub, weights = ~.w),
        error = function(e) NULL
      )
      if (is.null(fit)) next
      cf <- coef(fit)
      yrFE <- fixef(fit)$year
      yr_i <- as.character(sales$year[i])
      yrEff <- if (yr_i %in% names(yrFE)) yrFE[yr_i] else mean(yrFE)
      yhat[i] <- cf["logFSR"]  * sales$logFSR[i]  +
                 cf["logFSR2"] * sales$logFSR2[i] +
                 cf["logFSR3"] * sales$logFSR3[i] +
                 cf["w50"]     * sales$w50[i]     +
                 cf["age"]     * sales$age[i]     +
                 yrEff
    }
  }
  ok <- !is.na(yhat)
  list(h = h, rmse = sqrt(mean((sales$logPrice[ok] - yhat[ok])^2)),
       nPred = sum(ok))
}

# Permits to score (already 33'-only by construction)
dtPermitLocal <- dtPermitClean[useCat %in% c("Single","Duplex") &
                               year >= 2018 & year < 2024]
dtPermitLocal[, isDuplex := as.integer(useCat == "Duplex")]
cat("\nPermits for choice model (33' lots, single vs duplex):", nrow(dtPermitLocal),
    " | duplex share:", round(mean(dtPermitLocal$isDuplex), 3), "\n")

bandwidthGrid <- c(750, 1000, 1250, 1500, 1750, 2000)
cat("\nBandwidth CV:\n")
cvResults <- rbindlist(lapply(bandwidthGrid, function(h) {
  cat("  h =", h, "m... "); t0 <- Sys.time()
  r <- cvBandwidth(h, dtSalesRegSubset)
  cat("RMSE =", round(r$rmse, 4), " | n =", r$nPred,
      " | t =", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")
  as.data.table(r)
}))
hOptCV <- cvResults[which.min(rmse), h]
cat("CV-optimal h:", hOptCV, "m\n")

computePermitFeatures <- function(h, permits, sales) {
  feats <- rbindlist(lapply(seq_len(nrow(permits)), function(i) {
    r <- localFit(permits$permitXM[i], permits$permitYM[i], h, sales)
    data.table(localPremium = r$localPremium, localPPSF = r$localPPSF,
               nSales = r$nSales)
  }))
  cbind(permits[, .(isDuplex, useCat, CTNAME, neighbourhoodDescription,
                    permitXM, permitYM, year, MB_effective_year, priorAge)],
        feats)
}

cat("\nComputing permit features at each bandwidth...\n")
permitFeatures <- lapply(bandwidthGrid, function(h) {
  cat("  h =", h, "m... "); t0 <- Sys.time()
  dt <- computePermitFeatures(h, dtPermitLocal, dtSalesRegSubset)
  cat("n with features:", sum(!is.na(dt$localPremium)),
      " | t =", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")
  dt
})
names(permitFeatures) <- paste0("h", bandwidthGrid)

diagBW <- rbindlist(lapply(seq_along(bandwidthGrid), function(j) {
  h <- bandwidthGrid[j]
  dt <- permitFeatures[[j]][!is.na(localPremium) & !is.na(localPPSF)]
  if (nrow(dt) < 20) return(data.table(h = h))
  probit <- tryCatch(
    glm(isDuplex ~ localPremium + localPPSF, data = dt, family = binomial("probit")),
    error = function(e) NULL
  )
  if (is.null(probit)) {
    premCoef <- premSE <- ppsfCoef <- ppsfSE <- NA
  } else {
    cf <- coef(probit)
    vc <- sqrt(diag(vcov(probit)))
    premCoef <- cf["localPremium"]; premSE <- vc["localPremium"]
    ppsfCoef <- cf["localPPSF"];    ppsfSE <- vc["localPPSF"]
  }
  dtTr <- dt[, .(plexRate = mean(isDuplex),
                 meanPrem = mean(localPremium),
                 meanPPSF = mean(localPPSF),
                 n = .N), by = CTNAME][n >= 5]
  corTract <- if (nrow(dtTr) > 5) cor(dtTr$plexRate, dtTr$meanPrem) else NA
  data.table(h = h, nPermits = nrow(dt),
             sdPremium = sd(dt$localPremium),
             sdPPSF    = sd(dt$localPPSF),
             probitPrem = premCoef, probitPremSE = premSE,
             probitPPSF = ppsfCoef, probitPPSFSE = ppsfSE,
             tractCor = corTract)
}))
diagAll <- merge(cvResults, diagBW, by = "h")
print(diagAll)

# ---- Plots ----------------------------------------------------------------
pCV    <- ggplot(cvResults, aes(h, rmse)) + geom_line() + geom_point(size = 3) +
          scale_x_log10() +
          labs(x = "Bandwidth (m, log)", y = "10-fold CV RMSE",
               title = "Diagnostic 1: prediction error") + theme_minimal()
pDisp  <- ggplot(diagBW, aes(h, sdPremium)) + geom_line() + geom_point(size = 3) +
          scale_x_log10() +
          labs(x = "Bandwidth (m, log)", y = "SD of localPremium",
               title = "Diagnostic 2: spatial dispersion") + theme_minimal()
pStab  <- ggplot(diagBW, aes(h)) +
          geom_pointrange(aes(y = probitPrem,
                              ymin = probitPrem - 1.96 * probitPremSE,
                              ymax = probitPrem + 1.96 * probitPremSE),
                          color = "steelblue") +
          geom_hline(yintercept = 0, linetype = "dashed") +
          scale_x_log10() +
          labs(x = "Bandwidth (m, log)", y = "Probit coef on localPremium",
               title = "Diagnostic 3: premium coefficient stability") + theme_minimal()
pStabPPSF <- ggplot(diagBW, aes(h)) +
          geom_pointrange(aes(y = probitPPSF,
                              ymin = probitPPSF - 1.96 * probitPPSFSE,
                              ymax = probitPPSF + 1.96 * probitPPSFSE),
                          color = "darkred") +
          geom_hline(yintercept = 0, linetype = "dashed") +
          scale_x_log10() +
          labs(x = "Bandwidth (m, log)", y = "Probit coef on localPPSF",
               title = "Diagnostic 3b: PPSF coefficient stability") + theme_minimal()
pTrCor <- ggplot(diagBW, aes(h, tractCor)) + geom_line() + geom_point(size = 3) +
          geom_hline(yintercept = 0, linetype = "dashed") +
          scale_x_log10() +
          labs(x = "Bandwidth (m, log)", y = "Tract cor(plexRate, meanPremium)",
               title = "Diagnostic 4: tract-aggregate signal") + theme_minimal()
ggsave("text/bandwidth_cv.png",          pCV,       w = 6, h = 4)
ggsave("text/bandwidth_dispersion.png",  pDisp,     w = 6, h = 4)
ggsave("text/bandwidth_probit.png",      pStab,     w = 6, h = 4)
ggsave("text/bandwidth_probit_ppsf.png", pStabPPSF, w = 6, h = 4)
ggsave("text/bandwidth_tractcor.png",    pTrCor,    w = 6, h = 4)

# ============================================================================
# ---- Headline choice models at h = 1500m -----------------------------------
# ============================================================================
hHeadline <- 1500
dtBest <- permitFeatures[[paste0("h", hHeadline)]][!is.na(localPremium) & !is.na(localPPSF)]
cat("\n--- Permit-level choice models at h =", hHeadline, "m ---\n")
cat("\nCorrelations:\n")
print(round(cor(dtBest[, .(localPremium, localPPSF, isDuplex)]), 3))

probit <- glm(isDuplex ~ localPremium + localPPSF, data = dtBest, family = binomial("probit"))
logit  <- glm(isDuplex ~ localPremium + localPPSF, data = dtBest, family = binomial("logit"))
lpm    <- feols(isDuplex ~ localPremium + localPPSF, data = dtBest)
cat("\nProbit:\n"); print(summary(probit)$coefficients)
cat("\nLogit:\n");  print(summary(logit)$coefficients)
cat("\nLPM:\n");    print(etable(lpm))

# Univariate permit-level specs
simplePermit_ppsf <- feols(isDuplex ~ localPPSF,    data = dtBest)
simplePermit_prem <- feols(isDuplex ~ localPremium, data = dtBest)
cat("\nSimple permit-level LPMs (one regressor at a time):\n")
print(etable(simplePermit_ppsf, simplePermit_prem,
             headers = c("PPSF only","Premium only"), digits = 3))

dtTractAgg <- dtBest[, .(plexRate = mean(isDuplex),
                         meanPremium = mean(localPremium),
                         meanPPSF = mean(localPPSF),
                         n = .N), by = CTNAME][n >= 5]
cat("\nTract-aggregate at h =", hHeadline, "m:\n")
print(cor(dtTractAgg[, .(plexRate, meanPremium, meanPPSF)]))
print(feols(plexRate ~ meanPremium + meanPPSF, data = dtTractAgg, weights = ~n))

# ============================================================================
# ---- Tract-aggregate comparison: 750m vs 2000m, three specs ---------------
# ============================================================================
makeTractAgg <- function(h) {
  dt <- permitFeatures[[paste0("h", h)]][!is.na(localPremium) & !is.na(localPPSF)]
  dt[, .(plexRate    = mean(isDuplex),
         meanPremium = mean(localPremium),
         meanPPSF    = mean(localPPSF),
         n           = .N), by = CTNAME][n >= 5]
}

hsToShow <- c(750, 2000)

specList <- list()
for (h in hsToShow) {
  dtAgg <- makeTractAgg(h)
  specList[[paste0("h", h, "_ppsfOnly")]] <-
    feols(plexRate ~ meanPPSF,                data = dtAgg, weights = ~n)
  specList[[paste0("h", h, "_premOnly")]] <-
    feols(plexRate ~ meanPremium,             data = dtAgg, weights = ~n)
  specList[[paste0("h", h, "_both")]]    <-
    feols(plexRate ~ meanPremium + meanPPSF,  data = dtAgg, weights = ~n)
}

cat("\n=== Tract-aggregate plexRate regressions ===\n")
print(etable(specList,
             headers = c("750: PPSF","750: Prem","750: Both",
                         "2000: PPSF","2000: Prem","2000: Both"),
             digits = 3))

cat("\nCorrelations at each bandwidth:\n")
for (h in hsToShow) {
  cat("  h =", h, "m:\n")
  print(round(cor(makeTractAgg(h)[, .(plexRate, meanPremium, meanPPSF)]), 3))
}

# ============================================================================
# ---- Spatial heatmaps at headline bandwidth -------------------------------
# ============================================================================
dtMap <- copy(dtBest)
dtMap[, `:=`(xKm = permitXM / 1000, yKm = permitYM / 1000)]

pMapPPSF <- ggplot(dtMap, aes(x = xKm, y = yKm, color = localPPSF)) +
  geom_point(size = 1.2, alpha = 0.7) +
  scale_color_viridis_c(option = "plasma", name = "log P_ref") +
  coord_equal() +
  labs(x = "Easting (km)", y = "Northing (km)",
       title = paste0("Local log price at reference house, h = ", hHeadline, "m"),
       subtitle = "Each point is a 33' lot permit, 2018-2023") +
  theme_minimal()

pMapPrem <- ggplot(dtMap, aes(x = xKm, y = yKm, color = localPremium)) +
  geom_point(size = 1.2, alpha = 0.7) +
  scale_color_gradient2(low = "blue", mid = "grey90", high = "red",
                        midpoint = median(dtMap$localPremium, na.rm = TRUE),
                        name = "w50 premium\n(log points)") +
  coord_equal() +
  labs(x = "Easting (km)", y = "Northing (km)",
       title = paste0("Local 50' vs 33' premium, h = ", hHeadline, "m"),
       subtitle = "Each point is a 33' lot permit, 2018-2023") +
  theme_minimal()

dtMap[, choice := factor(ifelse(isDuplex == 1, "Duplex", "Single"),
                         levels = c("Single","Duplex"))]
pMapChoice <- ggplot(dtMap, aes(x = xKm, y = yKm, color = choice)) +
  geom_point(size = 1.2, alpha = 0.7) +
  scale_color_manual(values = c("Single" = "grey70", "Duplex" = "red"),
                     name = "Permit type") +
  coord_equal() +
  labs(x = "Easting (km)", y = "Northing (km)",
       title = "Duplex vs single permits, 33' lots, 2018-2023",
       subtitle = paste0("n = ", nrow(dtMap), " | duplex share = ",
                         round(mean(dtMap$isDuplex), 3))) +
  theme_minimal()

ggsave("text/heatmap_ppsf_h1500.png",    pMapPPSF,   w = 7, h = 6)
ggsave("text/heatmap_premium_h1500.png", pMapPrem,   w = 7, h = 6)
ggsave("text/heatmap_choice.png",        pMapChoice, w = 7, h = 6)

# ============================================================================
# ---- Save ------------------------------------------------------------------
# ============================================================================
saveRDS(list(cvResults = cvResults, diagBW = diagBW, diagAll = diagAll,
             permitFeatures = permitFeatures, hOptCV = hOptCV, hHeadline = hHeadline,
             dtTractAgg = dtTractAgg, specList = specList,
             dtNbhdCompare = dtNbhdCompare, dtTractCompare = dtTractCompare),
        "~/DropboxExternal/dataProcessed/vancouverLotPlex_results.rds")

cat("\nDone. h_CV =", hOptCV, "m | h_headline =", hHeadline, "m\n")
q("no")

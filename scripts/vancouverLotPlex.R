# vancouverLotPlex.R
# 33 vs 50 price gap, duplex takeup, and spatial averaging
# Tom Davidoff
# 05/13/26
#
# Spec:
#   - FSR 0.5-0.8, age 1-39, year 2014-2018 for the duplex-margin hedonic sample
#   - 3rd-order polynomial in logFSR
#   - Universal sample dtSalesRegSubset for all hedonics
#   - localElasticity from log(landWidth) regression on new builds (eff yr 2000-2018),
#     depth IQR-trimmed, evaluated at permit locations
#   - Permits NOT pre-filtered by width; choice models take a `widths` arg
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
dtPermit <- dtPermit[, .(PermitNumber, useCat, year, permitLat, permitLon,
                         TypeOfWork, ProjectValue)]

# ============================================================================
# ---- Spatial filters: R1-1 zoning, census tract, BCA parcel ---------------
# ============================================================================
dZ <- st_read("~/DropboxExternal/dataRaw/vancouver_zoning.geojson")
dZ <- dZ[dZ$zoning_district == "R1-1", "zoning_district"]

stShp <- st_read("~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp")
stShp <- st_make_valid(stShp)
stShp <- stShp[, "CTNAME"]

sfBCA <- st_as_sf(readRDS("~/DropboxExternal/dataProcessed/bca_vancouver_residential.rds"))
sfBCA <- st_transform(sfBCA, 3005)
sfBCA <- st_make_valid(sfBCA)
dZ    <- st_transform(dZ,    3005)
stShp <- st_transform(stShp, 3005)

sfBCA <- st_join(sfBCA, stShp, join = st_within)

sfBCA$landWidth <- as.numeric(sfBCA$landWidth)
sfBCA$landDepth <- as.numeric(sfBCA$landDepth)
if ("land_width" %in% names(sfBCA))
  sfBCA$landWidth[is.na(sfBCA$landWidth)] <- as.numeric(sfBCA$land_width[is.na(sfBCA$landWidth)])
if ("land_depth" %in% names(sfBCA))
  sfBCA$landDepth[is.na(sfBCA$landDepth)] <- as.numeric(sfBCA$land_depth[is.na(sfBCA$landDepth)])

# Join permits to R1-1, tract, and BCA parcel
sfPermit <- st_as_sf(dtPermit, coords = c("permitLon","permitLat"), crs = 4326)
sfPermit <- st_transform(sfPermit, 3005)
sfPermit <- st_join(sfPermit, dZ,    join = st_intersects)
sfPermit <- sfPermit[!is.na(sfPermit$zoning_district), ]
sfPermit <- st_join(sfPermit, stShp, join = st_within)
sfPermit <- st_join(sfPermit,
                    sfBCA[, c("folioID","landWidth","landDepth",
                              "MB_effective_year","neighbourhoodDescription")],
                    join = st_intersects)

matchRate <- mean(!is.na(sfPermit$folioID))
cat("\nPermits joined to a BCA parcel:", sum(!is.na(sfPermit$folioID)),
    "/", nrow(sfPermit), sprintf(" (%.1f%%)\n", 100 * matchRate))

sfPermit <- sfPermit[!is.na(sfPermit$folioID), ]

permitCoords <- st_coordinates(st_geometry(sfPermit))
dtPermit2 <- as.data.table(st_drop_geometry(sfPermit))
dtPermit2[, `:=`(permitXM = permitCoords[,1], permitYM = permitCoords[,2])]
dtPermit2[, landWidth := as.numeric(landWidth)]
dtPermit2[, landDepth := as.numeric(landDepth)]
dtPermit2[, roundWidth := round(landWidth)]
dtPermit2[, MB_effective_year := as.numeric(MB_effective_year)]
dtPermit2[, priorAge := year - MB_effective_year]

cat("Permit table after BCA join: nrow =", nrow(dtPermit2),
    " | with roundWidth =", sum(!is.na(dtPermit2$roundWidth)), "\n")

# ---- Diagnostic: prior-structure age by useCat (33' lots, 2018-2023) ------
cat("\nPrior structure age by useCat (33' lots, 2018-2023):\n")
print(dtPermit2[roundWidth == 33 & useCat %in% c("Single","Duplex") &
                year >= 2018 & year < 2024,
                .(.N,
                  medPriorAge   = median(priorAge, na.rm = TRUE),
                  meanPriorAge  = mean(priorAge,   na.rm = TRUE),
                  shareYoung    = mean(priorAge < 30, na.rm = TRUE),
                  shareVeryYng  = mean(priorAge < 15, na.rm = TRUE)),
                by = useCat])

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

# BCA parcel centroids for the sales merge
parcelCoords <- st_coordinates(st_centroid(st_geometry(sfBCA)))
dtBCA <- as.data.table(st_drop_geometry(sfBCA))
dtBCA[, `:=`(parcelXM = parcelCoords[,1], parcelYM = parcelCoords[,2])]
dtBCA <- dtBCA[, .(folioID,
                   parcelXM, parcelYM,
                   landWidth, landDepth,
                   MB_total_finished_area, MB_effective_year,
                   actualUseDescription, neighbourhoodDescription,
                   CTNAME)]

# All permits (no width restriction) -- subset at point of use
dtPermitAll <- dtPermit2[, .(PermitNumber, useCat, year,
                             permitXM, permitYM,
                             CTNAME, neighbourhoodDescription,
                             roundWidth, landWidth, landDepth,
                             MB_effective_year, priorAge, folioID)]
cat("\nAll permits after spatial filtering:", nrow(dtPermitAll), "\n")
print(table(dtPermitAll[roundWidth %in% c(33, 50), .(useCat, roundWidth)]))

# ============================================================================
# ---- Sales -----------------------------------------------------------------
# ============================================================================
con <- dbConnect(SQLite(), "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3")
dtSalesRaw <- dbGetQuery(con, "SELECT folioID, conveyanceDate, conveyancePrice,
                               conveyanceTypeDescription FROM sales") |> setDT()
dbDisconnect(con)

dtSalesRaw <- dtSalesRaw[conveyanceTypeDescription == "Improved Single Property Transaction"]
dtSalesRaw <- dtSalesRaw[, .(folioID, conveyanceDate, conveyancePrice)]

dtSalesAll <- merge(dtSalesRaw, dtBCA, by = "folioID")
dtSalesAll[, year       := as.numeric(substring(conveyanceDate, 1, 4))]
dtSalesAll[, logPrice   := log(as.numeric(conveyancePrice))]
dtSalesAll[, landWidth  := as.numeric(landWidth)]
dtSalesAll[, landDepth  := as.numeric(landDepth)]
dtSalesAll[, roundWidth := round(landWidth)]
dtSalesAll[, w50        := as.integer(roundWidth == 50)]
dtSalesAll[, w33        := as.integer(roundWidth == 33)]
dtSalesAll[, age        := year - as.numeric(MB_effective_year)]
dtSalesAll[, FSR        := as.numeric(MB_total_finished_area) / (landWidth * landDepth)]
dtSalesAll[, logFSR     := log(FSR)]
dtSalesAll[, logFSR2    := logFSR^2]
dtSalesAll[, logFSR3    := logFSR^3]
dtSalesAll[, logWidth   := log(landWidth)]

# ============================================================================
# ---- Two analysis samples --------------------------------------------------
# ============================================================================
# A) Universal hedonic sample (duplex-margin: 33 vs 50, FSR 0.5-0.8, age < 40)
CRITAGE <- 40
FSR_LO  <- 0.50
FSR_HI  <- 0.80
YR_LO   <- 2013
YR_HI   <- 2019

dtSalesRegSubset <- dtSalesAll[(w33 == 1 | w50 == 1) &
                               year > YR_LO & year < YR_HI &
                               age > 0 & age < CRITAGE &
                               actualUseDescription == "Single Family Dwelling" &
                               !is.na(FSR) & FSR > FSR_LO & FSR < FSR_HI &
                               !is.na(logPrice) & is.finite(logPrice) &
                               !is.na(parcelXM) & !is.na(parcelYM)]

cat("\nUniversal hedonic sample:", nrow(dtSalesRegSubset),
    " | w33:", sum(dtSalesRegSubset$w33),
    " | w50:", sum(dtSalesRegSubset$w50),
    " | tracts:", uniqueN(dtSalesRegSubset$CTNAME), "\n")

# B) Elasticity sample: new builds (eff yr 2000-2018), all widths,
#    depth IQR-trimmed within sample
EFFYR_LO <- 2000
EFFYR_HI <- 2018

dtSalesElast <- dtSalesAll[year > YR_LO & year < YR_HI &
                           MB_effective_year >= EFFYR_LO &
                           MB_effective_year <= EFFYR_HI &
                           age > 0 &
                           actualUseDescription == "Single Family Dwelling" &
                           !is.na(FSR) & FSR > FSR_LO & FSR < FSR_HI &
                           !is.na(landWidth) & landWidth > 0 &
                           !is.na(landDepth) & landDepth > 0 &
                           !is.na(logPrice) & is.finite(logPrice) &
                           !is.na(parcelXM) & !is.na(parcelYM)]

depthQ <- quantile(dtSalesElast$landDepth, c(0.25, 0.75), na.rm = TRUE)
cat("\nDepth IQR trim:", round(depthQ[1], 1), "to", round(depthQ[2], 1), "ft\n")
dtSalesElast <- dtSalesElast[landDepth >= depthQ[1] & landDepth <= depthQ[2]]

cat("Elasticity sample:", nrow(dtSalesElast),
    " | tracts:", uniqueN(dtSalesElast$CTNAME),
    " | width range:", round(min(dtSalesElast$landWidth), 1), "-",
    round(max(dtSalesElast$landWidth), 1), "ft\n")
cat("Width distribution in elasticity sample:\n")
print(quantile(dtSalesElast$landWidth, c(0.05, 0.25, 0.5, 0.75, 0.95)))

FSRctrl <- "logFSR + logFSR2 + logFSR3"

# ============================================================================
# ---- Neighbourhood-level hedonic + plex ratio (unchanged) -----------------
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

dtRatioNbhd <- dtPermitAll[roundWidth == 33 &
                           year >= 2018 & year < 2024 & !is.na(neighbourhoodDescription),
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
# ---- Tract-level hedonic + plex ratio (unchanged) -------------------------
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

dtRatioTract <- dtPermitAll[roundWidth == 33 &
                            year >= 2018 & year < 2024 & !is.na(CTNAME),
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
# ---- Local kernel features -------------------------------------------------
# ============================================================================
gaussKernel <- function(d, h) exp(-0.5 * (d / h)^2)

# Local fit on the 33/50 hedonic sample: gives localPremium (w50 dummy)
# and localPPSF (predicted log price at a reference 33' house).
localFitPremium <- function(xE, yE, h, sales, minN = 40, minPerGroup = 5,
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

# Local fit on the new-build elasticity sample: gives localElasticity
# (coefficient on log(landWidth)).
localFitElasticity <- function(xE, yE, h, sales, minN = 40,
                               minDistinctWidths = 5) {
  d <- sqrt((sales$parcelXM - xE)^2 + (sales$parcelYM - yE)^2)
  w <- gaussKernel(d, h)
  keep <- d < 3 * h & w > 1e-6
  if (sum(keep) < minN) {
    return(list(localElasticity = NA_real_, nSalesE = sum(keep)))
  }
  sub <- sales[keep]
  if (uniqueN(sub$roundWidth) < minDistinctWidths) {
    return(list(localElasticity = NA_real_, nSalesE = sum(keep)))
  }
  sub[, .w := w[keep]]
  fit <- tryCatch(
    feols(logPrice ~ logWidth + logFSR + logFSR2 + logFSR3 + age | year,
          data = sub, weights = ~.w),
    error = function(e) NULL
  )
  if (is.null(fit)) return(list(localElasticity = NA_real_, nSalesE = sum(keep)))
  list(localElasticity = unname(coef(fit)["logWidth"]),
       nSalesE         = sum(keep))
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

# ============================================================================
# ---- Permit set for local features: both 33' and 50' ----------------------
# ============================================================================
dtPermitLocal <- dtPermitAll[roundWidth %in% c(33, 50) &
                             useCat %in% c("Single","Duplex") &
                             year >= 2018 & year < 2024]
dtPermitLocal[, isDuplex := as.integer(useCat == "Duplex")]
cat("\nPermits for choice model (33' + 50', single vs duplex):", nrow(dtPermitLocal), "\n")
print(dtPermitLocal[, .(.N, duplexShare = mean(isDuplex)), by = roundWidth])

# ============================================================================
# ---- Bandwidth CV (on the duplex-margin hedonic sample) -------------------
# ============================================================================
bandwidthGrid <- c(750, 1000, 1250, 1500, 1750, 2000)
cat("\nBandwidth CV (predictive RMSE on hedonic sample):\n")
cvResults <- rbindlist(lapply(bandwidthGrid, function(h) {
  cat("  h =", h, "m... "); t0 <- Sys.time()
  r <- cvBandwidth(h, dtSalesRegSubset)
  cat("RMSE =", round(r$rmse, 4), " | n =", r$nPred,
      " | t =", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")
  as.data.table(r)
}))
hOptCV <- cvResults[which.min(rmse), h]
cat("CV-optimal h:", hOptCV, "m\n")

# ============================================================================
# ---- Compute features at every bandwidth ----------------------------------
# ============================================================================
computePermitFeatures <- function(h, permits, salesHed, salesElast) {
  feats <- rbindlist(lapply(seq_len(nrow(permits)), function(i) {
    rP <- localFitPremium(permits$permitXM[i], permits$permitYM[i], h, salesHed)
    rE <- localFitElasticity(permits$permitXM[i], permits$permitYM[i], h, salesElast)
    data.table(localPremium    = rP$localPremium,
               localPPSF       = rP$localPPSF,
               nSales          = rP$nSales,
               localElasticity = rE$localElasticity,
               nSalesE         = rE$nSalesE)
  }))
  cbind(permits[, .(isDuplex, useCat, CTNAME, neighbourhoodDescription,
                    roundWidth, permitXM, permitYM, year,
                    MB_effective_year, priorAge)],
        feats)
}

cat("\nComputing permit features at each bandwidth...\n")
permitFeatures <- lapply(bandwidthGrid, function(h) {
  cat("  h =", h, "m... "); t0 <- Sys.time()
  dt <- computePermitFeatures(h, dtPermitLocal, dtSalesRegSubset, dtSalesElast)
  cat("n with premium:",    sum(!is.na(dt$localPremium)),
      " | n with elast:",   sum(!is.na(dt$localElasticity)),
      " | t =", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")
  dt
})
names(permitFeatures) <- paste0("h", bandwidthGrid)

# ============================================================================
# ---- Diagnostics across bandwidths ----------------------------------------
# ============================================================================
diagBW <- rbindlist(lapply(seq_along(bandwidthGrid), function(j) {
  h  <- bandwidthGrid[j]
  dt <- permitFeatures[[j]][!is.na(localPremium) & !is.na(localPPSF) &
                            !is.na(localElasticity)]
  if (nrow(dt) < 20) return(data.table(h = h))

  probit <- tryCatch(
    glm(isDuplex ~ localPremium + localPPSF + localElasticity,
        data = dt[roundWidth == 33], family = binomial("probit")),
    error = function(e) NULL
  )
  if (is.null(probit)) {
    premCoef <- premSE <- ppsfCoef <- ppsfSE <- elastCoef <- elastSE <- NA
  } else {
    cf <- coef(probit); vc <- sqrt(diag(vcov(probit)))
    premCoef  <- cf["localPremium"];    premSE  <- vc["localPremium"]
    ppsfCoef  <- cf["localPPSF"];       ppsfSE  <- vc["localPPSF"]
    elastCoef <- cf["localElasticity"]; elastSE <- vc["localElasticity"]
  }

  corPE  <- cor(dt$localPPSF, dt$localElasticity)
  corPP  <- cor(dt$localPPSF, dt$localPremium)
  corEP  <- cor(dt$localElasticity, dt$localPremium)

  data.table(h = h, nPermits = nrow(dt),
             sdPremium    = sd(dt$localPremium),
             sdPPSF       = sd(dt$localPPSF),
             sdElasticity = sd(dt$localElasticity),
             corPPSFElast = corPE,
             corPPSFPrem  = corPP,
             corElastPrem = corEP,
             probitPrem   = premCoef,  probitPremSE  = premSE,
             probitPPSF   = ppsfCoef,  probitPPSFSE  = ppsfSE,
             probitElast  = elastCoef, probitElastSE = elastSE)
}))
diagAll <- merge(cvResults, diagBW, by = "h")
print(diagAll)

# ---- Plots ----------------------------------------------------------------
pCV    <- ggplot(cvResults, aes(h, rmse)) + geom_line() + geom_point(size = 3) +
          scale_x_log10() +
          labs(x = "Bandwidth (m, log)", y = "10-fold CV RMSE",
               title = "Diagnostic 1: prediction error") + theme_minimal()
pDisp  <- ggplot(melt(diagBW[, .(h, sdPremium, sdPPSF, sdElasticity)],
                      id.vars = "h"),
                 aes(h, value, color = variable)) +
          geom_line() + geom_point(size = 3) + scale_x_log10() +
          labs(x = "Bandwidth (m, log)", y = "SD across permits",
               title = "Diagnostic 2: spatial dispersion of local features") +
          theme_minimal()
pCor   <- ggplot(melt(diagBW[, .(h, corPPSFElast, corPPSFPrem, corElastPrem)],
                      id.vars = "h"),
                 aes(h, value, color = variable)) +
          geom_line() + geom_point(size = 3) +
          geom_hline(yintercept = 0, linetype = "dashed") +
          scale_x_log10() +
          labs(x = "Bandwidth (m, log)", y = "Cross-feature correlation",
               title = "Diagnostic 3: do local features measure the same thing?") +
          theme_minimal()
pStab  <- ggplot(diagBW, aes(h)) +
          geom_pointrange(aes(y = probitElast,
                              ymin = probitElast - 1.96 * probitElastSE,
                              ymax = probitElast + 1.96 * probitElastSE),
                          color = "darkgreen") +
          geom_hline(yintercept = 0, linetype = "dashed") +
          scale_x_log10() +
          labs(x = "Bandwidth (m, log)", y = "Probit coef on localElasticity",
               title = "Diagnostic 4: elasticity coefficient stability (33' permits)") +
          theme_minimal()
ggsave("text/bandwidth_cv.png",         pCV,   w = 6, h = 4)
ggsave("text/bandwidth_dispersion.png", pDisp, w = 7, h = 4)
ggsave("text/bandwidth_corr.png",       pCor,  w = 7, h = 4)
ggsave("text/bandwidth_probit.png",     pStab, w = 6, h = 4)

# ============================================================================
# ---- Headline choice models -----------------------------------------------
# ============================================================================
# `widths` argument controls which lots enter the choice model.
runChoiceModels <- function(dt, widths, label) {
  sub <- dt[roundWidth %in% widths &
            !is.na(localPremium) & !is.na(localPPSF) & !is.na(localElasticity)]
  if (nrow(sub) < 20) {
    cat("\n[", label, "] too few obs (", nrow(sub), "), skipping\n", sep = "")
    return(invisible(NULL))
  }
  cat("\n--- Choice models [", label, "]: widths =", paste(widths, collapse = ","),
      " | n =", nrow(sub), " | duplex share =", round(mean(sub$isDuplex), 3),
      " ---\n", sep = "")
  cat("Correlations:\n")
  print(round(cor(sub[, .(localPremium, localPPSF, localElasticity, isDuplex)]), 3))

  pr1 <- glm(isDuplex ~ localPPSF,                                    data = sub, family = binomial("probit"))
  pr2 <- glm(isDuplex ~ localElasticity,                              data = sub, family = binomial("probit"))
  pr3 <- glm(isDuplex ~ localPremium,                                 data = sub, family = binomial("probit"))
  pr4 <- glm(isDuplex ~ localPPSF + localElasticity,                  data = sub, family = binomial("probit"))
  pr5 <- glm(isDuplex ~ localPremium + localPPSF + localElasticity,   data = sub, family = binomial("probit"))
  cat("\nProbit, PPSF only:\n");                   print(summary(pr1)$coefficients)
  cat("\nProbit, elasticity only:\n");             print(summary(pr2)$coefficients)
  cat("\nProbit, premium only:\n");                print(summary(pr3)$coefficients)
  cat("\nProbit, PPSF + elasticity:\n");           print(summary(pr4)$coefficients)
  cat("\nProbit, premium + PPSF + elasticity:\n"); print(summary(pr5)$coefficients)
  invisible(list(pr1 = pr1, pr2 = pr2, pr3 = pr3, pr4 = pr4, pr5 = pr5))
}

hHeadline <- 1500
dtBest <- permitFeatures[[paste0("h", hHeadline)]]

choice33   <- runChoiceModels(dtBest, widths = 33,        label = "33' only")
choice50   <- runChoiceModels(dtBest, widths = 50,        label = "50' only")
choiceBoth <- runChoiceModels(dtBest, widths = c(33, 50), label = "33' + 50' pooled")

# ============================================================================
# ---- Tract-aggregate comparison at headline bandwidth ---------------------
# ============================================================================
makeTractAgg <- function(dt, widths) {
  sub <- dt[roundWidth %in% widths &
            !is.na(localPremium) & !is.na(localPPSF) & !is.na(localElasticity)]
  sub[, .(plexRate     = mean(isDuplex),
          meanPremium  = mean(localPremium),
          meanPPSF     = mean(localPPSF),
          meanElast    = mean(localElasticity),
          n            = .N), by = CTNAME][n >= 5]
}

for (wset in list(c(33), c(50), c(33, 50))) {
  lab <- paste(wset, collapse = "+")
  dtAgg <- makeTractAgg(dtBest, wset)
  cat("\n=== Tract-aggregate [widths =", lab, "] ===\n")
  print(round(cor(dtAgg[, .(plexRate, meanPremium, meanPPSF, meanElast)]), 3))
  print(etable(
    feols(plexRate ~ meanPPSF,                            data = dtAgg, weights = ~n),
    feols(plexRate ~ meanElast,                           data = dtAgg, weights = ~n),
    feols(plexRate ~ meanPremium,                         data = dtAgg, weights = ~n),
    feols(plexRate ~ meanPPSF + meanElast,                data = dtAgg, weights = ~n),
    feols(plexRate ~ meanPremium + meanPPSF + meanElast,  data = dtAgg, weights = ~n),
    headers = c("PPSF","Elast","Prem","PPSF+Elast","All three"),
    digits = 3))
}

# ============================================================================
# ---- Spatial heatmaps at headline bandwidth -------------------------------
# ============================================================================
dtMap <- copy(dtBest[roundWidth == 33 &
                     !is.na(localPremium) & !is.na(localPPSF) &
                     !is.na(localElasticity)])
dtMap[, `:=`(xKm = permitXM / 1000, yKm = permitYM / 1000)]

pMapPPSF <- ggplot(dtMap, aes(xKm, yKm, color = localPPSF)) +
  geom_point(size = 1.2, alpha = 0.7) +
  scale_color_viridis_c(option = "plasma", name = "log P_ref") +
  coord_equal() +
  labs(x = "Easting (km)", y = "Northing (km)",
       title = paste0("Local log price at reference house, h = ", hHeadline, "m"),
       subtitle = "33' lot permits, 2018-2023") + theme_minimal()

pMapPrem <- ggplot(dtMap, aes(xKm, yKm, color = localPremium)) +
  geom_point(size = 1.2, alpha = 0.7) +
  scale_color_gradient2(low = "blue", mid = "grey90", high = "red",
                        midpoint = median(dtMap$localPremium, na.rm = TRUE),
                        name = "w50 premium\n(log points)") +
  coord_equal() +
  labs(x = "Easting (km)", y = "Northing (km)",
       title = paste0("Local 50' vs 33' premium, h = ", hHeadline, "m"),
       subtitle = "33' lot permits, 2018-2023") + theme_minimal()

pMapElast <- ggplot(dtMap, aes(xKm, yKm, color = localElasticity)) +
  geom_point(size = 1.2, alpha = 0.7) +
  scale_color_gradient2(low = "blue", mid = "grey90", high = "red",
                        midpoint = median(dtMap$localElasticity, na.rm = TRUE),
                        name = "log P / log width") +
  coord_equal() +
  labs(x = "Easting (km)", y = "Northing (km)",
       title = paste0("Local lot-size elasticity (new builds), h = ", hHeadline, "m"),
       subtitle = "33' lot permits, 2018-2023") + theme_minimal()

# Choice maps: 33' and 50' faceted, plotted from dtBest (no feature filter)
dtMapChoice <- dtBest[roundWidth %in% c(33, 50)]
dtMapChoice[, choice := factor(ifelse(isDuplex == 1, "Duplex", "Single"),
                               levels = c("Single","Duplex"))]
dtMapChoice[, width := factor(paste0(roundWidth, "'"), levels = c("33'","50'"))]

pMapChoiceBoth <- ggplot(dtMapChoice, aes(permitXM/1000, permitYM/1000, color = choice)) +
  geom_point(size = 1.2, alpha = 0.7) +
  facet_wrap(~ width) +
  scale_color_manual(values = c("Single" = "grey70", "Duplex" = "red"),
                     name = "Permit type") +
  coord_equal() +
  labs(x = "Easting (km)", y = "Northing (km)",
       title = "Duplex vs single permits, 2018-2023") +
  theme_minimal()

ggsave("text/heatmap_ppsf_h1500.png",       pMapPPSF,       w = 7, h = 6)
ggsave("text/heatmap_premium_h1500.png",    pMapPrem,       w = 7, h = 6)
ggsave("text/heatmap_elasticity_h1500.png", pMapElast,      w = 7, h = 6)
ggsave("text/heatmap_choice_both.png",      pMapChoiceBoth, w = 12, h = 6)

# ============================================================================
# ---- Save ------------------------------------------------------------------
# ============================================================================
saveRDS(list(cvResults = cvResults, diagBW = diagBW, diagAll = diagAll,
             permitFeatures = permitFeatures, hOptCV = hOptCV, hHeadline = hHeadline,
             choice33 = choice33, choice50 = choice50, choiceBoth = choiceBoth,
             dtNbhdCompare = dtNbhdCompare, dtTractCompare = dtTractCompare,
             depthQ = depthQ),
        "~/DropboxExternal/dataProcessed/vancouverLotPlex_results.rds")

cat("\nDone. h_CV =", hOptCV, "m | h_headline =", hHeadline, "m\n")
q("no")

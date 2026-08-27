# vancouverMatch.R
# match permits to neighbourhoods, assessments, etc
# Objective -- correlation and plots: local price levels vs duplex / multiplex choice.
# Tom Davidoff
# 08/13/26  (rev: local level + elasticity, SFD-only comp pool)
#

SALE_MINYEAR    <- 2015    # comp sales window start (pre-rezoning)
SALE_MAXYEAR    <- 2017    # comp sales window end
CRITVAL         <- 0.5     # ProjectValue percentile threshold for non-new-SFD permits
MIN_PERMIT_YEAR <- 2018    # keep permit years strictly greater than this
USE_MIN_N       <- 100     # SpecificUseCategory kept if it has more than this many permits
MATCHDIST       <- 7.5     # somewhat arbitrary, lots at 1 or below

library(data.table)
library(sf)
library(ggplot2)
library(ggspatial)
library(RSQLite)
library(fixest)
library(parallel)
library(units)
library(scales)
library(patchwork)

sf_use_s2(TRUE)

# step 1: merge 2026 roll Number (start) geometry to 2018 sales and property characteristics
fGeo     <- "~/DropboxExternal/dataProcessed/bca26FolioGeometryVancouver.rds"
dirGpkg  <- "~/bigFiles/latestSpatialBCA/"
gpkgFiles <- list.files(dirGpkg, pattern = "\\.gpkg$", full.names = TRUE)
stopifnot(length(gpkgFiles) == 1)
fileGpkg <- gpkgFiles

if (!file.exists(fGeo)) {
  dfG <- st_read(fileGpkg, query =
    "SELECT ROLL_NUMBER, geom
     FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
     WHERE JURISDICTION_CODE = '200'")            # City of Vancouver

  # centroid lon/lat in WGS84 (parcel polygons -> single point per folio)
  dfG <- st_make_valid(dfG)
  dfG <- st_transform(dfG, 4326)
  ctr <- st_coordinates(st_centroid(st_geometry(dfG)))
  dtGeo <- as.data.table(st_drop_geometry(dfG))
  dtGeo[, `:=`(longitude = ctr[, 1], latitude = ctr[, 2])]
  dtGeo[, rollStart := floor(as.numeric(ROLL_NUMBER) / 1000)]
  dtGeo <- unique(dtGeo[, .(rollStart, longitude, latitude)])
  saveRDS(dtGeo, fGeo)
}
dtGeo <- readRDS(fGeo)
dtGeo[, rr := frank(rollStart), by = rollStart]
dtGeo <- dtGeo[rr == 1, .(rollStart, longitude, latitude)]

# BCA 2019 sales/inventory. Comp pool ground-oriented (condos purged in SQL).

fnameSingles <- "~/DropboxExternal/dataProcessed/bca19VancouverSingles.rds"
fnameComps   <- "~/DropboxExternal/dataProcessed/bca19VancouverCompSales.rds"
if (!file.exists(fnameSingles) | !file.exists(fnameComps)) {
	bca19 <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
	con <- dbConnect(RSQLite::SQLite(), bca19)
	dtBCA19 <- data.table(dbGetQuery(con, "
	  SELECT d.folioID, f.rollNumber, i.zoning, CAST(i.MB_effective_year AS NUMERIC) AS MB_effective_year, i.MB_total_finished_area,
		 CAST(i.land_width AS NUMERIC) AS land_width, CAST(i.land_depth AS NUMERIC) AS land_depth,
		 d.actualUseDescription, d.neighbourhoodDescription, CAST(d.landWidth AS NUMERIC) AS landWidth, CAST(d.landDepth AS NUMERIC) AS landDepth, CAST(v.landValue AS NUMERIC) AS landValue, CAST(v.improvementValue AS NUMERIC) AS improvementValue
	  FROM folio f
	  JOIN residentialInventory i ON i.roll_number = f.rollNumber
	  JOIN folioDescription d      ON d.folioID     = f.folioID
	  JOIN valuation v            ON v.folioID  = f.folioID
	  WHERE f.jurisdictionCode = '200'
	"))
	dtBCA19[, rollStart := floor(as.numeric(rollNumber) / 1000)]
	dtBCA19[is.na(landWidth), landWidth := land_width]
	dtBCA19[is.na(landDepth), landDepth := land_depth]
	dtBCA19 <- merge(dtBCA19, dtGeo, by = "rollStart", all.x = TRUE)
	dtSingles <- dtBCA19[actualUseDescription %in% c("Single Family Dwelling", "Residential Dwelling with Suite"),.(landWidth, landDepth, MB_total_finished_area, MB_effective_year, rollNumber, folioID, longitude, latitude,landValue)]
	dtSingles <- dtSingles[!is.na(longitude) & !is.na(latitude)]
	saveRDS(dtSingles, fnameSingles)

	dtSales <- data.table(dbGetQuery(con,
	  "SELECT folioID, conveyanceDate, conveyancePrice, conveyanceTypeDescription
	   FROM sales
	  WHERE conveyanceTypeDescription= 'Improved Single Property Transaction' "))
	dtSales <- merge(dtSales,dtBCA19[,.(folioID, longitude, latitude, MB_effective_year, MB_total_finished_area,landWidth, landDepth,actualUseDescription)], by = "folioID")
	print(head(dtSales))
	dtSales[,year:=as.numeric(substring(conveyanceDate, 1, 4))]
	dtSales <- dtSales[year %between% c(SALE_MINYEAR, SALE_MAXYEAR)]

	for (v in c("landWidth", "landDepth", "MB_total_finished_area", "MB_effective_year")) {
	  dtBCA19[, (v) := as.numeric(get(v))]
	}

	dtSales[, structAge := year - MB_effective_year]
	dtSales[structAge < 0, structAge := 0]
	dtSales <- dtSales[!is.na(longitude) & !is.na(latitude)	& !is.na(MB_total_finished_area) & !is.na(MB_effective_year)  ]
	saveRDS(dtSales, fnameComps)
}
dtSales <- readRDS(fnameComps)
cat("comp sales after filters (pre-coordinate merge):", nrow(dtSales), "\n")
dtSingles <- readRDS(fnameSingles)
dtSales[, MB_total_finished_area := as.numeric(MB_total_finished_area)]

# Now read permits and find matching single family dwelling where it exist
dtP <- fread(
  "~/DropboxExternal/dataRaw/issued-building-permitsDwellingUses.csv",
  select = c("SpecificUseCategory", "PermitNumber", "geo_point_2d",
             "PermitNumberCreatedDate", "TypeOfWork", "ProjectValue")
)
dtP[, c("lat", "lon") := tstrsplit(geo_point_2d, ",\\s*", type.convert = TRUE)]

# ---- zoning: R1-1 shapes (used for permits AND for the comp inR1 flag) -------
dgZ <- st_read("~/DropboxExternal/dataRaw/vancouver_zoning.geojson")
dgZ <- dgZ[dgZ$zoning_district == "R1-1", ]

dtPgeo <- st_as_sf(dtP[!is.na(lat), .(PermitNumber, lat, lon)],
                   coords = c("lon", "lat"), crs = 4326)
dtPgeo <- st_join(dtPgeo, dgZ, join = st_within)
r1Permits <- unique(as.data.table(dtPgeo)$PermitNumber)
cat("R1-1 permits:", length(r1Permits), "\n")

# DEFINE THE ANALYSIS PERMIT SAMPLE

dtP <- dtP[PermitNumber %in% r1Permits]
dtP[, year   := as.numeric(substr(PermitNumberCreatedDate, 1, 4))]
dtP <- dtP[year > MIN_PERMIT_YEAR]
useList  <- dtP[, .N, by = SpecificUseCategory][order(-N)][N > USE_MIN_N, SpecificUseCategory]
dtP <- dtP[SpecificUseCategory %in% useList]
dtP[, single := grepl("Single", SpecificUseCategory)]
minSpend <- quantile(dtP[TypeOfWork=="New Building" & single==1, ProjectValue], CRITVAL)
dtP <- dtP[TypeOfWork == "New Building"  | (ProjectValue > minSpend)]
dtP <- dtP[!is.na(lat) & !is.na(lon)]
dtP[, duplex := grepl("Duplex", SpecificUseCategory)]
dtP[, multiPlex := SpecificUseCategory == "Multiple Dwelling"]
cat("analysis permits after all filters:", nrow(dtP), "\n")
print(useList)

# try to match with a close dtSingle obs
# ---- lot geometry from nearest folio (for landWidth/landDepth splits) --------
dtPgeoNN <- st_as_sf(dtP[, .(PermitNumber, lat, lon)], coords = c("lon", "lat"), crs = 4326)
dfSingle <- st_as_sf(dtSingles, coords = c("longitude", "latitude"), crs = 4326)

idx  <- st_nearest_feature(dtPgeoNN, dfSingle)
dist <- as.numeric(st_distance(dtPgeoNN, dfSingle[idx, ], by_element = TRUE))  # meters, plain numeric
print(summary(dist))

matched <- as.data.table(st_drop_geometry(dfSingle))[idx]   # nearest folio's attrs, one row per permit
matched[dist > MATCHDIST, names(matched) := NA]             # gate at MATCHDIST m
matched[, PermitNumber := dtP$PermitNumber]                 # attach the key (order-safe: idx is in dtP order)
matched[, dist := dist]
matched <- matched[dist < MATCHDIST, .(PermitNumber, rollNumber, dist, MB_total_finished_area, MB_effective_year, landWidth, landDepth, landValue)]
dtP <- merge(dtP, matched, by = "PermitNumber")
print(nrow(dtP))

# ============================================================================
# LOCAL LEVEL + ELASTICITY  (replaces kEst1250 / kEst2500 / kEst3000)
# For each permit: WLS of log(price) ~ log(sqft) on nearby SFD comps,
# geographically weighted by an Epanechnikov kernel at radius KM. Extract:
#   kLevel = fitted log(price) at reference size REF_SQFT (p20 of SFD sqft)
#   kElast = slope d log(price) / d log(sqft)   [local price-size elasticity]
# Note: kElast(price) = kElast(ppsf) + 1, so a mostly-negative ppsf slope
#       becomes a positive price slope (bigger homes cost more in total).
# ============================================================================

MAXAGE   <- 50                                  # max comp structure age at sale
KM       <- 1.0                                 # geographic radius, km
REF_PCTL <- 0.20                                # reference size = p20 of SFD sqft
MIN_COMPS <- 8                                  # min in-window SFD comps to fit a slope

# comp pool: SFD only (single detached + detached-with-suite).
# Row Housing and Duplex categories are intentionally excluded so the level
# and elasticity reflect SFD product, not a mixture of product types.
sfdUse <- c("Single Family Dwelling", "Residential Dwelling with Suite")

dtSales[, ppsf := as.numeric(conveyancePrice) / MB_total_finished_area]
quants  <- quantile(dtSales$ppsf, c(0.01, 0.99), na.rm = TRUE)
dtSales <- dtSales[ppsf > quants[1] & ppsf < quants[2]]
dtSales <- dtSales[!is.na(longitude) & !is.na(latitude) &
                   !is.na(MB_total_finished_area) & MB_total_finished_area > 0 &
                   !is.na(ppsf) & ppsf > 0 &
                   !is.na(structAge) & structAge >= 0 & structAge <= MAXAGE &
                   actualUseDescription %in% sfdUse]

# demean log(price) by (year, ageBin) ONLY -- do NOT residualize out size,
# since the local slope on log(sqft) is exactly what we want to estimate.
dtSales[, price   := as.numeric(conveyancePrice)]
dtSales[, ageBin  := pmin(structAge %/% 5, 9)]               # 0-4,5-9,...,45-50
dtSales[, lprice  := log(price)]
dtSales[, lsqft   := log(MB_total_finished_area)]
dtSales[, yaMean  := mean(lprice), by = .(year, ageBin)]
dtSales[, lpriceAdj := lprice - yaMean]                      # time/age-adjusted log price

# reference size: p20 of SFD sqft (above teardown territory, near the body)
REF_SQFT <- quantile(dtSales$MB_total_finished_area, REF_PCTL, na.rm = TRUE)
LREF     <- log(REF_SQFT)
cat("SFD comps for local fits:", nrow(dtSales),
    "| REF_SQFT (p", REF_PCTL*100, " SFD): ", round(REF_SQFT), "\n", sep = "")

# ---- geometry + parallel vectors, ALL from the final dtSales ---------------
sfSales <- st_transform(st_as_sf(dtSales, coords = c("longitude","latitude"), crs = 4326), 3005)
sfPerm  <- st_transform(st_as_sf(dtP,     coords = c("lon","lat"),           crs = 4326), 3005)

sc <- st_coordinates(sfSales)
pc <- st_coordinates(sfPerm)
yv <- dtSales$lpriceAdj                                      # time/age-adjusted log price
xv <- dtSales$lsqft                                          # log sqft
stopifnot(nrow(sc) == length(yv), length(yv) == length(xv)) # guard against misalignment

epan <- function(u) 0.75 * (1 - u*u) * (abs(u) < 1)         # zero outside [-1,1]

# Closed-form weighted simple regression of yv ~ xv within each permit's kernel.
# Level recovered at LREF from the (uncentered) intercept + slope.
estOne <- function(i) {
  dx  <- sc[,1] - pc[i,1]
  dy  <- sc[,2] - pc[i,2]
  dkm <- sqrt(dx*dx + dy*dy) / 1000                          # km
  w   <- epan(dkm / KM)
  ok  <- w > 0
  n   <- sum(ok)
  if (n < MIN_COMPS) return(c(level = NA_real_, elast = NA_real_, n = n))

  xw <- xv[ok]; yw <- yv[ok]; ww <- w[ok]
  sw   <- sum(ww)
  xbar <- sum(ww * xw) / sw
  ybar <- sum(ww * yw) / sw
  sxx  <- sum(ww * (xw - xbar)^2)
  if (sxx <= 0) return(c(level = NA_real_, elast = NA_real_, n = n))  # no local size variation
  sxy  <- sum(ww * (xw - xbar) * (yw - ybar))
  b1   <- sxy / sxx                                          # elasticity (price-size slope)
  b0   <- ybar - b1 * xbar                                   # intercept (adj log price at lsqft=0)
  level <- b0 + b1 * LREF                                    # adj log price at REF_SQFT
  c(level = level, elast = b1, n = n)
}

res <- t(vapply(seq_len(nrow(sfPerm)), estOne, c(level = NA_real_, elast = NA_real_, n = NA_real_)))
dtP[, `:=`(kLevel = res[, 1],
           kElast = res[, 2],
           kN     = as.integer(res[, 3]))]

cat("permits with a local fit:", sum(!is.na(dtP$kLevel)), "of", nrow(dtP), "\n")
cat("dropped (kN <", MIN_COMPS, "):", sum(dtP$kN < MIN_COMPS), "\n")
print(summary(dtP[, .(kLevel, kElast, kN)]))
cat("cor(kLevel, kElast):", round(cor(dtP$kLevel, dtP$kElast, use = "complete.obs"), 3),
    " -- if > ~0.7, evaluate level at local-mean sqft instead of REF_SQFT\n")

# ---- cartolight heatmap of level and elasticity ----------------------------
dtP <- dtP[landDepth %between% c(100,150) & landWidth %between% c(30,60)]
dtP[, landValuePsf := landValue / (landWidth * landDepth)]
sfP <- st_as_sf(dtP[!is.na(kLevel)], coords = c("lon","lat"), crs = 4326)

mapOne <- function(col, ttl, opt = "magma", dir = -1)
  ggplot(sfP) +
    annotation_map_tile(type = "cartolight", zoomin = -1, progress = "none") +
    geom_sf(aes(colour = .data[[col]]), size = 0.9, alpha = 0.85) +
    scale_colour_viridis_c(option = opt, direction = dir, name = NULL) +
    labs(title = ttl) +
    theme_void(base_size = 12) +
    theme(plot.title = element_text(face = "bold", hjust = 0.01))

pp <- mapOne("kLevel", "Local log price @ p20 SFD sqft") +
      mapOne("kElast", "Local price-size elasticity") +
      mapOne("landValuePsf", "Land value / sqft (BC Assessment)")

ggsave("text/fittedLocalLevelElastMaps.png", pp,
       width = 14, height = 7, dpi = 200, bg = "white")

# ---- level vs elasticity scatter, coloured by duplex -----------------------
ggplot(dtP[year < 2024 & (single==1 | duplex==1)],
       aes(x = kLevel, y = kElast, colour = duplex)) +
  geom_point(size = .5, alpha = 0.5) +
  scale_colour_manual(values = c(`FALSE` = "blue", `TRUE` = "red"),
                      labels = c(`FALSE` = "Other", `TRUE` = "Duplex"),
                      name = NULL) +
  labs(x = "Local log price @ p20 sqft", y = "Local price-size elasticity",
       title = "Local level vs elasticity (pre-2024)") +
  theme_minimal(base_size = 12)
ggsave("text/localLevelElastXY.png", width = 7, height = 7, dpi = 200, bg = "white")

# ---- duplex / multiplex choice vs local level + elasticity -----------------
print(summary(feols(duplex ~ kLevel +  landWidth + landDepth | year, data = dtP[year < 2024 & (single==1|duplex==1)], vcov = vcov_conley(lat="lat", lon="lon", cutoff=2.0))))
print(summary(feols(duplex ~ kLevel + kElast + landWidth + landDepth | year, data = dtP[year < 2024 & (single==1|duplex==1)], vcov = vcov_conley(lat="lat", lon="lon", cutoff=2.0))))


print(summary(feols(multiPlex ~ kLevel + kElast + landWidth + landDepth | year,
                    data = dtP[year > 2023 & (single==1|duplex==1|multiPlex==1)],
                    vcov = vcov_conley(lat="lat", lon="lon", cutoff=2.0))))

print("table of permit choice by year")
print(dtP[,.N,by=.(year,SpecificUseCategory)])

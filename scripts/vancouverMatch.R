# vancouverMatch.R
# match permits to neighbourhoods, assessments, etc
# Objective -- correlation and plots: local price levels vs duplex / multiplex choice.
# Tom Davidoff
# 08/13/26
#

SALE_MINYEAR <- 2015    # comp sales window start (pre-rezoning)
SALE_MAXYEAR <- 2017    # comp sales window end
CRITVAL         <- 0.5  # ProjectValue percentile threshold for non-new-SFD permits
MIN_PERMIT_YEAR <- 2018 # keep permit years strictly greater than this
USE_MIN_N       <- 100  # SpecificUseCategory kept if it has more than this many permits
MATCHDIST <- 7.5 # somewhat arbitrary, lots at 1 or below

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
print(head(dtSales))
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
print(head(dtP))
print(nrow(dtP))

# ============================================================================
# Now do sales within x KM of permit
# ---- 2D kernel price estimates at 1250 & 2500 sqft for each permit ----------
# distance dims: geographic (km) and floor area (sqft). Product of Epanechnikov kernels.
# ============================================================================

MAXAGE  <- 50                                   # A: max comp structure age at sale
KM      <- 1.0                                  # d: geographic radius, km
TARGETS <- c(1250, 2500)

# ---- build ppsf and residual on dtSales, then do ALL row filtering, BEFORE
#      extracting any parallel vectors. sc / p / a must derive from ONE table. ----
dtSales[, ppsf := as.numeric(conveyancePrice) / MB_total_finished_area]
quants  <- quantile(dtSales$ppsf, c(0.01, 0.99), na.rm = TRUE)
dtSales <- dtSales[ppsf > quants[1] & ppsf < quants[2]]
dtSales <- dtSales[!is.na(longitude) & !is.na(latitude) &
                   !is.na(MB_total_finished_area) & MB_total_finished_area > 0 &
                   !is.na(ppsf) & ppsf > 0 &
                   !is.na(structAge) & structAge <= MAXAGE &
                   !is.na(actualUseDescription)]

dtSales[, sqftDecile := round(frank(MB_total_finished_area, na.last = "keep",
                                    ties.method = "first") / nrow(dtSales) * 10)]
dtSales[, groupMean := mean(ppsf), by = .(year, structAge, actualUseDescription, sqftDecile)]
dtSales[, residual  := ppsf - groupMean]

for (q in unique(dtSales[, actualUseDescription])) {
	print(q)
	print(summary(dtSales[actualUseDescription == q, .(ppsf, MB_total_finished_area, structAge)]))
}

# keep ground-oriented comps; drop luxury condo / thin exotic categories
dtSales <- dtSales[actualUseDescription %in% c("Single Family Dwelling",
                                               "Residential Dwelling with Suite",
                                               "Row Housing (Single Unit Ownership)") |
                   grepl("Duplex", actualUseDescription)]
dtSales <- dtSales[!is.na(residual)]

# bandwidth AFTER final comp pool is fixed, so it reflects the pool actually used
AREABW  <- sd(dtSales$MB_total_finished_area)   # size bandwidth = 1 SD of area
cat("area bandwidth (1 SD sqft):", round(AREABW), "\n")

# geometry + parallel vectors, ALL from the final dtSales
sfSales <- st_transform(st_as_sf(dtSales, coords = c("longitude","latitude"), crs = 4326), 3005)
sfPerm  <- st_transform(st_as_sf(dtP,     coords = c("lon","lat"),           crs = 4326), 3005)

pc <- st_coordinates(sfPerm)
sc <- st_coordinates(sfSales)
p  <- dtSales$residual
a  <- dtSales$MB_total_finished_area
stopifnot(nrow(sc) == length(p), length(p) == length(a))   # guard against vector misalignment

epan <- function(u) 0.75 * (1 - u*u) * (abs(u) < 1)   # zero outside [-1,1]

estOne <- function(i) {
  dx <- sc[,1] - pc[i,1]
  dy <- sc[,2] - pc[i,2]
  dgeo <- sqrt(dx*dx + dy*dy) / 1000                  # km
  wgeo <- epan(dgeo / KM)                             # geo kernel

  out <- numeric(length(TARGETS))
  nn  <- integer(length(TARGETS))
  for (k in seq_along(TARGETS)) {
    warea <- epan((a - TARGETS[k]) / AREABW)          # size kernel around this target
    w  <- wgeo * warea
    ok <- w > 0
    nn[k]  <- sum(ok)
    out[k] <- if (any(ok)) weighted.mean(p[ok], w[ok], na.rm = TRUE) else NA_real_
  }
  c(out, nn)
}

res <- t(vapply(seq_len(nrow(sfPerm)), estOne, numeric(2 * length(TARGETS))))
dtP[, `:=`(kEst1250 = res[,1], kEst2500 = res[,2],
           kN1250   = as.integer(res[,3]), kN2500 = as.integer(res[,4]))]

cat("permits with >=1 comp @1250:", sum(dtP$kN1250 > 0),
    " @2500:", sum(dtP$kN2500 > 0), "of", nrow(dtP), "\n")
print(summary(dtP[, .(kEst1250, kEst2500, kN1250, kN2500)]))
# confirm every NA estimate is genuinely zero in-window comps, not misalignment
print(dtP[is.na(kEst2500), .N, by = .(zeroComps = kN2500 == 0)])

# ---- cartolight heatmap of fitted price for each target -------------------
dtP <- dtP[landDepth %between% c(100,150) & landWidth %between% c(30,60)]
dtP[, landValuePsf := landValue / (landWidth * landDepth)]
sfP <- st_as_sf(dtP, coords = c("lon","lat"), crs = 4326)

mapOne <- function(col, ttl)
  ggplot(sfP) +
    annotation_map_tile(type = "cartolight", zoomin = -1, progress = "none") +
    geom_sf(aes(colour = .data[[col]]), size = 0.9, alpha = 0.85) +
    scale_colour_viridis_c(option = "magma", direction = -1, name = NULL) +
    labs(title = ttl) +
    theme_void(base_size = 12) +
    theme(plot.title = element_text(face = "bold", hjust = 0.01))

pp <- mapOne("kEst1250", "Fitted price resid — 1250 sqft") +
      mapOne("kEst2500", "Fitted price resid — 2500 sqft") +
      mapOne("landValuePsf", "Land value / sqft (BC Assessment)")

ggsave("text/fittedKernelPriceMaps.png", pp,
       width = 14, height = 7, dpi = 200, bg = "white")
print(pp)

# plot simple xy fitted kEst1250 vs kEst2500 (these are ppsf residuals, not $)
# plot simple xy fitted kEst1250 vs kEst2500 (these are ppsf residuals, not $)
# colour by duplex: red = duplex, blue = otherwise
ggplot(dtP, aes(x = kEst1250, y = kEst2500, colour = duplex)) +
  geom_point(size=.5, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, colour = "grey40", linetype = "dashed") +
  scale_colour_manual(values = c(`FALSE` = "blue", `TRUE` = "red"),
                      labels = c(`FALSE` = "Other", `TRUE` = "Duplex"),
                      name = NULL) +
  labs(x = "Fitted resid @1250 sqft", y = "Fitted resid @2500 sqft",
       title = "Kernel-fitted price residuals at two target sizes") +
  theme_minimal(base_size = 12)
ggsave("text/fittedKernelPriceXY.png", width = 7, height = 7, dpi = 200, bg = "white")

# ---- duplex / multiplex choice vs local fitted prices ---------------------
print(summary(feols(duplex ~ log(kEst2500) + landWidth + landDepth + log(kEst1250),
                    data = dtP[year < 2024], vcov = vcov_conley(lat="lat", lon="lon", cutoff=2.0))))
print(summary(feols(multiPlex ~ log(kEst2500) + landWidth + landDepth + log(kEst1250),
                    data = dtP[year > 2024], vcov = vcov_conley(lat="lat", lon="lon", cutoff=2.0))))

# ---- sub-1500 comps coloured by sqft --------------------------------------
sfSmall <- st_as_sf(dtSales[MB_total_finished_area < 1500 &
                            !is.na(longitude) & !is.na(latitude) &
                            MB_total_finished_area > 0 & structAge <= MAXAGE],
                    coords = c("longitude","latitude"), crs = 4326)

ggplot(sfSmall) +
  annotation_map_tile(type = "cartolight", zoomin = -1, progress = "none") +
  geom_sf(aes(colour = MB_total_finished_area), size = 0.7, alpha = 0.7) +
  scale_colour_viridis_c(option = "magma", direction = -1) +
  labs(title = "Sub-1500 sqft comps — sqft") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.01),
        legend.position = "right")
ggsave("text/vancouverSub1500.png")

# size distribution by use — is sub-1500 detached actually rare?
dtSales[, .N, by = actualUseDescription][order(-N)]
dtSales[MB_total_finished_area < 1500, .N, by = actualUseDescription][order(-N)]
dtSales[MB_total_finished_area < 1250, .N, by = actualUseDescription][order(-N)]
print(quantile(dtSales$MB_total_finished_area, seq(.05,1,.05), na.rm = TRUE))
q("no")

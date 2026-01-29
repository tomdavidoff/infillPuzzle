# permitsPortland.R 
# R code to grab portland permits for single family / infill areas
# Tom Davidoff started by Google AI
# 12/25/25
# Modified by Claude 12/26/25: Added ZIP/Tract toggle
# Modified by google AI to aggregate lot/date

library(data.table)
library(sf)
library(ggplot2)
library(tigris)

# ==========================================
# TOGGLE: "zip" or "tract"
# ==========================================
GEOGRAPHY <- "zip"
# ==========================================

options(timeout = 600)
options(tigris_use_cache = TRUE)

# --- PORTLAND PERMITS ---
pdx_file <- "~/OneDrive - UBC/dataRaw/portlandPermit-Search-Results.csv"

if (!file.exists(pdx_file)) {
  pdx_url <- "https://opendata.arcgis.com/api/v3/datasets/60341c09939e4407b4d135d55b0a3d6f_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1"
  download.file(pdx_url, pdx_file, method = "libcurl")
}

dtPdx <- fread(pdx_file)
setnames(dtPdx, tolower(names(dtPdx)))

# Filter for the RIP Era (2021-2025) and Major Construction
dtPdxMajor <- dtPdx[
  as.IDate(issued) >= "2021-01-01" &
  work %in% c("New Construction", "Addition")
]

# Portland Infill Logic:
dtPdxMajor[, isPlex := new_units >= 2 |
             grepl("duplex|triplex|fourplex", description, ignore.case = TRUE) |
             #(work == "Addition" & new_units > 0) |
             grepl("Townhouse", type, ignore.case = TRUE)]

dtPdxMajor <- dtPdxMajor[!is.na(x_web_mercator)]

sfPermit <- st_as_sf(dtPdxMajor,
                     coords = c("x_web_mercator", "y_web_mercator"),
                     crs = 3857)

# --- PORTLAND ZONING DISTRICTS ---
zoning_dir <- "~/OneDrive - UBC/dataRaw/portlandZoning"
shp_path <- list.files(zoning_dir, pattern = "\\.shp$", full.names = TRUE)
sfZoning <- st_read(shp_path)
names(sfZoning) <- tolower(names(sfZoning))
sfZoning <- st_transform(sfZoning, 3857)

# --- SPATIAL JOIN: PERMITS + ZONING ---
sfPermitZoned <- st_join(sfPermit, sfZoning)

# ==========================================
# GEOGRAPHY-SPECIFIC JOIN
# ==========================================

if (GEOGRAPHY == "tract") {
  sfTracts <- tracts(state = "OR", county = "Multnomah", year = 2021, cb = TRUE)
  sfTracts <- st_transform(sfTracts, 3857)
  sfFinal <- st_join(sfPermitZoned, sfTracts[, c("GEOID", "NAMELSAD")])
  dtFinal <- as.data.table(sfFinal)
  setnames(dtFinal, c("GEOID", "NAMELSAD"), c("geo_id", "geo_name"), skip_absent = TRUE)
} else {
  sfZips <- zctas(year = 2020, cb = TRUE)
  sfZips <- sfZips[grepl("^972", sfZips$ZCTA5CE20), ]
  sfZips <- st_transform(sfZips, 3857)
  sfFinal <- st_join(sfPermitZoned, sfZips[, c("ZCTA5CE20")])
  dtFinal <- as.data.table(sfFinal)
  setnames(dtFinal, "ZCTA5CE20", "geo_id", skip_absent = TRUE)
  dtFinal[, geo_name := paste("ZIP", geo_id)]
}

# ==========================================
# LOT-LEVEL COLLAPSE (The "Nakamura/Zen" fix)
# ==========================================

# 1. Filter for R-Zones
dtAnalysis <- dtFinal[grepl("^R2.5|^R5|^R7|^R10|^R20", zone)]


# 2. Collapse multiple permits per property_id into a single Lot record
# We define an 'Infill Lot' as any lot that had at least one Plex permit issued.
dtLots <- dtAnalysis[!is.na(geo_id) & !is.na(property_id), .(
  isPlexLot = any(isPlex, na.rm = TRUE),
  total_sqft_lot = sum(total_sqft, na.rm = TRUE),
  permit_count = .N
), by = .(property_id, geo_id, zone, geo_name)]

# 2a. Side project: figure out if new construction projects are different sized
dtNews <- dtAnalysis[work == "New Construction" & !is.na(geo_id) & !is.na(property_id), .(
  isPlexLot = any(isPlex, na.rm = TRUE),
  total_sqft_lot = sum(total_sqft, na.rm = TRUE),
  permit_count = .N
), by = .(property_id, geo_id, zone, geo_name)]
print(dtNews[,.(mean(total_sqft_lot),median(total_sqft_lot,by=isPlexlot)),by=isPlexLot])


# ==========================================
# GEOGRAPHY-LEVEL PROPENSITY (Based on Lots)
# ==========================================

dtGeo <- dtLots[, .(
  infill_lot_count = sum(isPlexLot, na.rm = TRUE),
  total_lots_active = .N,
  propensity = sum(isPlexLot, na.rm = TRUE) / .N
), by = .(geo_id, zone, geo_name)]

setorder(dtGeo, -total_lots_active)

# ==========================================
# SAVE & SUMMARY
# ==========================================
output_file <- sprintf("~/OneDrive - UBC/dataProcessed/portland_%s_propensity.rds", GEOGRAPHY)
saveRDS(dtGeo, output_file)

cat("\n=== LOT-LEVEL SUMMARY BY", toupper(GEOGRAPHY), "===\n")
cat("Total Active Lots (R-Zones):", nrow(dtLots), "\n")
cat("Total Infill Lots (Plexes):", sum(dtLots$isPlexLot), "\n")
cat("Overall Infill Propensity (Lot-based):",
    round(sum(dtGeo$infill_lot_count) / sum(dtGeo$total_lots_active), 4), "\n")

print(head(dtGeo, 15))

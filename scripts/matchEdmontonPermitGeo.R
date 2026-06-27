library(data.table)
library(sf)

# --- Load ---
perm <- fread("~/DropboxExternal/dataRaw/edmonton/edmontonBuildingPermits.csv")
prop <- fread("~/DropboxExternal/dataRaw/edmonton/Property_Information_(Current_Calendar_Year)_20260612.csv")

# --- Filter: RS permits, year 2024 ---
perm <- perm[ZONING == "RS" & YEAR == 2024]

# --- Normalize addresses for exact match ---
norm_addr <- function(x) {
  x <- toupper(trimws(x))
  x <- gsub("\\s+", " ", x)
  x <- gsub("[.,]", "", x)
  x
}

# Build a comparable address on the property side
prop[, prop_addr := norm_addr(paste(`House Number`, `Street Name`))]
perm[, perm_addr := norm_addr(ADDRESS)]

# --- Step 1: exact address match ---
prop_addr_dt <- prop[!is.na(prop_addr) & prop_addr != "",
                     .(prop_addr, account_addr = `Account Number`)]
# keep one property per address (first); adjust if you want all
prop_addr_dt <- unique(prop_addr_dt, by = "prop_addr")

perm <- merge(perm, prop_addr_dt, by.x = "perm_addr", by.y = "prop_addr",
              all.x = TRUE, sort = FALSE)

# --- Step 2: nearest-neighbour for unmatched (needs coords on both) ---
perm[, LATITUDE  := as.numeric(LATITUDE)]
perm[, LONGITUDE := as.numeric(LONGITUDE)]
prop[, Latitude  := as.numeric(Latitude)]
prop[, Longitude := as.numeric(Longitude)]

unmatched <- is.na(perm$account_addr) & !is.na(perm$LATITUDE) & !is.na(perm$LONGITUDE)

prop_geo <- prop[!is.na(Latitude) & !is.na(Longitude)]
prop_sf  <- st_as_sf(prop_geo, coords = c("Longitude", "Latitude"), crs = 4326)
prop_utm <- st_transform(prop_sf, 32612)   # UTM 12N (Edmonton)

if (any(unmatched)) {
  perm_sf  <- st_as_sf(perm[unmatched], coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  perm_utm <- st_transform(perm_sf, 32612)

  nn <- st_nearest_feature(perm_utm, prop_utm)
  dist_m <- as.numeric(st_distance(perm_utm, prop_utm[nn, ], by_element = TRUE))

  perm[unmatched, account_nn   := prop_geo$`Account Number`[nn]]
  perm[unmatched, nn_dist_m    := dist_m]
}

# --- Final account: address match wins, else nearest ---
perm[, matched_account := fifelse(!is.na(account_addr), account_addr, account_nn)]
perm[, match_type      := fifelse(!is.na(account_addr), "address", "nearest")]

# --- Attach full property record ---
result <- merge(perm, prop, by.x = "matched_account", by.y = "Account Number",
                all.x = TRUE, sort = FALSE)
print(result)
print(result[grepl("Row House",BUILDING_TYPE) | grepl("Single Detached",BUILDING_TYPE),summary(as.numeric(lot_size)),by=BUILDING_TYPE])

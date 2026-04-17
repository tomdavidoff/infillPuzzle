# permitsUSA.R
# Unified permit analysis for Portland and Minneapolis
# Tom Davidoff
# Consolidation by Claude, April 2026

library(data.table)
library(sf)
library(ggplot2)
library(tigris)
library(duckdb)

options(timeout = 600)
options(tigris_use_cache = TRUE)

# ==========================================
# TOGGLES
# ==========================================
GEOGRAPHY <- "tract"
CITIES <- c("minneapolis", "portland")  # run both, or subset
# ==========================================

# ==========================================
# DUCKDB CONNECTION (for ATTOM parquet reads)
# ==========================================
con <- dbConnect(duckdb())

# ==========================================
# ADDRESS NORMALIZATION
# Standardizes directional placement, type
# abbreviations, and strips unit numbers
# ==========================================
normalize_address <- function(addr) {
  x <- toupper(trimws(addr))
  x <- gsub("\\s+", " ", x)

  # Strip unit/apt suffixes: "123 MAIN ST APT 4" -> "123 MAIN ST"
  x <- sub("\\s+(APT|UNIT|STE|#|SUITE)\\s*.*$", "", x)

  # Expand common abbreviations to canonical short forms
  x <- gsub("\\bAVENUE\\b", "AVE", x)
  x <- gsub("\\bPLACE\\b", "PL", x)
  x <- gsub("\\bSTREET\\b", "ST", x)
  x <- gsub("\\bDRIVE\\b", "DR", x)
  x <- gsub("\\bLANE\\b", "LN", x)
  x <- gsub("\\bCOURT\\b", "CT", x)
  x <- gsub("\\bBOULEVARD\\b", "BLVD", x)
  x <- gsub("\\bPARKWAY\\b", "PKWY", x)
  x <- gsub("\\bTERRACE\\b", "TER", x)
  x <- gsub("\\bCIRCLE\\b", "CIR", x)
  x <- gsub("\\bROAD\\b", "RD", x)

  # Normalize directional placement:
  # Move post-number directionals (e.g. "1000 NE MAIN ST") to end ("1000 MAIN ST NE")
  # Move post-type directionals stay as-is (already canonical)
  # Pattern: number + directional + street -> number + street + directional
  dirs <- "\\b(N|S|E|W|NE|NW|SE|SW)\\b"
  # Capture: (number) (directional) (rest)
  # Only if directional is right after the house number
  x <- sub("^(\\d+)\\s+(N|S|E|W|NE|NW|SE|SW)\\s+(.+)$", "\\1 \\3 \\2", x)

  # Also handle directional before type at end: "116 E 22ND ST" -> need to move E to end
  # Pattern: (number) (dir) (ordinal/name) (type) -> (number) (ordinal/name) (type) (dir)
  x <- sub("^(\\d+)\\s+(N|S|E|W|NE|NW|SE|SW)\\s+(\\S+)\\s+(ST|AVE|DR|LN|PL|CT|WAY|BLVD|RD|TER|CIR|PKWY)$",
           "\\1 \\3 \\4 \\2", x)

  # Clean up any double spaces from transformations
  x <- gsub("\\s+", " ", trimws(x))
  x
}

# ==========================================
# CITY CONFIGURATIONS
# ==========================================
city_config <- list(
  minneapolis = list(
    city_name    = "Minneapolis",
    crs          = 26915,
    # --- permit data ---
    permit_file  = "~/DropboxExternal/dataRaw/shp_econ_residential_building_permts/ResidentialPermits.shp",
    permit_type  = "shp",
    # --- permit filters ---
    filter_permits = function(dt) {
      dt <- dt[year >= 2020 & year <= 2025]
      qmin <- quantile(dt[res_permit=="NU", permit_val], 0.2, na.rm = TRUE)
      dt <- dt[res_permit == "NU" | permit_val >= qmin]
      dt <- dt[ctu_name == "Minneapolis"]
      dt <- dt[(co_code == "053") | (co_code == 53)]
      dt
    },
    # --- plex logic ---
    flag_plex = function(dt) {
      dt[, isPlex := units >= 2 |
           !housing_ty %in% c("SFD") |
           grepl("duplex|triplex|fourplex|twinhome|townhouse",
                 housing__1, ignore.case = TRUE)]
    },
    # --- zoning ---
    zoning_file  = "~/DropboxExternal/dataRaw/Planning_Primary_Zoning/Planning_Primary_Zoning.shp",
    zone_field   = "land_use_c",
    zone_filter  = "UN1|UN2|UN3",
    zone_plot_values = c("UN1", "UN2", "UN3"),
    topZone = "UN3",
    zone_colors  = c("UN1" = "lightyellow", "UN2" = "lightblue", "UN3" = "lightgreen"),
    # --- parcel id ---
    parcel_id    = "address",
    # --- units field ---
    units_field  = "units",
    # --- geography ---
    tract_state  = "MN",
    tract_county = "Hennepin",
    zip_pattern  = "^55[34]",
    # --- ATTOM lot size (assessor history — better address matching for Minneapolis) ---
    ah_parquet = "~/DropboxExternal/dataProcessed/AH_state_MN.parquet",
    contemp_parquet = "~/DropboxExternal/dataProcessed/by_property_states/tax_assessor_state_MN.parquet",
    attom_city_filter = "MINNEAPOLIS",
    permit_address_field = "address",
    mandate_thresholds = NULL
  ),

  portland = list(
    city_name    = "Portland",
    crs          = 3857,
    # --- permit data ---
    permit_file  = "~/DropboxExternal/dataRaw/portlandPermit-Search-Results.csv",
    permit_type  = "csv",
    csv_coords   = c("x_web_mercator", "y_web_mercator"),
    # --- permit filters ---
    filter_permits = function(dt) {
      dt <- dt[as.IDate(issued) >= "2021-01-01"]
      qmin <- quantile(dt[work == "New Construction", total_sqft], 0.2, na.rm = TRUE)
      dt <- dt[work == "New Construction" | total_sqft >= qmin]
      dt <- dt[!is.na(x_web_mercator)]
      dt
    },
    # --- plex logic ---
    flag_plex = function(dt) {
      dt[, isPlex := new_units >= 2 |
           grepl("duplex|triplex|fourplex", description, ignore.case = TRUE) |
           grepl("Townhouse", type, ignore.case = TRUE)]
    },
    # --- zoning ---
    zoning_file  = "~/DropboxExternal/dataRaw/portlandZoning",
    zoning_is_dir = TRUE,
    zone_field   = "zone",
    zone_filter  = "^R2.5|^R5|^R7|^R10|^R20",
    zone_plot_values = c("R2.5", "R5", "R7", "R10", "R20"),
    topZone = "R2.5",
    zone_colors  = c("R2.5" = "lightyellow", "R5" = "lightblue",
                      "R7" = "lightgreen", "R10" = "wheat", "R20" = "lavender"),
    # --- parcel id ---
    parcel_id    = "property_id",
    # --- units field ---
    units_field  = "new_units",
    # --- geography ---
    tract_state  = "OR",
    tract_county = "Multnomah",
    zip_pattern  = "^972",
    # --- ATTOM lot size (4-step cascade: AH address → AH normalized → contemp→AH ID → contemp latlon→AH) ---
    ah_parquet = "~/DropboxExternal/dataProcessed/AH_state_OR.parquet",
    contemp_parquet = "~/DropboxExternal/dataProcessed/by_property_states/tax_assessor_state_OR.parquet",
    attom_city_filter = "PORTLAND",
    permit_address_field = "address",
    # --- mandate thresholds: lots >= this size MUST build multiplex, flag them ---
    mandate_thresholds = list(
      "R7"  = 14000,
      "R5"  = 10000,
      "R2.5" = 5000
    )
  )
)

# ==========================================
# MAIN LOOP
# ==========================================
results <- list()

for (CITY in CITIES) {
  cfg <- city_config[[CITY]]
  message("\n========== Processing: ", cfg$city_name, " ==========\n")

  # ------------------------------------------
  # 1. READ PERMITS
  # ------------------------------------------
  if (cfg$permit_type == "shp") {
    sfPermit <- st_read(cfg$permit_file)
    names(sfPermit) <- tolower(names(sfPermit))
    sfPermit <- st_transform(sfPermit, cfg$crs)
    dtPermit <- as.data.table(sfPermit)
  } else {
    dtPermit <- fread(cfg$permit_file)
    setnames(dtPermit, tolower(names(dtPermit)))
  }
  print("HERE ARE THE PERMIT FIELDS")
  print(names(dtPermit))

  # ------------------------------------------
  # 2. FILTER PERMITS
  # ------------------------------------------
  dtPermit <- cfg$filter_permits(dtPermit)

  # ------------------------------------------
  # 3. FLAG PLEX
  # ------------------------------------------
  cfg$flag_plex(dtPermit)

  message("Permits after filtering: ", nrow(dtPermit))

  # ------------------------------------------
  # 4. MERGE ATTOM LOT SIZE — 4-STEP CASCADE
  #    Goal: get pre-split lot size from AH file.
  #    Step 1: AH raw address match
  #    Step 2: AH normalized address match
  #    Step 3: Permit→contemporary(address)→ATTOM ID→AH
  #    Step 4: Permit→contemporary(lat/lon nearest)→ATTOM ID→AH
  # ------------------------------------------
  if (!is.null(cfg$ah_parquet)) {
    message("=== 4-step ATTOM lot size cascade ===")

    # --- Load AH: earliest year per ATTOM ID ---
    message("Loading AH file...")
    dtAH <- as.data.table(dbGetQuery(con, sprintf(
      "SELECT SA_SITE_HOUSE_NBR, SA_SITE_DIR, SA_SITE_STREET_NAME,
              SA_SITE_SUF, SA_SITE_POST_DIR, SA_LOTSIZE,
              AH_HISTORY_YR, \"[ATTOM ID]\"
       FROM read_parquet('%s')
       WHERE SA_SITE_CITY = '%s'",
      cfg$ah_parquet, cfg$attom_city_filter
    )))
    dtAH[, SA_LOTSIZE := as.numeric(SA_LOTSIZE)]
    dtAH <- dtAH[!is.na(SA_LOTSIZE) & SA_LOTSIZE > 0]
    setorder(dtAH, `[ATTOM ID]`, AH_HISTORY_YR)
    dtAH <- dtAH[!duplicated(`[ATTOM ID]`)]

    # Build AH raw address (directionals at end)
    dtAH[, addrRaw := toupper(trimws(paste(
      fifelse(is.na(SA_SITE_HOUSE_NBR), "", SA_SITE_HOUSE_NBR),
      fifelse(is.na(SA_SITE_STREET_NAME), "", SA_SITE_STREET_NAME),
      fifelse(is.na(SA_SITE_SUF), "", SA_SITE_SUF),
      fifelse(is.na(SA_SITE_POST_DIR), "", SA_SITE_POST_DIR),
      fifelse(is.na(SA_SITE_DIR), "", SA_SITE_DIR)
    )))]
    dtAH[, addrRaw := gsub("\\s+", " ", trimws(addrRaw))]

    # Also build normalized version
    dtAH[, addrNorm := normalize_address(addrRaw)]

    # AH keyed by ATTOM ID for later lookups
    ahByID <- dtAH[, .(`[ATTOM ID]`, SA_LOTSIZE)]
    setkey(ahByID, `[ATTOM ID]`)

    message(sprintf("AH: %d unique parcels for %s", nrow(dtAH), cfg$attom_city_filter))

    # --- Build permit address keys ---
    dtPermit[, addrRaw := toupper(trimws(gsub("\\s+", " ", get(cfg$permit_address_field))))]
    dtPermit[, addrNorm := normalize_address(get(cfg$permit_address_field))]
    dtPermit[, AreaLotSF := NA_real_]
    dtPermit[, matchStep := NA_character_]

    # ==========================================
    # STEP 1: AH raw address match
    # ==========================================
    ahAddr1 <- dtAH[!duplicated(addrRaw), .(addrRaw, lotSF_1 = SA_LOTSIZE)]
    dtPermit <- merge(dtPermit, ahAddr1, by = "addrRaw", all.x = TRUE)
    matched1 <- !is.na(dtPermit$lotSF_1)
    dtPermit[matched1 & is.na(AreaLotSF), `:=`(AreaLotSF = lotSF_1, matchStep = "1_AH_raw")]
    dtPermit[, lotSF_1 := NULL]
    message(sprintf("Step 1 (AH raw address): %d matched", sum(matched1)))

    # ==========================================
    # STEP 2: AH normalized address match
    # ==========================================
    ahAddr2 <- dtAH[!duplicated(addrNorm), .(addrNorm, lotSF_2 = SA_LOTSIZE)]
    dtPermit <- merge(dtPermit, ahAddr2, by = "addrNorm", all.x = TRUE)
    matched2 <- !is.na(dtPermit$lotSF_2) & is.na(dtPermit$matchStep)
    dtPermit[matched2, `:=`(AreaLotSF = lotSF_2, matchStep = "2_AH_norm")]
    dtPermit[, lotSF_2 := NULL]
    message(sprintf("Step 2 (AH normalized): %d new matches", sum(matched2)))

    # ==========================================
    # STEP 3: Contemporary address → ATTOM ID → AH
    # ==========================================
    if (!is.null(cfg$contemp_parquet)) {
      message("Loading contemporary file for ATTOM ID bridge...")
      dtContemp <- as.data.table(dbGetQuery(con, sprintf(
        "SELECT \"[ATTOM ID]\", PropertyAddressFull, PropertyLatitude, PropertyLongitude
         FROM read_parquet('%s') WHERE PropertyAddressCity = '%s'",
        cfg$contemp_parquet, cfg$attom_city_filter
      )))
      dtContemp[, addrNorm := normalize_address(PropertyAddressFull)]
      dtContemp[, PropertyLatitude := as.numeric(PropertyLatitude)]
      dtContemp[, PropertyLongitude := as.numeric(PropertyLongitude)]

      # Match permit normalized address → contemporary → get ATTOM ID → look up in AH
      still_unmatched <- is.na(dtPermit$matchStep)
      if (sum(still_unmatched) > 0) {
        # Join unmatched permits to contemporary on normalized address
        contempAddr <- dtContemp[!duplicated(addrNorm), .(addrNorm, contempID = `[ATTOM ID]`)]
        dtPermit <- merge(dtPermit, contempAddr, by = "addrNorm", all.x = TRUE)

        # Look up contemporary ATTOM ID in AH
        dtPermit[, ahLotSF_3 := ahByID[.(`[ATTOM ID]` = contempID), SA_LOTSIZE]]
        matched3 <- !is.na(dtPermit$ahLotSF_3) & is.na(dtPermit$matchStep)
        dtPermit[matched3, `:=`(AreaLotSF = ahLotSF_3, matchStep = "3_contemp_addr_AH")]
        dtPermit[, c("contempID", "ahLotSF_3") := NULL]
        message(sprintf("Step 3 (contemp address → AH): %d new matches", sum(matched3)))
      }

      # ==========================================
      # STEP 4: Lat/lon nearest → contemporary → ATTOM ID → AH
      # ==========================================
      still_unmatched <- is.na(dtPermit$matchStep)
      if (sum(still_unmatched) > 0 && cfg$permit_type == "csv") {
        message("Step 4: spatial nearest-neighbor match...")

        # Get permit coordinates (need to convert from CRS to lat/lon if needed)
        # Portland permits have x_web_mercator, y_web_mercator
        unmatchedPermits <- dtPermit[still_unmatched & !is.na(get(cfg$csv_coords[1]))]

        if (nrow(unmatchedPermits) > 0) {
          # Convert permit coords to WGS84 lat/lon
          sfUnmatched <- st_as_sf(unmatchedPermits,
                                  coords = cfg$csv_coords, crs = cfg$crs)
          sfUnmatched <- st_transform(sfUnmatched, 4326)
          coordsPermit <- st_coordinates(sfUnmatched)
          unmatchedPermits[, `:=`(pLon = coordsPermit[,1], pLat = coordsPermit[,2])]

          # Contemporary parcels with valid coords and AH lot size
          dtContempWithAH <- dtContemp[!is.na(PropertyLatitude) & !is.na(PropertyLongitude)]
          dtContempWithAH <- merge(dtContempWithAH,
                                   ahByID[, .(contempID = `[ATTOM ID]`, ahLotSF = SA_LOTSIZE)],
                                   by.x = "[ATTOM ID]", by.y = "contempID", all.x = FALSE)

          if (nrow(dtContempWithAH) > 0) {
            # Simple nearest-neighbor: for each unmatched permit, find closest contemp parcel
            # Using Euclidean distance on lat/lon (fine for small areas within a city)
            sfContemp <- st_as_sf(dtContempWithAH,
                                  coords = c("PropertyLongitude", "PropertyLatitude"),
                                  crs = 4326)

            sfUnmatched2 <- st_as_sf(unmatchedPermits, coords = c("pLon", "pLat"), crs = 4326)
            nn <- st_nearest_feature(sfUnmatched2, sfContemp)
            nn_dist <- as.numeric(st_distance(sfUnmatched2, sfContemp[nn,], by_element = TRUE))

            # Only accept matches within 50 meters
            close_enough <- nn_dist <= 20
            unmatchedPermits[close_enough, `:=`(
              AreaLotSF = dtContempWithAH$ahLotSF[nn[close_enough]],
              matchStep = "4_latlon_AH"
            )]

            # Write back to dtPermit
            matchIdx <- which(still_unmatched)[which(
              dtPermit[still_unmatched, get(cfg$csv_coords[1])] %in% unmatchedPermits[, get(cfg$csv_coords[1])]
            )]
            # Simpler: use addrRaw as key to write back
            for (i in which(close_enough)) {
              ar <- unmatchedPermits$addrRaw[i]
              dtPermit[addrRaw == ar & is.na(matchStep),
                       `:=`(AreaLotSF = unmatchedPermits$AreaLotSF[i],
                            matchStep = "4_latlon_AH")]
            }

            message(sprintf("Step 4 (lat/lon → contemp → AH, <=20m): %d new matches", sum(close_enough)))
          }
        }
      } else if (sum(still_unmatched) > 0 && cfg$permit_type == "shp") {
        # Minneapolis: permits already have geometry
        message("Step 4: spatial nearest-neighbor match (shp)...")
        unmatchedPermits <- dtPermit[still_unmatched]

        if (nrow(unmatchedPermits) > 0) {
          # Get permit lat/lon from geometry
          sfUnmatched <- st_as_sf(unmatchedPermits)
          sfUnmatched <- st_transform(sfUnmatched, 4326)
          coordsPermit <- st_coordinates(sfUnmatched)
          unmatchedPermits[, `:=`(pLon = coordsPermit[,1], pLat = coordsPermit[,2])]

          dtContempWithAH <- dtContemp[!is.na(PropertyLatitude) & !is.na(PropertyLongitude)]
          dtContempWithAH <- merge(dtContempWithAH,
                                   ahByID[, .(contempID = `[ATTOM ID]`, ahLotSF = SA_LOTSIZE)],
                                   by.x = "[ATTOM ID]", by.y = "contempID", all.x = FALSE)

          if (nrow(dtContempWithAH) > 0) {
            sfContemp <- st_as_sf(dtContempWithAH,
                                  coords = c("PropertyLongitude", "PropertyLatitude"),
                                  crs = 4326)
            sfUnmatched2 <- st_as_sf(unmatchedPermits, coords = c("pLon", "pLat"), crs = 4326)
            nn <- st_nearest_feature(sfUnmatched2, sfContemp)
            nn_dist <- as.numeric(st_distance(sfUnmatched2, sfContemp[nn,], by_element = TRUE))

            close_enough <- nn_dist <= 20
            for (i in which(close_enough)) {
              ar <- unmatchedPermits$addrRaw[i]
              dtPermit[addrRaw == ar & is.na(matchStep),
                       `:=`(AreaLotSF = dtContempWithAH$ahLotSF[nn[i]],
                            matchStep = "4_latlon_AH")]
            }
            message(sprintf("Step 4 (lat/lon → contemp → AH, <=20m): %d new matches", sum(close_enough)))
          }
        }
      }
    }

    # ==========================================
    # SUMMARY
    # ==========================================
    n_total <- nrow(dtPermit)
    n_matched <- sum(!is.na(dtPermit$matchStep))
    message(sprintf("\n=== MATCH SUMMARY for %s ===", cfg$city_name))
    message(sprintf("Total matched: %d of %d (%.1f%%)", n_matched, n_total, n_matched/n_total*100))
    cat("By step:\n")
    print(dtPermit[, .N, by = matchStep][order(matchStep)])
    message("")
  }

  # ------------------------------------------
  # 5. CONVERT TO SF
  # ------------------------------------------
  if (cfg$permit_type == "csv") {
    sfPermit <- st_as_sf(dtPermit,
                         coords = cfg$csv_coords,
                         crs = cfg$crs)
  } else {
    sfPermit <- st_as_sf(dtPermit)
    sfPermit <- st_transform(sfPermit, cfg$crs)
  }

  # ------------------------------------------
  # 6. READ ZONING & SPATIAL JOIN
  # ------------------------------------------
  if (isTRUE(cfg$zoning_is_dir)) {
    shp_path <- list.files(cfg$zoning_file, pattern = "\\.shp$", full.names = TRUE)
    sfZoning <- st_read(shp_path)
  } else {
    sfZoning <- st_read(cfg$zoning_file)
  }
  names(sfZoning) <- tolower(names(sfZoning))
  sfZoning <- st_transform(sfZoning, cfg$crs)

  message("Zoning fields: ", paste(names(sfZoning), collapse = ", "))

  sfPermitZoned <- st_join(sfPermit, sfZoning)

  # ------------------------------------------
  # 7. GEOGRAPHY JOIN (TRACT or ZIP)
  # ------------------------------------------
  if (GEOGRAPHY == "tract") {
    message("Fetching census tracts...")
    sfTracts <- tracts(state = cfg$tract_state, county = cfg$tract_county,
                       year = 2021, cb = TRUE)
    sfTracts <- st_transform(sfTracts, cfg$crs)
    sfFinal <- st_join(sfPermitZoned, sfTracts[, c("GEOID", "NAMELSAD")])
    sfTopZone <- st_union(sfZoning[sfZoning[[cfg$zone_field]] == cfg$topZone, ])
    sfFinal$dist_to_topZone <- as.numeric(st_distance(sfFinal, sfTopZone))
    print("TOPZONE")
    print(summary(sfFinal$dist_to_topZone))
    dtFinal <- as.data.table(sfFinal)
    setnames(dtFinal, c("GEOID", "NAMELSAD"), c("geo_id", "geo_name"),
             skip_absent = TRUE)
  } else {
    message("Fetching ZCTAs...")
    sfZips <- zctas(year = 2020, cb = TRUE)
    sfZips <- sfZips[grepl(cfg$zip_pattern, sfZips$ZCTA5CE20), ]
    sfZips <- st_transform(sfZips, cfg$crs)
    sfFinal <- st_join(sfPermitZoned, sfZips[, c("ZCTA5CE20")])
    sfTopZone <- st_union(sfZoning[sfZoning[[cfg$zone_field]] == cfg$topZone, ])
    sfFinal$dist_to_topZone <- as.numeric(st_distance(sfFinal, sfTopZone))
    print("TOPZONE")
    print(summary(sfFinal$dist_to_topZone))
    dtFinal <- as.data.table(sfFinal)
    setnames(dtFinal, "ZCTA5CE20", "geo_id", skip_absent = TRUE)
    dtFinal[, geo_name := paste("ZIP", geo_id)]
  }

  # ------------------------------------------
  # 8. FILTER TO RESIDENTIAL ZONES
  # ------------------------------------------
  zone_col <- cfg$zone_field
  dtAnalysis <- dtFinal[grepl(cfg$zone_filter, get(zone_col), ignore.case = TRUE)]
  message("Permits in residential zones: ", nrow(dtAnalysis))

  # ------------------------------------------
  # 8b. FLAG MANDATED MULTIPLEX LOTS (Portland)
  #     Lots above zone-specific thresholds MUST
  #     build multiplex -- flag rather than drop
  #     so we can control for it in analysis.
  # ------------------------------------------
  dtAnalysis[, isMandated := FALSE]
  if (!is.null(cfg$mandate_thresholds)) {
    for (z in names(cfg$mandate_thresholds)) {
      thresh <- cfg$mandate_thresholds[[z]]
      mask <- dtAnalysis[[zone_col]] == z & !is.na(dtAnalysis$AreaLotSF) & as.numeric(dtAnalysis$AreaLotSF) >= thresh
      n_mandated <- sum(mask)
      message(sprintf("  Flagging %d mandated lots in %s (>= %d sqft)", n_mandated, z, thresh))
      dtAnalysis[mask, isMandated := TRUE]
    }
    message(sprintf("Total mandated lots: %d of %d (%.1f%%)",
                    sum(dtAnalysis$isMandated), nrow(dtAnalysis),
                    mean(dtAnalysis$isMandated) * 100))
    # Summary: mandate status by zone and plex
    cat("\nMandate status by zone:\n")
    print(dtAnalysis[, .(n = .N, n_plex = sum(isPlex, na.rm = TRUE),
                         plex_share = round(mean(isPlex, na.rm = TRUE), 3)),
                     by = .(zone = get(zone_col), isMandated)])
  }

  # ------------------------------------------
  # 9. EXTRACT COORDINATES & COLLAPSE TO LOTS
  # ------------------------------------------
  dtAnalysis[, `:=`(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  )]

  pid <- cfg$parcel_id
  lot_vars <- c(pid, "geo_id", zone_col, "geo_name")

  if (!"AreaLotSF" %in% names(dtAnalysis)) dtAnalysis[, AreaLotSF := NA_real_]
  if (!"isMandated" %in% names(dtAnalysis)) dtAnalysis[, isMandated := FALSE]

  # Create permit year (Portland: from issued date; Minneapolis: already has year)
  if ("issued" %in% names(dtAnalysis)) {
    dtAnalysis[, permit_year := as.integer(format(as.IDate(issued), "%Y"))]
  } else if ("year" %in% names(dtAnalysis)) {
    dtAnalysis[, permit_year := as.integer(year)]
  }

  dtLots <- dtAnalysis[!is.na(geo_id) & !is.na(get(pid)), .(
    isPlexLot    = any(isPlex, na.rm = TRUE),
    isMandated   = any(isMandated, na.rm = TRUE),
    total_units  = sum(get(cfg$units_field), na.rm = TRUE),
    distTopZone  = min(dist_to_topZone, na.rm = TRUE),
    AreaLotSF    = AreaLotSF[1],
    permit_year  = min(permit_year, na.rm = TRUE),
    permit_count = .N,
    lon = lon[1],
    lat = lat[1]
  ), by = lot_vars]
  saveRDS(dtLots, sprintf("~/DropboxExternal/dataProcessed/%s_lot_level.rds", CITY))

  sfLots <- st_as_sf(dtLots, coords = c("lon", "lat"), crs = cfg$crs)

  message("Total lots with permits: ", nrow(dtLots))
  message("Lots with plex permits: ", sum(dtLots$isPlexLot))

  # ------------------------------------------
  # 10. PLOT
  # ------------------------------------------
  sfZoningRes <- sfZoning[sfZoning[[zone_col]] %in% cfg$zone_plot_values, ]

  p <- ggplot() +
    geom_sf(data = sfZoningRes,
            aes(fill = .data[[zone_col]]),
            alpha = 0.5, color = "grey50", linewidth = 0.1) +
    geom_sf(data = sfLots,
            aes(color = isPlexLot, size = isPlexLot), shape = 16) +
    scale_fill_manual(values = cfg$zone_colors, name = "Zone") +
    scale_color_manual(
      values = c("FALSE" = "grey50", "TRUE" = "red"),
      labels = c("FALSE" = "Non-Plex", "TRUE" = "Plex"),
      name = "Permit Type"
    ) +
    scale_size_manual(
      values = c("FALSE" = 1, "TRUE" = 1.5),
      labels = c("FALSE" = "Non-Plex", "TRUE" = "Plex"),
      name = "Permit Type"
    ) +
    labs(title = sprintf("%s New Construction Permits by Zoning District",
                         cfg$city_name)) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    guides(
      fill = guide_legend(order = 1),
      color = guide_legend(order = 2, override.aes = list(size = 3)),
      size = "none"
    )

  plot_file <- sprintf("text/permitLocation%s.png", cfg$city_name)
  ggsave(plot_file, p, width = 10, height = 8, dpi = 300)
  message("Saved plot: ", plot_file)

  # ------------------------------------------
  # 11. GEOGRAPHY-LEVEL SUMMARY
  # ------------------------------------------
  dtGeo <- dtLots[, .(
    infill_lot_count  = sum(isPlexLot, na.rm = TRUE),
    total_lots_active = .N,
    total_units       = sum(total_units, na.rm = TRUE),
    propensity        = sum(isPlexLot, na.rm = TRUE) / .N
  ), by = c("geo_id", zone_col, "geo_name")]

  setorder(dtGeo, -total_lots_active)

  output_file <- sprintf("~/DropboxExternal/dataProcessed/%s_%s_propensity.rds",
                          CITY, GEOGRAPHY)
  saveRDS(dtGeo, output_file)

  cat(sprintf("\n=== %s LOT-LEVEL SUMMARY BY %s ===\n",
              toupper(cfg$city_name), toupper(GEOGRAPHY)))
  cat("Total Active Lots:", nrow(dtLots), "\n")
  cat("Total Infill Lots:", sum(dtLots$isPlexLot), "\n")
  cat("Overall Propensity:",
      round(sum(dtGeo$infill_lot_count) / sum(dtGeo$total_lots_active), 4), "\n")
  print(head(dtGeo, 15))

  # Store for cross-city comparison
  results[[CITY]] <- list(dtLots = dtLots, dtGeo = dtGeo,
                          sfLots = sfLots, sfZoning = sfZoning)
}

# ==========================================
# CLEANUP
# ==========================================
dbDisconnect(con, shutdown = TRUE)

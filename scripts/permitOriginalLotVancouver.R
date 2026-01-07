# ------------------------------------------------------------
# Vancouver infill puzzle: Permits x Parcels x BCA(2016)
# Output per original lot:
#   (a) outcome ∈ {sf_rebuild_or_upgrade, duplex, multiplex}
#   (b) lot_size_original = landWidth * landDepth (ignore land_area)
#
# Requires: data.table, DBI, RSQLite, jsonlite, stringi
# ------------------------------------------------------------

library(data.table)
library(DBI)
library(RSQLite)
library(jsonlite)
library(stringi)

# ---- paths (edit if needed)
dir_raw <- "~/OneDrive - UBC/dataRaw"
dir_bca <- "~/OneDrive - UBC/Documents/data/bca"
f_perm  <- file.path(dir_raw, "vancouver_permits_full.csv")
f_parc  <- file.path(dir_raw, "property-parcel-polygons.geojson")
f_bca   <- file.path(dir_bca, "REVD16_and_inventory_extracts.sqlite3")
f_out   <- "lot_outcomes_lotsize_2016.csv"

# ---------------------------
# helpers: normalization
# ---------------------------
norm_str <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- toupper(trimws(x))
  x <- stri_replace_all_regex(x, "[^A-Z0-9 ]+", " ")
  x <- stri_replace_all_regex(x, "\\s+", " ")
  trimws(x)
}

numify <- function(x) suppressWarnings(as.numeric(gsub(",", "", x)))

extract_civic_num <- function(addr) {
  as.integer(stri_extract_first_regex(addr, "\\b\\d{1,6}\\b")) }

extract_street_chunk <- function(addr) {
  # remove leading civic number + trailing city/province bits
  a <- norm_str(addr)
  a <- stri_replace_first_regex(a, "^\\s*\\d{1,6}\\s+", "")
  a <- stri_replace_all_regex(a, ",\\s*VANCOUVER.*$", "")
  a <- stri_replace_all_regex(a, "\\s+", " ")
  trimws(a)
}

street_type_list <- c(
  "ST","AVE","AV","BLVD","RD","DR","CRES","WAY","PL","LANE","LN",
  "PKWY","HWY","CIR","CT","SQ","TER","TRL","TRAIL","WALK","MEWS"
)

split_street_name_type <- function(street_chunk) {
  s <- norm_str(street_chunk)
  toks <- strsplit(s, " ", fixed = TRUE)
  out_name <- character(length(s)); out_type <- character(length(s))
  for (i in seq_along(s)) {
    tt <- toks[[i]]
    if (length(tt) == 0 || (length(tt) == 1 && tt == "")) {
      out_name[i] <- NA_character_; out_type[i] <- NA_character_; next
    }
    last <- tt[length(tt)]
    if (last %in% street_type_list) {
      out_type[i] <- last
      out_name[i] <- paste(tt[-length(tt)], collapse = " ")
    } else {
      out_type[i] <- NA_character_
      out_name[i] <- paste(tt, collapse = " ")
    }
  }
  list(streetName = out_name, streetType = out_type)
}

# ---------------------------
# outcome classifier (editable)
# ---------------------------
classify_outcome <- function(propertyuse, usecat, desc, work) {
  pu <- norm_str(propertyuse)
  uc <- norm_str(usecat)
  ds <- norm_str(desc)
  wk <- norm_str(work)

  # Multiplex signals
  is_multi_text <- stri_detect_regex(ds, "(FOURPLEX|TRIPLEX|MULTIPLEX|MULTI\\s*FAMILY|MULTIPLE\\s*DWELLING|TOWNHOUSE|ROW\\s*HOUSE)") |
                   stri_detect_regex(uc, "(MULTIPLEX|TOWNHOUSE|ROW)") |
                   stri_detect_regex(pu, "(MULTI\\s*FAMILY|APARTMENT|MULTIPLE\\s*DWELLING)")

  # Duplex signals
  is_duplex_text <- stri_detect_regex(ds, "\\bDUPLEX\\b|TWO\\s*FAMILY") |
                    stri_detect_regex(uc, "\\bDUPLEX\\b") |
                    stri_detect_regex(pu, "\\bDUPLEX\\b")

  # Single-family rebuild/upgrade (broad)
  is_sf_text <- stri_detect_regex(pu, "(SINGLE\\s*FAMILY|ONE\\s*FAMILY|DWELLING)") &
               stri_detect_regex(wk, "(NEW|ADDITION|ALTERATION|RENOVATION|REPAIR|CONVERSION)")

  out <- ifelse(is_multi_text, "multiplex",
         ifelse(is_duplex_text, "duplex",
         ifelse(is_sf_text, "sf_rebuild_or_upgrade", NA_character_)))
  out
}

# ============================================================
# 1) Load permits (CSV)
# ============================================================
perm <- fread(f_perm, sep = ";", na.strings = c("", "NA"))

library(stringi)
library(data.table)

norm_str <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- toupper(trimws(x))
  x <- stri_replace_all_regex(x, "[^A-Z0-9 ]+", " ")
  x <- stri_replace_all_regex(x, "\\s+", " ")
  trimws(x)
}

# common street type normalization
norm_street_types <- function(x) {
  x <- norm_str(x)
  x <- stri_replace_all_regex(x, "\\bSTREET\\b", "ST")
  x <- stri_replace_all_regex(x, "\\bAVENUE\\b", "AV")
  x <- stri_replace_all_regex(x, "\\bBOULEVARD\\b", "BLVD")
  x <- stri_replace_all_regex(x, "\\bROAD\\b", "RD")
  x <- stri_replace_all_regex(x, "\\bDRIVE\\b", "DR")
  x <- stri_replace_all_regex(x, "\\bCRESCENT\\b", "CRES")
  x <- stri_replace_all_regex(x, "\\bLANE\\b", "LN")
  x <- stri_replace_all_regex(x, "\\bPLACE\\b", "PL")
  x <- stri_replace_all_regex(x, "\\bCOURT\\b", "CT")
  x <- stri_replace_all_regex(x, "\\bTERRACE\\b", "TER")
  x <- stri_replace_all_regex(x, "\\s+", " ")
  trimws(x)
}

# Extract civic number (handles ranges like 1234-1238)
extract_civic_num <- function(addr) {
  a <- norm_str(addr)
  as.integer(stri_extract_first_regex(a, "\\b\\d{1,6}\\b"))
}

# Clean a full Vancouver address down to "STREET PART" only
# Removes: unit numbers, VANCOUVER/BC, postal codes, etc.
extract_street_only <- function(addr) {
  a <- norm_str(addr)

  # remove leading civic number (and possible range tail)
  a <- stri_replace_first_regex(a, "^\\s*\\d{1,6}(\\s*\\-\\s*\\d{1,6})?\\s+", "")

  # drop trailing city/province/postal patterns
  a <- stri_replace_all_regex(a, "\\bVANCOUVER\\b.*$", "")
  a <- stri_replace_all_regex(a, "\\bBC\\b.*$", "")
  a <- stri_replace_all_regex(a, "\\bV[0-9][A-Z]\\s*[0-9][A-Z][0-9]\\b.*$", "")

  # if a unit number remains at end (e.g., "... STREET 109"), drop it
  a <- stri_replace_all_regex(a, "\\s+\\d{1,6}$", "")

  # normalize street types
  a <- norm_street_types(a)

  a
}

# Apply to permits table `perm`
perm[, civic_number := extract_civic_num(address)]
perm[, street_name := extract_street_only(address)]
perm[, addr_key1 := paste(civic_number, street_name)]

# required fields per your header: address, propertyuse, specificusecategory, typeofwork,
# projectdescription, issueyear, issuedate, geolocalarea, etc.
perm[, civic_number := extract_civic_num(address)]
perm[, street_chunk := extract_street_chunk(address)]
tmp <- split_street_name_type(perm$street_chunk)
perm[, street_name := tmp$streetName]
perm[, street_type := tmp$streetType]

# keys for matching
perm[, addr_key1 := paste(civic_number, street_name)]                  # no type
perm[, addr_key2 := paste(civic_number, street_name, norm_str(street_type))]

# classify outcome
perm[, outcome := classify_outcome(propertyuse, specificusecategory, projectdescription, typeofwork)]

# keep only classified records
perm_evt <- perm[!is.na(outcome) & civic_number > 0 & nzchar(street_name),
                 .(permitnumber, issuedate, issueyear, geolocalarea,
                   civic_number, street_name, street_type, addr_key1, addr_key2, outcome)]

# If multiple permits at same address, keep earliest per outcome (you can change this rule)
setorder(perm_evt, civic_number, street_name, issuedate)
perm_evt <- perm_evt[, .SD[1], by = .(addr_key1, outcome)]

# ============================================================
# 2) Load parcel fabric (GeoJSON) via jsonlite (no GDAL needed)
#    properties: civic_number, streetname, site_id, tax_coord, geo_point_2d
# ============================================================
parc_json <- fromJSON(f_parc, simplifyDataFrame = TRUE)
parc <- as.data.table(parc_json$features$properties)

# normalize parcel address fields
parc[, civic_number := as.integer(civic_number)]
parc[, street_name := norm_str(streetname)]
parc[, addr_key1 := paste(civic_number, street_name)]

# de-dup parcels on addr_key1; keep first (or prefer smallest site_id, etc.)
setkey(parc, addr_key1)
parc_uniq <- parc[!is.na(civic_number) & civic_number > 0 & nzchar(street_name),
                  .SD[1], by = addr_key1]

# ============================================================
# 3) Permits -> Parcels (address bridge)
# ============================================================
setkey(perm_evt, addr_key1)
setkey(parc_uniq, addr_key1)
perm_parc <- parc_uniq[perm_evt, on = "addr_key1", nomatch = 0L]

# ============================================================
# 4) Pull BCA(2016) tables from SQLite
#    We use address -> folioID -> rollNumber -> (width, depth) from:
#      residentialInventory.land_width/land_depth
#      folioDescription.landWidth/landDepth as fallback
# ============================================================
con <- dbConnect(RSQLite::SQLite(), f_bca)

bca_addr <- as.data.table(dbGetQuery(con, "
  SELECT folioID, streetNumber, streetName, streetType, city, province, postalCode, primaryFlag
  FROM address
"))

bca_folio <- as.data.table(dbGetQuery(con, "
  SELECT folioID, rollNumber FROM folio
"))

# width/depth from both sources; rollNumber aligns them
bca_dims <- as.data.table(dbGetQuery(con, "
  SELECT
    ri.roll_number AS roll_number,
    ri.land_width  AS land_width_ri,
    ri.land_depth  AS land_depth_ri,
    fd.landWidth   AS land_width_fd,
    fd.landDepth   AS land_depth_fd
  FROM residentialInventory ri
  LEFT JOIN folio f
    ON ri.roll_number = f.rollNumber
  LEFT JOIN folioDescription fd
    ON fd.folioID = f.folioID
"))


# optional PID (nice stable ID if present)
bca_pid <- as.data.table(dbGetQuery(con, "
  SELECT folioID, PID
  FROM legalDescription
"))

dbDisconnect(con)

# build BCA address keys
bca_addr[, civic_number := as.integer(streetNumber)]
bca_addr[, street_name  := norm_str(streetName)]
bca_addr[, street_type  := norm_str(streetType)]
bca_addr[, addr_key1 := paste(civic_number, street_name)]
bca_addr[, addr_key2 := paste(civic_number, street_name, street_type)]

# ============================================================
# 5) Match permits/parcels -> BCA address (with and without street type)
# ============================================================
# Normalize permit street type and create addr_key2
perm_parc[, street_type_n := norm_str(street_type)]
perm_parc[, addr_key2 := paste(civic_number, street_name, street_type_n)]

# First: strict match on addr_key2
setkey(bca_addr, addr_key2)
setkey(perm_parc, addr_key2)
m1 <- bca_addr[perm_parc, on = "addr_key2", nomatch = 0L]

# Fallback: match on addr_key1 for remaining
matched_permits <- unique(m1$permitnumber)
unmatched <- perm_parc[!permitnumber %in% matched_permits]
setkey(bca_addr, addr_key1)
unmatched[, addr_key1 := paste(civic_number, street_name)]
m2 <- bca_addr[unmatched, on = "addr_key1", nomatch = 0L]

m <- rbindlist(list(m1, m2), fill = TRUE)

# ============================================================
# 6) Join to rollNumber -> width/depth -> lot size; add PID
# ============================================================
setkey(bca_folio, folioID)
setkey(m, folioID)
m <- bca_folio[m, on = "folioID"]

# dimensions by roll_number
m[, roll_number := rollNumber]
setkey(bca_dims, roll_number)
setkey(m, roll_number)
m <- bca_dims[m, on = "roll_number"]

# PID
setkey(bca_pid, folioID)
m <- bca_pid[m, on = "folioID"]

# Compute unified width/depth and lot size (ignore land_area entirely)
m[, land_width := fcoalesce(numify(land_width_ri), numify(land_width_fd))]
m[, land_depth := fcoalesce(numify(land_depth_ri), numify(land_depth_fd))]
m[, lot_size_original := land_width * land_depth]

m[, lot_size_source := fifelse(!is.na(land_width_ri) & !is.na(land_depth_ri),
                               "residentialInventory",
                        fifelse(!is.na(land_width_fd) & !is.na(land_depth_fd),
                               "folioDescription", NA_character_))]

# stable lot id
m[, lot_id := fifelse(!is.na(PID) & nzchar(PID), PID, folioID)]

# ============================================================
# 7) Collapse to one row per lot_id (earliest permit event)
# ============================================================
setorder(m, lot_id, issuedate)

lot_panel <- m[, .(
  first_issue_date = issuedate[1],
  first_issue_year = issueyear[1],
  outcome          = outcome[1],
  lot_size_original = lot_size_original[1],
  lot_size_source   = lot_size_source[1],
  land_width        = land_width[1],
  land_depth        = land_depth[1],
  roll_number       = roll_number[1],
  folioID           = folioID[1],
  PID               = PID[1],
  geolocalarea      = geolocalarea[1],
  site_id           = site_id[1],
  tax_coord         = tax_coord[1]
), by = lot_id]

# basic QA
lot_panel[, lot_size_missing := is.na(lot_size_original) | lot_size_original <= 0]
cat("Lots:", nrow(lot_panel),
    "| missing lot size:", lot_panel[lot_size_missing == TRUE, .N], "\n")

# quick sanity summary (you can comment out)
print(lot_panel[lot_size_missing==0,
  .(n=.N,
    p50_w=median(land_width, na.rm=TRUE),
    p50_d=median(land_depth, na.rm=TRUE),
    p50_area=median(lot_size_original, na.rm=TRUE),
    p95_area=quantile(lot_size_original, 0.95, na.rm=TRUE))])

# write
fwrite(lot_panel, f_out)
cat("Wrote:", f_out, "\n")

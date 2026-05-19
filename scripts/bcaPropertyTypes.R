# bcaPropertyTypes.R
# Principled discovery of BCA ActualUseDescription categories.
# Tabulates frequencies (overall and within jurisdiction), prints dominant
# categories, and writes a small `propertyTypeMap` list to disk for
# downstream scripts to source. Run once, eyeball output, trust the cache.
#
# Output:
#   ~/DropboxExternal/dataProcessed/propertyTypeMap.rds
#     list(singleFamily = c(...), strata = c(...), duplex = c(...),
#          discoveryDate = <Date>, notes = <character>)
#
# Tom Davidoff
# 05/18/26

library(data.table)
library(DBI)
library(RSQLite)

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
SQLITE_FILE   <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
MAP_OUT       <- "~/DropboxExternal/dataProcessed/propertyTypeMap.rds"
VANCOUVER_JUR <- "200"       # City of Vancouver jurisdiction code
TOP_N_PRINT   <- 25          # how many categories to print per table
SHARE_FLOOR   <- 0.005       # ignore categories below this share when proposing map -- .0025 captures non-strata duplex

# ---------------------------------------------------------------------------
# Load folioDescription joined to folio (for jurisdictionCode)
# ---------------------------------------------------------------------------
loadFolioUse <- function(sqliteFile = SQLITE_FILE) {
  con <- dbConnect(SQLite(), sqliteFile)
  qDes <- "SELECT folioID, actualUseDescription FROM folioDescription"
  qFol <- "SELECT folioID, rollNumber, jurisdictionCode FROM folio"
  des  <- as.data.table(dbGetQuery(con, qDes))
  fol  <- as.data.table(dbGetQuery(con, qFol))
  dbDisconnect(con)
  setkey(des, folioID); setkey(fol, folioID)
  dt <- des[fol, nomatch = 0]
  dt[, jurisdiction := trimws(as.character(jurisdictionCode))]
  dt <- dt[!is.na(actualUseDescription) & actualUseDescription != ""]
  dt[]
}

# ---------------------------------------------------------------------------
# Frequency table with shares, sorted descending
# ---------------------------------------------------------------------------
useShareTable <- function(dt, subsetExpr = NULL, label = "") {
  d <- if (is.null(subsetExpr)) dt else dt[eval(subsetExpr)]
  tab <- d[, .(n = .N), by = actualUseDescription]
  tab[, share := n / sum(n)]
  setorder(tab, -share)
  tab[, label := label]
  tab[]
}

prettyPrint <- function(tab, topN = TOP_N_PRINT, header = "") {
  cat(sprintf("\n========== %s ==========\n", header))
  cat(sprintf("Total parcels: %d   distinct categories: %d\n",
              sum(tab$n), nrow(tab)))
  print(tab[seq_len(min(topN, nrow(tab))),
            .(actualUseDescription, n, share = round(share, 4))])
  cat(sprintf("(cumulative share of top %d: %.3f)\n",
              min(topN, nrow(tab)),
              sum(tab[seq_len(min(topN, nrow(tab))), share])))
}

# ---------------------------------------------------------------------------
# Pattern-based proposal: regex hits, filtered by share floor.
# Easy to override by hand after eyeballing the printed tables.
# ---------------------------------------------------------------------------
proposeMap <- function(tabCOV, floor = SHARE_FLOOR) {
  use <- tabCOV[share >= floor, actualUseDescription]
  hit <- function(pat) grep(pat, use, value = TRUE, ignore.case = TRUE)
  list(
    # Single-family land: house +/- suite. Excludes duplex, strata, multifamily.
    singleFamily = setdiff(
      hit("single family|residential dwelling with suite"),
      hit("strata|duplex|condominium|apartment|multi")
    ),
    # Strata / condo: anything that's a strata unit, including strata SFD.
    # But drop duplex, rental or commercial
    # Floor-area ppsf is the natural elasticity LHS here.
    strata = setdiff(hit("strata|condominium|apartment"),
		     hit("duplex|rental|commercial|hotel|block")
    ),
    # Duplex: explicit duplex categories.
    duplex = hit("duplex")
  )
}

# ---------------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------------
dt <- loadFolioUse()

cat(sprintf("\nLoaded %d folio-description rows across %d jurisdictions.\n",
            nrow(dt), uniqueN(dt$jurisdiction)))

tabAll <- useShareTable(dt, label = "all_BC")
prettyPrint(tabAll, header = "ActualUseDescription -- ALL jurisdictions")

tabCOV <- useShareTable(dt, quote(jurisdiction == VANCOUVER_JUR),
                        label = "COV")
prettyPrint(tabCOV, header = sprintf("ActualUseDescription -- COV (jur=%s)",
                                     VANCOUVER_JUR))

# Propose a map from the COV table; print for inspection.
proposed <- proposeMap(tabCOV)
cat("\n========== PROPOSED propertyTypeMap (COV) ==========\n")
for (nm in names(proposed)) {
  cat(sprintf("\n-- %s --\n", nm))
  if (length(proposed[[nm]]) == 0) {
    cat("  (no matches)\n")
  } else {
    for (s in proposed[[nm]]) cat("  ", s, "\n")
  }
  shareSum <- tabCOV[actualUseDescription %in% proposed[[nm]], sum(share)]
  cat(sprintf("  total COV share: %.4f\n", shareSum))
}

# Save the map + provenance.
propertyTypeMap <- c(proposed,
                     list(discoveryDate = Sys.Date(),
                          sqliteFile    = SQLITE_FILE,
                          jurisdiction  = VANCOUVER_JUR,
                          shareFloor    = SHARE_FLOOR,
                          notes = paste(
                            "Auto-proposed from COV ActualUseDescription shares.",
                            "Review the printed tables and edit by hand if any",
                            "dominant category was mis-bucketed.")))
saveRDS(propertyTypeMap, path.expand(MAP_OUT))
cat(sprintf("\nWrote %s\n", MAP_OUT))
cat("Downstream scripts should `readRDS(MAP_OUT)` and consume\n",
    "propertyTypeMap$singleFamily / $strata / $duplex.\n", sep = "")

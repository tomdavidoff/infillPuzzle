# vancouverPermitLogit.R
# Product choice on new R1 permits (single / duplex / multiplex) vs tract price level,
# 2018-23 duplex era and post-Oct-2023 R1-1 era, on 33-ft and 50-ft lots.
# Under IIA, the duplex-vs-single logit (estimated on single+duplex permits) recovers the MNL
# duplex-vs-single coefficients in both eras, so the level slope x R1-1 interaction is a quasi-DiD.
# Multiplex-vs-single is R1-1 era only. Permits cached in dataRaw (refreshed if > 48 h old).
# Tom Davidoff, 11/10/26

if (!exists("sA")) source("scripts/vancouverPriceElasticity.R")
stopifnot(GEO == "CTNAME")
D_REFORM     <- as.IDate("2018-09-18")   # duplexes permitted in RS
R11_SWITCH   <- as.IDate("2023-10-01")   # R1-1 (multiplex) in force
PERMIT_START <- as.IDate("2015-01-01")
ONTARIO      <- -123.1036                # East/West divide

# ---- tract price level: pooled R1 SFD (33 + 50 ft), tract FE in log ppsf ---------
# pooled across widths so mostly-50-ft West tracts aren't dropped
mL  <- feols(log(conveyancePrice / MB_total_finished_area) ~ lage | gid + w + convMonth, sA)
feL <- fixef(mL)$gid
tr  <- merge(data.table(gid = names(feL), lppsf = as.numeric(feL)),
             el[, .(gid, elas)], by = "gid", all.x = TRUE)
tr[, `:=`(zL = (lppsf - mean(lppsf)) / sd(lppsf),
          zE = (elas - mean(elas, na.rm = TRUE)) / sd(elas, na.rm = TRUE))]
print(tr[, .(nTracts = .N, corElasLevel = cor(elas, lppsf, use = "complete.obs"))])

# ---- permits: cached download, refreshed if older than MAX_AGE_HRS --------------
url <- paste0("https://opendata.vancouver.ca/api/explore/v2.1/catalog/datasets/",
              "issued-building-permits/exports/csv?delimiter=%3B")
PERMIT_CACHE <- path.expand("~/DropboxExternal/dataRaw/vancouverIssuedBuildingPermits.csv")
MAX_AGE_HRS  <- 48
ageHrs <- if (file.exists(PERMIT_CACHE))
  as.numeric(difftime(Sys.time(), file.mtime(PERMIT_CACHE), units = "hours")) else Inf
if (ageHrs > MAX_AGE_HRS) {
  tmp <- paste0(PERMIT_CACHE, ".part")
  oldTO <- options(timeout = 1200)
  ok <- tryCatch(download.file(url, tmp, mode = "wb", quiet = TRUE) == 0, error = function(e) FALSE)
  options(oldTO)
  if (ok && file.exists(tmp) && file.size(tmp) > 1e6) {
    file.rename(tmp, PERMIT_CACHE)
    message("Permits downloaded to ", PERMIT_CACHE)
  } else {
    unlink(tmp)
    if (!file.exists(PERMIT_CACHE)) stop("Permit download failed and no cached copy exists")
    warning(sprintf("Permit download failed; using cached copy %.0f hours old", ageHrs))
  }
} else message(sprintf("Using cached permits (%.1f hours old)", ageHrs))
pm <- fread(PERMIT_CACHE, sep = ";", encoding = "UTF-8")
setnames(pm, tolower(names(pm)))

pm <- pm[typeofwork == "New Building"]
pm[, use := tolower(specificusecategory)]
pm <- pm[!grepl("infill|laneway", use)]      # second buildings on a lot, not the principal product
pm[, prodP := fcase(grepl("multiplex|multiple dwelling", use),              "MULTI",  # R1-only below
                    grepl("duplex", use),                                   "DUP",
                    grepl("single detached|one-family|single family", use), "SFD",
                    default = NA_character_)]
pm <- pm[!is.na(prodP)]
dateCol <- if ("permitnumbercreateddate" %in% names(pm)) "permitnumbercreateddate" else "issuedate"
pm[, appDate := as.IDate(substr(get(dateCol), 1, 10))]
pm <- pm[appDate >= PERMIT_START]
xy <- tstrsplit(pm$geo_point_2d, ",\\s*")
pm[, `:=`(pLat = as.numeric(xy[[1]]), pLon = as.numeric(xy[[2]]))]
pm <- pm[!is.na(pLat) & !is.na(pLon)]

pS <- st_transform(st_as_sf(pm[, .(permitnumber, appDate, prodP, pLon, pLat)],
                            coords = c("pLon", "pLat"), crs = 4326, remove = FALSE), 3005)
pS <- st_join(pS, zn,  join = st_intersects)
pS <- st_join(pS, ctV, join = st_intersects)
pS <- pS[!duplicated(pS$permitnumber), ]

# ---- lot width: permit point -> parcel -> 2019 inventory land_width ---------------
con <- dbConnect(SQLite(), "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3")
inv <- as.data.table(dbGetQuery(con, "SELECT roll_number, land_width FROM residentialInventory"))
dbDisconnect(con)
inv[, `:=`(rollStart = floor(as.numeric(roll_number) / 1000),
           land_width = suppressWarnings(as.numeric(land_width)))]
inv <- inv[land_width > 0, .(w19 = median(land_width)), by = rollStart]

gAll <- st_read(gpkg, quiet = TRUE, query =
  "SELECT ROLL_NUMBER, geom FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
   WHERE JURISDICTION_CODE = '200'")
gAll <- gAll[!st_is_empty(gAll), ]
gAll$rollStart <- floor(as.numeric(gAll$ROLL_NUMBER) / 1000)
gAll <- st_make_valid(st_transform(gAll[!duplicated(gAll$rollStart), "rollStart"], 3005))
pw <- as.data.table(st_drop_geometry(st_join(pS[, "permitnumber"], gAll, join = st_intersects)))
pw <- merge(pw[!duplicated(permitnumber)], inv, by = "rollStart", all.x = TRUE)

# ---- data-driven width windows: where does the heap around 33 / 50 fall back to background? ---
# Distribution of 2019 widths over all R1 parcels (not just permits, for power). Count parcels in
# BIN-ft bins; background = median bin count in the flanks (6-12 ft from the centre); the window
# grows outward from the centre bin until the first bin on each side with count < K x background.
# The window is the heap; widths outside it are as rare as generic odd lots.
gPt1 <- st_point_on_surface(gAll)
gPt1 <- st_join(gPt1, zn[, "zoning_category"], join = st_intersects)
r1w  <- merge(as.data.table(st_drop_geometry(gPt1))[zoning_category %in% "R1", .(rollStart)],
              inv, by = "rollStart")$w19
BIN <- 0.5; K <- 3
heapWindow <- function(ctr, w = r1w, bin = BIN, k = K) {
  brk <- seq(ctr - 12 - bin / 2, ctr + 12 + bin / 2, by = bin)
  cnt <- as.numeric(table(cut(w, brk)))
  mid <- head(brk, -1) + bin / 2
  bg  <- median(cnt[abs(mid - ctr) >= 6])
  i0  <- which.min(abs(mid - ctr))
  lo <- i0; while (lo > 1          && cnt[lo - 1] >= k * bg) lo <- lo - 1
  hi <- i0; while (hi < length(cnt) && cnt[hi + 1] >= k * bg) hi <- hi + 1
  out <- c(lo = brk[lo], hi = brk[hi + 1], background = bg,
           nIn = sum(w > brk[lo] & w <= brk[hi + 1], na.rm = TRUE))
  print(data.table(mid = mid, n = cnt, inWindow = seq_along(cnt) %between% c(lo, hi))[abs(mid - ctr) <= 6])
  print(out)
  out
}
win33 <- heapWindow(33)
win50 <- heapWindow(50)
drawWidths <- function() {
  hist(r1w[r1w > 25 & r1w <= 65], breaks = seq(25, 65, by = BIN), main = "R1 lot widths, 2019",
       xlab = "land width (ft)", col = "gray85", border = "gray60")
  abline(v = c(win33[c("lo", "hi")], win50[c("lo", "hi")]), col = "red", lty = 2)
}
drawWidths()
png("text/r1WidthWindows.png", width = 900, height = 600, res = 120); drawWidths(); dev.off()

# ---- permit panel -------------------------------------------------------------------
pp <- as.data.table(st_drop_geometry(pS))
pp <- pp[zoning_category %in% "R1" & !is.na(CTNAME)]
pp[, gid := CTNAME]
pp <- merge(pp, tr, by = "gid")
pp <- merge(pp, pw[, .(permitnumber, w19)], by = "permitnumber", all.x = TRUE)
pp[, wbin := fcase(w19 > win33["lo"] & w19 <= win33["hi"], "33",
                   w19 > win50["lo"] & w19 <= win50["hi"], "50",
                   !is.na(w19),                            "other",
                   default = NA_character_)]
pp[, wbin := factor(wbin, levels = c("33", "50", "other"))]
pp[, side := fifelse(pLon < ONTARIO, "West", "East")]
pp[, era := factor(fcase(appDate < D_REFORM,   "0pre",
                         appDate < R11_SWITCH, "1duplex",
                         default =             "2R11"))]
pp[, `:=`(appYear = year(appDate), appQ = paste0(year(appDate), "Q", quarter(appDate)),
          isDup = as.numeric(prodP == "DUP"), isMulti = as.numeric(prodP == "MULTI"),
          r11 = as.numeric(era == "2R11"))]
pp <- pp[!is.na(wbin)]
print(dcast(pp[, .N, by = .(wbin, era, prodP)], wbin + era ~ prodP, value.var = "N", fill = 0))

# ---- shares by East/West x lot width ----------------------------------------------------
sh <- pp[wbin != "other", .(N = .N, SFD = mean(prodP == "SFD"), DUP = mean(prodP == "DUP"),
             MULTI = mean(prodP == "MULTI")), keyby = .(side, wbin, appYear)]
drawShares <- function() {
  op <- par(mfrow = c(2, 2), mar = c(4, 4, 2.5, 1))
  cols <- c(SFD = "gray30", DUP = "red", MULTI = "blue")
  for (s in c("West", "East")) for (wb in c("33", "50")) {
    d <- sh[side == s & wbin == wb]
    matplot(d$appYear, as.matrix(d[, .(SFD, DUP, MULTI)]), type = "b", pch = 19, lty = 1,
            col = cols, ylim = c(0, 1), xlab = "application year",
            ylab = "share of new R1 permits", main = sprintf("%s, %s-ft lots", s, wb))
    abline(v = c(2018 + 8.5/12, 2023 + 9/12), col = "gray", lty = 2)
    text(d$appYear, 0.97, d$N, cex = 0.55, col = "gray40")     # permits per year
    if (s == "West" && wb == "33") legend("left", names(cols), col = cols, pch = 19, lty = 1, bty = "n")
  }
  par(op)
}
drawShares()
png("text/permitSharesEastWest.png", width = 1100, height = 850, res = 120); drawShares(); dev.off()

# ---- logits ------------------------------------------------------------------------------
# dup vs single: duplex + R1-1 eras pooled; zL:r11 = change in level slope at R1-1 (quasi-DiD)
# multi vs single: R1-1 era only
# pooled sample ("all"): adds width-category FE and width x R1-1 (33 ft is the reference)
# twfe = TRUE: tract + quarter FE. Tract FE absorb zL, zE and west, so only their x R1-1
#   interactions are identified (pure DiD). The multi model is R1-1-era only, so a tract FE
#   would absorb everything; it keeps quarter FE alone in both versions.
pp[, west := as.numeric(side == "West")]

fitLogits <- function(wb, twfe = FALSE) {
  d   <- if (wb == "all") pp else pp[wbin == wb]
  dd  <- d[prodP %in% c("SFD", "DUP") & era != "0pre"]
  dm  <- d[prodP %in% c("SFD", "MULTI") & era == "2R11"]
  wD  <- if (wb == "all") "+ i(wbin, r11, ref = '33')" else ""
  feD <- paste("|", if (twfe) "gid + appQ" else "appQ", if (wb == "all") "+ wbin" else "")
  feM <- paste("| appQ", if (wb == "all") "+ wbin" else "")
  rhs <- if (twfe) c("zL:r11", "west:r11", "(zL + zE):r11") else
                   c("zL + zL:r11", "west + west:r11", "zL + zE + (zL + zE):r11")
  fD  <- function(s) as.formula(paste("isDup ~", s, wD, feD))
  list(feglm(fD(rhs[1]), dd, family = binomial, cluster = ~gid),
       feglm(fD(rhs[2]), dd, family = binomial, cluster = ~gid),
       feglm(fD(rhs[3]), dd, family = binomial, cluster = ~gid),
       feglm(as.formula(paste("isMulti ~ zL", feM)), dm, family = binomial, cluster = ~gid))
}
lgDict <- c(zL = "price level (z)", zE = "size elasticity (z)", west = "West Side",
            "zL:r11" = "level x R1-1 era", "zE:r11" = "elasticity x R1-1 era",
            "west:r11" = "West Side x R1-1 era", wbin = "width", gid = "tract", appQ = "quarter")
lgHead <- function(lab) list(lot = setNames(list(4), lab),
                             choice = c("dup vs SFD", "dup vs SFD", "dup vs SFD", "multi vs SFD (R1-1)"))

for (tw in c(FALSE, TRUE)) {
  tag <- if (tw) ", tract + quarter FE" else ", quarter FE"
  cat("\n==========", if (tw) "TWFE" else "no tract FE", "==========\n")
  print(etable(fitLogits("33",  tw), headers = lgHead(paste0("33 ft", tag)),      dict = lgDict))
  print(etable(fitLogits("50",  tw), headers = lgHead(paste0("50 ft", tag)),      dict = lgDict))   # (a)
  print(etable(fitLogits("all", tw), headers = lgHead(paste0("all widths", tag)), dict = lgDict))   # (b)
}

# LPM check on the TWFE DiD (no incidental-parameters bias)
ddAll <- pp[prodP %in% c("SFD", "DUP") & era != "0pre"]
print(etable(
  feols(isDup ~ zL:r11 | gid + appQ, ddAll[wbin == "33"], cluster = ~gid),
  feols(isDup ~ zL:r11 | gid + appQ, ddAll[wbin == "50"], cluster = ~gid),
  feols(isDup ~ zL:r11 + i(wbin, r11, ref = "33") | gid + appQ + wbin, ddAll, cluster = ~gid),
  headers = c("LPM 33", "LPM 50", "LPM all"), dict = lgDict))

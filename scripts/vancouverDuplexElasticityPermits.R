# vancouverDuplexElasticityPermits.R
# Part 2 of vancouverPriceElasticity.R -- run after it (needs sfd, sA, el, p33, s33, m2_33, k33, df, ctV, zn, gpkg).
# Q1: is the legacy duplex-SFD ppsf gap's loading on elasticity more than "duplexes are small houses"?
# Q2: does SFD size elasticity predict duplex share of new permits conditional on lppsf,
#     and does that shift with the Oct 2023 R1-1 reform?
# Permits pulled from City of Vancouver open data; cached in dataRaw, refreshed if older than 48 hours.
# Tom Davidoff, 11/9/26

if (!exists("p33")) source("scripts/vancouverPriceElasticity.R")
stopifnot(GEO == "CTNAME")
MEAS         <- "33"                  # "33" = 33-ft SFD tract measures (p33); "all" = R1 33+50 pooled
D_REFORM     <- as.IDate("2018-09-18") # duplexes permitted in RS
R11_SWITCH   <- as.IDate("2023-10-01") # R1-1 (multiplex) in force
PERMIT_START <- as.IDate("2015-01-01")

# ---- Q1: duplex gap beyond size ---------------------------------------------
# If duplexes sit on the tract hedonic, log ppsf = a_g + (e_g - 1) lsqft, so
# coefficient on lsqft:elas ~ 1, on lsqft ~ -1, and dup:elas -> 0.
hrSamp <- df[round(land_width) == 33 | dup == TRUE]
hr0 <- feols(log(conveyancePrice / MB_total_finished_area) ~ dup * (elas + lppsf) | gid + convMonth,
             hrSamp, cluster = ~gid)
hr1 <- feols(log(conveyancePrice / MB_total_finished_area) ~ dup * (elas + lppsf) + lsqft + lsqft:elas |
               gid + convMonth, hrSamp, cluster = ~gid)
print(etable(hr0, hr1, headers = c("as run", "+ size on tract curve")))

# ---- tract measures ------------------------------------------------------------
if (MEAS == "33") {
  tr <- p33[, .(gid, lppsf, elas)]
} else {
  mL <- feols(log(conveyancePrice / MB_total_finished_area) ~ lage | gid + w + convMonth, sA)
  feL <- fixef(mL)$gid
  tr <- merge(data.table(gid = names(feL), lppsf = as.numeric(feL)), el[, .(gid, elas)], by = "gid")
}
tr[, `:=`(zE = (elas - mean(elas)) / sd(elas), zL = (lppsf - mean(lppsf)) / sd(lppsf))]
print(tr[, .(nTracts = .N, corElasLevel = cor(elas, lppsf))])

# ---- permits: cached download, refreshed if older than MAX_AGE_HRS --------------
url <- paste0("https://opendata.vancouver.ca/api/explore/v2.1/catalog/datasets/",
              "issued-building-permits/exports/csv?delimiter=%3B")
PERMIT_CACHE <- path.expand("~/DropboxExternal/dataRaw/vancouverIssuedBuildingPermits.csv")
MAX_AGE_HRS  <- 48
ageHrs <- if (file.exists(PERMIT_CACHE))
  as.numeric(difftime(Sys.time(), file.mtime(PERMIT_CACHE), units = "hours")) else Inf
if (ageHrs > MAX_AGE_HRS) {
  tmp <- paste0(PERMIT_CACHE, ".part")             # download to temp; a stalled/failed pull never replaces the cache
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
need <- c("permitnumber", "issuedate", "typeofwork", "specificusecategory", "geo_point_2d")
if (length(setdiff(need, names(pm)))) stop("missing columns: ", paste(setdiff(need, names(pm)), collapse = ", "))

pm <- pm[typeofwork == "New Building"]
pm[, use := tolower(specificusecategory)]
print(pm[, .N, keyby = specificusecategory][order(-N)][1:40])   # check the coding below against this

pm[, prodP := fcase(grepl("multiplex|multiple dwelling", use),                "MULTI",   # R1-only below, so MD = plex
                    grepl("duplex", use),                                     "DUP",
                    grepl("single detached|one-family|single family", use),   "SFD",
                    default = NA_character_)]
pm <- pm[!is.na(prodP)]

dateCol <- if ("permitnumbercreateddate" %in% names(pm)) "permitnumbercreateddate" else "issuedate"
pm[, appDate := as.IDate(substr(get(dateCol), 1, 10))]
pm <- pm[appDate >= PERMIT_START]

xy <- tstrsplit(pm$geo_point_2d, ",\\s*")
pm[, `:=`(pLat = as.numeric(xy[[1]]), pLon = as.numeric(xy[[2]]))]
pm <- pm[!is.na(pLat) & !is.na(pLon)]

pS <- st_transform(st_as_sf(pm[, .(permitnumber, appDate, prodP, pLon, pLat)],
                            coords = c("pLon", "pLat"), crs = 4326), 3005)
pS <- st_join(pS, zn,  join = st_intersects)
pS <- st_join(pS, ctV, join = st_intersects)
pS <- pS[!duplicated(pS$permitnumber), ]

# ---- lot width for permits: parcel -> 2019 inventory land_width -----------------
con <- dbConnect(SQLite(), "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3")
inv <- as.data.table(dbGetQuery(con, "SELECT roll_number, land_width FROM residentialInventory"))
dbDisconnect(con)
inv[, `:=`(rollStart = floor(as.numeric(roll_number) / 1000), land_width = as.numeric(land_width))]
inv <- inv[land_width > 0, .(w19 = median(land_width)), by = rollStart]

gAll <- st_read(gpkg, quiet = TRUE, query =
  "SELECT ROLL_NUMBER, geom FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
   WHERE JURISDICTION_CODE = '200'")
gAll <- gAll[!st_is_empty(gAll), ]
gAll$rollStart <- floor(as.numeric(gAll$ROLL_NUMBER) / 1000)
gAll <- st_make_valid(st_transform(gAll[!duplicated(gAll$rollStart), "rollStart"], 3005))

pw <- as.data.table(st_drop_geometry(st_join(pS[, "permitnumber"], gAll, join = st_intersects)))
pw <- merge(pw[!duplicated(permitnumber)], inv, by = "rollStart", all.x = TRUE)

# ---- permit panel ----------------------------------------------------------------
pp <- as.data.table(st_drop_geometry(pS))
pp <- pp[zoning_category %in% "R1" & !is.na(CTNAME)]
pp[, gid := CTNAME]
pp <- merge(pp, tr, by = "gid")
pp <- merge(pp, pw[, .(permitnumber, w19)], by = "permitnumber", all.x = TRUE)
pp[, era := factor(fcase(appDate < D_REFORM,   "0pre",
                         appDate < R11_SWITCH, "1duplex",
                         default =             "2R11"))]
pp[, appMonth := format(appDate, "%Y-%m")]
pp[, `:=`(isDup = as.numeric(prodP == "DUP"), isMulti = as.numeric(prodP == "MULTI"))]
pp[, isNeither := (1 - isDup) * (1 - isMulti)]
pp[, lot33 := round(w19) == 33]
print(dcast(pp[, .N, by = .(era, prodP)], era ~ prodP, value.var = "N", fill = 0))
print(pp[, .(N = .N, widthMissing = mean(is.na(w19))), by = era])   # check the match rate

# ---- duplex era: level vs elasticity ----------------------------------------------
dEra <- pp[era == "1duplex"]
print(etable(
  feols(isDup ~ zL      | appMonth, dEra, cluster = ~gid),
  feols(isDup ~ zE + zL | appMonth, dEra, cluster = ~gid),
  feols(isDup ~ zE      | appMonth, dEra, cluster = ~gid),
  headers = c("level", "both", "elas")))

# reliability of the tract elasticity: share of cross-tract variance that is signal
eHat <- coef(m2_33)[k33]; eSE <- se(m2_33)[k33]
print(c(varElas = var(eHat), meanSE2 = mean(eSE^2), reliability = 1 - mean(eSE^2) / var(eHat)))

# split-sample IV: elasticity from one random half of folios, instrumented by the other half
elasHalf <- function(d) {
  m <- feols(log(conveyancePrice) ~ i(gid, lsqft) + lage | gid + convMonth, d)
  k <- grep("^gid::", names(coef(m)))
  data.table(gid = sub("^gid::(.*):lsqft$", "\\1", names(coef(m))[k]), e = coef(m)[k])
}
set.seed(20261109)
fol <- data.table(rollStart = unique(s33$rollStart))
fol[, fhalf := sample(0:1, .N, replace = TRUE)]       # whole folios, so repeat sales stay together
s33h <- merge(s33, fol, by = "rollStart")
eA <- elasHalf(s33h[fhalf == 0]); setnames(eA, "e", "eA")
eB <- elasHalf(s33h[fhalf == 1]); setnames(eB, "e", "eB")
ivDat <- merge(merge(dEra, eA, by = "gid"), eB, by = "gid")
ivDat[, `:=`(zEA = (eA - mean(eA)) / sd(eA), zEB = (eB - mean(eB)) / sd(eB))]
ivFit <- feols(isDup ~ zL | appMonth | zEA ~ zEB, ivDat, cluster = ~gid)
print(etable(
  feols(isDup ~ zEA + zL | appMonth, ivDat, cluster = ~gid),     # OLS, half-sample elas
  ivFit,                                                          # IV corrects attenuation
  headers = c("OLS half", "IV split")))
print(fitstat(ivFit, "ivf"))                                      # first-stage F

# ---- R1-1 era on 33-ft lots ---------------------------------------------------------
print(pp[era == "2R11", .(N = .N, dupShare = mean(isDup), multiShare = mean(isMulti)), keyby = lot33])
r33 <- pp[era == "2R11" & lot33 == TRUE]
print(etable(
  feols(isNeither ~ zE + zL | appMonth, r33, cluster = ~gid),
  feols(isDup     ~ zE + zL | appMonth, r33, cluster = ~gid),
  headers = c("neither | 33, R1-1", "dup | 33, R1-1")))

# ---- Q2: horse race, era-specific slopes ---------------------------------------
print(pp[era == "2R11", .N, keyby = .(prodP)])
sd2 <- pp[prodP != "MULTI"]
q2 <- list(
  "dup | all"        = feols(isDup   ~ i(era, zE) + i(era, zL) | appMonth, pp,  cluster = ~gid),
  "dup | sfd+dup"    = feols(isDup   ~ i(era, zE) + i(era, zL) | appMonth, sd2, cluster = ~gid),
  "dup | elas only"  = feols(isDup   ~ i(era, zE)              | appMonth, pp,  cluster = ~gid),
  "dup | level only" = feols(isDup   ~ i(era, zL)              | appMonth, pp,  cluster = ~gid))
if (sum(pp$isMulti) >= 10) {   # multiplex only exists post-R1-1; estimate on that era
  q2[["multi | R1-1 era"]] <- feols(isMulti ~ zE + zL | appMonth, pp[era == "2R11"], cluster = ~gid)
} else {
  message("Only ", sum(pp$isMulti), " MULTI permits: check the specificusecategory table above")
}
print(etable(q2, headers = names(q2)))

# shift relative to the 2018-23 duplex era (interaction terms = change in slope)
q2d <- list(
  "dup | all"     = feols(isDup ~ zE + zL + i(era, zE, ref = "1duplex") + i(era, zL, ref = "1duplex") |
                            appMonth, pp,  cluster = ~gid),
  "dup | sfd+dup" = feols(isDup ~ zE + zL + i(era, zE, ref = "1duplex") + i(era, zL, ref = "1duplex") |
                            appMonth, sd2, cluster = ~gid))
print(etable(q2d, headers = names(q2d)))

# ---- rolling partial coefficients (horse-race version of the monthly cor plot) --
pp[, half := year(appDate) + fifelse(month(appDate) <= 6, 0, 0.5)]
roll <- pp[, {
  if (.N < 40 || uniqueN(gid) < 15 || sum(isDup) < 5) NULL else {
    ct <- coeftable(feols(isDup ~ zE + zL, .SD, cluster = ~gid))
    list(term = c("elas", "lppsf"), b = ct[c("zE", "zL"), 1], se = ct[c("zE", "zL"), 2], N = .N)
  }}, by = half]

drawRoll <- function() {
  yl <- range(roll[, c(b - 1.96 * se, b + 1.96 * se)])
  plot(NA, xlim = range(roll$half) + c(0, 0.5), ylim = yl,
       xlab = "application half-year", ylab = "coef on standardized measure (duplex LPM)",
       main = sprintf("Duplex permit share on elas + lppsf jointly (R1, %s-ft measures)", MEAS))
  abline(h = 0, lty = 3); abline(v = c(2018 + 8.5/12, 2023 + 9/12), col = "gray")
  off <- c(elas = 0.17, lppsf = 0.33); col <- c(elas = "red", lppsf = "blue")
  for (tm in names(off)) with(roll[term == tm], {
    segments(half + off[tm], b - 1.96 * se, half + off[tm], b + 1.96 * se, col = col[tm])
    points(half + off[tm], b, pch = 19, col = col[tm])
  })
  legend("bottomleft", c("elasticity", "lppsf level"), col = col, pch = 19, bty = "n")
}
drawRoll()
png("text/duplexPermitPartials.png", width = 900, height = 650, res = 120); drawRoll(); dev.off()

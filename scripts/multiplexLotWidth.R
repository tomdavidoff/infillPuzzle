# multiplexLotWidth.R
# New-building permits (all issued 2017+) linked to 2018 single-family lots in R1-1 (BCA 2019 roll); links saved for
# vancouverPermitMap.R. Share of 2024+ multiplexes on 33-ft lots, multiplex share of new builds by lot width, and
# post-reform (applied on/after R1-1 start) multiplex share on 33 vs 50 ft lots, East and West.
# Type of work restricted to New Building and Addition / Alteration (no demolition, salvage/abatement, temporary, outdoor).
# Products as in vancouverPermitLogit.R: drop infill/laneway categories; single = single detached, duplex = duplex.
# New build = New Building, or any permit type at/above the 95th pct value of non-new-build principal permits.
# Multiplex = specific use exactly "Multiple Dwelling" (not combinations with parking garage, retail, office, etc.; 11/22/26)
# whose description says multiplex (or two-unit front/rear building), applied on/after R1-1 start; other "Multiple Dwelling"
# categories (apartments, townhouses, rebuilt multifamily, mixed use) are kept as otherMultiple and excluded from shares.
# Tom Davidoff 11/15/26

library(data.table)
library(sf)
library(RSQLite)

SFX <- if (Sys.getenv("ZONESET") == "RT") "RT" else ""   # ZONESET=RT: 2018 SFD lots now in RT-* zones, outputs suffixed RT
MATCHDIST <- 5      # m, permit point to 2018 SFD centroid (as in vancouverR1FSRDate.R)
YEAR_MIN  <- 2024     # issue-year floor for the 2024+ tables
HIGHVAL_Q <- 0.95      # high-value non-new-build permits count as new builds
ONT       <- -123.101 # Ontario St: East/West divide
fLinks    <- paste0("~/DropboxExternal/dataProcessed/permitLotLinks", SFX, ".rds")
fCutoffs  <- "~/DropboxExternal/dataProcessed/permitNewBuildCutoffs.rds"
WORK      <- c("New Building", "Addition / Alteration")
STREETDIST <- 30     # m, street-side geocode search radius
ADDRDIST  <- 60     # m, max permit-to-lot distance for an address match (E/W ambiguity, street-side geocodes)
fPermit   <- "~/DropboxExternal/dataRaw/vancouverIssuedBuildingPermits.csv"
fSingles  <- "~/DropboxExternal/dataProcessed/bca19VancouverSingles.rds"
fZoning   <- "~/DropboxExternal/dataRaw/vancouver_zoning.geojson"
fGeo26    <- "~/DropboxExternal/dataProcessed/bca26FolioGeometryVancouver.rds"  # rollStart centroids, 2026 gpkg (see vancouverR1FSRDate.R)
fGpkg26   <- list.files("~/bigFiles/latestSpatialBCA/", pattern = "\\.gpkg$", full.names = TRUE)
fBCA19    <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
# freshness guard; SKIP_FRESH=1 reruns on the existing extract (11/22/26: rerun on the Nov 15 2026 extract)
if (Sys.getenv("SKIP_FRESH") != "1") stopifnot(difftime(Sys.time(), file.mtime(fPermit), units = "hours") < 48)

lost <- function(n0, n1, what) cat(sprintf("%-40s %6d -> %6d (lost %.1f%%)\n", what, n0, n1, 100 * (1 - n1 / n0)))

dtS <- readRDS(fSingles)
dtS <- unique(dtS[, .(rollNumber, folioID, landWidth, longitude, latitude)])  # file repeats folios across valuation rows
stopifnot(!anyDuplicated(dtS$rollNumber))
dtS[, rollStart := floor(as.numeric(rollNumber) / 1000)]
n0 <- nrow(dtS); dtS <- dtS[landWidth > 0 & is.finite(longitude)]; lost(n0, nrow(dtS), "2018 SFD lots: width>0, located")
n0 <- nrow(dtS); dtS <- unique(dtS, by = "rollStart"); lost(n0, nrow(dtS), "one lot per rollStart")
sfS <- st_transform(st_as_sf(dtS, coords = c("longitude", "latitude"), crs = 4326), 26910)
zR1 <- st_transform(st_make_valid(st_read(fZoning, quiet = TRUE)), 26910)
zR1 <- zR1[if (SFX == "RT") grepl("^RT", zR1$zoning_district) else zR1$zoning_district == "R1-1", ]
inR1 <- lengths(st_intersects(sfS, zR1)) > 0
n0 <- nrow(dtS); dtS <- dtS[inR1]; sfS <- sfS[inR1, ]; lost(n0, nrow(dtS), if (SFX == "RT") "in RT zones" else "in R1-1")

addrKey <- function(num, name) toupper(gsub("\\s+", " ", trimws(paste(num, name))))
con <- dbConnect(SQLite(), path.expand(fBCA19))
adr <- as.data.table(dbGetQuery(con, "SELECT folioID, streetNumber, streetName FROM address WHERE primaryFlag = 'true'"))
dbDisconnect(con)
adr <- adr[folioID %in% dtS$folioID & streetNumber != ""]
adr[, key := addrKey(streetNumber, streetName)]
adr <- merge(adr[, .(folioID, key)], dtS[, .(folioID, rollStart)], by = "folioID")
adr <- unique(adr[, .(key, rollStart)])
cat("2018 R1-1 lots with an address key:", round(adr[, uniqueN(rollStart)] / nrow(dtS), 3),
    "| keys shared by >1 lot (E/W):", adr[, uniqueN(rollStart), by = key][V1 > 1, .N], "\n")

dtP <- fread(fPermit)
setnames(dtP, tolower(names(dtP)))
stopifnot(!anyDuplicated(dtP$permitnumber))
dtP[, issued := as.IDate(issuedate)]
dtP[, applied := as.IDate(permitnumbercreateddate)]
dtP[, u := tolower(specificusecategory)]
print(dtP[, .N, by = typeofwork][order(-N)])
n0 <- nrow(dtP); dtP <- dtP[typeofwork %chin% WORK]; lost(n0, nrow(dtP), "New Building or Addition / Alteration")
n0 <- nrow(dtP); dtP <- dtP[!grepl("infill|laneway", u)]; lost(n0, nrow(dtP), "drop infill/laneway categories")
# new build = typed New Building, or any other permit type at/above the HIGHVAL_Q quantile of value among non-new-build
# permits for the principal products (single / duplex / multiple dwelling)
prin <- dtP[, grepl("multiplex|multiple dwelling|duplex|single detached|one-family|single family", u)]
# cutoff alternatives (main = A); every principal permit is linked, the rule is applied afterwards
cutoffs <- c(A = dtP[prin & typeofwork != "New Building", quantile(projectvalue, HIGHVAL_Q, na.rm = TRUE)],
             B = dtP[prin, quantile(projectvalue, HIGHVAL_Q, na.rm = TRUE)],
             C = dtP[typeofwork == "New Building" & grepl("duplex|single detached|one-family|single family", u) &
                     !grepl("multiple dwelling", u), quantile(projectvalue, 1/3, na.rm = TRUE)])
names(cutoffs) <- c("A", "B", "C")
cutLab <- c(A = "q95 of non-new-build principal permits (main)", B = "q95 of all principal permits",
            C = "q33 of new-build single + duplex permits")
for (k in names(cutoffs)) cat(sprintf("cutoff %s: $%s  (%s)\n", k, format(cutoffs[k], big.mark = ","), cutLab[k]))
if (SFX == "") saveRDS(cutoffs, fCutoffs)
hvTh <- cutoffs["A"]
dtP[, highVal := typeofwork != "New Building" & projectvalue >= hvTh]
print(dtP[prin & highVal == TRUE, .N, by = typeofwork])
n0 <- nrow(dtP); dtP <- dtP[typeofwork == "New Building" | projectvalue >= min(cutoffs)]; lost(n0, nrow(dtP), "New Building or value >= lowest cutoff")
R1_START <- as.IDate("2023-10-17")
dtP[, isMplx := grepl("multiplex|multi-plex|two-units (front|rear)", tolower(projectdescription)) &
                as.IDate(permitnumbercreateddate) >= R1_START]
print(dtP[grepl("Multiple Dwelling", specificusecategory), .N, by = .(multiplexByDescAndDate = isMplx)])
dtP[, product := fcase(u == "multiple dwelling" & isMplx, "multiplex",
                       grepl("multiplex|multiple dwelling", u), "otherMultiple",
                       grepl("duplex", u), "duplex",
                       grepl("single detached|one-family|single family", u), "single",
                       default = NA_character_)]
print(dtP[is.na(product), .N, by = specificusecategory][order(-N)][1:8])
n0 <- nrow(dtP); dtP <- dtP[!is.na(product) & geo_point_2d != ""]; lost(n0, nrow(dtP), "principal product, geocoded")
dtP[, c("lat", "lon") := tstrsplit(geo_point_2d, ",\\s*", type.convert = TRUE)]
sfP <- st_transform(st_as_sf(dtP, coords = c("lon", "lat"), crs = 4326), 26910)
# match 0 (main): nearest 2026 BCA folio centroid within MATCHDIST -> its rollStart -> 2018 SFD lot on that rollStart
g26 <- readRDS(fGeo26)
sfG <- st_transform(st_as_sf(g26, coords = c("longitude", "latitude"), crs = 4326), 26910)
ng  <- st_nearest_feature(sfP, sfG)
dtP[, `:=`(dist26 = as.numeric(st_distance(sfP, sfG[ng, ], by_element = TRUE)), roll26 = g26$rollStart[ng])]
dtP[, roll26ok := dist26 <= MATCHDIST & roll26 %in% dtS$rollStart]
print(dtP[, .(N = .N, near26 = round(mean(dist26 <= MATCHDIST), 3), roll26in2018 = round(mean(roll26ok), 3)), by = product])
# match 1 (fallback): nearest 2018 lot centroid within MATCHDIST
nn  <- st_nearest_feature(sfP, sfS)
dtP[, `:=`(dist = as.numeric(st_distance(sfP, sfS[nn, ], by_element = TRUE)), rollNear = dtS$rollStart[nn])]
# match 2 (main): civic address, nearest candidate lot within ADDRDIST; fall back to match 1. permit "2636 E 23RD AVENUE, Vancouver, BC ..." -> "2636 E 23RD"
dtP[, street := toupper(trimws(sub("#.*$", "", sub(",.*$", "", address))))]
dtP[, street := sub("\\s+(AVENUE|AVE|AV|STREET|ST|DRIVE|DR|ROAD|RD|BOULEVARD|BLVD|CRESCENT|CRES|PLACE|PL|COURT|CRT|WAY|LANE|MEWS|MALL|SQUARE|DIVERSION|PARKWAY|TERRACE|GATE|WALK|PARK)$", "", street)]
dtP[, key := gsub("\\s+", " ", sub("^([0-9]+[A-Z]?) (E|W|N|S) ", "\\1 ", street))]
cand <- merge(dtP[, .(permitnumber, key, lon, lat)], adr, by = "key", allow.cartesian = TRUE)
cand <- merge(cand, dtS[, .(rollStart, lonS = longitude, latS = latitude)], by = "rollStart")
pc <- st_transform(st_as_sf(cand, coords = c("lon", "lat"), crs = 4326), 26910)
sc <- st_transform(st_as_sf(cand, coords = c("lonS", "latS"), crs = 4326), 26910)
cand[, dAddr := as.numeric(st_distance(pc, sc, by_element = TRUE))]
cand <- cand[dAddr <= ADDRDIST][order(permitnumber, dAddr)][, .SD[1], by = permitnumber]
n0 <- nrow(dtP); dtP <- merge(dtP, cand[, .(permitnumber, rollStart, dAddr)], by = "permitnumber", all.x = TRUE); stopifnot(nrow(dtP) == n0)
print(dtP[, .(N = .N, address = round(mean(!is.na(rollStart)), 3), near5 = round(mean(dist <= MATCHDIST), 3)), by = product])
cat("address vs 5m agreement where both match:", dtP[!is.na(rollStart) & dist <= MATCHDIST, round(mean(rollStart == rollNear), 3)],
    " N =", dtP[!is.na(rollStart) & dist <= MATCHDIST, .N], "\n")
# match 3: 2026 BCA civic address (multiplexes get new civic numbers) -> 2026 roll -> rollStart -> 2018 lot
stopifnot(length(fGpkg26) == 1)
con <- dbConnect(SQLite(), fGpkg26)
a26 <- as.data.table(dbGetQuery(con, "SELECT ROLL_NUMBER, STREET_NUMBER, STREET_NAME FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_ADDRESSES_SV WHERE JURISDICTION_CODE = '200'"))
dbDisconnect(con)
a26 <- unique(a26[!is.na(STREET_NUMBER) & STREET_NUMBER != "", .(key = addrKey(STREET_NUMBER, STREET_NAME), rollStart = floor(as.numeric(ROLL_NUMBER) / 1000))])
gc26 <- g26[, .(lonS = mean(longitude), latS = mean(latitude)), by = rollStart]
c26 <- merge(dtP[, .(permitnumber, key, lon, lat)], a26, by = "key", allow.cartesian = TRUE)
c26 <- merge(c26, gc26, by = "rollStart")
c26[, d := as.numeric(st_distance(st_transform(st_as_sf(c26, coords = c("lon", "lat"), crs = 4326), 26910),
                                  st_transform(st_as_sf(c26, coords = c("lonS", "latS"), crs = 4326), 26910), by_element = TRUE))]
c26 <- c26[d <= ADDRDIST][order(permitnumber, d)][, .SD[1], by = permitnumber]
n0 <- nrow(dtP); dtP <- merge(dtP, c26[, .(permitnumber, rollA26 = rollStart)], by = "permitnumber", all.x = TRUE); stopifnot(nrow(dtP) == n0)
dtP[, a26ok := !is.na(rollA26) & rollA26 %in% dtS$rollStart]
print(dtP[, .(N = .N, addr26 = round(mean(!is.na(rollA26)), 3), addr26in2018 = round(mean(a26ok), 3)), by = product])
cat("2026-address vs centroid agreement where both match:", dtP[a26ok & roll26ok, round(mean(rollA26 == roll26), 3)], " N =", dtP[a26ok & roll26ok, .N], "\n")
# match 4 (last resort): street-side geocodes. Nearest 2018 SFD lot polygon within STREETDIST whose 2019/2026 address
# is on the permit's street. Validated against address-linked off-centroid permits (see factlog): 86% agree, 96% if gap > 2 m.
sfP <- st_transform(st_as_sf(dtP, coords = c("lon", "lat"), crs = 4326, remove = FALSE), 26910)   # dtP re-sorted by merges
P26 <- st_read(fGpkg26, query = "SELECT ROLL_NUMBER, geom FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV WHERE JURISDICTION_CODE = '200'", quiet = TRUE)
P26 <- st_transform(st_make_valid(P26), 26910)
P26$rollStart <- floor(as.numeric(P26$ROLL_NUMBER) / 1000)
Ps <- P26[P26$rollStart %in% dtS$rollStart & !duplicated(P26$rollStart), ]
streetOf  <- function(k) sub("^[0-9]+[A-Z]? ", "", k)
lotStreet <- unique(rbind(adr[, .(rollStart, st = streetOf(key))], a26[rollStart %in% dtS$rollStart, .(rollStart, st = streetOf(key))]))
w <- st_is_within_distance(sfP, Ps, STREETDIST)
cs <- data.table(i = rep(seq_len(nrow(dtP)), lengths(w)), j = unlist(w))
cs[, `:=`(rollC = Ps$rollStart[j], st = streetOf(dtP$key[i]))]
cs <- merge(cs, lotStreet, by.x = c("rollC", "st"), by.y = c("rollStart", "st"))
cs[, d := as.numeric(st_distance(sfP[i, ], Ps[j, ], by_element = TRUE))]
cs <- cs[order(i, d)][, .(rollSt = rollC[1], gapSt = if (.N > 1) d[2] - d[1] else Inf), by = i]
dtP[, c("rollSt", "gapSt") := list(NA_real_, NA_real_)]
dtP[cs$i, `:=`(rollSt = cs$rollSt, gapSt = cs$gapSt)]
cat("street-nearest vs 2026 address, address-linked off-centroid permits: agree =",
    dtP[a26ok & !roll26ok & !is.na(rollSt), round(mean(rollSt == rollA26), 3)], " N =", dtP[a26ok & !roll26ok & !is.na(rollSt), .N], "\n")
cat("rollStart vs address agreement where both match:", dtP[roll26ok & !is.na(rollStart), round(mean(roll26 == rollStart), 3)],
    " N =", dtP[roll26ok & !is.na(rollStart), .N], "\n")
dtP[, how := fcase(roll26ok, "rollStart26", a26ok, "address26", !is.na(rollStart), "address19", dist <= MATCHDIST, "near2018",
                   !is.na(rollSt), "streetNear", default = NA_character_)]
dtP[how == "address26", rollStart := rollA26]
dtP[roll26ok == TRUE, rollStart := roll26]
dtP[is.na(rollStart) & dist <= MATCHDIST, rollStart := rollNear]
dtP[how == "streetNear", rollStart := rollSt]
print(dcast(dtP[, .N, by = .(product, how)], product ~ how, fill = 0))
n0 <- nrow(dtP); dtP <- dtP[!is.na(rollStart)]; lost(n0, nrow(dtP), "matched to 2018 R1-1 SFD lot")
print(dtP[, .N, by = product])
links <- merge(dtP[, .(permitnumber, rollStart, product, applied, issued, how, gapSt, highVal, typeofwork, projectvalue, pLon = lon, pLat = lat)],
               dtS[, .(rollStart, landWidth, lotLon = longitude, lotLat = latitude)], by = "rollStart")
stopifnot(nrow(links) == nrow(dtP), !anyDuplicated(links$permitnumber))

isNB <- function(d, th) d$typeofwork == "New Building" | d$projectvalue >= th
links[, newBuild := isNB(links, hvTh)]
saveRDS(links, fLinks)
if (SFX == "RT") {
  zi <- st_intersects(st_transform(st_as_sf(links, coords = c("lotLon", "lotLat"), crs = 4326), 26910), zR1)
  links[, zone := zR1$zoning_district[sapply(zi, `[`, 1)]]
  cat("\nRT lots linked, new builds by zone x era x product:\n")
  print(dcast(links[newBuild == TRUE, .N, by = .(zone, era = fifelse(applied < R1_START, "pre", "post"), product)], zone + era ~ product, fill = 0))
  quit(save = "no")
}
linksAll <- links
links <- linksAll[newBuild == TRUE]
n0 <- nrow(links); dt <- links[year(issued) >= YEAR_MIN]; lost(n0, nrow(dt), "issued 2024+")
n0 <- nrow(dt); dt <- dt[order(rollStart, -issued)][, .SD[1], by = rollStart]; lost(n0, nrow(dt), "latest permit per lot")
stopifnot(!anyDuplicated(dt$rollStart))
dt[, widthBin := fcase(landWidth > 30 & landWidth <= 36, "33",
                       landWidth > 36 & landWidth <= 41, "37-41",
                       landWidth > 41 & landWidth <= 50, "42-50",
                       landWidth > 50, ">50", default = "<=30")]
dt[, widthBin := factor(widthBin, c("<=30", "33", "37-41", "42-50", ">50"))]

cat("\n=== share of 2024+ multiplexes by lot width ===\n")
print(dt[product == "multiplex", .N, keyby = widthBin][, share := round(N / sum(N), 3)][])
cat("\n=== multiplex share on 33 by link method ===\n")
print(dt[product == "multiplex", .(N = .N, share33 = round(mean(widthBin == "33"), 3)), by = how])
cat("excluding street-nearest links:", dt[product == "multiplex" & how != "streetNear", round(mean(widthBin == "33"), 3)],
    " N =", dt[product == "multiplex" & how != "streetNear", .N], "\n")
cat("street-nearest only if clear gap > 2 m:", dt[product == "multiplex" & (how != "streetNear" | gapSt > 2), round(mean(widthBin == "33"), 3)],
    " N =", dt[product == "multiplex" & (how != "streetNear" | gapSt > 2), .N], "\n")
cat("\n=== by issue year: multiplex N and share on 33 ===\n")
print(dt[product == "multiplex", .(N = .N, share33 = round(mean(widthBin == "33"), 3)), keyby = .(year = year(issued))])
cat("\n=== product mix within width bin (2024+) ===\n")
print(dcast(dt[product != "otherMultiple", .N, by = .(widthBin, product)], widthBin ~ product, fill = 0)[, `:=`(
  total = duplex + multiplex + single, multiplexShare = round(multiplex / (duplex + multiplex + single), 3))][])
cat("\nbaseline: share of 2018 R1-1 SFD lots that are 33:", round(dtS[, mean(landWidth > 30 & landWidth <= 36)], 3), "\n")
cat("latest issue date in data:", format(max(dtP$issued)), "\n")

# ---- post-reform (applied on/after R1-1 start): multiplex share of new builds, 33 vs 50 ft, East vs West ----
pr <- links[applied >= R1_START & product != "otherMultiple"][order(rollStart, -issued)][, .SD[1], by = rollStart]
pr[, w := fcase(round(landWidth) == 33, "33", round(landWidth) == 50, "50", default = NA_character_)]
pr[, side := fifelse(lotLon < ONT, "West", "East")]
ew <- pr[!is.na(w), .(N = .N, mx = sum(product == "multiplex"), dup = sum(product == "duplex")), keyby = .(side, w)][, share := mx / N]
print(ew)
stopifnot(nrow(ew) == 4)
cat("post-reform lots counted via high-value non-new-build permits:\n"); print(pr[, .N, by = .(product, highVal)])

# ---- cutoff sensitivity: key shares under each new-build value cutoff ----
sens <- rbindlist(lapply(names(cutoffs), function(k) {
  L <- linksAll[isNB(linksAll, cutoffs[k]) & product != "otherMultiple"]
  P <- L[applied >= R1_START][order(rollStart, -issued)][, .SD[1], by = rollStart]
  P[, side := fifelse(lotLon < ONT, "West", "East")]
  sh <- function(d) d[, mean(product == "multiplex")]
  Q <- L[year(issued) >= YEAR_MIN][order(rollStart, -issued)][, .SD[1], by = rollStart]
  E <- L[round(landWidth) == 33][, era := fcase(applied >= as.IDate("2018-09-18") & applied < R1_START, "dup",
                                                  applied >= R1_START, "r11", default = NA_character_)][!is.na(era)]
  E <- E[order(rollStart, era, -issued)][, .SD[1], by = .(rollStart, era)][, side := fifelse(lotLon < ONT, "West", "East")]
  dv <- function(e, s) E[era == e & side == s & product != "multiplex", mean(product == "duplex")]
  data.table(cutoff = k, dollars = cutoffs[k],
             nonNBadded = L[typeofwork != "New Building", .N],
             mx33_2024 = sh(Q[round(landWidth) == 33]), mxWider_2024 = sh(Q[round(landWidth) > 33]),
             E33 = sh(P[side == "East" & round(landWidth) == 33]), E50 = sh(P[side == "East" & round(landWidth) == 50]),
             W33 = sh(P[side == "West" & round(landWidth) == 33]), W50 = sh(P[side == "West" & round(landWidth) == 50]),
             nE33 = P[side == "East" & round(landWidth) == 33, .N], nW33 = P[side == "West" & round(landWidth) == 33, .N],
             dupE_dupEra = dv("dup", "East"), dupW_dupEra = dv("dup", "West"), dupE_r11 = dv("r11", "East"), dupW_r11 = dv("r11", "West"))
}))
cat("\n=== new-build cutoff sensitivity (shares) ===\n")
print(sens[, lapply(.SD, function(x) if (is.numeric(x) && max(abs(x), na.rm = TRUE) <= 1) round(x, 3) else x)])

# ---- macros for the draft: rounded-33 lots, multiplex share of new builds on 33s vs wider ----
lotShare33 <- dtS[, mean(round(landWidth) == 33)]
dt[, r33 := round(landWidth) == 33]
mx <- dt[product != "otherMultiple" & round(landWidth) >= 33, .(N = .N, mx = sum(product == "multiplex")), by = r33][, share := mx / N]
print(mx)
stopifnot(nrow(mx) == 2)
mac <- c(
  sprintf("\\newcommand{\\newBuildValueQuantile}{%.0f}", 100 * HIGHVAL_Q),
  sprintf("\\newcommand{\\newBuildValueThreshold}{%s}", formatC(hvTh, format = "d", big.mark = ",")),
  sprintf("\\newcommand{\\lotShareThirtyThreeRone}{%.0f}", 100 * lotShare33),
  sprintf("\\newcommand{\\lotsRoneSFD}{%s}", formatC(nrow(dtS), format = "d", big.mark = ",")),
  sprintf("\\newcommand{\\mplxShareThirtyThree}{%.0f}", 100 * mx[r33 == TRUE, share]),
  sprintf("\\newcommand{\\mplxShareWider}{%.0f}", 100 * mx[r33 == FALSE, share]),
  sprintf("\\newcommand{\\mplxShareGap}{%.0f}", 100 * (mx[r33 == FALSE, share] - mx[r33 == TRUE, share])),
  sprintf("\\newcommand{\\mplxNewBuildsThirtyThree}{%s}", formatC(mx[r33 == TRUE, N], format = "d", big.mark = ",")),
  sprintf("\\newcommand{\\mplxNewBuildsWider}{%s}", formatC(mx[r33 == FALSE, N], format = "d", big.mark = ",")),
  ew[, sprintf("\\newcommand{\\mplxShare%s%s}{%.0f}", side, fifelse(w == "33", "ThirtyThree", "Fifty"), 100 * share)],
  ew[, sprintf("\\newcommand{\\mplxN%s%s}{%s}", side, fifelse(w == "33", "ThirtyThree", "Fifty"), formatC(N, format = "d", big.mark = ","))])
writeLines(c("% generated by scripts/multiplexLotWidth.R; do not edit", mac), "../text/multiplexLotWidth.tex")
cat(mac, sep = "\n")

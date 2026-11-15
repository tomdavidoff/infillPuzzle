# multiplexLotWidth.R
# New-building permits issued 2024+, matched to 2018 single-family lots in R1-1 (BCA 2019 roll):
# share of multiplexes on 33-ft lots, and multiplex share of new builds by lot width.
# Tom Davidoff 11/15/26

library(data.table)
library(sf)
library(RSQLite)

MATCHDIST <- 5      # m, permit point to 2018 SFD centroid (as in vancouverR1FSRDate.R)
YEAR_MIN  <- 2024
ADDRDIST  <- 60     # m, max permit-to-lot distance for an address match (E/W ambiguity, street-side geocodes)
fPermit   <- "~/DropboxExternal/dataRaw/vancouverIssuedBuildingPermits.csv"
fSingles  <- "~/DropboxExternal/dataProcessed/bca19VancouverSingles.rds"
fZoning   <- "~/DropboxExternal/dataRaw/vancouver_zoning.geojson"
fGeo26    <- "~/DropboxExternal/dataProcessed/bca26FolioGeometryVancouver.rds"  # rollStart centroids, 2026 gpkg (see vancouverR1FSRDate.R)
fGpkg26   <- list.files("~/bigFiles/latestSpatialBCA/", pattern = "\\.gpkg$", full.names = TRUE)
fBCA19    <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
stopifnot(difftime(Sys.time(), file.mtime(fPermit), units = "hours") < 48)

lost <- function(n0, n1, what) cat(sprintf("%-40s %6d -> %6d (lost %.1f%%)\n", what, n0, n1, 100 * (1 - n1 / n0)))

dtS <- readRDS(fSingles)
dtS <- unique(dtS[, .(rollNumber, folioID, landWidth, longitude, latitude)])  # file repeats folios across valuation rows
stopifnot(!anyDuplicated(dtS$rollNumber))
dtS[, rollStart := floor(as.numeric(rollNumber) / 1000)]
n0 <- nrow(dtS); dtS <- dtS[landWidth > 0 & is.finite(longitude)]; lost(n0, nrow(dtS), "2018 SFD lots: width>0, located")
n0 <- nrow(dtS); dtS <- unique(dtS, by = "rollStart"); lost(n0, nrow(dtS), "one lot per rollStart")
sfS <- st_transform(st_as_sf(dtS, coords = c("longitude", "latitude"), crs = 4326), 26910)
zR1 <- st_transform(st_make_valid(st_read(fZoning, quiet = TRUE)), 26910)
zR1 <- zR1[zR1$zoning_district == "R1-1", ]
inR1 <- lengths(st_intersects(sfS, zR1)) > 0
n0 <- nrow(dtS); dtS <- dtS[inR1]; sfS <- sfS[inR1, ]; lost(n0, nrow(dtS), "in R1-1")

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
n0 <- nrow(dtP); dtP <- dtP[typeofwork == "New Building" & year(issued) >= YEAR_MIN]; lost(n0, nrow(dtP), "New Building, issued 2024+")
dtP[, product := fcase(grepl("Multiple Dwelling", specificusecategory), "multiplex",
                       grepl("Duplex",            specificusecategory), "duplex",
                       grepl("Single",            specificusecategory), "single",
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
cat("rollStart vs address agreement where both match:", dtP[roll26ok & !is.na(rollStart), round(mean(roll26 == rollStart), 3)],
    " N =", dtP[roll26ok & !is.na(rollStart), .N], "\n")
dtP[, how := fcase(roll26ok, "rollStart26", a26ok, "address26", !is.na(rollStart), "address19", dist <= MATCHDIST, "near2018", default = NA_character_)]
dtP[how == "address26", rollStart := rollA26]
dtP[roll26ok == TRUE, rollStart := roll26]
dtP[is.na(rollStart) & dist <= MATCHDIST, rollStart := rollNear]
print(dcast(dtP[, .N, by = .(product, how)], product ~ how, fill = 0))
n0 <- nrow(dtP); dtP <- dtP[!is.na(rollStart)]; lost(n0, nrow(dtP), "matched to 2018 R1-1 SFD lot")
print(dtP[, .N, by = product])
n0 <- nrow(dtP); dtP <- dtP[order(rollStart, -issued)][, .SD[1], by = rollStart]; lost(n0, nrow(dtP), "latest permit per lot")

n0 <- nrow(dtP)
dt <- merge(dtP[, .(rollStart, product, issued)], dtS[, .(rollStart, landWidth)], by = "rollStart")
stopifnot(nrow(dt) == n0, !anyDuplicated(dt$rollStart))
dt[, widthBin := fcase(landWidth > 30 & landWidth <= 36, "33",
                       landWidth > 36 & landWidth <= 41, "37-41",
                       landWidth > 41 & landWidth <= 50, "42-50",
                       landWidth > 50, ">50", default = "<=30")]
dt[, widthBin := factor(widthBin, c("<=30", "33", "37-41", "42-50", ">50"))]

cat("\n=== share of 2024+ multiplexes by lot width ===\n")
print(dt[product == "multiplex", .N, keyby = widthBin][, share := round(N / sum(N), 3)][])
cat("\n=== by issue year: multiplex N and share on 33 ===\n")
print(dt[product == "multiplex", .(N = .N, share33 = round(mean(widthBin == "33"), 3)), keyby = .(year = year(issued))])
cat("\n=== product mix within width bin (2024+) ===\n")
print(dcast(dt[, .N, by = .(widthBin, product)], widthBin ~ product, fill = 0)[, `:=`(
  total = duplex + multiplex + single, multiplexShare = round(multiplex / (duplex + multiplex + single), 3))][])
cat("\nbaseline: share of 2018 R1-1 SFD lots that are 33:", round(dtS[, mean(landWidth > 30 & landWidth <= 36)], 3), "\n")
cat("latest issue date in data:", format(max(dtP$issued)), "\n")

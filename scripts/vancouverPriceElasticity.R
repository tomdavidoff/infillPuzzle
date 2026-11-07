# vancouverPriceElasticity.R
# price and elasticity by geography
# Tom Davidoff
# 10/29/26

library(sf)
library(DBI)
library(RSQLite)
library(data.table)
library(fixest)

yrs <- 2012:2018   # sale window for medians and elasticities

# 2019 data with sales through 2018
con <- dbConnect(SQLite(), "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3")
q <- "SELECT s.conveyancePrice, s.conveyanceDate, f.rollNumber, i.MB_effective_year,
  i.MB_total_finished_area, i.land_width, i.land_depth, d.actualUseDescription, d.neighbourhoodDescription
FROM folio f
JOIN sales s ON s.folioID = f.folioID
JOIN folioDescription d ON d.folioID = f.folioID
JOIN residentialInventory i ON i.roll_number = f.rollNumber
WHERE f.jurisdictionCode = '200'
  AND s.conveyanceTypeDescription = 'Improved Single Property Transaction'
  AND d.actualUseDescription IN ('Residential Dwelling with Suite', 'Single Family Dwelling')"
df <- as.data.table(dbGetQuery(con, q))
dbDisconnect(con)
print(table(df$actualUseDescription))

num <- c("conveyancePrice", "MB_total_finished_area", "land_width", "land_depth", "MB_effective_year")
df[, (num) := lapply(.SD, as.numeric), .SDcols = num]
df[, convDate := as.IDate(conveyanceDate)]
df <- df[year(convDate) %in% yrs]
df[, rollStart := floor(as.numeric(rollNumber) / 1000)]

# tracts; 2026 parcels -> tract (centroid)
ct <- st_read("~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp", quiet = TRUE)
ct <- st_transform(st_make_valid(ct[ct$PRUID == "59", "CTNAME"]), 3005)

gpkg <- list.files("~/bigFiles/latestSpatialBCA", pattern = "\\.gpkg$", full.names = TRUE)[1]
g <- st_read(gpkg, quiet = TRUE, query =
  "SELECT ROLL_NUMBER, geom FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
   WHERE JURISDICTION_CODE = '200'")
g$rollStart <- floor(as.numeric(g$ROLL_NUMBER) / 1000)
g <- g[g$rollStart %in% df$rollStart & !duplicated(g$rollStart), ]
g <- st_transform(st_make_valid(g), 3005)
# ---- zoning: R1-1 shapes (used for permits AND for the comp inR1 flag) -------
dgZ <- st_read("~/DropboxExternal/dataRaw/vancouver_zoning.geojson")
dgZ <- dgZ[dgZ$zoning_district == "R1-1", ]
print(head(dgZ))
print(head(g))

# 1. Put zoning in the parcels' CRS (BC Albers, metres)
dgZ <- st_transform(dgZ, st_crs(g))
dgZ <- st_make_valid(dgZ)                  # city polygons sometimes have slivers/self-intersections
dgZ <- dgZ[, c("zoning_district", "zoning_category")]   # drop geo_point_2d text col etc.

# 2. One interior point per parcel (guaranteed inside, unlike a centroid)
gPt <- st_point_on_surface(g)

# 3. Point-in-polygon join
gPt <- st_join(gPt, dgZ, join = st_within, left = TRUE)

# 4. Attach back to the parcel polygons
g <- merge(g, st_drop_geometry(gPt)[, c("ROLL_NUMBER", "zoning_district", "zoning_category")],
           by = "ROLL_NUMBER", all.x = TRUE)

geo <- as.data.table(st_drop_geometry(st_join(st_point_on_surface(g), ct, join = st_within)))
geo <- geo[!is.na(CTNAME), .(rollStart, CTNAME,zoning_district,zoning_category)]
geo <- geo[zoning_category=="R1"]

df <- merge(df, geo, by = "rollStart", all.x = TRUE)
print(head(df))
print(table(is.na(df$CTNAME)))
print(table(df[is.na(CTNAME), neighbourhoodDescription]))

# lot dims: 0 is the missing sentinel
df[land_width == 0, land_width := NA]
df[land_depth == 0, land_depth := NA]
df <- df[!is.na(land_depth) & !is.na(land_width)]
qc <- quantile(df$land_depth, c(0.2, 0.8))
df <- df[land_depth %between% qc]
df[, w := round(land_width)]
df[w>47 & w<53, w := 50] # try to juice lot size

s <- df[w %in% c(33, 50) & conveyancePrice > 0 & MB_total_finished_area > 0 & !is.na(CTNAME)]
s[, convMonth := format(convDate, "%Y-%m")]

s[, `:=`(lsqft = log(MB_total_finished_area),
         lage  = log(pmax(year(convDate) - MB_effective_year, 0) + 1),
         tw    = paste(CTNAME, w, sep = "_"))]
s <- s[!is.na(lage)]
MAXAGE <- 30
s <- s[lage <= log(MAXAGE + 1)]
s[, N := .N, by = tw]
s <- s[N >= 20]

split <- FALSE   # TRUE: level and elasticity from disjoint folios
sA <- if (split) s[rollStart %% 2 == 0] else s
sB <- if (split) s[rollStart %% 2 == 1] else s

# level: tract x width FE in log ppsf, net of log age and month
m1 <- feols(log(conveyancePrice / MB_total_finished_area) ~ lage | tw + convMonth, sA)
fe <- fixef(m1)$tw

# elasticity: one regression, tract x width slopes on log sqft
m2 <- feols(log(conveyancePrice) ~ i(tw, lsqft) + lage | tw + convMonth, sB)
k  <- grep("^tw::", names(coef(m2)))
el <- data.table(tw = sub("^tw::(.*):lsqft$", "\\1", names(coef(m2))[k]),
                 elas = coef(m2)[k], se = se(m2)[k])

dPEl <- merge(merge(unique(s[, .(tw, CTNAME, w, N)]),
                    data.table(tw = names(fe), lppsf = as.numeric(fe)), by = "tw"),
              el, by = "tw")
print(summary(dPEl))
print(dPEl[, .(medLppsf = median(lppsf), medElas = median(elas), nTracts = .N), keyby = w])
for (p in c(0, .25, .5, .75))
  print(dPEl[N >= quantile(N, p), .(pN = p, cor = cor(lppsf, elas), nTracts = .N), keyby = w])

elW <- dcast(dPEl, CTNAME ~ w, value.var = c("lppsf", "elas", "N"))
print(elW[, cor(elas_33, elas_50, use = "complete.obs")])
print(elW[, cor(lppsf_33, lppsf_50, use = "complete.obs")])
print(head(elW))
print(summary(elW))
print(cor(elW[N_33>30 & N_50>30, .(elas_33,elas_50,lppsf_33, lppsf_50)], use = "complete.obs"))

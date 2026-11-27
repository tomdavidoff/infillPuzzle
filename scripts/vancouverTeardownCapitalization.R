library(data.table); library(sf); library(fixest); library(ggplot2); library(DBI)

bcaPath <- list.files("~/bigFiles/latestSpatialBCA", "\\.gpkg$", full.names = TRUE)[1]
rawDir  <- "~/DropboxExternal/dataRaw"
ods     <- "https://opendata.vancouver.ca/api/explore/v2.1/catalog/datasets/"
westLon <- -123.101  # Ontario St
events  <- setNames(as.IDate(c("2018-09-18", "2023-01-15")), c("duplex", "r11"))
horizon <- 24  # months from sale to demo/new-build permit
SPLIT   <- Sys.getenv("SPLIT", "east")  # east: East vs West of Ontario; width: 50' vs 33' lots (2018 SFD roll)
lab     <- if (SPLIT == "width") c("50'", "33'") else c("East", "West")
tdq     <- 0.25  # roll teardown flag: improvement share in bottom tdq of event sample

q <- \(lyr, cols) sprintf("SELECT %s FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_%s_SV WHERE JURISDICTION_CODE = '200'", cols, lyr)
cached <- function(f, url) {
  f <- file.path(rawDir, f)
  if (!file.exists(f) || difftime(Sys.time(), file.mtime(f), units = "hours") > 48)
    download.file(url, f, mode = "wb")
  f
}
first <- \(x, y) vapply(st_intersects(x, y), \(i) i[1], integer(1))

sales <- st_read(bcaPath, query = q("SALES", "FOLIO_ID, SALES_ID, CONVEYANCE_DATE, CONVEYANCE_PRICE, CONVEYANCE_TYPE_DESCRIPTION, geom"), quiet = TRUE)
desc  <- st_read(bcaPath, query = q("DESCRIPTIONS", "NEIGHBOURHOOD, geom"), quiet = TRUE)
strata <- st_read(bcaPath, query = paste(q("LEGAL_DESCRIPTS", "FOLIO_ID"), "AND STRATA_LOT IS NOT NULL AND STRATA_LOT <> ''"), quiet = TRUE)$FOLIO_ID

# parcels: one polygon per sold folio
parc <- sales[!duplicated(sales$FOLIO_ID), "FOLIO_ID"]
cen  <- st_point_on_surface(parc)
zn   <- st_read(cached("zoning-districts-and-labels.geojson", paste0(ods, "zoning-districts-and-labels/exports/geojson")), quiet = TRUE)
zn   <- st_transform(zn, st_crs(parc))
pt <- data.table(folio = parc$FOLIO_ID,
                 lon = st_coordinates(st_transform(cen, 4326))[, 1],
                 zone = zn$zoning_district[first(cen, zn)],
                 nbhd = desc$NEIGHBOURHOOD[first(cen, desc)],
                 lotArea = as.numeric(st_area(parc)))

# permits, cached 48h; matched to every sold folio polygon containing the point
pm <- fread(cached("issued-building-permits.csv", paste0(ods, "issued-building-permits/exports/csv?delimiter=%2C")))
pm <- pm[typeofwork %in% c("Demolition / Deconstruction", "New Building") & geo_point_2d != ""]
pm[, c("lat", "lon") := tstrsplit(geo_point_2d, ",\\s*", type.convert = TRUE)]
ix <- st_intersects(st_transform(st_as_sf(pm, coords = c("lon", "lat"), crs = 4326), st_crs(parc)), parc)
pm <- data.table(folio = parc$FOLIO_ID[unlist(ix)], pdate = as.IDate(pm$issuedate)[rep(seq_along(ix), lengths(ix))])

# sales: non-strata, single-property, RS/R1-1 lots
s <- unique(as.data.table(st_drop_geometry(sales)), by = c("FOLIO_ID", "SALES_ID"))
setnames(s, c("folio", "sid", "date", "price", "ctype"))
s[, .N, ctype][order(-N)]
n0 <- nrow(s); s <- pt[s, on = "folio"]; stopifnot(nrow(s) == n0, !anyDuplicated(pt$folio))
for (f in alist(!folio %in% strata, zone %like% "^(RS|R1-1)", ctype %like% "Single Property", price > 1e5)) {
  n0 <- nrow(s); s <- s[eval(f)]; cat(deparse(f), "drops", round(1 - nrow(s) / n0, 3), "\n")
}
s[, `:=`(date = as.IDate(date), east = lon > westLon)]
if (SPLIT == "width") {
  lw <- unique(as.data.table(readRDS("~/DropboxExternal/dataProcessed/bca19VancouverSingles.rds"))[, .(folio = folioID, ft = round(landWidth))])
  stopifnot(!anyDuplicated(lw$folio))
  n0 <- nrow(s); s <- lw[s, on = "folio"]; stopifnot(nrow(s) == n0)
  cat("no 2018 SFD width", round(mean(is.na(s$ft)), 3), "; width 33/50 among matched", round(mean(s$ft %in% c(33, 50)[!is.na(s$ft)]), 3), "\n")
  s <- s[ft %in% c(33, 50)]; cat("width sample", nrow(s), "\n")
  s[, g := ft == 50]
} else s[, g := east]
s[, `:=`(lppl = log(price / lotArea), ym = format(date, "%Y-%m"), end = date + round(horizon * 30.44))]
s[, td := FALSE][pm, on = .(folio, date <= pdate, end >= pdate), td := TRUE]

w <- rbindlist(lapply(names(events), \(e) {
  x <- copy(s[abs(as.numeric(date - events[e])) <= 365])
  x[, `:=`(event = e, post = date >= events[e],
           rel = as.integer(floor(as.numeric(date - events[e]) / 91.31)))]
  x
}))
stopifnot(nrow(w) > 0, w[, uniqueN(sid) == .N, event]$V1)

# rolls before each event: 2019 roll (July 2018 values, Oct 2018 condition) for duplex; 2022 roll (July 2021) for R1-1
con <- dbConnect(RSQLite::SQLite(), file.path(rawDir, "REVD19_and_inventory_extracts.sqlite3"))
r19 <- as.data.table(dbGetQuery(con, "SELECT v.folioID AS folio, v.propertyClassCode AS cls, v.landValue AS land, v.improvementValue AS impr
  FROM valuation v JOIN folio f USING (folioID) WHERE trim(f.jurisdictionCode) = '200'"))
dbDisconnect(con)
r22 <- as.data.table(st_drop_geometry(st_read("~/bigFiles/bca_folios.gpkg", quiet = TRUE,
  query = q("GNRL_PROP_VALUES", "FOLIO_ID AS folio, GEN_PROPERTY_CLASS_CODE AS cls, GEN_GROSS_LAND_VALUE AS land, GEN_GROSS_IMPROVEMENT_VALUE AS impr"))))
roll <- unique(rbind(r19[, event := "duplex"], r22[, event := "r11"]), by = c("event", "folio", "cls"))[, lapply(.SD, \(v) sum(as.numeric(v))), .(event, folio), .SDcols = c("land", "impr")]
roll[, ishare := impr / (land + impr)]
stopifnot(roll[, uniqueN(.SD) == .N, .SDcols = c("event", "folio")])
n0 <- nrow(w)
w <- roll[w, on = .(event, folio)]
stopifnot(nrow(w) == n0)
print(w[, .(n = .N, noRoll = mean(is.na(ishare)), badShare = mean(!is.na(ishare) & !(ishare %between% c(0, 1)))), event])
w <- w[ishare %between% c(0, 1)]
cut <- unique(w, by = c("event", "folio"))[, .(cut = quantile(ishare, tdq)), event]
print(cut)
w <- cut[w, on = "event"]
setnames(w, "td", "tdPermit")
w[, td := ishare <= cut]

# validation: roll flag vs ex-post permit flag
print(w[, .(n = .N, permitRate = mean(tdPermit)), .(event, td)][order(event, td)])
print(w[, .(n = .N, permitRate = round(mean(tdPermit), 3), share = round(mean(ishare), 3)),
        .(event, dec = cut(ishare, unique(quantile(ishare, 0:10 / 10)), include.lowest = TRUE, labels = FALSE))][order(event, dec)])
etable(feglm(tdPermit ~ ishare | nbhd + ym, w, split = ~event, family = binomial, cluster = ~nbhd))
w[, .N, .(event, g, td)][order(event, g, td)]

fml <- \(v) as.formula(sprintf("lppl ~ %1$s + g:%1$s + post:%1$s + post:g + post:g:%1$s | nbhd + ym", v))
est <- lapply(c(roll = "td", permit = "tdPermit", share = "ishare"), \(v) feols(fml(v), w, split = ~event, cluster = ~nbhd))
etable(est$roll, est$permit, est$share, headers = rep(names(est), each = 2))

w[, r := resid(feols(lppl ~ 1 | nbhd^event, w, fixef.rm = "none"))]
p <- w[, .(r = mean(r), n = .N), .(event, rel, side = fifelse(g, lab[1], lab[2]), td)]
ggplot(p, aes(rel, r, colour = side, linetype = td)) +
  geom_line() + geom_point(aes(size = n)) + geom_vline(xintercept = -0.5) +
  facet_wrap(~event) + labs(x = "quarters from event", y = "log price / lot area, nbhd-demeaned")
ggsave(if (SPLIT == "width") "text/vancouverTeardownCapWidth.png" else "text/vancouverTeardownCap.png", width = 9, height = 4.5)

# lot-size elasticity of price by month, teardowns vs not (roll flag), each event window
if (SPLIT == "east") {
passed <- as.IDate("2023-09-14")  # R1-1 approved by Council (effective 2023-10-17)
ve <- copy(s[date >= events["r11"] - 365])[, `:=`(event = "r11", post = date >= events["r11"])]  # R1-1: to end of data
setnames(ve, "td", "tdPermit")
n0 <- nrow(ve); ve <- cut[roll[ve, on = .(event, folio)], on = "event"]; stopifnot(nrow(ve) == n0)
ve <- ve[ishare %between% c(0, 1)][, td := ishare <= cut]
v <- rbind(w[event == "duplex"], ve, fill = TRUE)
cat("R1-1 extended window:", format(range(ve$date)), "n =", nrow(ve), "\n")
v[, `:=`(lp = log(price), lla = log(lotArea), cls = fifelse(td, "teardown", "not teardown"),
         period = fcase(!post, "1 pre", event == "r11" & date >= passed, "3 passed", default = "2 post"))]
v[, llc := lla - mean(lla), event]  # centred: uncentred lla:post is near-collinear with post (absorbed by ym)
print(v[, .(n = .N, months = uniqueN(ym), minPerMonth = min(table(ym)), sdLogArea = sd(lla)), .(event, cls)])
el <- rbindlist(lapply(names(events), \(e) rbindlist(lapply(c("teardown", "not teardown"), \(k) {
  ct <- as.data.table(coeftable(feols(lp ~ i(ym, lla) | nbhd + ym, v[event == e & cls == k], vcov = "hetero")), keep.rownames = "term")
  ct[, .(event = e, cls = k, ym = sub("ym::([0-9-]+):lla", "\\1", term), b = Estimate, se = `Std. Error`)]
}))))
el[, month := as.IDate(paste0(ym, "-15"))]
for (e in names(events)) {
  cat("\n==== lot elasticity,", e, "====\n")
  print(etable(feols(lp ~ llc | nbhd + ym, v[event == e], split = ~paste(cls, period), vcov = "hetero")))
  print(etable(feols(lp ~ llc + llc:i(period, ref = "1 pre") + llc:td + llc:td:i(period, ref = "1 pre") | nbhd^td + ym^td, v[event == e], vcov = "hetero")))
}
ggplot(el, aes(month, b, colour = cls)) +
  geom_hline(yintercept = c(0, 1), colour = "grey70") +
  geom_vline(data = data.table(event = names(events), x = events), aes(xintercept = x)) +
  geom_vline(data = data.table(event = "r11", x = passed), aes(xintercept = x), linetype = "dashed") +
  geom_pointrange(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se), position = position_dodge(width = 12), size = 0.2) +
  geom_smooth(aes(weight = 1 / se^2), method = "loess", span = 0.6, se = FALSE) +
  facet_grid(~event, scales = "free_x", space = "free_x", labeller = as_labeller(c(duplex = "Duplex vote, Sept 2018", r11 = "R1-1 announcement, Jan 2023"))) +
  coord_cartesian(ylim = c(-0.5, 2)) + theme_bw() + theme(legend.position = "bottom") +
  labs(x = NULL, y = "elasticity of price w.r.t. lot area", colour = NULL,
       caption = "Vancouver non-strata RS/R1-1 sales; duplex ±1 yr, R1-1 from Jan 2022 to end of data (dashed: R1-1 passed, Sept 14 2023).\nTeardown = bottom quartile improvement share in pre-event roll; nbhd + month FE; 95% CI hetero-robust.")
ggsave("text/vancouverLotElasticity.png", width = 11, height = 5)
}

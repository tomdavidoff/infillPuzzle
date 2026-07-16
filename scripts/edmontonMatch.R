#via Claude
# match permits to assessment history to get parent land area
library(data.table)
library(sf)

asmt <- fread("assessment_history.csv")
prm  <- fread("permits.csv")

setnames(asmt,
  c("Account Number","Assessment Year","House Number","Street Name","Legal Description",
    "Latitude","Longitude","Neighbourhood","Zoning","Lot Size","Assessed Value","Assessment Class 1"),
  c("acct","yr","hn","street","legal","lat","lon","nbhd","zone","lotsize","av","class1"))
asmt[, `:=`(lotsize = as.numeric(gsub(",", "", lotsize)),
            av = as.numeric(gsub("[$,]", "", av)),
            hn = as.integer(hn))]

# parent snapshot: last pre-2023 obs per account, SF-type residential
par <- asmt[yr <= 2022 & class1 == "RESIDENTIAL" & zone %chin% c("RF1","RF2","RF3","RF4","RS")
          ][order(acct, yr)][, .SD[.N], acct]

parse_addr <- function(dt, col = "ADDRESS") {
  dt[, base := sub("^[^,]*,\\s*", "", get(col))]
  dt[, `:=`(hn = as.integer(sub("\\s*-.*$", "", base)),
            street = trimws(sub("^\\S+\\s*-\\s*", "", base)))]
}

# --- infill new builds, clustered to projects ---
prm[, zvec := lapply(strsplit(ZONING, ",\\s*"), unique)]
inf <- prm[YEAR >= 2023 &
           (WORK_TYPE %like% "\\(01\\)" | grepl("to construct", JOB_DESCRIPTION, ignore.case = TRUE)) &
           !grepl("demol|alteration", JOB_DESCRIPTION, ignore.case = TRUE) &
           sapply(zvec, function(z) any(z %chin% c("RS","RSF","RSM"))) &
           (BUILDING_TYPE %like% "Row House|Semi-|Duplex" | UNITS_ADDED >= 2 |
            grepl("row hous|semi-detached|duplex|multi-unit", JOB_DESCRIPTION, ignore.case = TRUE))]
parse_addr(inf)
# project = base address x street (unit permits share the parent address modulo suite prefix)
inf[, proj := .GRP, .(hn, street)]

# --- demos, text-coded (structured WORK_TYPE dies in 2019) ---
demo <- prm[grepl("demol", JOB_DESCRIPTION, ignore.case = TRUE) &
            grepl("single detached|semi-detached|duplex|house|dwelling", JOB_DESCRIPTION, ignore.case = TRUE) &
            !grepl("interior|partial|garage|shed|accessory", JOB_DESCRIPTION, ignore.case = TRUE) &
            YEAR >= 2020]
parse_addr(demo)
demo[, did := .I]

# --- tier 1/2: project <-> demo by address ---
m1 <- merge(inf[, .(proj, hn, street, byr = YEAR)],
            demo[, .(did, hn, street, dyr = YEAR)], by = c("hn","street"))[byr >= dyr]
m2 <- inf[, .(proj, street, hn, byr = YEAR)
        ][demo[, .(did, street, dhn = hn, dyr = YEAR)], on = "street",
          allow.cartesian = TRUE, nomatch = NULL
        ][abs(hn - dhn) <= 6 & abs(hn - dhn) > 0 & byr >= dyr]
pd <- unique(rbind(m1[, .(proj, did, tier = 1L)], m2[, .(proj, did, tier = 2L)]))
setorder(pd, proj, did, tier); pd <- pd[, .SD[1], .(proj, did)]

# spatial validation of tier 2: demo point within 20m of any build point in project
to_sf <- function(dt, lo, la) st_transform(
  st_as_sf(dt[!is.na(get(la)) & !is.na(get(lo))], coords = c(lo, la), crs = 4326), 3776)
inf_sf  <- to_sf(inf,  "LONGITUDE", "LATITUDE")
demo_sf <- to_sf(demo, "LONGITUDE", "LATITUDE")
pd2 <- pd[tier == 2L][inf_sf$proj != 0]  # placeholder no-op; keep structure
chk <- merge(pd[tier == 2L],
             data.table(did = demo_sf$did, dgeo = st_geometry(demo_sf)), by = "did")
chk <- merge(chk, data.table(proj = inf_sf$proj, bgeo = st_geometry(inf_sf)),
             by = "proj", allow.cartesian = TRUE)
chk[, d := as.numeric(st_distance(st_sfc(dgeo, crs = 3776), st_sfc(bgeo, crs = 3776), by_element = TRUE))]
ok2 <- unique(chk[d <= 20, .(proj, did)])
pd <- rbind(pd[tier == 1L, .(proj, did)], ok2)

# --- demo -> parent account ---
# (a) legal join where demo legal survived migration
demo_legal <- demo[LEGAL_DESCRIPTION != "", {
  s <- LEGAL_DESCRIPTION
  lots <- if (grepl("Lots?\\s", s)) {
    raw <- gsub("\\s", "", sub(".*Lots?\\s+([0-9A-Za-z,\\-]+).*", "\\1", s))
    unlist(lapply(strsplit(raw, ",")[[1]], function(p)
      if (grepl("^\\d+-\\d+$", p)) { r <- as.integer(strsplit(p, "-")[[1]]); as.character(r[1]:r[2]) } else p))
  } else NA_character_
  .(plan  = sub(".*Plan\\s+(\\S+).*", "\\1", s),
    block = ifelse(grepl("Blk", s), sub(".*Blk\\s+(\\S+).*", "\\1", s), NA_character_),
    lot   = lots)
}, did]
asmt_legal <- par[, {
  segs <- strsplit(legal, "\\s*/\\s*")[[1]]
  rbindlist(lapply(segs, function(s) data.table(
    plan  = sub(".*Plan:\\s*(\\S+).*", "\\1", s),
    block = ifelse(grepl("Block:", s), sub(".*Block:\\s*(\\S+).*", "\\1", s), NA_character_),
    lot   = ifelse(grepl("Lot:",   s), sub(".*Lot:\\s*(\\S+).*",   "\\1", s), NA_character_))))
}, acct]
da_legal <- merge(demo_legal, asmt_legal, by = c("plan","block","lot"))[, .(did, acct)]

# (b) exact address join demo -> parent
da_addr <- merge(demo[, .(did, hn, street)], par[, .(acct, hn, street)],
                 by = c("hn","street"))[, .(did, acct)]

# (c) spatial: demo point -> parent within 15m
par_sf <- to_sf(par, "lon", "lat")
nb <- st_is_within_distance(demo_sf, par_sf, dist = 15)
da_sp <- rbindlist(lapply(seq_along(nb), function(i) if (length(nb[[i]]))
  data.table(did = demo_sf$did[i], acct = par_sf$acct[nb[[i]]])))

da <- unique(rbind(da_legal, da_addr, da_sp))

# --- fallback for projects with no demo match: build point -> parent within 20m ---
proj_nodemo <- setdiff(inf$proj, pd$proj)
nb2 <- st_is_within_distance(inf_sf[inf_sf$proj %in% proj_nodemo, ], par_sf, dist = 20)
sub_sf <- inf_sf[inf_sf$proj %in% proj_nodemo, ]
pa_fall <- unique(rbindlist(lapply(seq_along(nb2), function(i) if (length(nb2[[i]]))
  data.table(proj = sub_sf$proj[i], acct = par_sf$acct[nb2[[i]]]))))

# --- assemble: project -> unique parent accounts -> total land area ---
pa <- unique(rbind(merge(pd, da, by = "did", allow.cartesian = TRUE)[, .(proj, acct)], pa_fall))
proj_area <- merge(pa, par[, .(acct, lotsize, nbhd, zone, av)], by = "acct"
                 )[, .(n_parents = uniqueN(acct),
                       parent_area = sum(lotsize),
                       parent_av = sum(av),
                       nbhd = nbhd[1]), proj]
proj_out <- merge(inf[, .(proj, base = base[1], byr = min(YEAR),
                          units = sum(UNITS_ADDED, na.rm = TRUE),
                          btype = BUILDING_TYPE[which.max(nchar(BUILDING_TYPE))]), proj],
                  proj_area, by = "proj")
proj_out[, upa := units / parent_area]

fwrite(pa, "project_parent_xwalk.csv")
fwrite(proj_out, "project_parent_area.csv")

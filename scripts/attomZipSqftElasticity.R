library(data.table)
library(httr2)
library(haven)
library(arrow)
library(fixest)
library(ggplot2)

download  <- TRUE   # fetch/slim; existing parquets lacking AreaLotSF are re-slimmed
rankLo    <- 10     # CBSAs ranked by file size
rankHi    <- 80
yrs       <- 2015:2019
minN      <- 30     # sales per ZIP
minZip    <- 10     # ZIPs per CBSA for a correlation
thrs      <- c(0, 0.5, 0.7, 0.8, 0.9)  # ZCTA share of units 1-unit detached (ACS 2015-19 B25024)
minSfdZip <- 0.9    # threshold for the binscatter

slimDir <- path.expand("~/DropboxExternal/dataProcessed/attomSlim")
keep <- c("ATTOM_ID", "TransactDate", "ValidTransact", "InvalidPrice", "TransferAmount",
          "PropertyType_chars", "UnitsCount", "AreaBuilding", "AreaLotSF", "YearBuilt_Gen",
          "PropertyAddressZIP", "CBSACode", "TaxMarketValueLand", "TaxMarketValueTotal")

# ---- download and slim ----
if (download) {
  lnk <- "https://www.dropbox.com/scl/fo/ha6138r8xb4u0a0s0jvhg/AAo_esxOG3OtkthvprNk28o?rlkey=69yad6m2gd0kdcbdv7ndb9sdj"
  readRenviron("~/.Renviron")
  tok <- Sys.getenv("DROPBOX_TOKEN")
  if (!nzchar(tok)) stop("DROPBOX_TOKEN not set")
  dir.create(slimDir, showWarnings = FALSE, recursive = TRUE)

  dbx <- function(ep, body) request(paste0("https://api.dropboxapi.com/2/", ep)) |>
    req_auth_bearer_token(tok) |> req_body_json(body) |>
    req_error(body = \(resp) resp_body_string(resp)) |> req_perform() |> resp_body_json()

  lsDbx <- function(path = "") {
    r <- dbx("files/list_folder", list(path = path, shared_link = list(url = lnk)))
    e <- r$entries
    while (isTRUE(r$has_more)) {
      r <- dbx("files/list_folder/continue", list(cursor = r$cursor))
      e <- c(e, r$entries)
    }
    rbindlist(lapply(e, \(x) if (x$.tag == "folder") lsDbx(paste0(path, "/", x$name))
                             else list(path = paste0(path, "/", x$name), size = x$size)))
  }

  files <- lsDbx()[grepl("_cbsa_\\d+\\.dta$", path)][, cbsa := sub(".*_cbsa_(\\d+)\\.dta$", "\\1", path)][order(-size)]
  sel <- files[rankLo:min(rankHi, .N)]

  for (i in seq_len(nrow(sel))) {
    out <- file.path(slimDir, sprintf("cbsa_%s.parquet", sel$cbsa[i]))
    if (file.exists(out) && "AreaLotSF" %in% open_dataset(out)$schema$names) next
    tmp <- path.expand(sprintf("~/Downloads/cbsa_%s.dta", sel$cbsa[i]))
    request("https://content.dropboxapi.com/2/sharing/get_shared_link_file") |>
      req_auth_bearer_token(tok) |> req_method("POST") |>
      req_headers(`Dropbox-API-Arg` = jsonlite::toJSON(list(url = lnk, path = sel$path[i]), auto_unbox = TRUE)) |>
      req_error(body = \(resp) resp_body_string(resp)) |> req_timeout(3600) |> req_perform(path = tmp)
    write_parquet(zap_labels(read_dta(tmp, col_select = tidyselect::any_of(keep))), out)
    unlink(tmp)
  }
}

# ---- ZCTA detached share (NHGIS, needs IPUMS_API_KEY in ~/.Renviron) ----
zctaFile <- path.expand("~/DropboxExternal/dataRaw/ipums/nhgisZcta_B25024_201519.csv")
if (!file.exists(zctaFile)) {
  library(ipumsr)
  readRenviron("~/.Renviron")
  if (!nzchar(Sys.getenv("IPUMS_API_KEY"))) stop("IPUMS_API_KEY not set")
  ext <- define_extract_agg("nhgis", description = "ZCTA units in structure 2015-19",
           datasets = ds_spec("2015_2019_ACS5a", data_tables = "B25024", geog_levels = "zcta")) |>
    submit_extract() |> wait_for_extract()
  zf <- download_extract(ext, download_dir = path.expand("~/Downloads"), overwrite = TRUE)
  fwrite(as.data.table(read_ipums_agg(zf)), zctaFile)
}
zc  <- fread(zctaFile)
tot <- grep("E001$", names(zc), value = TRUE)
det <- grep("E002$", names(zc), value = TRUE)
sfdZip <- zc[get(tot) > 0, .(zip = as.integer(ZCTA5A), sfdShare = get(det) / get(tot))]

# ---- sample ----
d <- rbindlist(lapply(list.files(slimDir, full.names = TRUE), read_parquet), fill = TRUE)
d <- d[ValidTransact == 1 & InvalidPrice == 0 & TransferAmount > 0 & PropertyType_chars == "SFR" &
       (is.na(UnitsCount) | UnitsCount <= 1) & year(TransactDate) %in% yrs &
       between(AreaBuilding, 500, 8000) & YearBuilt_Gen > 1800 & !is.na(PropertyAddressZIP)]
d[, `:=`(zip = as.character(PropertyAddressZIP), yr = year(TransactDate),
         yb10 = pmax(10 * (YearBuilt_Gen %/% 10), 1900),
         lp = log(TransferAmount), ls = log(AreaBuilding), lppsf = log(TransferAmount / AreaBuilding),
         llot = fifelse(AreaLotSF > 0, log(AreaLotSF), NA_real_))]
d[, `:=`(lo = quantile(lppsf, .01), hi = quantile(lppsf, .99)), by = CBSACode]
d <- d[between(lppsf, lo, hi)][, zipi := as.integer(zip)][sfdZip, on = .(zipi = zip), sfdShare := i.sfdShare]

# ---- level (ZIP FE, pooled hedonic) and elasticity (separate ZIP regressions) ----
est <- function(thr, lot) {
  x <- if (thr > 0) d[sfdShare >= thr] else d
  if (lot) x <- x[!is.na(llot)]
  x <- x[, if (.N >= minN) .SD, by = .(CBSACode, zip)]
  f1 <- if (lot) lp ~ ls + llot | zip + YearBuilt_Gen + yr else lp ~ ls | zip + YearBuilt_Gen + yr
  lev <- x[, {fe <- fixef(feols(f1, .SD))$zip; .(zip = names(fe), lev = unname(fe))}, by = CBSACode]
  el  <- x[, {b <- coef(feols(lp ~ i(zip, ls) | zip^yb10 + zip^yr, .SD))
              .(zip = sub("^zip::(.*):ls$", "\\1", names(b)), elast = unname(b))}, by = CBSACode]
  lev[el, on = .(CBSACode, zip), nomatch = 0][x[, .(n = .N), by = .(CBSACode, zip)], on = .(CBSACode, zip),
      nomatch = 0][, `:=`(thr = thr, lot = lot)]
}
zz <- rbindlist(lapply(c(FALSE, TRUE), \(l) rbindlist(lapply(thrs, est, lot = l))))

# within-CBSA correlations
res <- zz[, if (.N >= minZip) .(nZip = .N, pearson = cor(lev, elast),
                                spearman = cor(lev, elast, method = "spearman")), by = .(thr, lot, CBSACode)]
res[, .(cbsas = .N, zips = sum(nZip), medPearson = median(pearson), medSpearman = median(spearman),
        sharePos = mean(spearman > 0)), keyby = .(lot, thr)]

# pooled slope of elasticity on level, CBSA FE
path <- zz[, {m0 <- feols(elast ~ lev | CBSACode, .SD, cluster = ~CBSACode)
              m1 <- feols(elast ~ lev | CBSACode, .SD, weights = ~n, cluster = ~CBSACode)
              .(wt = c("unweighted", "sales-weighted"), b = c(coef(m0), coef(m1)), se = c(se(m0), se(m1)),
                cbsas = uniqueN(CBSACode), zips = .N)}, keyby = .(lot, thr)]
path[, spec := fifelse(lot, "with log lot", "no lot")]
path

ggplot(path, aes(thr, b, colour = wt)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_pointrange(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se), position = position_dodge(0.03)) +
  geom_line(position = position_dodge(0.03)) +
  facet_wrap(~spec) +
  labs(x = "minimum ZCTA share of units single detached", y = "d elasticity / d ZIP price level", colour = NULL)
ggsave("text/attomZipLevElastPath.pdf", width = 8, height = 4)

# binscatter at the main threshold, with lot
z <- zz[thr == minSfdZip & lot == TRUE]
z[, levd := lev - mean(lev), by = CBSACode][, bin := cut(levd, quantile(levd, 0:20 / 20), include.lowest = TRUE)]
ggplot(z[, .(levd = mean(levd), elast = mean(elast)), by = bin], aes(levd, elast)) + geom_point() +
  labs(x = "ZIP price level (FE, demeaned within CBSA)", y = "mean ZIP size elasticity")
ggsave("text/attomZipLevElastBin.pdf", width = 6, height = 4)

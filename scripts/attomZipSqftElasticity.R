library(data.table)
library(httr2)
library(haven)
library(arrow)
library(fixest)
library(ggplot2)
setFixest_notes(FALSE)

download  <- TRUE   # TRUE to fetch/slim (needs fresh token); parquets lacking AreaLotSF are re-slimmed
rankLo    <- 1      # CBSAs ranked by file size
rankHi    <- 80
yrs       <- 2015:2019
minN      <- 30     # sales per ZIP
minZip    <- 10     # ZIPs per CBSA for a correlation
thrs      <- c(0, 0.5, 0.7, 0.8, 0.9)  # ZCTA share of units 1-unit detached (ACS 2015-19 B25024)
minSfdZip <- 0.9    # threshold for the binscatter

slimDir  <- path.expand("~/DropboxExternal/dataProcessed/attomSlim")
rankFile <- path.expand("~/DropboxExternal/dataProcessed/attomCbsaRanks.csv")
keep <- c("ATTOM_ID", "TransactDate", "ValidTransact", "InvalidPrice", "TransferAmount",
          "PropertyType_chars", "UnitsCount", "AreaBuilding", "AreaLotSF", "YearBuilt_Gen",
          "PropertyAddressZIP", "CBSACode", "TaxMarketValueLand", "TaxMarketValueTotal")

# ---- download and slim ----
if (download) {
  lnk <- "https://www.dropbox.com/scl/fo/ha6138r8xb4u0a0s0jvhg/AAo_esxOG3OtkthvprNk28o?rlkey=69yad6m2gd0kdcbdv7ndb9sdj"
  tok <- gsub("\\s", "", paste(readLines("~/.dropboxToken", warn = FALSE), collapse = ""))
  if (!startsWith(tok, "sl.") || nchar(tok) > 2000) stop("~/.dropboxToken should hold exactly one token")
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
  fwrite(files[, .(cbsa, size, rank = .I)], rankFile)
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
getZcta <- function(tab) {
  f <- path.expand(sprintf("~/DropboxExternal/dataRaw/ipums/nhgisZcta_%s_201519.csv", tab))
  if (!file.exists(f)) {
    library(ipumsr)
    readRenviron("~/.Renviron")
    if (!nzchar(Sys.getenv("IPUMS_API_KEY"))) stop("IPUMS_API_KEY not set")
    ext <- define_extract_agg("nhgis", description = paste("ZCTA", tab, "2015-19"),
             datasets = ds_spec("2015_2019_ACS5a", data_tables = tab, geog_levels = "zcta")) |>
      submit_extract() |> wait_for_extract()
    zf <- download_extract(ext, download_dir = path.expand("~/Downloads"), overwrite = TRUE)
    fwrite(as.data.table(read_ipums_agg(zf)), f)
  }
  fread(f)
}
zc  <- getZcta("B25024")  # units in structure
zi  <- getZcta("B19013")  # median household income
inc <- zi[, .(zip = as.integer(ZCTA5A), linc = log(get(grep("E001$", names(zi), value = TRUE))))][is.finite(linc)]
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
  el  <- x[, {m <- feols(lp ~ i(zip, ls) | zip^yb10 + zip^yr, .SD, vcov = "hetero")
              .(zip = sub("^zip::(.*):ls$", "\\1", names(coef(m))), elast = unname(coef(m)),
                seEl = unname(se(m)))}, by = CBSACode]
  lev[el, on = .(CBSACode, zip), nomatch = 0][x[, .(n = .N, levP = median(lppsf)), by = .(CBSACode, zip)], on = .(CBSACode, zip),
      nomatch = 0][, `:=`(thr = thr, lot = lot)]
}
zz <- rbindlist(lapply(c(FALSE, TRUE), \(l) rbindlist(lapply(thrs, est, lot = l))))
zz <- zz[is.finite(seEl) & seEl > 0]

# true cross-ZIP variance of elasticity (within CBSA), method of moments; weights
zz[, tau2 := max(0, var(elast - ave(elast, CBSACode)) - mean(seEl^2)), by = .(lot, thr)]
zz[, `:=`(wIv = 1 / seEl^2, wRe = 1 / (seEl^2 + tau2))]

# distribution of ZIP elasticities and their precision; rel = signal share of cross-ZIP variance
zz[, c(as.list(quantile(elast, c(.01, .1, .25, .5, .75, .9, .99))),
       .(medSe = median(seEl), shareOut = mean(elast < 0 | elast > 1.5),
         tau = sqrt(tau2[1]), rel = tau2[1] / (tau2[1] + mean(seEl^2)))), keyby = .(lot, thr)]

ggplot(zz[lot == TRUE], aes(elast)) + geom_histogram(bins = 60) + facet_wrap(~thr, scales = "free_y") +
  coord_cartesian(xlim = c(-0.5, 2)) + labs(x = "ZIP size elasticity", y = "ZIPs")
ggsave("text/attomZipElastHist.pdf", width = 8, height = 5)

# two level measures: hedonic ZIP FE (size-adjusted) vs median log ppsf (Vancouver definition)
zl <- melt(zz, measure.vars = c("lev", "levP"), variable.name = "meas", value.name = "lv")
zl[, meas := fifelse(meas == "lev", "hedonic FE", "median ppsf")]
zl[, zipi := as.integer(zip)][inc, on = .(zipi = zip), linc := i.linc]

# within-CBSA correlations
res <- zl[, if (.N >= minZip) .(nZip = .N, pearson = cor(lv, elast),
                                spearman = cor(lv, elast, method = "spearman")), by = .(meas, thr, lot, CBSACode)]
res[, .(cbsas = .N, zips = sum(nZip), medPearson = median(pearson), medSpearman = median(spearman),
        sharePos = mean(spearman > 0)), keyby = .(meas, lot, thr)]

# ---- heterogeneity by CBSA size and CBSA price level ----
cb <- d[, .(lnN = log(.N), cbsaPpsf = median(lppsf)), by = CBSACode]  # size proxy: 2015-19 SFR sales
r2 <- res[cb, on = "CBSACode", nomatch = 0][nZip > 3][, `:=`(z = atanh(spearman), wz = nZip - 3)]

# meta-regression of Fisher-z within-CBSA correlations
het <- r2[, {m <- feols(z ~ lnN + cbsaPpsf, .SD, weights = ~wz, vcov = "hetero")
             .(term = names(coef(m)), b = coef(m), se = se(m), cbsas = .N)}, keyby = .(meas, lot, thr)]
het

# median correlation by CBSA price and size terciles (lot spec)
r2[lot == TRUE, `:=`(ppsfT = cut(cbsaPpsf, quantile(cbsaPpsf, 0:3 / 3), labels = c("cheap", "mid", "expensive"), include.lowest = TRUE),
                    sizeT = cut(lnN, quantile(lnN, 0:3 / 3), labels = c("small", "mid", "big"), include.lowest = TRUE)),
   by = .(meas, thr)]
dcast(r2[lot == TRUE, .(medSp = round(median(spearman), 3)), by = .(meas, thr, ppsfT)], meas + thr ~ ppsfT, value.var = "medSp")
dcast(r2[lot == TRUE, .(medSp = round(median(spearman), 3)), by = .(meas, thr, sizeT)], meas + thr ~ sizeT, value.var = "medSp")

# pooled: level slope varying with CBSA ppsf and size (centred), CBSA FE, RE weights
zl <- zl[cb, on = "CBSACode", nomatch = 0][, `:=`(ppsfC = cbsaPpsf - mean(cbsaPpsf), lnNC = lnN - mean(lnN)), by = .(meas, lot, thr)]
hetP <- zl[, {m <- feols(elast ~ lv + lv:ppsfC + lv:lnNC | CBSACode, .SD, weights = ~wRe, cluster = ~CBSACode)
              .(term = names(coef(m)), b = coef(m), se = se(m))}, keyby = .(meas, lot, thr)]
hetP

# ---- income sorting (ZCTA median household income, ACS 2015-19) ----
show <- \(x) x[lot == TRUE & thr %in% c(0, 0.8)]  # keep output readable
incC <- zl[!is.na(linc), .(incCor = cor(lv, linc, method = "spearman"), incSd = sd(linc)),
           by = .(meas, lot, thr, CBSACode)]
r3 <- r2[incC, on = .(meas, lot, thr, CBSACode), nomatch = 0]

# does income-price sorting line up with metro price and with the level-elasticity correlation?
show(r3)[, .(cor_incCor_ppsf = cor(incCor, cbsaPpsf), cor_incCor_z = cor(incCor, z),
             cor_incSd_z = cor(incSd, z), cbsas = .N), keyby = .(meas, thr)]

# meta-regression: is it sorting rather than price per se?
het2 <- show(r3)[, {m1 <- feols(z ~ cbsaPpsf + incCor, .SD, weights = ~wz, vcov = "hetero")
                    m2 <- feols(z ~ cbsaPpsf + incCor + incSd + lnN, .SD, weights = ~wz, vcov = "hetero")
                    rbind(data.table(model = "ppsf + incCor", term = names(coef(m1)), b = coef(m1), se = se(m1)),
                          data.table(model = "+ incSd + lnN", term = names(coef(m2)), b = coef(m2), se = se(m2)))},
                 keyby = .(meas, thr)]
het2

# ZIP level: elasticity on income; income vs level horse race; income slope by metro price
mech <- show(zl)[!is.na(linc), {
  ms <- list(feols(elast ~ linc | CBSACode, .SD, weights = ~wRe, cluster = ~CBSACode),
             feols(elast ~ lv + linc | CBSACode, .SD, weights = ~wRe, cluster = ~CBSACode),
             feols(elast ~ linc + linc:ppsfC | CBSACode, .SD, weights = ~wRe, cluster = ~CBSACode))
  rbindlist(lapply(1:3, \(k) data.table(model = c("income", "level + income", "income x metro ppsf")[k],
                                        term = names(coef(ms[[k]])), b = coef(ms[[k]]), se = se(ms[[k]]))))},
  keyby = .(meas, thr)]
mech

# ---- out of sample: fit on CBSA ranks 10+, predict the top 9 ----
rk <- fread(rankFile)[, .(cbsaChr = as.character(cbsa), rank)]
r3[, cbsaChr := as.character(CBSACode)][rk, on = "cbsaChr", rank := i.rank]
xw <- path.expand("~/Downloads/list1_2020.xls")
if (!file.exists(xw)) download.file("https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2020/delineation-files/list1_2020.xls", xw, mode = "wb")
nm <- unique(setDT(readxl::read_excel(xw, skip = 2))[, .(cbsaChr = as.character(suppressWarnings(as.integer(`CBSA Code`))),
                                                        name = `CBSA Title`)][!is.na(cbsaChr)])
oos <- show(r3)[, {f0  <- feols(z ~ cbsaPpsf, .SD[rank >= 10], weights = ~wz, vcov = "hetero")
                   f1  <- feols(z ~ cbsaPpsf + incCor, .SD[rank >= 10], weights = ~wz, vcov = "hetero")
                   top <- .SD[rank < 10]
                   .(cbsaChr = top$cbsaChr, rank = top$rank, nZip = top$nZip, cbsaPpsf = round(top$cbsaPpsf, 2),
                     incCor = round(top$incCor, 2), predPpsf = round(tanh(predict(f0, top)), 3),
                     predPpsfInc = round(tanh(predict(f1, top)), 3), actual = round(top$spearman, 3))},
                keyby = .(meas, thr)]
oos[nm, on = "cbsaChr", name := i.name][order(meas, thr, rank)]

ggplot(r2[lot == TRUE & thr == 0], aes(cbsaPpsf, spearman, size = nZip)) +
  geom_hline(yintercept = 0, linetype = 2) + geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", mapping = aes(weight = wz), se = FALSE, show.legend = FALSE) +
  facet_wrap(~meas) + labs(x = "CBSA median log ppsf", y = "within-CBSA Spearman(level, elasticity)", size = "ZIPs")
ggsave("text/attomCbsaHet.pdf", width = 8, height = 4)

# pooled slope of elasticity on level, CBSA FE
path <- zl[, {ms <- lapply(list(NULL, ~n, ~wIv, ~wRe), \(w)
                feols(elast ~ lv | CBSACode, .SD, weights = w, cluster = ~CBSACode))
              .(wt = c("unweighted", "sales", "inverse variance", "random effects"),
                b = sapply(ms, coef), se = sapply(ms, se), cbsas = uniqueN(CBSACode), zips = .N)},
           keyby = .(meas, lot, thr)]
path[, spec := fifelse(lot, "with log lot", "no lot")]
path

ggplot(path, aes(thr, b, colour = wt)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_pointrange(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se), position = position_dodge(0.03)) +
  geom_line(position = position_dodge(0.03)) +
  facet_grid(meas ~ spec) +
  labs(x = "minimum ZCTA share of units single detached", y = "d elasticity / d ZIP price level", colour = NULL)
ggsave("text/attomZipLevElastPath.pdf", width = 8, height = 7)

# binscatter at the main threshold, with lot
z <- zz[thr == minSfdZip & lot == TRUE]
z[, levd := lev - mean(lev), by = CBSACode][, bin := cut(levd, quantile(levd, 0:20 / 20), include.lowest = TRUE)]
ggplot(z[, .(levd = mean(levd), elast = mean(elast)), by = bin], aes(levd, elast)) + geom_point() +
  labs(x = "ZIP price level (FE, demeaned within CBSA)", y = "mean ZIP size elasticity")
ggsave("text/attomZipLevElastBin.pdf", width = 6, height = 4)

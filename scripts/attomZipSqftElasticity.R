library(data.table)
library(httr2)
library(haven)
library(arrow)
library(fixest)
library(ggplot2)

download <- FALSE  # TRUE to fetch/slim; delete attomSlim/*.parquet first to re-slim with new columns
rankLo <- 20       # CBSAs ranked by file size
rankHi <- 50
yrs    <- 2015:2019
minN   <- 50       # sales per ZIP
minZip <- 10       # ZIPs per CBSA for a correlation

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
    if (file.exists(out)) next
    tmp <- path.expand(sprintf("~/Downloads/cbsa_%s.dta", sel$cbsa[i]))
    request("https://content.dropboxapi.com/2/sharing/get_shared_link_file") |>
      req_auth_bearer_token(tok) |> req_method("POST") |>
      req_headers(`Dropbox-API-Arg` = jsonlite::toJSON(list(url = lnk, path = sel$path[i]), auto_unbox = TRUE)) |>
      req_error(body = \(resp) resp_body_string(resp)) |> req_timeout(3600) |> req_perform(path = tmp)
    write_parquet(zap_labels(read_dta(tmp, col_select = tidyselect::any_of(keep))), out)
    unlink(tmp)
  }
}

# ---- estimation ----
d <- rbindlist(lapply(list.files(slimDir, full.names = TRUE), read_parquet), fill = TRUE)
hasLot <- "AreaLotSF" %in% names(d)
d <- d[ValidTransact == 1 & InvalidPrice == 0 & TransferAmount > 0 & PropertyType_chars == "SFR" &
       (is.na(UnitsCount) | UnitsCount <= 1) & year(TransactDate) %in% yrs &
       between(AreaBuilding, 500, 8000) & YearBuilt_Gen > 1800 & !is.na(PropertyAddressZIP)]
if (hasLot) d <- d[AreaLotSF > 0][, llot := log(AreaLotSF)]
d[, `:=`(zip = as.character(PropertyAddressZIP), yr = year(TransactDate),
         yb10 = pmax(10 * (YearBuilt_Gen %/% 10), 1900),
         lp = log(TransferAmount), ls = log(AreaBuilding), lppsf = log(TransferAmount / AreaBuilding))]
print(quantile(exp(d$ls)))
d[, `:=`(lo = quantile(lppsf, .01), hi = quantile(lppsf, .99)), by = CBSACode]
d <- d[between(lppsf, lo, hi)]
d[, `:=`(lo = quantile(ls, .2), hi = quantile(ls, .9))]#, by = CBSACode]
d <- d[between(ls, lo, hi)]
d <- d[, if (.N >= minN) .SD, by = .(CBSACode, zip)]

# level: ZIP FE from pooled CBSA hedonic, common slopes
print(hasLot)
f1 <- if (hasLot) lp ~ ls + llot | zip + YearBuilt_Gen + yr else lp ~ ls | zip + YearBuilt_Gen + yr
lev <- d[, {fe <- fixef(feols(f1, .SD))$zip; .(zip = names(fe), lev = unname(fe))}, by = CBSACode]

# elasticity: separate ZIP regressions (ZIP-specific vintage and sale-year effects)
el <- d[, {b <- coef(feols(lp ~ i(zip, ls) | zip^yb10 + zip^yr, .SD))
           .(zip = sub("^zip::(.*):ls$", "\\1", names(b)), elast = unname(b))}, by = CBSACode]

z <- lev[el, on = .(CBSACode, zip), nomatch = 0][d[, .(n = .N), by = .(CBSACode, zip)], on = .(CBSACode, zip), nomatch = 0]

res <- z[, if (.N >= minZip) .(nZip = .N, pearson = cor(lev, elast),
                               spearman = cor(lev, elast, method = "spearman")), by = CBSACode]
res[, .(cbsas = .N, medPearson = median(pearson), wtdPearson = weighted.mean(pearson, nZip),
        medSpearman = median(spearman), sharePos = mean(pearson > 0))]

etable(feols(elast ~ lev | CBSACode, z, cluster = ~CBSACode),
       feols(elast ~ lev | CBSACode, z, weights = ~n, cluster = ~CBSACode))

# binscatter, level demeaned within CBSA
z[, levd := lev - mean(lev), by = CBSACode][, bin := cut(levd, quantile(levd, 0:20 / 20), include.lowest = TRUE)]
ggplot(z[, .(levd = mean(levd), elast = mean(elast)), by = bin], aes(levd, elast)) + geom_point() +
  labs(x = "ZIP price level (FE, demeaned within CBSA)", y = "mean ZIP size elasticity")
ggsave("text/attomZipLevElastBin.pdf", width = 6, height = 4)

ggplot(res, aes(pearson)) + geom_histogram(bins = 30) + geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "within-CBSA ZIP cor(price level, size elasticity)", y = "CBSAs")
ggsave("text/attomZipPpsfElast.pdf", width = 6, height = 4)

print(res)

library(data.table); library(sf); library(fixest); library(ggplot2)

g26     <- list.files("~/bigFiles/latestSpatialBCA", "\\.gpkg$", full.names = TRUE)[1]  # 2026 roll (July 2025 values), sales to Apr 2026
g22     <- "~/bigFiles/bca_folios.gpkg"                                              # 2022 roll (July 2021 values)
events  <- setNames(as.IDate(c("2024-06-24", "2025-12-15")), c("ssmuh", "cutback"))  # R1 SSMUH adoption; envelope cutback (approx. date)
rolls   <- c(ssmuh = g22, cutback = g26)
bill44  <- as.IDate("2023-11-01")  # provincial SSMUH bill introduced
win     <- 8                       # quarters either side
tdq     <- 0.25                    # teardown: improvement share in bottom tdq of event sample
NOTD    <- Sys.getenv("NOTD", "break")  # comparison: break = eff. age below estimated kink; 20 = below 20; all = every non-teardown
sfx     <- c(`break` = "", `20` = "Age20", all = "AllLots")[NOTD]

q   <- \(lyr, cols) sprintf("SELECT %s FROM WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_%s_SV WHERE JURISDICTION_CODE = '301'", cols, lyr)
rd  <- \(g, ...) as.data.table(st_drop_geometry(st_read(g, query = q(...), quiet = TRUE)))
lost <- \(n0, n1, what) cat(sprintf("%-45s %7d -> %7d (lost %.1f%%)\n", what, n0, n1, 100 * (1 - n1 / n0)))

# lots: single-family (incl. with suite) in the 2022 roll, i.e. before Bill 44
d22 <- st_read(g22, query = q("DESCRIPTIONS", "FOLIO_ID, ACTUAL_USE_DESCRIPTION, NEIGHBOURHOOD, LAND_WIDTH, geom"), quiet = TRUE)
d22 <- d22[d22$ACTUAL_USE_DESCRIPTION %in% c("Single Family Dwelling", "Residential Dwelling with Suite"), ]
d22 <- d22[!duplicated(d22$FOLIO_ID), ]
lot <- data.table(folio = d22$FOLIO_ID, use = d22$ACTUAL_USE_DESCRIPTION, nbhd = d22$NEIGHBOURHOOD,
                  width = d22$LAND_WIDTH, lotArea = as.numeric(st_area(d22)))
print(lot[, .N, use])

# rolls before each event, summed over property classes
roll <- rbindlist(lapply(names(rolls), \(e)
  rd(rolls[e], "GNRL_PROP_VALUES", "FOLIO_ID AS folio, GEN_PROPERTY_CLASS_CODE AS cls, GEN_GROSS_LAND_VALUE AS land, GEN_GROSS_IMPROVEMENT_VALUE AS impr")[, event := e]))
roll <- unique(roll, by = c("event", "folio", "cls"))[, .(land = sum(land), impr = sum(impr)), .(event, folio)]
roll[, ishare := impr / (land + impr)]
stopifnot(!anyDuplicated(roll, by = c("event", "folio")))

# sales from the 2026 extract
s <- unique(rd(g26, "SALES", "FOLIO_ID AS folio, SALES_ID AS sid, CONVEYANCE_DATE AS date, CONVEYANCE_PRICE AS price, CONVEYANCE_TYPE_DESCRIPTION AS ctype"), by = c("folio", "sid"))
print(s[, .N, ctype][order(-N)])
s[, date := as.IDate(substr(date, 1, 10))]
n0 <- nrow(s); s <- lot[s, on = "folio", nomatch = NULL]; lost(n0, nrow(s), "sales on 2022 single-family lots")
for (f in alist(ctype %like% "Single Property", price > 1e5, lotArea > 0)) {
  n0 <- nrow(s); s <- s[eval(f)]; lost(n0, nrow(s), deparse(f))
}
s[, `:=`(lppl = log(price / lotArea), ym = format(date, "%Y-%m"))]

w <- rbindlist(lapply(names(events), \(e) {
  x <- copy(s[, rel := as.integer(floor(as.numeric(date - events[e]) / 91.31))][rel %between% c(-win, win - 1)])
  x[, `:=`(event = e, post = date >= events[e])]
  x
}))
stopifnot(!anyDuplicated(w, by = c("event", "sid")))
n0 <- nrow(w); w <- roll[w, on = .(event, folio)]; stopifnot(nrow(w) == n0)
print(w[, .(n = .N, noRoll = round(mean(is.na(ishare)), 4)), event])
w <- w[ishare %between% c(0, 1)]
cut <- unique(w, by = c("event", "folio"))[, .(cut = quantile(ishare, tdq)), event]
print(cut)
w <- cut[w, on = "event"][, td := ishare <= cut]

# comparison group: young houses (effective age at event < cap, or rebuilt since 2018 before the event's roll); see burnabyRedevAge.R
ra <- readRDS("~/DropboxExternal/dataProcessed/burnabyEffYear.rds")
cap <- switch(NOTD, `break` = ra$kink, `20` = 20L, all = NA)
cat("comparison group:", NOTD, "cap", cap, "\n")
n0 <- nrow(w); w <- ra$lots[, .(folio, ey, r22, r26)][w, on = "folio"]; stopifnot(nrow(w) == n0)
w[, young := (year(events[event]) - ey < cap) | fifelse(event == "ssmuh", r22, r26) %in% "rebuilt"]
print(w[, .(n = .N, noEffYear = sum(is.na(ey)), young = sum(young, na.rm = TRUE), td = sum(td), tdAndYoung = sum(td & young, na.rm = TRUE)), event])
if (NOTD != "all") w <- w[td != (young %in% TRUE)]
ctlLab <- if (NOTD == "all") "not teardown" else sprintf("young (eff. age < %d)", cap)
print(w[, .(n = .N, teardowns = sum(td), first = min(date), last = max(date)), .(event, post)][order(event, post)])

# event study: teardown x quarter, ref = quarter before event; DiD: teardown x post
w[, tdPost := td & post]
fit <- \(f) setNames(lapply(names(events), \(e) feols(f, w[event == e], cluster = ~nbhd)), names(events))
es <- lapply(c(roll = "td", share = "ishare"), \(v) fit(as.formula(sprintf("lppl ~ %1$s + i(rel, %1$s, ref = -1) | nbhd + ym", v))))
dd <- fit(lppl ~ td + tdPost | nbhd + ym)
print(rbindlist(lapply(names(dd), \(e) data.table(event = e, b = coef(dd[[e]])["tdPostTRUE"],
  seNbhd = se(dd[[e]])["tdPostTRUE"], seHetero = se(summary(dd[[e]], vcov = "hetero"))["tdPostTRUE"]))))  # ~13 nbhd clusters
etable(c(es$roll, es$share, dd), headers = list(event = rep(names(events), 3), spec = rep(c("roll", "share", "DiD"), each = 2)))

cf <- rbindlist(lapply(names(events), \(e) {
  ct <- as.data.table(coeftable(es$roll[[e]]), keep.rownames = "term")[term %like% "^rel::"]
  ct[, .(event = e, rel = as.integer(sub("rel::(-?\\d+):.*", "\\1", term)), b = Estimate, se = `Std. Error`)]
}))
cf <- rbind(cf, data.table(event = names(events), rel = -1L, b = 0, se = 0))
cf[, event := factor(event, names(events), c("R1 SSMUH adoption, Jun 2024", "Envelope cutback, Dec 2025"))]
b44 <- data.table(event = factor("R1 SSMUH adoption, Jun 2024", levels(cf$event)), x = floor(as.numeric(bill44 - events["ssmuh"]) / 91.31) + 0.5)
ggplot(cf, aes(rel, b)) +
  geom_hline(yintercept = 0, colour = "grey50") + geom_vline(xintercept = -0.5) +
  geom_vline(data = b44, aes(xintercept = x), linetype = "dashed", colour = "grey40") +
  geom_pointrange(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se)) +
  facet_wrap(~event, scales = "free_x") + theme_bw() +
  labs(x = "quarters from event (dashed: Bill 44 introduced)", y = sprintf("teardown premium vs %s\nlog price / lot area, relative to quarter -1", ctlLab))
ggsave(sprintf("text/burnabyCap%s.png", sfx), width = 9, height = 4.5)

# lot-size elasticity of price by month, teardowns vs not (SSMUH window, 2022-roll flag)
v <- w[event == "ssmuh"][, `:=`(lp = log(price), lla = log(lotArea), cls = fifelse(td, "teardown", "not teardown"))]
print(v[, .(n = .N, months = uniqueN(ym), minPerMonth = min(table(ym)), sdLogArea = sd(lla)), cls])
el <- rbindlist(lapply(c("teardown", "not teardown"), \(k) {
  m  <- feols(lp ~ i(ym, lla) | nbhd + ym, v[cls == k], vcov = "hetero")
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  ct[, .(cls = k, ym = sub("ym::([0-9-]+):lla", "\\1", term), b = Estimate, se = `Std. Error`)]
}))
el <- v[, .N, .(cls, ym)][el, on = .(cls, ym)][, month := as.IDate(paste0(ym, "-15"))]
v[, llc := lla - mean(lla)]  # centred: uncentred lla:post is near-collinear with post, which ym FE absorb except in the event month
pp <- feols(lp ~ llc | nbhd + ym, v, split = ~paste(cls, fifelse(post, "post", "pre")), vcov = "hetero")
etable(pp)
pj <- feols(lp ~ llc + llc:post + llc:td + llc:td:post | nbhd^td + ym^td, v, vcov = "hetero")
etable(pj)
ggplot(el, aes(month, b, colour = cls)) +
  geom_hline(yintercept = c(0, 1), colour = "grey70") +
  geom_vline(xintercept = c(bill44, events["ssmuh"]), linetype = c("dashed", "solid")) +
  geom_pointrange(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se), position = position_dodge(width = 12), size = 0.2) +
  geom_smooth(aes(weight = 1 / se^2), method = "loess", span = 0.5, se = FALSE) +
  scale_colour_discrete(labels = c(`not teardown` = ctlLab, teardown = "teardown")) +
  coord_cartesian(ylim = c(-0.5, 2)) + theme_bw() + theme(legend.position = "bottom") +
  labs(x = NULL, y = "elasticity of price w.r.t. lot area", colour = NULL,
       caption = "Burnaby 2022 single-family lots; nbhd + month FE; 95% CI hetero-robust. Dashed: Bill 44 introduced; solid: R1 SSMUH adopted.")
ggsave(sprintf("text/burnabyLotElasticity%s.png", sfx), width = 9, height = 5)

# permitEventStudy.R
# Event-study version of eq. ShareDiD (permitShareDiD.R): permit-level LPM of duplex (vs single) on new-build permits on
# 33-ft 2018 SFD lots now R1-1, on period x tract log ppsf (ppsf33), tract and period FE, SEs clustered by tract (no
# aggregation to cells). Periods from permitShareDiD.R (11/22/26): calendar years 2019-2022 (pre) and 2024-2026 (post,
# 2026 partial); 2018 (partial) and 2023 (announcement and post-announcement) excluded. Reference period -1 (2022). Joint Wald test that the pre-period coefficients are zero.
# Figure: estimated on S$pmAll without 2018, with 2023 as its own period, so it gets a coefficient too; date axis with points at period midpoints (first/last applications); dashed
# lines at Jan 1 2023 (anticipation evident) and Oct 17 2023 (R1-1 in force). The CSV and HonestDiD use the main model
# (2023 excluded); the two sets of coefficients differ only through the tract FE.
# Writes text/eventStudyPpsf.png, ~/DropboxExternal/dataProcessed/eventStudyPpsf.csv
# Tom Davidoff 11/21/26, permit level and new periods 11/22/26

library(data.table)
library(fixest)
library(ggplot2)

S  <- readRDS("~/DropboxExternal/dataProcessed/shareDiDPermits.rds")   # from permitShareDiD.R
n0 <- nrow(S$pm); d <- merge(S$pm, S$pT, by = "CTUID"); cat(sprintf("permits %d -> %d with tract ppsf\n", n0, nrow(d)))
lab <- d[, .(n = .N, lab = paste0(format(min(applied), "%b %Y"), "-\n", format(max(applied), "%b %Y"))), keyby = per]
print(lab)
m  <- feols(dup ~ i(per, ppsf, ref = -1) | CTUID + per, d, cluster = ~CTUID)
m0 <- feols(dup ~ post:ppsf | CTUID + per, d, cluster = ~CTUID)
print(coeftable(m))
w <- wald(m, keep = "per::-[2-9]:ppsf", print = FALSE)
cat(sprintf("joint pre-period test F = %.2f, p = %.3f (%d restrictions); pooled post x ppsf = %.3f (%.3f)\n",
            w$stat, w$p, w$df1, coef(m0), se(m0)))
es <- data.table(per = as.numeric(sub("^per::(-?[0-9]+):ppsf$", "\\1", names(coef(m)))), b = coef(m), se = se(m))
es <- rbind(es, data.table(per = -1, b = 0, se = 0))[order(per)]
fwrite(es, "~/DropboxExternal/dataProcessed/eventStudyPpsf.csv")
# figure model: dropped window as its own period
dA <- merge(S$pmAll, S$pT, by = "CTUID")
dA <- dA[year(applied) >= 2019]
dA[, perD := fifelse(is.na(per), "2023", as.character(per))]
mA <- feols(dup ~ i(perD, ppsf, ref = "-1") | CTUID + perD, dA, cluster = ~CTUID)
print(coeftable(mA))
eA <- data.table(perD = sub("^perD::(.+):ppsf$", "\\1", names(coef(mA))), b = coef(mA), se = se(mA))
eA <- rbind(eA, data.table(perD = "-1", b = 0, se = 0))
cat("max |figure - main| over common periods:",
    round(max(abs(merge(eA[perD != "2023", .(per = as.numeric(perD), bA = b)], es, by = "per")[, bA - b])), 3), "\n")
lA <- dA[, .(n = .N, from = min(applied), to = max(applied)), by = perD]
eA <- merge(eA, lA, by = "perD")[, mid := from + as.integer((to - from) / 2)][order(mid)]
print(eA)
p <- ggplot(eA, aes(mid, b)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_vline(xintercept = as.IDate(c("2023-01-01", "2023-10-17")), linetype = 2, colour = "grey40") +
  annotate("text", x = as.IDate(c("2023-01-01", "2023-10-17")), y = Inf, label = c("Jan 2023 ", " R1-1 in force"),
           hjust = c(1, 0), vjust = 1.5, size = 3, colour = "grey30") +
  geom_errorbar(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se), width = 60) +
  geom_point(size = 2) +
  geom_text(aes(y = b - 1.96 * se, label = paste0("n = ", n)), vjust = 1.6, size = 2.7, colour = "grey30") +
  scale_x_date(breaks = as.Date(paste0(2019:2026, "-07-01")), labels = 2019:2026, minor_breaks = NULL) +
  labs(x = "Permit application year (2023 estimated separately; excluded from main sample)", y = "Coefficient on tract log ppsf (reference: 2022)") +
  theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank())
ggsave("../text/eventStudyPpsf.png", p, width = 7.5, height = 4.8, dpi = 300)

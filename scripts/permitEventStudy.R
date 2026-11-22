# permitEventStudy.R
# Event-study version of eq. ShareDiD (permitShareDiD.R): duplex share of single+duplex new-build permits on 33-ft 2018
# SFD lots now R1-1, area x 12-month period (relative to the 2023-10-17 R1-1 start) cells weighted by permits, on
# period x local log ppsf (ppsf33) with area and period FE; reference period -1 (Oct 2022 - Oct 2023). Tract and BCA
# neighbourhood; SEs clustered by area; joint Wald test that pre-period coefficients are zero.
# Writes text/eventStudyPpsf.png
# Tom Davidoff 11/21/26

library(data.table)
library(fixest)
library(ggplot2)

S  <- readRDS("~/DropboxExternal/dataProcessed/shareDiDPermits.rds")   # from permitShareDiD.R
pm <- S$pm
lab <- pm[, .(lab = paste0(format(min(applied), "%b %Y"), "-\n", format(max(applied), "%b %Y"))), keyby = per]
print(pm[, .N, keyby = per])

fit <- function(area, pa, what) {
  n0 <- nrow(pm); d <- merge(pm[!is.na(get(area))], pa, by = area)
  cat(sprintf("%s: permits %d -> %d\n", what, n0, nrow(d)))
  dc <- d[, .(y = mean(dup), w = .N, ppsf = ppsf[1]), by = .(area = get(area), per)]
  m  <- feols(y ~ i(per, ppsf, ref = -1) | area + per, dc, weights = ~w, cluster = ~area)
  m0 <- feols(y ~ post:ppsf | area + per, dc[, post := as.numeric(per >= 0)], weights = ~w, cluster = ~area)
  print(coeftable(m)); cat("pooled post x ppsf:", round(coef(m0), 3), "(", round(se(m0), 3), ")\n")
  wPre <- wald(m, keep = "per::-[2-5]:ppsf", print = FALSE)
  cat(sprintf("%s: joint pre-period test F = %.2f, p = %.3f (%d restrictions)\n", what, wPre$stat, wPre$p, wPre$df1))
  k <- names(coef(m))
  out <- data.table(per = as.numeric(sub("^per::(-?[0-9]+):ppsf$", "\\1", k)), b = coef(m), se = se(m))
  out <- rbind(out, data.table(per = -1, b = 0, se = 0))
  out[, `:=`(level = sprintf("%s (%d areas; pre-trend p = %.2f)", what, uniqueN(dc$area), wPre$p))]
  out
}
es <- rbind(fit("CTUID", S$pT, "Census tract"), fit("nbhd", S$pN, "Neighbourhood"))
es <- merge(es, lab, by = "per")
dodge <- position_dodge(width = 0.35)
p <- ggplot(es, aes(factor(per), b, colour = level, shape = level)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_vline(xintercept = 5.5, linetype = 2, colour = "grey40") +
  geom_errorbar(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se), width = 0.15, position = dodge) +
  geom_point(size = 2, position = dodge) +
  scale_x_discrete(labels = setNames(lab$lab, lab$per)) +
  scale_colour_manual(values = c("black", "grey55"), name = NULL) + scale_shape_manual(values = c(16, 17), name = NULL) +
  labs(x = "Permit application period", y = "Coefficient on log ppsf (reference: Oct 2022-Oct 2023)") +
  theme_bw(base_size = 10) + theme(legend.position = "bottom", panel.grid.minor = element_blank())
ggsave("../text/eventStudyPpsf.png", p, width = 7.5, height = 4.8, dpi = 300)
fwrite(es, "~/DropboxExternal/dataProcessed/eventStudyPpsf.csv")

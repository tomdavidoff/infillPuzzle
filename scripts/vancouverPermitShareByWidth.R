# permitShareByWidth.R  --  ADDENDUM to categorizeVancouverSingle.R
# Run categorizeVancouverSingle.R FIRST; this reuses `kj` (permits joined to
# BCA width, with width_ft, use_class, side, year, zoning already built).
#
# Duplex propensity by FINE width bin, 2019-2023 (duplex-era R1), duplex/(duplex+SFD).
# In R1 2019-2023 the principal-building choice set is ~duplex vs single, so this
# two-way share ~ the whole redevelopment decision. We PRINT the dropped share per
# bin to verify duplex+single really is ~everything.
#
# Bins (contiguous, left-open/right-closed) -- edit CUTS to taste:
#   (30,34]  "33"      (the narrow standard lot)
#   (34,40]  "34-40"
#   (40,45]  "40-45"
#   (45,49]  "45-49"
#   (49,51]  "50"      (the standard wide lot, +/-1)
#   (51,80]  "50+"
# Tom Davidoff 09/13/26

library(data.table); library(ggplot2)
stopifnot(exists("kj"))     # from categorizeVancouverSingle.R
out_dir <- "text"; dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

DUP_ERA <- 2019:2023
CUTS <- c(30,34,40,45,49,51,80)
LABS <- c("33","34-40","40-45","45-49","50","50+")

K <- as.data.table(copy(kj))
K <- K[!is.na(width_ft) & width_ft > 30 & width_ft <= 80 & year %in% DUP_ERA]
K[, wbin := cut(width_ft, breaks = CUTS, labels = LABS, right = TRUE)]

# principal-building single vs duplex. "single" = Plain-Jane (bare detached);
# SFD+suite is a single with a suite -> still a single principal building, INCLUDE.
K[, principal := fifelse(use_class == "Duplex", "duplex",
                 fifelse(use_class %in% c("Plain-Jane","SFD+suite"), "single", NA_character_))]

# --- denominator honesty: what share of permits in each bin is NEITHER? ---
cat("=== 2019-2023 R1: per-bin permit mix, and dropped (non single/duplex) share ===\n")
mix <- K[, .(n = .N,
             single = sum(principal=="single", na.rm=TRUE),
             duplex = sum(principal=="duplex", na.rm=TRUE),
             other  = sum(is.na(principal))), by = wbin]
mix[, otherShare := round(other/n, 3)]
setorder(mix, wbin); print(mix[])

# --- duplex share of (duplex+single), by bin x side ---
P <- K[!is.na(principal)]
sh <- P[, .(nDup = sum(principal=="duplex"), n = .N), by = .(side, wbin)]
sh[, dupShare := nDup / n]
setorder(sh, side, wbin)
cat("\n=== duplex share of (duplex+single), 2019-2023, by width bin x side ===\n")
print(dcast(sh, wbin ~ side, value.var = "dupShare"))
cat("\n=== underlying n (duplex+single) by bin x side ===\n")
print(dcast(sh, wbin ~ side, value.var = "n", fill = 0))

# --- pooled share (both sides) for the headline monotonicity read ---
shP <- P[, .(dupShare = mean(principal=="duplex"), n = .N), by = wbin]
setorder(shP, wbin)
cat("\n=== pooled duplex share by width bin (monotonicity check) ===\n")
print(shP[])

# --- PNG: duplex share vs width bin, East vs West ---
sh[, wbin := factor(wbin, levels = LABS)]
p <- ggplot(sh, aes(wbin, dupShare, color = side, group = side)) +
  geom_line(linewidth = 1) + geom_point(aes(size = n)) +
  scale_size_area(max_size = 5, guide = "none") +
  scale_color_manual(values = c(East = "grey30", West = "firebrick")) +
  ylim(0, NA) + theme_minimal() +
  labs(title = "Duplex propensity by lot width, R1 permits 2019-2023",
       subtitle = "Duplex / (duplex + single) principal-building permits. Point size ~ n.",
       x = "lot width (ft)", y = "duplex share", color = NULL)
ggsave(file.path(out_dir, "permit_duplexShare_byWidthBin.png"), p,
       width = 8, height = 5, dpi = 150)
cat("\n  wrote", file.path(out_dir, "permit_duplexShare_byWidthBin.png"), "\n")

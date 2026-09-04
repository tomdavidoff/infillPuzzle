# edmontonCapRate.R
# Tom Davidoff
# 09/03/26
#
# Claim: the "cap rate" (gross yield = rent/value) FALLS as property value rises.
# Test: across Edmonton CMA census tracts (2021), regress log(rent) on log(value).
#   log(rent) = a + b*log(value);  b = elasticity;  b < 1  <=>  cap rate falls.
#
# Measurement-error worry (Tom): census median value is self-reported & noisy, so
# OLS b is attenuated toward 0 -- helpful for b<1 but not a clean estimate.
# Two-measurement fix: census value and tract-median ASSESSED value are two noisy
# proxies for the same latent log value V*.  Three estimators:
#   (1) OLS  rent ~ census value          -- most attenuated (lower bound)
#   (2) OLS  rent ~ assessed value        -- cleaner regressor, less attenuated
#   (3) IV   rent ~ census value | assessed value
#            assessed instruments census: purges attenuation IF the two errors are
#            orthogonal (cov(u_census,u_assessed)=0).  CAVEAT: matching/mislocation
#            error is shared across both measures, so orthogonality is imperfect and
#            IV is corrected-but-not-fully-consistent (biased same direction as OLS,
#            by less).  Report as such, not as clean identification.
#
# The three OLS/IV directions bracket the errors-in-variables truth; b_fwd*b_rev=R^2.
# Rent left monthly: b is scale-invariant; only intercepts move.

library(data.table)
library(cancensus)
library(fixest)     # feols() for IV: y ~ 1 | fitted ~ instrument
library(sf)
library(ggplot2)

set_cancensus_api_key("CensusMapper_4e1a1c2a1c23a336aec2ea4ce443330c")
sf_use_s2(TRUE)

assessorHistoricalPath <- "~/DropboxExternal/dataRaw/edmonton/Property_Assessment_Data_(Historical)_20260611.csv"
assessorCurrentPath    <- "~/DropboxExternal/dataRaw/edmonton/Property_Information_(Current_Calendar_Year)_20260612.csv"
tractShp               <- "~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"

# ============================================================================
# 1. CENSUS -- CT-level rent + value, Edmonton CMA
# ============================================================================

ct <- get_census(dataset = "CA21", regions = list(CMA = "48835"),
                 vectors = c(rent = "v_CA21_4317", value = "v_CA21_4311"),
                 level = "CT", geo_format = NA, labels = "short", quiet = TRUE)
setDT(ct)
ct[, CTUID := as.character(GeoUID)]
ct[, rent  := as.numeric(rent)]
ct[, value := as.numeric(value)]

# ============================================================================
# 2. ASSESSED -- tract-median assessed value of RS accounts (2nd proxy for V*)
#    Aggregate current-year RS accounts to CT median, spatial-join to tracts.
# ============================================================================

dtAH <- fread(assessorHistoricalPath,
              select = c("Account Number","Assessment Year","Assessed Value"))
dtAH[, maxYear := max(`Assessment Year`), by = `Account Number`]
dtAH <- dtAH[`Assessment Year` == maxYear]
dtAH[, assessed := as.numeric(gsub(",", "", gsub("\\$", "", `Assessed Value`)))]

dtAC <- fread(assessorCurrentPath,
              select = c("zoning","Account Number","Latitude","Longitude"))
dtAC <- dtAC[zoning == "RS"]
setkey(dtAH, "Account Number"); setkey(dtAC, "Account Number")
dtA <- dtAH[dtAC, nomatch = 0]
dtA[, lon := as.numeric(Longitude)]
dtA[, lat := as.numeric(Latitude)]
dtA <- dtA[!is.na(assessed) & assessed > 0 & !is.na(lon) & !is.na(lat)]
dtA <- unique(dtA, by = "Account Number")

# spatial join accounts -> CT (same tract shapefile as horseRace)
dfT  <- st_make_valid(st_transform(st_read(tractShp, quiet = TRUE), 4326))
sfA  <- st_join(st_as_sf(dtA, coords = c("lon","lat"), crs = 4326),
                dfT[, "CTUID"], join = st_within)
dtAj <- as.data.table(st_drop_geometry(sfA))
dtAj[, CTUID := as.character(CTUID)]

MIN_ACCTS <- 20   # tracts with too few RS accounts give a noisy median; drop them
assessAgg <- dtAj[!is.na(CTUID), .(assessedMed = median(assessed), nAcct = .N),
                  by = CTUID][nAcct >= MIN_ACCTS]

# ============================================================================
# 3. MERGE + SAMPLE  (single shared CT sample for all three estimators)
# ============================================================================

setkey(ct, CTUID); setkey(assessAgg, CTUID)
m <- merge(ct[, .(CTUID, rent, value)], assessAgg, by = "CTUID", all = FALSE)

samp <- m[!is.na(rent) & rent > 0 & !is.na(value) & value > 0 &
          !is.na(assessedMed) & assessedMed > 0]
samp[, lrent   := log(rent)]
samp[, lvalue  := log(value)]
samp[, lassess := log(assessedMed)]
samp[, capRate := rent / value]

# ============================================================================
# 4. THREE ESTIMATORS
# ============================================================================

reg_c  <- lm(lrent ~ lvalue,  data = samp)                       # (1) OLS census
reg_a  <- lm(lrent ~ lassess, data = samp)                       # (2) OLS assessed
reg_iv <- feols(lrent ~ 1 | lvalue ~ lassess, data = samp)       # (3) IV: assessed instr census

b_c  <- unname(coef(reg_c)["lvalue"]);  se_c <- summary(reg_c)$coefficients["lvalue","Std. Error"]
b_a  <- unname(coef(reg_a)["lassess"]); se_a <- summary(reg_a)$coefficients["lassess","Std. Error"]
b_iv <- unname(coef(reg_iv)["fit_lvalue"])
se_iv<- unname(se(reg_iv)["fit_lvalue"])

# R^2-implied bracket from the census pair: b_fwd * b_rev = R^2
reg_rev <- lm(lvalue ~ lrent, data = samp)
b_rev   <- unname(coef(reg_rev)["lrent"])       # slope of reverse reg
b_hi    <- 1 / b_rev                            # implied forward slope if rent carried the noise
r2_c    <- summary(reg_c)$r.squared

t1 <- function(b, se) (b - 1) / se              # t-stat for H0: b = 1

# ============================================================================
# 5. UNIT TESTS / SANITY -- printed FIRST
# ============================================================================

pass <- function(name, ok) cat(sprintf("[%s] %s\n", if (isTRUE(ok)) "PASS" else "FAIL", name))
cat("================ UNIT TESTS ================\n")

pass("T1 CTUID unique in census", uniqueN(ct$CTUID) == nrow(ct))
pass("T1b CTUID unique in assessed agg", uniqueN(assessAgg$CTUID) == nrow(assessAgg))
pass("T1c CTUID unique in merged sample", uniqueN(samp$CTUID) == nrow(samp))

pass("T2 median rent in [500,3000]",
     samp[, median(rent)] > 500 & samp[, median(rent)] < 3000)
pass("T2b median census value in [1e5,2e6]",
     samp[, median(value)] > 1e5 & samp[, median(value)] < 2e6)
pass("T2c median assessed value in [1e5,2e6]",
     samp[, median(assessedMed)] > 1e5 & samp[, median(assessedMed)] < 2e6)

# T3: the two value proxies are positively correlated (they measure the same V*)
#     -- if this is weak, the IV is weak; print it so weakness is visible
rho <- samp[, cor(lvalue, lassess)]
pass("T3 census & assessed value positively correlated", rho > 0)
cat("   cor(log census value, log assessed median):", round(rho, 3), "\n")

# T4: IV first stage is strong enough to trust (F > 10 rule of thumb)
fstat <- fitstat(reg_iv, "ivf")$ivf1$stat
pass("T4 IV first-stage F > 10", fstat > 10)
cat("   first-stage F:", round(fstat, 1), "\n")

# T5: all three estimators ran on the SAME sample (comparability)
pass("T5 same n across estimators",
     nobs(reg_c) == nrow(samp) && nobs(reg_a) == nrow(samp) && reg_iv$nobs == nrow(samp))

# T6: ordering the theory predicts -- attenuation: b_census <= b_assessed <= b_IV
#     (census noisiest regressor -> most attenuated; IV corrects furthest up)
pass("T6 b_census <= b_assessed (assessed less attenuated)", b_c <= b_a + 1e-9)
pass("T6b b_assessed <= b_IV (IV corrects furthest)", b_a <= b_iv + 1e-9)

# T7: THE FINDING survives every measurement-error story -- all three below 1
pass("T7 all three elasticities < 1", b_c < 1 & b_a < 1 & b_iv < 1)

# T8: IV slope inside the R^2-implied bracket [b_census, 1/b_reverse]
pass("T8 b_IV within [b_census, 1/b_reverse] bracket",
     b_iv >= b_c - 1e-6 & b_iv <= b_hi + 1e-6)

# ============================================================================
# 6. HEADLINE OUTPUT
# ============================================================================

cat("\n================ RESULT ================\n")
cat("Edmonton CMA census tracts (2021), n =", nrow(samp), "\n\n")
cat(sprintf("(1) OLS  rent ~ census value  : b = %.3f (SE %.3f)  t[b=1] = %.2f\n", b_c,  se_c,  t1(b_c,se_c)))
cat(sprintf("(2) OLS  rent ~ assessed med  : b = %.3f (SE %.3f)  t[b=1] = %.2f\n", b_a,  se_a,  t1(b_a,se_a)))
cat(sprintf("(3) IV   census | assessed    : b = %.3f (SE %.3f)  t[b=1] = %.2f\n", b_iv, se_iv, t1(b_iv,se_iv)))
cat(sprintf("\nR^2-implied bracket (census pair): [%.3f, %.3f]   (R^2 = %.3f)\n", b_c, b_hi, r2_c))
cat(sprintf("mean monthly cap rate: %.4f  (%.2f%% annualized)\n",
            samp[, mean(capRate)], 1200 * samp[, mean(capRate)]))

# ============================================================================
# 7. SCATTER -- census-value fit, with IV slope overlaid
# ============================================================================

mx <- samp[, mean(lvalue)]; my <- samp[, mean(lrent)]
ggplot(samp, aes(x = lvalue, y = lrent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +                         # (1) OLS census
  geom_abline(slope = b_iv, intercept = my - b_iv * mx,
              linetype = "dotted", color = "blue") +                               # (3) IV slope
  geom_abline(slope = 1, intercept = my - mx,
              linetype = "dashed", color = "red") +                                # slope-1 ref
  labs(title = sprintf("Cap rate falls in value: b_OLS=%.2f  b_IV=%.2f", b_c, b_iv),
       subtitle = "black = OLS(census); blue dotted = IV; red dashed = slope-1 reference",
       x = "log median dwelling value (census)", y = "log median monthly rent") +
  theme_bw()
ggsave("text/edmontonCapRate.png", width = 6, height = 4, dpi = 300)

cat("\nwrote text/edmontonCapRate.png\n")

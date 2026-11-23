# permitMonthly2023.R
# Monthly look at the 2023 run-up to R1-1 (in force 2023-10-17) for the permit event study (permitEventStudy.R).
# (1) Raw counts and duplex share by application month, split by tract log ppsf (ppsf33) above/below the permit median.
# (2) Permit-level LPM: dup ~ month x ppsf | tract + month FE, one coefficient per 2023 month, 2024+ pooled,
#     reference = all applications 2018-2022. SEs clustered by tract.
# Uses S$pmAll (anticipation window not dropped). Tom Davidoff 11/22/26

library(data.table)
library(fixest)

S <- readRDS("~/DropboxExternal/dataProcessed/shareDiDPermits.rds")   # from permitShareDiD.R
n0 <- nrow(S$pmAll); d <- merge(S$pmAll, S$pT, by = "CTUID"); cat(sprintf("permits %d -> %d with tract ppsf\n", n0, nrow(d)))
d[, ym := format(applied, "%Y-%m")]
d[, hi := ppsf > median(ppsf)]

cat("\nDuplex share by application month (hi/lo = tract ppsf above/below permit median)\n")
yr <- d[, .(n = .N, share = mean(dup), nHi = sum(hi), shareHi = mean(dup[hi]), nLo = sum(!hi), shareLo = mean(dup[!hi])),
        keyby = .(bin = fifelse(applied < as.IDate("2022-01-01"), paste0(year(applied), " (year)"),
                         fifelse(applied >= as.IDate("2024-01-01"), paste0(year(applied), " (year)"), ym)))]
print(yr[, lapply(.SD, function(x) if (is.numeric(x) && max(x) <= 1) round(x, 2) else x)])

d[, mo := fifelse(applied < as.IDate("2023-01-01"), "pre2023", fifelse(applied >= as.IDate("2024-01-01"), "2024+", ym))]
m <- feols(dup ~ i(mo, ppsf, ref = "pre2023") | CTUID + mo, d, cluster = ~CTUID)
cat("\nLPM, reference = applications before 2023:\n"); print(round(coeftable(m)[, 1:3], 3))

# ---- (3) same table by month of issue, and application-to-issue lags ----
cat(sprintf("\nmissing issued: %d\n", d[is.na(issued), .N]))
cat("\nDuplex share by ISSUE month\n")
iy <- d[!is.na(issued), .(n = .N, share = mean(dup), nHi = sum(hi), shareHi = mean(dup[hi]), nLo = sum(!hi), shareLo = mean(dup[!hi])),
        keyby = .(bin = fifelse(issued < as.IDate("2022-01-01") | issued >= as.IDate("2025-01-01"), paste0(year(issued), " (year)"),
                         format(issued, "%Y-%m")))]
print(iy[, lapply(.SD, function(x) if (is.numeric(x) && max(x) <= 1) round(x, 2) else x)])
cat("\nDays from application to issue, by application window and product\n")
d[, win := fcase(applied < as.IDate("2022-11-01"), "1 before Nov 2022", applied < as.IDate("2023-08-01"), "2 Nov 2022-Jul 2023",
                 applied < as.IDate("2023-10-17"), "3 Aug 2023-16 Oct 2023", applied < as.IDate("2024-01-01"), "4 17 Oct-Dec 2023",
                 default = "5 2024+")]
print(d[!is.na(issued), .(n = .N, medDays = as.numeric(median(issued - applied)), p25 = as.numeric(quantile(issued - applied, .25)),
                          p75 = as.numeric(quantile(issued - applied, .75))), keyby = .(win, product)])
cat("\nApplication month x issue quarter, applications Jun-Dec 2023\n")
print(dcast(d[applied >= as.IDate("2023-06-01") & applied < as.IDate("2024-01-01")],
            ym + product ~ paste0(year(issued), "Q", quarter(issued)), fun.aggregate = length))

library(data.table)
library(fixest)
library(ggplot2)

minSfd    <- 0.95   # share of aggregate owner value in 1-unit detached (B25080)
minTracts <- 20
topCode   <- 2e6    # ACS value top-code; UQ at/above it is censored

f <- "~/DropboxExternal/dataRaw/ipums/nhgis0079_csv/nhgis0079_ds249_20205_tract.csv"

# NHGIS CSVs may carry a second, descriptive header row
twoHdr <- grepl("GIS Join Match Code", readLines(f, n = 2)[2])
dt <- if (twoHdr) fread(f, skip = 2, header = FALSE, col.names = names(fread(f, nrows = 0))) else fread(f)

# CBSAA is blank at tract level; assign from county via 2020 delineation file
xw <- "~/Downloads/list1_2020.xls"
if (!file.exists(xw)) download.file("https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2020/delineation-files/list1_2020.xls", xw, mode = "wb")
cbsa <- setDT(readxl::read_excel(xw, skip = 2))[!is.na(`FIPS County Code`),
  .(CBSAA = as.integer(`CBSA Code`), cbsaName = `CBSA Title`, metro = `Metropolitan/Micropolitan Statistical Area`,
    STATEA = as.integer(`FIPS State Code`), COUNTYA = as.integer(`FIPS County Code`))]
dt[, CBSAA := NULL][, `:=`(STATEA = as.integer(STATEA), COUNTYA = as.integer(COUNTYA))]
dt <- cbsa[metro == "Metropolitan Statistical Area"][dt, on = .(STATEA, COUNTYA)]

d <- dt[!is.na(CBSAA) & AMWAE001 > 0 & !is.na(AMWBE001) & AMWCE001 < topCode & AMWEE001 > 0,
        .(CBSAA, cbsaName, GISJOIN, med = AMWBE001, ratio = AMWCE001 / AMWAE001, sfd = AMWEE002 / AMWEE001)]

cbsaCor <- function(x) x[, if (.N >= minTracts) .(n = .N,
                           pearson  = cor(med, ratio),
                           spearman = cor(med, ratio, method = "spearman")), by = .(CBSAA, cbsaName)]
res <- rbind(cbsaCor(d[sfd >= minSfd])[, sample := "sfd"],
             cbsaCor(d)[, sample := "all"])

res[, .(cbsas = .N, medPearson = median(pearson), wtdPearson = weighted.mean(pearson, n),
        medSpearman = median(spearman), sharePos = mean(pearson > 0)), by = sample]

etable(feols(log(ratio) ~ log(med) | CBSAA, d[sfd >= minSfd], cluster = ~CBSAA),
       feols(log(ratio) ~ log(med) | CBSAA, d, cluster = ~CBSAA),
       headers = c("sfd", "all"))

ggplot(res, aes(pearson)) + geom_histogram(bins = 40) + facet_wrap(~sample, ncol = 1) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "within-CBSA tract cor(median value, UQ/LQ)", y = "CBSAs")
ggsave("text/cbsaTractValueCor.pdf", width = 6, height = 5)

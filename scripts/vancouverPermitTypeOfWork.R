# vancouverPermitTypeOfWork.R
# Enumerates typeofwork in the Vancouver issued-permit data and classifies residential Addition / Alteration permits
# by description: unit-adding conversion (to duplex / two-family / multiple conversion dwelling / multiplex), added
# secondary suite, high value (>= new-build cutoff A from multiplexLotWidth.R), and other (maintenance, repair, decks).
# Classes are exclusive in that order. Counts by issue year and East/West. Saves the classification.
# Tom Davidoff 11/17/26

library(data.table)

ONT     <- -123.101
fPermit <- "~/DropboxExternal/dataRaw/vancouverIssuedBuildingPermits.csv"
fOut    <- "~/DropboxExternal/dataProcessed/permitAddAltClass.rds"
cutA    <- readRDS("~/DropboxExternal/dataProcessed/permitNewBuildCutoffs.rds")["A"]
stopifnot(difftime(Sys.time(), file.mtime(fPermit), units = "hours") < 48)

lost <- function(n0, n1, what) cat(sprintf("%-45s %6d -> %6d (lost %.1f%%)\n", what, n0, n1, 100 * (1 - n1 / n0)))

d <- fread(fPermit)
stopifnot(!anyDuplicated(d$permitnumber))
d[, res := grepl("Dwelling", propertyuse)]
cat("\n=== typeofwork x residential ===\n")
print(d[, .(N = .N, residential = sum(res), medValue = median(projectvalue, na.rm = TRUE)), by = typeofwork][order(-N)])
print(dcast(d[, .N, by = .(typeofwork, issueyear)], typeofwork ~ issueyear, value.var = "N", fill = 0))

n0 <- nrow(d); a <- d[typeofwork == "Addition / Alteration"]; lost(n0, nrow(a), "Addition / Alteration")
n0 <- nrow(a); a <- a[res == TRUE]; lost(n0, nrow(a), "residential (propertyuse has Dwelling)")
# description body after the "<stream> - Addition / Alteration - " prefix
a[, body := tolower(sub("^.*?Addition / Alteration - ", "", projectdescription, perl = TRUE))]
TGT <- "(two-family|two family|duplex|multiple conversion|multi conversion|multiplex|[0-9]+[- ]unit|(two|three|four|five|six) (dwelling |strata )?units)"
a[, conv := grepl(paste0("(convert|change (of )?use|to change)\\b.{0,150}\\bto (a |an )?(new )?(.{0,40})?", TGT), body)]
a[, suite := grepl("(add|adding|new|create|creating|construct|install|provide|convert.{0,80}\\bto|change.{0,80}\\bto)\\b.{0,60}secondary suite", body) &
             !grepl("existing (one|single)[- ]family dwelling with (a )?secondary suite", body)]
a[, class := fcase(conv, "conversion (adds units)",
                   suite, "secondary suite",
                   projectvalue >= cutA, "high value (>= cutoff A)",
                   default = "other")]
a[, c("lat", "lon") := tstrsplit(geo_point_2d, ",\\s*", type.convert = TRUE)]
a[, side := fifelse(lon < ONT, "West", "East")]

cat(sprintf("\ncutoff A = $%s\n", format(cutA, big.mark = ",")))
print(a[, .(N = .N, share = round(.N / nrow(a), 3), medValue = median(projectvalue)), by = class][order(-N)])
print(dcast(a[, .N, by = .(class, issueyear)], class ~ issueyear, value.var = "N", fill = 0))
print(dcast(a[!is.na(side), .N, by = .(class, side)], class ~ side, value.var = "N", fill = 0))
print(a[class == "conversion (adds units)", .N, by = specificusecategory][order(-N)][1:8])
set.seed(1)
for (k in c("conversion (adds units)", "secondary suite", "high value (>= cutoff A)"))
  { cat("\n--", k, "examples --\n"); print(a[class == k][sample(.N, 4), substr(body, 1, 160)]) }
saveRDS(a[, .(permitnumber, issueyear, projectvalue, specificusecategory, class, side, lat, lon)], fOut)

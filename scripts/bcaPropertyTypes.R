# bcaPropertyTypes.R
# Tabulate BCA ActualUseDescription shares, propose property type map, cache to RDS.
# Tom Davidoff 05/18/26

library(data.table)
library(RSQLite)

SQLITE_FILE <- "~/DropboxExternal/dataRaw/REVD19_and_inventory_extracts.sqlite3"
MAP_OUT     <- "~/DropboxExternal/dataProcessed/propertyTypeMap.rds"
SHARE_FLOOR <- 0.005 # .0025 captures non-strata duplex

con <- dbConnect(SQLite(), SQLITE_FILE)
des <- as.data.table(dbGetQuery(con, "SELECT folioID, actualUseDescription FROM folioDescription"))
fol <- as.data.table(dbGetQuery(con, "SELECT folioID, jurisdictionCode FROM folio"))
dbDisconnect(con)

dt <- merge(des, fol, by = "folioID")
dt <- dt[actualUseDescription != "" & !is.na(actualUseDescription)]

tab <- dt[trimws(jurisdictionCode) == "200", .N, by = actualUseDescription] # COV
tab[, share := N / sum(N)]
setorder(tab, -share)
print(tab[1:25])

use <- tab[share >= SHARE_FLOOR, actualUseDescription]
hit <- function(pat) grep(pat, use, value = TRUE, ignore.case = TRUE)
propertyTypeMap <- list(
  singleFamily = setdiff(hit("single family|residential dwelling with suite"),
                         hit("strata|duplex|condominium|apartment|multi")),
  strata = setdiff(hit("strata|condominium|apartment"),
                   hit("duplex|rental|commercial|hotel|block")),
  duplex = hit("duplex")
)
print(propertyTypeMap)
for (nm in c("singleFamily", "strata", "duplex"))
  cat(nm, "COV share:", tab[actualUseDescription %in% propertyTypeMap[[nm]], sum(share)], "\n")

saveRDS(propertyTypeMap, path.expand(MAP_OUT))

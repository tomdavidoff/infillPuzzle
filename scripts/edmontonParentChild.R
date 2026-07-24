# edmontonParentChild.R
# Identify post-2023 child parcels and link them to their 2023 parents.
# Classifies each child as assembly (many parents -> one child),
# subdivision (one parent -> many children), or 1:1 replacement.
# Companion to edmontonMatch.R
# 07/23/26

library(data.table)
library(fixest)
library(sf)

# ------------------------------------------------------------------------------
# 0. Parameters
# ------------------------------------------------------------------------------
RAW        <- "~/DropboxExternal/dataRaw/edmonton/"
ASSESS     <- file.path(RAW, "Property_Assessment_Data_(Historical)_20260611.csv")
PERMITS    <- file.path(RAW, "edmontonBuildingPermits.csv")

PARENT_YR  <- 2023                 # last pre-reform assessment vintage
CHILD_YR   <- 2025                 # post-reform vintage; set 2026 if available
BUFFERS    <- c(15, 25, 35)        # metres; sensitivity on the spatial link
AREA_TOL   <- c(0.90, 1.10)        # child area / summed parent area must land here
CRS_M      <- 3776                 # NAD83 / Alberta 3TM ref merid 114 W, metres

# ------------------------------------------------------------------------------
# 1. Read and normalize
# ------------------------------------------------------------------------------
modWord <- function(word) {
	x <- tolower(word)
	x <- gsub("_", " ", x)
	x <- gsub("\\s+([a-z])", "\\U\\1", x, perl = TRUE)
	x <- gsub(" ", "", x)
	x <- gsub("^([A-Za-z])", "\\L\\1", x, perl = TRUE)
	return(x)
}

normLegal <- function(x) {
	x <- gsub("Block:", "Blk", x)
	x <- gsub(":", "", x)
	x <- gsub("\\s+", " ", trimws(x))
	toupper(x)
}

dtA <- fread(ASSESS)
dtP <- fread(PERMITS)
setnames(dtA, names(dtA), modWord(names(dtA)))
setnames(dtP, names(dtP), modWord(names(dtP)))

dtA[, assessedValue := as.numeric(gsub("\\$", "", gsub(",", "", assessedValue)))]
dtA[, lotSize       := as.numeric(gsub(",", "", lotSize))]
dtA[, legalN        := normLegal(legalDescription)]
dtP[, legalN        := normLegal(legalDescription)]

# Residential parcels only, but keep ALL assessment years -- the post-2023
# years are what reveal which accounts died and which were born.
dtA <- dtA[assessmentClass1 == "RESIDENTIAL"]
LOWDENS <- c("RF1","RF2","RF3","RF4","RS")
dtA <- dtA[assessmentClass1 == "RESIDENTIAL" & zoning %in% LOWDENS]

dtP <- dtP[year >= 2024 & zoning == "RS"]
dtP <- dtP[grepl("Building - New", workType, ignore.case = TRUE)]

# ------------------------------------------------------------------------------
# 2. Parent and child snapshots
# ------------------------------------------------------------------------------
par0 <- dtA[assessmentYear == PARENT_YR]
kid0 <- dtA[assessmentYear == CHILD_YR]

cat("\n--- snapshot sizes ---\n")
cat("parents (", PARENT_YR, "):", nrow(par0), "\n")
cat("children (", CHILD_YR, "):", nrow(kid0), "\n")

# Accounts present in one snapshot but not the other. Account numbers are
# reissued on parcel change, so death/birth is the parcel-change signal.
died <- par0[!accountNumber %in% kid0$accountNumber]
born <- kid0[!accountNumber %in% par0$accountNumber]

cat("died  (parent candidates):", nrow(died), "\n")
cat("born  (child  candidates):", nrow(born), "\n")

# Guard: geometry is required for the spatial link.
died <- died[!is.na(longitude) & !is.na(latitude) & !is.na(lotSize)]
born <- born[!is.na(longitude) & !is.na(latitude) & !is.na(lotSize)]
cat("with usable geometry -- died:", nrow(died), " born:", nrow(born), "\n")

sfDied <- st_transform(st_as_sf(died, coords = c("longitude", "latitude"),
                                crs = 4326, remove = FALSE), CRS_M)
sfBorn <- st_transform(st_as_sf(born, coords = c("longitude", "latitude"),
                                crs = 4326, remove = FALSE), CRS_M)

# ------------------------------------------------------------------------------
# 3. Spatial link, one pass per buffer radius
# ------------------------------------------------------------------------------
linkAt <- function(radius) {
	hits <- st_intersects(st_buffer(sfBorn, radius), sfDied)
	keep <- which(lengths(hits) > 0)
	if (!length(keep)) return(data.table())

	rbindlist(lapply(keep, function(i) {
		j <- hits[[i]]
		data.table(
			childAcct   = sfBorn$accountNumber[i],
			childLegal  = sfBorn$legalN[i],
			childLot    = sfBorn$lotSize[i],
			childNbhd   = sfBorn$neighbourhood[i],
			childLon    = sfBorn$longitude[i],
			childLat    = sfBorn$latitude[i],
			parentAcct  = sfDied$accountNumber[j],
			parentLot   = sfDied$lotSize[j],
			parentVal   = sfDied$assessedValue[j],
			parentLon   = sfDied$longitude[j],
			parentLat   = sfDied$latitude[j],
			radius      = radius
		)
	}))
}

linkAll <- rbindlist(lapply(BUFFERS, linkAt))

# ------------------------------------------------------------------------------
# 4. Area-conservation test
# ------------------------------------------------------------------------------
# Land area is conserved through consolidation and subdivision, so the true
# parent set is the one whose areas sum to the child's. Ratios far from 1
# mean the buffer swept in unrelated parcels or missed one.
chk <- linkAll[, .(
		nParent    = .N,
		sumParLot  = sum(parentLot),
		sumParVal  = sum(parentVal),
		meanParVal = mean(parentVal),
		childLot   = childLot[1],
		childNbhd  = childNbhd[1],
		childLegal = childLegal[1]
	), by = .(radius, childAcct)]

chk[, ratio    := childLot / sumParLot]
chk[, areaOK   := ratio %between% AREA_TOL]
chk[, assembly := nParent >= 2 & areaOK]
chk[, oneToOne := nParent == 1 & areaOK]

# Subdivision runs the same logic with the roles reversed: group the children
# that share a parent and require their areas to sum to the parent's.
sub <- linkAll[, .(
		nChild    = uniqueN(childAcct),
		sumKidLot = sum(unique(data.table(childAcct, childLot))$childLot),
		parentLot = parentLot[1]
	), by = .(radius, parentAcct)]
sub[, ratio       := sumKidLot / parentLot]
sub[, subdivision := nChild >= 2 & ratio %between% AREA_TOL]

cat("\n--- classification by buffer radius ---\n")
print(chk[, .(children  = .N,
              assembly  = sum(assembly),
              oneToOne  = sum(oneToOne),
              ambiguous = sum(!areaOK),
              medRatio  = round(median(ratio, na.rm = TRUE), 3)), by = radius])

cat("\n--- subdivision by buffer radius ---\n")
print(sub[, .(parents = .N, subdivision = sum(subdivision)), by = radius])

# ------------------------------------------------------------------------------
# 5. Validation
# ------------------------------------------------------------------------------
# (a) Does the method recover the trivial case? For nParent == 1 the child
#     lot size should equal the parent's almost exactly.
cat("\n--- validation (a): 1:1 area recovery ---\n")
print(chk[nParent == 1, .(n = .N,
                          med = round(median(ratio, na.rm = TRUE), 4),
                          q10 = round(quantile(ratio, .10, na.rm = TRUE), 4),
                          q90 = round(quantile(ratio, .90, na.rm = TRUE), 4)),
          by = radius])

# (b) Are linked parents contiguous? Pairwise centroid distance among the
#     parents of one child should be about one lot width, not a block away.
parSpread <- linkAll[, {
		if (.N < 2) .(maxPairDist = NA_real_) else {
			m <- as.matrix(data.table(parentLon, parentLat))
			p <- st_transform(st_as_sf(data.frame(lon = m[,1], lat = m[,2]),
			                  coords = c("lon","lat"), crs = 4326), CRS_M)
			.(maxPairDist = max(as.numeric(st_distance(p))))
		}
	}, by = .(radius, childAcct)]

cat("\n--- validation (b): parent spread, metres (multi-parent only) ---\n")
print(parSpread[!is.na(maxPairDist),
                .(n = .N, med = round(median(maxPairDist), 1),
                  q90 = round(quantile(maxPairDist, .90), 1)), by = radius])

# (c) Does the geometric flag agree with the permit text? Assembled sites
#     often carry several lot numbers in the legal description.
dtP[, nLotTok  := lengths(regmatches(legalN, gregexpr("LOT", legalN)))]
dtP[, hasRange := grepl("LOT[S]? *[0-9A-Z]+ *(-|TO|&|,) *[0-9A-Z]+", legalN)]
dtP[, textAsm  := nLotTok > 1 | hasRange]

cat("\n--- validation (c): permit-text assembly signal ---\n")
print(dtP[, .N, by = .(nLotTok, hasRange)][order(-N)])

# ------------------------------------------------------------------------------
# 6. Attach to permits
# ------------------------------------------------------------------------------
# Exact legal match first; nearest child parcel for the remainder.
main <- chk[radius == 25]

dtP[, permitId := rowId]
byLegal <- merge(dtP, main, by.x = "legalN", by.y = "childLegal", all.x = TRUE)

unmatched <- byLegal[is.na(childAcct) & !is.na(longitude) & !is.na(latitude)]
if (nrow(unmatched) && nrow(sfBorn)) {
	sfU  <- st_transform(st_as_sf(unmatched, coords = c("longitude","latitude"),
	                              crs = 4326, remove = FALSE), CRS_M)
	nn   <- st_nearest_feature(sfU, sfBorn)
	dst  <- as.numeric(st_distance(sfU, sfBorn[nn, ], by_element = TRUE))
	fill <- data.table(permitId  = sfU$permitId,
	                   nnAcct    = sfBorn$accountNumber[nn],
	                   nnDist    = dst)
	cat("\n--- nearest-neighbour fill: distance distribution (m) ---\n")
	print(summary(fill$nnDist))
	fill <- fill[nnDist <= 30]
	byLegal[fill, on = "permitId", `:=`(childAcct = i.nnAcct)]
	byLegal <- merge(byLegal[, setdiff(names(byLegal),
	                    setdiff(names(main), "childAcct")), with = FALSE],
	                 main, by = "childAcct", all.x = TRUE)
}

perm <- byLegal[!is.na(childAcct)]
perm[, assembled := as.integer(assembly)]

cat("\n--- permits linked ---\n")
cat("permits in:", nrow(dtP), " linked:", nrow(perm),
    " assembled:", sum(perm$assembled, na.rm = TRUE), "\n")

# Geometric vs textual agreement on the linked set.
cat("\n--- geometric vs textual assembly flag ---\n")
print(table(geometric = perm$assembled, textual = perm$textAsm, useNA = "ifany"))

# ------------------------------------------------------------------------------
# 7. Is assembly more common where land is dearer?
# ------------------------------------------------------------------------------
meanP <- par0[, .(meanVal = mean(assessedValue, na.rm = TRUE)), by = neighbourhood]
perm  <- merge(perm, meanP, by.x = "childNbhd", by.y = "neighbourhood", all.x = TRUE)

est <- perm[!is.na(meanVal) & !is.na(sumParLot) & buildingType != "Duplex (210)"]

cat("\n--- assembly on neighbourhood land value ---\n")
print(summary(feols(assembled ~ log(meanVal), data = est, cluster = ~childNbhd)))
print(summary(feols(assembled ~ log(meanVal) + i(buildingType),
                    data = est, cluster = ~childNbhd)))
print(summary(feols(assembled ~ log(meanVal) + i(buildingType) + poly(sumParLot, 2),
                    data = est, cluster = ~childNbhd)))

# Parent land value is the pre-development measure you actually want, and for
# assemblies it is correctly summed across parents rather than taken from one
# arbitrary parcel.
cat("\n--- construction value on summed parent value ---\n")
print(summary(feols(log(constructionValue) ~ log(sumParVal) + i(buildingType) +
                    poly(sumParLot, 2) | year, data = est, cluster = ~childNbhd)))

fwrite(perm, "~/Downloads/edmontonPermitParent.csv")
cat("\nwrote ~/Downloads/edmontonPermitParent.csv\n")

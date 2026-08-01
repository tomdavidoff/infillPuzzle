# edmontonMatch.R
# Match building permits to assessment
# Strategy: do 2026 and late 2025 permits so old address still valid maybe
# Tom Davidoff
# 07/16/26

library(data.table)
library(fixest)
library(sf)

dtA <- fread("~/DropboxExternal/dataRaw/edmonton/Property_Assessment_Data_(Historical)_20260611.csv")
dtC <- fread("~/DropboxExternal/dataRaw/edmonton/Property_Information_(Current_Calendar_Year)_20260612.csv")
dtP  <- fread("~/DropboxExternal/dataRaw/edmonton/edmontonBuildingPermits.csv")


# first question: Suites in RS 2025+ completions
print(head(dtC))
dtCs <- dtC[zoning=="RS" & year_built>=2025 ]
print(dtCs[,.(N=.N,hasSuite=sum(Suite!=""))])
# Answer: no suites, that's not how rowhouse for sale would be identified

# edmontonMatch_skeleton.R
# Structure for the three-file linkage. NOT runnable — stubs mark where
# real logic (canon(), knn match, demo chain, area-conservation) plugs in.
# Division of labour:
#   dtP (permits)      = CLASSIFIER  -> built form, units, suites
#   dtA (historical)   = PARENT      -> pre-redev lot_size (2023 vintage)
#   dtC (current year) = CHILD       -> new post-redev parcels (for assemblies)
#
# Column provenance (what each join produces):
#   builtForm, unitsAdded, suiteFlag   <- dtP        (never from roll)
#   parentLotSize, parentValue         <- dtA        via permit->parent join
#   n_parents, parent_area (assembly)  <- dtC<->dtA  child->parent join
#   builtForm MUST ride onto both the parent-lot number AND the assembly number

library(data.table); library(sf); library(nabor)

# ---------------------------------------------------------------------------
# 0. canonical legal + shared helpers  (apply IDENTICALLY to every file)
# ---------------------------------------------------------------------------
canon <- function(x) {
  x <- toupper(x)
  x <- gsub("BLOCK:?","BLK",x); x <- gsub("LOT:?","LOT",x)
  x <- gsub(":","",x); gsub("\\s+"," ",trimws(x))
}
proj <- function(dt) st_coordinates(                      # lon/lat -> UTM12N m
  st_transform(st_as_sf(dt, coords=c("lon","lat"), crs=4326), 26912))

# ---------------------------------------------------------------------------
# 1. CLASSIFY  — everything that decides "what was built" lives on the permit
# ---------------------------------------------------------------------------
# dtP[, builtForm  := classify(building_type, JOB_DESCRIPTION)]   # row/SF/semi/...
# dtP[, unitsAdded := UNITS_ADDED]
# dtP[, suiteFlag  := unitsAdded>=2 & grepl("SUITE|SECONDARY", toupper(JOB_DESCRIPTION))]
# dtP[, legalC := canon(legalDescription)]
# -> builtForm is the label that must survive to the final table on BOTH paths below.

# ---------------------------------------------------------------------------
# 2. PARENT side — historical roll, restricted to pre-redevelopment vintage
# ---------------------------------------------------------------------------
# dtA <- dtA[assessmentYear < 2024]
# dtA[, maxYear := max(assessmentYear), by=legalC]
# dtA <- dtA[assessmentYear == maxYear]        # DECISION: latest pre-2024, not hard ==2023
# dtA[, legalC := canon(legalDescription)]
#   carries: legalC, lot_size, assessedValue, lon/lat

# ---------------------------------------------------------------------------
# 3a. JOIN A  permit -> parent   (gets lot_size for the classified project)
#     Three tiers, because new legals often don't exist in the 2023 roll:
# ---------------------------------------------------------------------------
#   tier 1: legal match      dtP$legalC %in% dtA$legalC        (unsubdivided cases)
#   tier 2: demo chain       permit -> demo permit -> parent legal   (subdivided)
#   tier 3: spatial fallback knn(proj(dtA), proj(dtP), k=5) + 20m cap + sequencing
#   result: dtP gains parentLotSize, parentValue  — builtForm already on dtP. GOOD.

# ---------------------------------------------------------------------------
# 3b. JOIN B  child -> parent(s)   (assemblies: new dtC account -> dying dtA accts)
# ---------------------------------------------------------------------------
#   childNew <- dtC[account NOT in 2023 vintage]         # post-redev parcels
#   parents  <- dtA[account dies 2023->2025]
#   link via buffer spatial join + area-conservation (childArea / sum(parentArea)
#            within 0.90–1.10); n_parents, parent_area per child.
#   >>> CRITICAL: childNew has NO builtForm yet — it's a roll record. Attach the
#       permit's builtForm to the child BEFORE attributing an assembly, else the
#       assembly gets classified off roll fields (the bug you're fixing).
#   childNew <- merge(childNew, dtP[,.(builtForm, ...key...)], by=<addr/spatial>)

# ---------------------------------------------------------------------------
# 4. ASSEMBLE final table — builtForm from permit governs on both branches
# ---------------------------------------------------------------------------
# out <- rbind(
#   projectsFromJoinA[, .(projectId, builtForm, lotSize=parentLotSize, n_parents=1L)],
#   projectsFromJoinB[, .(projectId, builtForm, lotSize=parent_area,   n_parents)]
# )  # then dedup where a project appears on both paths, preferring assembly-aware area

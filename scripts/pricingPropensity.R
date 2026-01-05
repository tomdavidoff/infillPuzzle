# pricingPropensity.R
# pricing and propensity for multiplex
# Tom Davidoff
# 12/24/25


library(data.table)
library(fixest)
library(ggplot2)
library(scales)
library(stringr)
library(RSQLite)
library(sf)

# ==========================================
# 1. SETTINGS & DATA SOURCE PATHS
# ==========================================
GEO_LEVEL <- "census_tract" # Options: "bca_nbhd", "census_tract", "fsa_plus_1"
PCCF_RAW  <- "data/raw/pccfNat_fccpNat_062017.txt"
PCCF_RDS  <- "data/pccf_bc.rds"
BCA_DB    <- "data/raw/REVD16_and_inventory_extracts.sqlite3"
PERMIT_F  <- "data/raw/vancouver_permits_full.csv"
ZONING_F  <- "data/raw/vancouver_zoning.geojson"

clean_pc  <- function(x) gsub(" ", "", toupper(trimws(x)))

# ==========================================
# 2. CROSSWALK (PCCF) PREP
# ==========================================
if (!file.exists(PCCF_RDS)) {
    dtPccf <- fread(PCCF_RAW, sep = "\n", header = FALSE, encoding = "Latin-1")
    Encoding(dtPccf$V1) <- "latin1"
    dtPccf[, `:=`(
        postalCode  = substr(V1, 1, 6),
        cma         = substr(V1, 99, 101),
        censusTract = substr(V1, 103, 109),
        sli         = substr(V1, 162, 162)
    )]
    # Keep Vancouver (933) and Single Link Indicator
    saveRDS(dtPccf[cma == "933" & sli == "1", .(postalCode, censusTract)], PCCF_RDS)
}

# ==========================================
# 3. BCA DATA: SINGLE SQL JOIN & CLEAN
# ==========================================
db <- dbConnect(SQLite(), BCA_DB)
# Single query approach is much cleaner than multiple R merges
dtBCA <- as.data.table(dbGetQuery(db, "
    SELECT s.folioID, s.conveyancePrice, s.conveyanceDate,
           i.MB_effective_year, i.MB_total_finished_area,
           d.actualUseDescription, d.neighbourhoodDescription, a.postalCode
    FROM sales s
    JOIN folio f ON s.folioID = f.folioID
    JOIN residentialInventory i ON f.rollNumber = i.roll_number
    JOIN folioDescription d ON f.folioID = d.folioID
    JOIN address a ON f.folioID = a.folioID
    WHERE s.conveyanceTypeDescription = 'Improved Single Property Transaction'
      AND i.jurisdiction = 200 AND d.actualUseDescription = 'Single Family Dwelling'
"))
dbDisconnect(db)

# Numeric conversion and basic cleaning
cols <- c("conveyancePrice", "MB_effective_year", "MB_total_finished_area")
dtBCA[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
dtBCA <- dtBCA[!is.na(MB_total_finished_area) & conveyancePrice > 100000]

# Metrics & Outliers
CUT <- 0.05
dtBCA[, `:=`(age = 2016 - MB_effective_year, ppsf = conveyancePrice / MB_total_finished_area, cleanPostal = clean_pc(postalCode))]
dtBCA <- dtBCA[age >= 0 &
               MB_total_finished_area %between% quantile(MB_total_finished_area, c(CUT, 1-CUT))]

# ==========================================
# 4. PERMIT DATA & SPATIAL JOIN
# ==========================================
if (!file.exists(PERMIT_F)) download.file("https://opendata.vancouver.ca/api/explore/v2.1/catalog/datasets/issued-building-permits/exports/csv?delimiter=%3B", PERMIT_F)
if (!file.exists(ZONING_F)) download.file("https://opendata.vancouver.ca/api/explore/v2.1/catalog/datasets/zoning-districts-and-labels/exports/geojson", ZONING_F)

dtPermit <- fread(PERMIT_F)
setnames(dtPermit, make.names(names(dtPermit)))

# Filter Major Infill Projects
dtMajor <- dtPermit[as.IDate(issuedate) >= "2019-01-01" &
                    typeofwork %in% c("New Building", "Addition / Alteration") &
                    projectvalue > 200000]

dtMajor[, `:=`(
    infillType = ifelse(grepl("uplex|Multiple Dwelling", specificusecategory, ignore.case=T) |
                        grepl("convert|two-family|2-family", projectdescription, ignore.case=T),
                        "Infill/Duplex", "Major SFD Reno"),
    cleanPostal = clean_pc(str_sub(address, -7))
)]

print(head(dtMajor))
q("no")

# Extract Lat/Long and Spatial Join
dtMajor[, c("Lat", "Lon") := tstrsplit(geo_point_2d, ", ", type.convert = TRUE)]
sfPermit <- st_join(st_as_sf(dtMajor[!is.na(Lat)], coords=c("Lon","Lat"), crs=4326),
                    st_read(ZONING_F, quiet=T))

# Filter for R-Zone (using R1-1 as requested)
dtRZone <- as.data.table(sfPermit)[grepl("R1-1", zoning_district)]

# ==========================================
# 5. GEOGRAPHIC CROSSWALK
# ==========================================
if (GEO_LEVEL == "bca_nbhd") {
    crosswalk <- dtBCA[, .N, by = .(cleanPostal, geo_id = neighbourhoodDescription)][order(cleanPostal, -N)][, .(geo_id = geo_id[1]), by = cleanPostal]
} else if (GEO_LEVEL == "census_tract") {
    crosswalk <- readRDS(PCCF_RDS)[, .(cleanPostal = postalCode, geo_id = censusTract)]
} else {
    crosswalk <- dtBCA[, .(geo_id = str_sub(cleanPostal, 1, 4)), by = cleanPostal]
}

# ==========================================
# 6. ANALYSIS & VIZ
# ==========================================
dtBCAGeo   <- merge(dtBCA, crosswalk, by = "cleanPostal")
dtPermitGeo <- merge(dtRZone, crosswalk, by = "cleanPostal")

# Step 1: BCA Pricing Slope (Concavity)
m_slopes <- feols(ppsf ~ log(age + 1) + i(geo_id, MB_total_finished_area) | geo_id, data = dtBCAGeo)
dtSlopes <- as.data.table(broom::tidy(m_slopes))[grepl("MB_total_finished_area", term)]
dtSlopes[, geo_id := gsub("geo_id::(.*):MB_total_finished_area", "\\1", term)]

# Calculate Mean Price per SqFt <<<
dtBcaMeans <- dtBCAGeo[, .(meanPPSF = mean(ppsf, na.rm = TRUE)), by = geo_id]


# Step 2: Permit Propensity
dtInfill <- dtPermitGeo[, .(propensity = sum(infillType == "Infill/Duplex") / .N, totalProjects = .N), by = geo_id]

# Final Merge for Plot
dtFinal <- merge(dtInfill, dtSlopes, by = "geo_id")
dtFinal <- merge(dtFinal, dtBcaMeans, by = "geo_id")

print(dtFinal[,.(.N,mean(total_sqft,na.rm=TRUE),median(total_sqft,na.rm=TRUE)),by=type])


ggplot(dtFinal[totalProjects > 10], aes(x = estimate, y = propensity)) +
    geom_point(aes(size = totalProjects), alpha = 0.4) +
    geom_smooth(method = "lm", color = "firebrick") +
    scale_y_continuous(labels = percent) +
    labs(title = paste("Concavity vs Infill:", GEO_LEVEL), x = "BCA Pricing Slope", y = "Infill Propensity") +
    theme_minimal()
ggsave(paste0("text/pricingPropensity", GEO_LEVEL, ".png"), width = 8, height = 6)

mu <- feols(propensity ~ meanPPSF, data = dtFinal[totalProjects > 10])
mc <- feols(propensity ~ estimate + meanPPSF, data = dtFinal[totalProjects > 10])
# reg summary from fixest package
print(etable(mu, mc))


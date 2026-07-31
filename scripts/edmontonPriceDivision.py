# ==============================================================================
# edmontonPriceDivision.py
# Tom Davidoff, July 30 2026
#
# Q1 (confirm): higher neighbourhood mean price -> higher P(division/rowhouse)
# Q2 (explore): price-size elasticity within neighbourhoods, compared across them
# Pre-step   : verify condos/suites identifiable by zero/null lot size
# ==============================================================================
import os
import numpy as np
import polars as pl

DATA = os.path.expanduser("~/DropboxExternal/dataRaw/edmonton")
CURR = f"{DATA}/Property_Information_(Current_Calendar_Year)_20260612.csv"
HIST = f"{DATA}/Property_Assessment_Data_(Historical)_20260611.csv"
PERM = f"{DATA}/edmontonBuildingPermits.csv"

PRICE_YEAR = 2023          # pre-reform assessed value for neighbourhood price tiers
NEW_YEAR   = 2024          # reform / permit cohort

def num(col, dtype=pl.Float64):
    return (pl.col(col).str.strip_chars().str.replace_all(r"[$,]", "")
            .cast(dtype, strict=False))

hist = pl.read_csv(HIST, infer_schema_length=0)
curr = pl.read_csv(CURR, infer_schema_length=0)
perm = pl.read_csv(PERM, infer_schema_length=0)

# ==============================================================================
# 0. VERIFY: do condos / suites show zero or null lot size?
#    Logic: a strata/condo unit owns no land -> lot_size 0 or null.
#    A suite (secondary unit) may share a parcel -> check its lot pattern too.
# ==============================================================================
print("="*70, "\n0. CONDO / SUITE vs LOT SIZE\n", "="*70, sep="")
c = curr.with_columns(
    lot = num("lot_size"),
    has_suite = pl.col("Suite").is_not_null() & (pl.col("Suite").str.strip_chars() != ""),
)
print("Lot-size status overall (current roll):")
print(c.select(
    total     = pl.len(),
    lot_zero  = (pl.col("lot") == 0).sum(),
    lot_null  = pl.col("lot").is_null().sum(),
    lot_pos   = (pl.col("lot") > 0).sum(),
))
print("\nLot-size status among parcels WITH a suite value:")
print(c.filter(pl.col("has_suite")).select(
    suited_total = pl.len(),
    lot_zero = (pl.col("lot") == 0).sum(),
    lot_null = pl.col("lot").is_null().sum(),
    lot_pos  = (pl.col("lot") > 0).sum(),
))
# cross zoning against zero-lot to see if zero-lot really means condo-type zones
print("\nZoning of ZERO/NULL-lot parcels (top 15) -- expect condo/RA/multi codes:")
print(c.filter((pl.col("lot") == 0) | pl.col("lot").is_null())
      .group_by("zoning").agg(n=pl.len()).sort("n", descending=True).head(15))

# ==============================================================================
# 1. NEIGHBOURHOOD MEAN PRICE (pre-reform) from the historical panel
# ==============================================================================
print("\n" + "="*70, "\n1. NEIGHBOURHOOD PRICE\n", "="*70, sep="")
h = hist.with_columns(
    ayear    = num("Assessment Year", pl.Int64),
    assessed = num("Assessed Value"),
    lot      = num("Lot Size"),
    yb       = num("Actual Year Built", pl.Int64),
)
# residential land parcels only for a clean price measure (drop zero-lot condos)
nbhd_price = (
    h.filter((pl.col("ayear") == PRICE_YEAR)
             & (pl.col("Assessment Class 1") == "RESIDENTIAL")
             & (pl.col("lot") > 0)
             & pl.col("assessed").is_not_null())
    .group_by("Neighbourhood")
    .agg(mean_price   = pl.col("assessed").mean(),
         median_price = pl.col("assessed").median(),
         mean_lot     = pl.col("lot").mean(),
         n_parcels    = pl.len())
    .filter(pl.col("n_parcels") >= 25)          # drop tiny neighbourhoods
)
print(f"Neighbourhoods with >=25 residential parcels in {PRICE_YEAR}: {nbhd_price.height}")
print(nbhd_price.sort("mean_price", descending=True).head(5))
print(nbhd_price.sort("mean_price").head(5))

# ==============================================================================
# 2. DIVISION OUTCOME per neighbourhood, from permits (leads the roll)
#    division = Row House (330) permit in RS, post-reform; base = Row House + Single
# ==============================================================================
print("\n" + "="*70, "\n2. DIVISION RATE vs PRICE\n", "="*70, sep="")
p = perm.with_columns(
    y     = num("YEAR", pl.Int64),
    units = num("UNITS_ADDED", pl.Int64),
)
res = p.filter(
    (pl.col("y") >= NEW_YEAR)
    & pl.col("ZONING").str.contains(r"\bRS\b")
    & pl.col("BUILDING_TYPE").is_in(["Row House (330)", "Single Detached House (110)"])
).with_columns(
    is_row = (pl.col("BUILDING_TYPE") == "Row House (330)").cast(pl.Int8)
)

nbhd_div = (
    res.group_by("NEIGHBOURHOOD")
    .agg(permits   = pl.len(),
         rowhouses = pl.col("is_row").sum(),
         div_rate  = pl.col("is_row").mean())
    .filter(pl.col("permits") >= 5)             # need a few permits to trust a rate
)

# join price to division. NOTE: neighbourhood name casing differs between files
# (roll "Neighbourhood" vs permit "NEIGHBOURHOOD") -> normalise to upper for join.
merged = (
    nbhd_div.with_columns(nb = pl.col("NEIGHBOURHOOD").str.strip_chars().str.to_uppercase())
    .join(
        nbhd_price.with_columns(nb = pl.col("Neighbourhood").str.strip_chars().str.to_uppercase()),
        on="nb", how="inner")
)
print(f"Neighbourhoods with both price and >=5 RS permits: {merged.height}")

# raw correlation: does higher price -> higher division rate?
mp = merged.select(
    corr_meanprice_div = pl.corr("mean_price", "div_rate"),
    corr_medprice_div  = pl.corr("median_price", "div_rate"),
    corr_lot_div       = pl.corr("mean_lot", "div_rate"),
)
print("\nNeighbourhood-level correlations:")
print(mp)

# quick tercile view: division rate by price tier
merged = merged.with_columns(
    price_tier = pl.col("mean_price").qcut(3, labels=["low", "mid", "high"])
)
print("\nDivision rate by neighbourhood price tercile:")
print(merged.group_by("price_tier").agg(
    n_nbhd    = pl.len(),
    div_rate  = pl.col("div_rate").mean(),
    mean_lot  = pl.col("mean_lot").mean(),
    permits   = pl.col("permits").sum(),
).sort("price_tier"))
# ==============================================================================
# 3. PRICE–SQFT ELASTICITY by neighbourhood  (log assessed ~ log gross area)
#    Pools houses + condos (condos = the low/zero-lot floor-area obs, kept in).
#    Run for: all / suite-only / no-suite.
#    CAVEAT: gross-area pool includes some condos; that's intended.
# ==============================================================================
he = hist.with_columns(
    ayear    = num("Assessment Year", pl.Int64),
    assessed = num("Assessed Value"),
    gross    = num("Total Gross Area"),
    has_suite = pl.col("Suite").is_not_null() & (pl.col("Suite").str.strip_chars() != ""),
)

base23 = hist.with_columns(ayear=num("Assessment Year", pl.Int64)).filter(
    (pl.col("ayear")==2023) & (pl.col("Assessment Class 1")=="RESIDENTIAL"))
print(f"2023 residential parcels: {base23.height:,}")
print(f"  surviving to 2026 roll: {he.height:,} "
      f"({he.height/base23.height*100:.1f}%)")


def elasticity_by_nbhd(df, label, suite_filter=None):
    d = df.filter(
        (pl.col("ayear") == PRICE_YEAR)
        & (pl.col("Assessment Class 1") == "RESIDENTIAL")
        & (pl.col("gross") > 0) & (pl.col("assessed") > 0)
    )
    if suite_filter is not None:
        d = d.filter(pl.col("has_suite") == suite_filter)
    d = d.with_columns(log_val = pl.col("assessed").log(),
                       log_sqft = pl.col("gross").log())
    out = (
        d.group_by("Neighbourhood")
        .agg(
            n    = pl.len(),
            beta = (((pl.col("log_sqft") - pl.col("log_sqft").mean())
                     * (pl.col("log_val") - pl.col("log_val").mean())).sum()
                    / ((pl.col("log_sqft") - pl.col("log_sqft").mean())**2).sum()),
            mean_price = pl.col("assessed").mean(),
        )
        .filter(pl.col("n") >= 30)
    )
    print(f"\n[{label}] neighbourhoods={out.height}, "
          f"beta p10/p50/p90 = "
          f"{out['beta'].quantile(.1):.3f} / {out['beta'].median():.3f} / {out['beta'].quantile(.9):.3f}")
    print(f"         corr(beta, mean_price) = {out.select(pl.corr('beta','mean_price')).item():.3f}")
    return out

elas_all  = elasticity_by_nbhd(he, "ALL (houses+condos)")
elas_suite= elasticity_by_nbhd(he, "SUITE only",    suite_filter=True)
elas_nosu = elasticity_by_nbhd(he, "NO SUITE",      suite_filter=False)


print("A. Does the low-beta-in-fancy result survive using IMPROVEMENT value only?")
#    (strip out land, which is the imputed/location-driven part)
#    -- need an improvement/structure value column; check curr/hist for it
print([col for col in curr.columns if 'mprove' in col.lower() or 'structure' in col.lower()
       or 'building' in col.lower() or 'land' in col.lower()])
print([col for col in hist.columns if 'mprove' in col.lower() or 'land' in col.lower()])

# B. Is it a sqft-variance artifact? var(log sqft) by price tier
tier_var = (elas_base
    .join(nbhd_price.select("Neighbourhood","median_price"), on="Neighbourhood", how="inner")
    .with_columns(tier=pl.col("median_price").qcut(3, labels=["low","mid","high"]))
    .group_by("tier").agg(
        var_logsqft=pl.col("log_sqft").var(),
        var_logval=pl.col("log_val").var(),
        median_sqft=pl.col("gross").median(),
        n=pl.len()))
print(tier_var.sort("tier"))

# C. Add age control: within-nbhd beta of logV on logS AND log(age)
#    (a multivariate slope; if beta->1 once age is controlled, the raw beta was
#     confounded by old-fancy-neighbourhood quality)

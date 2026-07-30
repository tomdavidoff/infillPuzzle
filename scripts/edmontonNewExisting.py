# ==============================================================================
# edmontonNewExisting.py  — new vs pre-existing RS parcels via the assessment panel
# Tom Davidoff, July 2026
# ==============================================================================
import os, numpy as np, polars as pl
from pyproj import Transformer
from scipy.spatial import cKDTree

DATA = os.path.expanduser("~/DropboxExternal/dataRaw/edmonton")
CURR = f"{DATA}/Property_Information_(Current_Calendar_Year)_20260612.csv"
HIST = f"{DATA}/Property_Assessment_Data_(Historical)_20260611.csv"
PERM = f"{DATA}/edmontonBuildingPermits.csv"

NEW_YEAR = 2024          # construction cohort of interest
RS_EXACT = "RS"          # strict current RS district (your call: RS is the meat)

# --- load everything as text (no inference crashes), cast only what we use ----
def num(col, dtype=pl.Float64):
    return pl.col(col).str.strip_chars().str.replace_all(r"[$,]", "").cast(dtype, strict=False)

curr = pl.read_csv(CURR, infer_schema_length=0)
hist = pl.read_csv(HIST, infer_schema_length=0)
perm = pl.read_csv(PERM, infer_schema_length=0)

# --- audit: did any non-empty numeric cell silently become null on cast? ------
for df, name, cols in [(hist, "hist", ["Lot Size", "Assessed Value", "Actual Year Built"]),
                       (curr, "curr", ["year_built","lot_size"])]:
    for c in cols:
        if c in df.columns:
            nonempty = df.filter(pl.col(c).str.strip_chars().is_in(["", "N/A", "n/a", "-", "NA"]).not_()
                                 & pl.col(c).is_not_null()).height
            parsed = df.select(num(c)).to_series().is_not_null().sum()
            if nonempty - parsed:
                print(f"  ⚠ {name}.{c}: {nonempty-parsed} non-empty cells failed to parse")

# ==============================================================================
# 1. Collapse the assessment panel to one row per account
#    -> earliest year the account was ever assessed, plus current traits
# ==============================================================================
acct = (
    hist.with_columns(ayear=num("Assessment Year", pl.Int64))
    .sort("ayear")
    .group_by("Account Number")
    .agg(
        first_ayear = pl.col("ayear").min(),
        yb          = num("Actual Year Built", pl.Int64).drop_nulls().last(),
        lot_size    = num("Lot Size").drop_nulls().last(),
        zoning_now  = pl.col("Zoning").drop_nulls().last(),
        class1      = pl.col("Assessment Class 1").drop_nulls().last(),
    )
)
PANEL_MIN = acct["first_ayear"].min()
print(f"\nPanel spans {PANEL_MIN}–{acct['first_ayear'].max()}, "
      f"{acct.height:,} accounts")

is_rs  = pl.col("zoning_now") == RS_EXACT
is_res = pl.col("class1") == "RESIDENTIAL"

# ==============================================================================
# 2. Classify: NEW parcel = account absent before NEW_YEAR *and* built ~NEW_YEAR
#    (both conditions guard against administrative renumbering)
# ==============================================================================
acct = acct.with_columns(
    new_account = pl.col("first_ayear") >= NEW_YEAR,
    is_new = is_rs & is_res
             & (pl.col("first_ayear") >= NEW_YEAR)
             & (pl.col("yb") >= NEW_YEAR)
             & (pl.col("first_ayear") > PANEL_MIN),
    is_existing = is_rs & is_res & (pl.col("first_ayear") < NEW_YEAR),
)

# validity check: of RS accounts first seen >= NEW_YEAR, are they actually new builds?
chk = acct.filter(is_rs & pl.col("new_account"))
print("\nRS accounts first appearing >= NEW_YEAR, by year_built:")
print(chk.select(total=pl.len(),
                 yb_new=(pl.col("yb") >= NEW_YEAR).sum(),
                 yb_old=(pl.col("yb") < NEW_YEAR).sum(),
                 yb_null=pl.col("yb").is_null().sum()))

n_new = acct.filter(pl.col("is_new")).height
n_old = acct.filter(pl.col("is_existing")).height
print(f"\nNEW RS parcels:      {n_new:,}")
print(f"EXISTING RS parcels: {n_old:,}")

# ==============================================================================
# 3. Lot size: the subdivision signature (expect NEW << EXISTING)
# ==============================================================================
for flag, label in [("is_new", "NEW RS"), ("is_existing", "EXISTING RS")]:
    s = acct.filter(pl.col(flag))["lot_size"].drop_nulls()
    if s.len():
        print(f"{label}: n={s.len():,}, median={s.median():.0f} m², "
              f"p10={s.quantile(.1):.0f}, p90={s.quantile(.9):.0f}")

# ==============================================================================
# 4. Does permit merge rate differ for NEW vs EXISTING RS parcels?
#    Bridge: acct (has Account Number) -> current roll (has coords) -> permits
# ==============================================================================
# tag every account with its class, then attach current-roll coordinates
acct_class = acct.select(
    "Account Number",
    parcel_class = pl.when(pl.col("is_new")).then(pl.lit("new"))
                    .when(pl.col("is_existing")).then(pl.lit("existing"))
                    .otherwise(pl.lit("other"))
).filter(pl.col("parcel_class") != "other")

roll = pl.read_csv(CURR, infer_schema_length=0).with_columns(
    lat=num("Latitude"), lon=num("Longitude"))
roll_tagged = roll.join(acct_class, on="Account Number", how="inner").filter(
    pl.col("lat").is_not_null() & pl.col("lon").is_not_null())

# 2024+ RS rowhouse/SFD permits with coords
perm2024 = perm.with_columns(
    y=num("YEAR", pl.Int64), lat=num("LATITUDE"), lon=num("LONGITUDE")
).filter((pl.col("y") >= NEW_YEAR)
         & pl.col("ZONING").str.contains(r"\bRS\b")
         & pl.col("lat").is_not_null())

# spatial match parcels -> permits at 10m, compare hit rate by class
to_utm = Transformer.from_crs("EPSG:4326", "EPSG:26912", always_xy=True)
def xy(d): return np.column_stack(to_utm.transform(d["lon"].to_numpy(), d["lat"].to_numpy()))
tree = cKDTree(xy(perm2024))
hits = [len(h) > 0 for h in tree.query_ball_point(xy(roll_tagged), r=10.0)]
roll_tagged = roll_tagged.with_columns(permit_hit=pl.Series(hits))

print("\nPermit merge rate by parcel class (10m):")
print(roll_tagged.group_by("parcel_class").agg(
    parcels=pl.len(),
    matched=pl.col("permit_hit").sum(),
    rate=(pl.col("permit_hit").mean() * 100).round(1)))

# current year median lot size by year built 2015 through 2026 -- convert both to numeric first
roll = roll.with_columns(
    lot_size=num("lot_size"),
    year_built=num("year_built", pl.Int64)
)
median_by_year = (
    roll
    .filter(
        (pl.col("zoning") == "RS")
        & pl.col("year_built").is_between(2015, 2026)
    )
    .group_by("year_built")
    .agg(
        median_lot = pl.col("lot_size").median(),
        n = pl.len(),
    )
    .sort("year_built")
)
print(median_by_year)

# assembled parcels = NEW accounts (post-2024) that are LARGE, not small
new_rs_lots = acct.filter(pl.col("is_new"))["lot_size"].drop_nulls()
print(f"NEW RS parcels: median={new_rs_lots.median():.0f}")
print(f"  small (<400, subdivision): {(new_rs_lots<400).sum()}")
print(f"  large (>840, assembly?):   {(new_rs_lots>840).sum()}")

# For 2025 RS parcels, split by account-newness AND lot size,
# and check unit intensity — are the BIG old-account lots multi-unit (rowhouse on undivided lot)?
roll_classed = (
    roll.filter((pl.col("zoning")=="RS") & (pl.col("year_built")==2025))
    .join(acct.select("Account Number","is_new"), on="Account Number", how="left")
    .with_columns(
        lot_bucket = pl.when(pl.col("lot_size")<400).then(pl.lit("small <400"))
                      .when(pl.col("lot_size")<700).then(pl.lit("mid 400-700"))
                      .otherwise(pl.lit("large >700")),
        acct_type  = pl.when(pl.col("is_new")).then(pl.lit("new")).otherwise(pl.lit("existing")),
    )
)
print(roll_classed.group_by(["acct_type","lot_bucket"]).agg(
    n=pl.len(), median_lot=pl.col("lot_size").median()).sort(["acct_type","lot_bucket"]))

roll_classed = (
    roll.filter((pl.col("zoning")=="RS") & (pl.col("year_built")==2025))
    .with_columns(gross_area = num("Total Gross Area"))
    .join(acct.select("Account Number","is_new"), on="Account Number", how="left")
    .with_columns(
        far = pl.col("gross_area") / pl.col("lot_size"),
        lot_bucket = pl.when(pl.col("lot_size")<400).then(pl.lit("small <400"))
                      .when(pl.col("lot_size")<700).then(pl.lit("mid 400-700"))
                      .otherwise(pl.lit("large >700")),
        acct_type  = pl.when(pl.col("is_new")).then(pl.lit("new")).otherwise(pl.lit("existing")),
    )
)
print(roll_classed.group_by(["acct_type","lot_bucket"]).agg(
    n            = pl.len(),
    median_lot   = pl.col("lot_size").median(),
    median_area  = pl.col("gross_area").median(),
    median_far   = pl.col("far").median(),
).sort(["acct_type","lot_bucket"]))


# Parse house number + street into numeric components for adjacency
def parse_addr(df, hn="House Number", st="Street Name"):
    return df.with_columns(
        hnum = pl.col(hn).str.extract(r"(\d+)", 1).cast(pl.Int64, strict=False),
        street = pl.col(st).str.strip_chars().str.to_uppercase(),
    )

# accounts present in 2024 but GONE by 2025 (candidate absorbed parcels)
last_seen = (hist.with_columns(ayear=num("Assessment Year", pl.Int64))
             .group_by("Account Number")
             .agg(last_ayear=pl.col("ayear").max(),
                  hn=pl.col("House Number").drop_nulls().last(),
                  st=pl.col("Street Name").drop_nulls().last(),
                  zoning=pl.col("Zoning").drop_nulls().last()))
disappeared = parse_addr(last_seen.filter(
    (pl.col("last_ayear") == 2024) & pl.col("zoning").str.contains(r"(?i)RS")),
    "hn", "st").select("hnum", "street").drop_nulls()

# 2025-built RS parcels: do they have a NEIGHBOR (±2 house numbers) that disappeared?
survivors = parse_addr(
    roll.filter((pl.col("zoning")=="RS") & (pl.col("year_built")==2025))
).select("hnum", "street", "lot_size", gross=num("Total Gross Area")).drop_nulls()

# join survivors to disappeared-neighbors on same street, house number within ±2
cand = survivors.join(
    disappeared.rename({"hnum": "gone_hnum"}),
    on="street", how="inner"
).filter((pl.col("hnum") - pl.col("gone_hnum")).abs().is_between(1, 2))

print(f"2025 RS parcels with a disappeared RS neighbor (±2 addr): "
      f"{cand['hnum'].n_unique()} distinct survivors")
print(cand.group_by(["street","hnum"]).agg(
    n_gone_neighbors=pl.len(),
    lot_size=pl.col("lot_size").first(),
    gross=pl.col("gross").first()).sort("n_gone_neighbors", descending=True).head(20))

print("CASE STUDY")

print(perm.filter(pl.col("ADDRESS").str.contains("10546") &
    pl.col("ADDRESS").str.to_uppercase().str.contains("72 AV")).select(
    "PERMIT_DATE","YEAR","BUILDING_TYPE","WORK_TYPE","UNITS_ADDED","ZONING"))

print(hist.filter(pl.col("Account Number") == "7793946").select(
    "Assessment Year","Lot Size","Actual Year Built","Zoning").sort("Assessment Year"))

print("in hist:", hist.filter(pl.col("Account Number")=="7793946").height)
print("in curr:", roll.filter(pl.col("Account Number")=="7793946").height)

# Row house by year
print("\nRow House permit counts and units added by year:")
print(perm.with_columns(y=num("YEAR", pl.Int64), u=num("UNITS_ADDED", pl.Int64))
      .filter((pl.col("ZONING").str.contains(r"\bRS\b"))
              & (pl.col("BUILDING_TYPE")=="Row House (330)"))
      .group_by("y").agg(permits=pl.len(), total_units=pl.col("u").sum(),
                         median_units=pl.col("u").median())
      .sort("y"))

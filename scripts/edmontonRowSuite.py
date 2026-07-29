# edmontonRowSuite.py
# do new row houses have suite numbers?
# Messing with Polars for kicks
# Tom Davidoff
# 07/28/26
"""
Suite incidence in 2024 RS-zone permits: rowhouse vs single-detached.

Question: under Edmonton's post-2024 RS-by-right regime, do newly-built
rowhouse parcels carry suite/unit numbers more often than single-detached?

Linkage: 2024 permits -> current property roll by spatial proximity
(nearest roll parcels within a radius), restricted to NEW construction
(year_built >= 2024) so we score the built units, not the pre-redevelopment
parcel. Roll assessment lags permits, so this is the fast-completing
subsample -- a snapshot, not the full 2024 population.
"""

import numpy as np
import polars as pl
from scipy.spatial import cKDTree
from pyproj import Transformer

PERMITS  = "/Users/davidoff/DropboxExternal/dataRaw/edmonton/edmontonBuildingPermits.csv"
ROLL     = "/Users/davidoff/DropboxExternal/dataRaw/edmonton/Property_Information_(Current_Calendar_Year)_20260612.csv"
RADIUS_M = 5.0          # spatial match radius, meters
NEW_YEAR = 2024          # count only parcels built in/after the permit year

# --- load ------------------------------------------------------------------
permits = pl.read_csv(PERMITS, infer_schema_length=10000, ignore_errors=True)
roll = pl.read_csv(
    ROLL, infer_schema_length=10000,
    schema_overrides={"Suite": pl.Utf8, "House Number": pl.Utf8,
                      "Account Number": pl.Utf8},
).filter(pl.col("Latitude").is_not_null() & pl.col("Longitude").is_not_null())

# --- filter permits: 2024, literal RS zone, by building type ---------------
in_rs = pl.col("ZONING")=="RS"   # RS only, not RSF/RSL/RSM
base = permits.filter( (pl.col("YEAR") == 2024) & in_rs & pl.col("LATITUDE").is_not_null() & pl.col("LONGITUDE").is_not_null())
row_2024 = base.filter(pl.col("BUILDING_TYPE") == "Row House (330)")
sfd_2024 = base.filter(pl.col("BUILDING_TYPE") == "Single Detached House (110)")

# --- project to UTM 12N (meters) and index the roll ------------------------
to_utm = Transformer.from_crs("EPSG:4326", "EPSG:32612", always_xy=True)

def xy(df, lon, lat):
    X, Y = to_utm.transform(df[lon].to_numpy(), df[lat].to_numpy())
    return np.column_stack([X, Y])

tree      = cKDTree(xy(roll, "Longitude", "Latitude"))
roll_year = roll.select(pl.col("year_built").cast(pl.Int64, strict=False))["year_built"]
roll_suite = roll["Suite"]

def has_suite(i):
    v = roll_suite[i]
    return v is not None and str(v).strip() != ""

# --- link + score new-construction parcels ---------------------------------
def score(permit_df, label):
    neighbors = tree.query_ball_point(xy(permit_df, "LONGITUDE", "LATITUDE"), r=RADIUS_M)
    new_idx = sorted({i for nb in neighbors for i in nb
                      if roll_year[i] is not None and roll_year[i] >= NEW_YEAR})
    permits_hit = sum(1 for nb in neighbors
                      if any(roll_year[i] is not None and roll_year[i] >= NEW_YEAR
                             for i in nb))
    n = len(new_idx)
    k = sum(1 for i in new_idx if has_suite(i))
    print(f"\n=== {label} (RS, 2024) @ {RADIUS_M:.0f}m, new construction ===")
    print(f"  permits:                     {permit_df.height:>5}")
    print(f"  permits w/ a new parcel:     {permits_hit:>5}")
    print(f"  new parcels matched:         {n:>5}")
    if n:
        print(f"  with a Suite value:          {k:>5}  ({k/n:.1%})")

score(row_2024, "Row House (330)")
score(sfd_2024, "Single Detached House (110)")

# --- Table A: year_built of parcels matched to 2024 RS rowhouse permits ---
nb = tree.query_ball_point(xy(row_2024, "LONGITUDE", "LATITUDE"), r=10.0)
matched = sorted({i for lst in nb for i in lst})

tblA = (
    roll[matched]
    .select(
        yb = pl.col("year_built").cast(pl.Int64, strict=False),
        has_suite = pl.col("Suite").is_not_null() & (pl.col("Suite").str.strip_chars() != "")
    )
    .group_by("yb").agg(
        parcels = pl.len(),
        suited  = pl.col("has_suite").sum(),
    )
    .sort("yb", descending=True)
)
print("Table A — matched parcels near 2024 RS rowhouse permits, by year_built:")
print(tblA.head(15))

# --- Table B: ALL RS roll parcels by year_built (denominator check) -------
tblB = (
    roll.filter(pl.col("zoning").str.contains(r"(?i)\bRS\b"))
    .select(
        yb = pl.col("year_built").cast(pl.Int64, strict=False),
        has_suite = pl.col("Suite").is_not_null() & (pl.col("Suite").str.strip_chars() != "")
    )
    .group_by("yb").agg(
        parcels = pl.len(),
        suited  = pl.col("has_suite").sum(),
    )
    .sort("yb", descending=True)
)
print("\nTable B — ALL RS roll parcels by year_built (recent years):")
print(tblB.head(15))

for df, lab in [(row_2024, "Row House"), (sfd_2024, "Single Detached")]:
    u = df.select(pl.col("UNITS_ADDED").cast(pl.Int64, strict=False))["UNITS_ADDED"]
    print(f"\n{lab} (2024 RS): UNITS_ADDED")
    print(f"  non-null: {u.is_not_null().sum()}/{df.height}")
    print(f"  distribution:")
    print(df.group_by(pl.col("UNITS_ADDED").cast(pl.Int64, strict=False))
            .agg(pl.len().alias("permits"))
            .sort("UNITS_ADDED"))

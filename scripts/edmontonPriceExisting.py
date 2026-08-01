# ==============================================================================
# edmontonPriceDivision.py   —   unified
# Tom Davidoff, July 30 2026
#
# 0. VERIFY   condos identifiable by zero/null lot; suites mostly land parcels
# 1. PRICE    neighbourhood median house price (2023, cleaned of nominal parcels)
# 2. DIVISION rowhouse share of RS permits (2024+) vs neighbourhood price
# 3. ELASTIC  price-sqft elasticity per nbhd, 2023 values x stable floor area (C2)
#             pooling houses + condos; run all / suite / no-suite
#
# Data note: historical panel has NO floor-area column, so sqft comes from the
# current roll joined by Account Number, restricted to parcels not redeveloped
# after 2023 (curr year_built <= 2023) so current sqft ~ 2023 sqft.
# ==============================================================================
import os
import polars as pl
import matplotlib.pyplot as plt
import numpy as np
import sys

DATA = os.path.expanduser("~/DropboxExternal/dataRaw/edmonton")
CURR = f"{DATA}/Property_Information_(Current_Calendar_Year)_20260612.csv"
HIST = f"{DATA}/Property_Assessment_Data_(Historical)_20260611.csv"
PERM = f"{DATA}/edmontonBuildingPermits.csv"
OUT_BETAS = os.path.expanduser("~/projects/infillPuzzle/data/nbhd_elasticities.csv")

PRICE_YEAR = 2023          # pre-reform assessment vintage
NEW_YEAR   = 2024          # reform / permit cohort
MIN_CONSTRUCTION = 2005 # want newish builds
MIN_N      = 30            # min parcels for a neighbourhood elasticity
MIN_PERM   = 5             # min RS permits for a neighbourhood division rate

def num(col, dtype=pl.Float64):
    return (pl.col(col).str.strip_chars().str.replace_all(r"[$,]", "")
            .cast(dtype, strict=False))

def nb_key(col):                       # normalise neighbourhood names for joins
    return pl.col(col).str.strip_chars().str.to_uppercase()

print("Loading (all text; casting only what we use)...")
perm = pl.read_csv(PERM, infer_schema_length=0)
hist = pl.read_csv(HIST, infer_schema_length=0).filter(pl.col("Suite").is_null()) # drop suites (we'll use current roll to identify them)
#print(hist["Suite"].value_counts(sort=True).head())
curr = pl.read_csv(CURR, infer_schema_length=0).filter(pl.col("zoning")=="RS").filter("House Number"!="null")  # drop non-RS, nominal, and suite parcels
# create year built and filter
curr = curr.with_columns(year_built = num("year_built", pl.Int64)).filter(pl.col("year_built").is_between(MIN_CONSTRUCTION, PRICE_YEAR))
def acct_key(col="Account Number"):
    return (pl.col(col).str.strip_chars()
            .str.replace(r"\.0$", ""))          # drop trailing .0 if present

print("hist keys:", hist["Account Number"].head(8).to_list())
print("curr keys:", curr["Account Number"].head(8).to_list())
print("hist n unique acct:", hist["Account Number"].n_unique())
print("curr n unique acct:", curr["Account Number"].n_unique())
# raw overlap, no normalization
hset = set(hist["Account Number"].to_list())
cset = set(curr["Account Number"].to_list())
print("overlap:", len(hset & cset))
hist = hist.with_columns(acct = acct_key())
curr = curr.with_columns(acct = acct_key())
print(hist["acct"].head())
print(curr["acct"].head())
print([hist.height,curr.height])
hist = hist.join(curr.select("acct","Total Gross Area"), on="acct", how="inner")
print([hist.height,curr.height])

# merge hist with current roll to get floor area for 2023 parcels (no floor area in hist)
hist = hist.with_columns(gross = num("Total Gross Area"), lot = num("Lot Size"))
# drop if null gross

print(hist.head)
print(hist.columns)

# summary statistics on lot size and floor area
print("\n" + "="*70 + "\nSUMMARY STATISTICS\n" + "="*70)
print(hist.select("lot", "gross").describe())

# get the 5 neighbourhoods with highest median price in 2023
hist = hist.filter(pl.col("Assessment Year") == str(PRICE_YEAR))
hist = hist.with_columns(lot = num("Lot Size"), assessed = num("Assessed Value"))
nbhdPrice = hist.filter((pl.col("Assessment Class 1") == "RESIDENTIAL") & (pl.col("lot") > 150) & (pl.col("assessed") > 100_000) & pl.col("assessed").is_not_null()).group_by("Neighbourhood").agg(price = pl.col("assessed").median(), n = pl.len()).filter(pl.col("n") >= 25).sort("price", descending=True)
print("\nTop 5 neighbourhoods by median assessed value (2023):")
print(nbhdPrice)

#matplotlib price vertical against gross scatter plot for the top 5 nbhds only
# --- prep (once) ---
hist = hist.filter(pl.col("Assessment Year") == str(PRICE_YEAR))
hist = hist.with_columns(lot=num("Lot Size"), assessed=num("Assessed Value"))

nbhdPrice = (
    hist
    .filter(
        (pl.col("Assessment Class 1") == "RESIDENTIAL")
        & (pl.col("lot") > 150)
        & (pl.col("assessed") > 100_000)
        & pl.col("assessed").is_not_null()
    )
    .group_by("Neighbourhood")
    .agg(price=pl.col("assessed").median(), n=pl.len())
    .filter(pl.col("n") >= 25)
    .sort("price", descending=True)
)

hist = hist.filter(pl.col("Neighbourhood").is_not_null())


def scatter_reg(nbhds, label, fname):
    sub = hist.join(nbhds, on="Neighbourhood", how="inner")
    q25, q75 = sub["lot"].quantile(0.25), sub["lot"].quantile(0.75)
    sub = sub.filter(pl.col("lot").is_between(q25, q75))

    x, y = sub["gross"], sub["assessed"]
    slope, intercept = np.polyfit(x, y, 1)

    fig, ax = plt.subplots(figsize=(10, 6))
    ax.scatter(x, y)
    ax.axline((0, intercept), slope=slope, color="red", linestyle="--",
              label=f"y = {slope:.2f}x + {intercept:.2f}")
    ax.set_title(f"Assessed Value vs Total Gross Area ({label}, 2023)")
    ax.set_xlim(left=0)
    ax.legend()
    fig.savefig(fname)
    plt.close(fig)


scatter_reg(nbhdPrice.head(10), "Top 10 Neighbourhoods", "text/assessed_vs_gross_top10.png")
scatter_reg(nbhdPrice.tail(10), "Bottom 10 Neighbourhoods", "text/assessed_vs_gross_bottom10.png")
# get the middle 20 neighbourhoods
sorted_nbhds = nbhdPrice.sort("price")
nNbhd = sorted_nbhds.height
midNbhd = nNbhd//2
scatter_reg(nbhdPrice.slice(midNbhd-10,midNbhd+10 ), "Middle 20 Neighbourhoods", "text/assessed_vs_gross_middle20.png")
sys.exit(0)  # stop here for now

# ==============================================================================
# 0. VERIFY - condo vs lot size; suite vs lot size; condo floor area present
# ==============================================================================
print("\n" + "="*70 + "\n0. CONDO / SUITE VERIFICATION\n" + "="*70)
c = curr.with_columns(
    lot   = num("lot_size"),
    gross = num("Total Gross Area"),
    has_suite = pl.col("Suite").is_not_null() & (pl.col("Suite").str.strip_chars() != ""),
)
n_tot = c.height
n_suite = c.filter(pl.col("has_suite")).height
print(f"parcels={n_tot:,} | with suite={n_suite:,} ({n_suite/n_tot*100:.1f}%)")
print("\nzero/null-lot (candidate condos) - is floor area present?")
print(c.filter((pl.col("lot") == 0) | pl.col("lot").is_null()).select(
    n=pl.len(), gross_pos=(pl.col("gross") > 0).sum(),
    median_gross=pl.col("gross").filter(pl.col("gross") > 0).median()))
print("\nsuited parcels - land or condo? (expect mostly land: lot>0)")
print(c.filter(pl.col("has_suite")).select(
    n=pl.len(), lot_pos=(pl.col("lot") > 0).sum(),
    lot_zeronull=((pl.col("lot") == 0) | pl.col("lot").is_null()).sum()))


# ==============================================================================
# 1. NEIGHBOURHOOD PRICE - 2023, real houses only (drop nominal/strata parcels)
# ==============================================================================
print("\n" + "="*70 + "\n1. NEIGHBOURHOOD PRICE (2023, houses)\n" + "="*70)
h = hist.with_columns(
    ayear    = num("Assessment Year", pl.Int64),
    assessed = num("Assessed Value"),
    lot      = num("Lot Size"),
    hist_yb  = num("Actual Year Built", pl.Int64),
    has_suite= pl.col("Suite").is_not_null() & (pl.col("Suite").str.strip_chars() != ""),
)
nbhd_price = (
    h.filter((pl.col("ayear") == PRICE_YEAR)
             & (pl.col("Assessment Class 1") == "RESIDENTIAL")
             & (pl.col("lot") > 150)              # real house lots, not strata slivers
             & (pl.col("assessed") > 100_000)     # drop nominal-value parcels
             & pl.col("assessed").is_not_null())
    .group_by("Neighbourhood")
    .agg(median_price = pl.col("assessed").median(),
         median_lot   = pl.col("lot").median(),
         n_houses     = pl.len())
    .filter(pl.col("n_houses") >= 25)
    .with_columns(nb = nb_key("Neighbourhood"))
)
print(f"neighbourhoods with >=25 clean houses: {nbhd_price.height}")
print("cheapest / priciest (median house assessed value):")
print(nbhd_price.sort("median_price").select("Neighbourhood","median_price","n_houses").head(4))
print(nbhd_price.sort("median_price", descending=True).select("Neighbourhood","median_price","n_houses").head(4))


# ==============================================================================
# 2. DIVISION RATE vs PRICE - rowhouse share of RS permits, 2024+
# ==============================================================================
print("\n" + "="*70 + "\n2. DIVISION RATE vs PRICE\n" + "="*70)
res = (
    perm.with_columns(y = num("YEAR", pl.Int64))
    .filter((pl.col("y") >= NEW_YEAR)
            & pl.col("ZONING").str.contains(r"\bRS\b")
            & pl.col("BUILDING_TYPE").is_in(["Row House (330)", "Single Detached House (110)"]))
    .with_columns(is_row = (pl.col("BUILDING_TYPE") == "Row House (330)").cast(pl.Int8))
)
nbhd_div = (
    res.group_by("NEIGHBOURHOOD")
    .agg(permits=pl.len(), rowhouses=pl.col("is_row").sum(), div_rate=pl.col("is_row").mean())
    .filter(pl.col("permits") >= MIN_PERM)
    .with_columns(nb = nb_key("NEIGHBOURHOOD"))
)
merged = nbhd_div.join(nbhd_price, on="nb", how="inner")
print(f"neighbourhoods with price AND >={MIN_PERM} RS permits: {merged.height}")
print("\ncorrelations (neighbourhood level):")
print(merged.select(
    corr_price_div = pl.corr("median_price", "div_rate"),
    corr_lot_div   = pl.corr("median_lot", "div_rate")))
print("\ndivision rate by price tercile:")
print(merged.with_columns(tier = pl.col("median_price").qcut(3, labels=["low","mid","high"]))
      .group_by("tier").agg(
          n_nbhd=pl.len(), div_rate=pl.col("div_rate").mean(),
          median_lot=pl.col("median_lot").median(), permits=pl.col("permits").sum())
      .sort("tier"))


# ==============================================================================
# 3. PRICE-SQFT ELASTICITY (C2): 2023 value x stable floor area, per neighbourhood
#    pool houses + condos; caveat: condo sqft may include some strata units
# ==============================================================================
print("\n" + "="*70 + "\n3. PRICE-SQFT ELASTICITY (2023 value, stable sqft)\n" + "="*70)

curr_area = curr.select("Account Number",
                        gross=num("Total Gross Area"),
                        curr_yb=num("year_built", pl.Int64))

base23 = h.filter((pl.col("ayear") == PRICE_YEAR)
                  & (pl.col("Assessment Class 1") == "RESIDENTIAL")
                  & (pl.col("assessed") > 0))
elas_base = (
    base23.join(curr_area, on="Account Number", how="inner")
    .filter((pl.col("gross") > 0)
            & (pl.col("hist_yb") <= PRICE_YEAR)     # structure predates pricing year
            & (pl.col("curr_yb") <= PRICE_YEAR))    # NOT redeveloped after -> sqft stable
    .with_columns(log_val=pl.col("assessed").log(), log_sqft=pl.col("gross").log())
)
retain = elas_base.height / base23.height * 100
print(f"2023 residential parcels: {base23.height:,}")
print(f"  with stable floor area (kept): {elas_base.height:,} ({retain:.1f}%)  "
      f"[dropped = redeveloped / renumbered / no-area]")

def beta_expr():
    lx = pl.col("log_sqft"); ly = pl.col("log_val")
    return (((lx - lx.mean()) * (ly - ly.mean())).sum() / ((lx - lx.mean())**2).sum())

def elasticity(df, label, suite=None):
    d = df if suite is None else df.filter(pl.col("has_suite") == suite)
    out = (d.group_by("Neighbourhood")
           .agg(n=pl.len(), beta=beta_expr(),
                median_price=pl.col("assessed").median(),
                pct_condo=(pl.col("lot").fill_null(0) == 0).mean())
           .filter(pl.col("n") >= MIN_N)
           .with_columns(nb=nb_key("Neighbourhood")))
    print(f"\n[{label}]  nbhds={out.height}")
    print(f"  beta  p10/p50/p90 = {out['beta'].quantile(.1):.3f} / "
          f"{out['beta'].median():.3f} / {out['beta'].quantile(.9):.3f}")
    print(f"  corr(beta, price) = {out.select(pl.corr('beta','median_price')).item():.3f}")
    return out

elas_all   = elasticity(elas_base, "ALL houses+condos")
elas_suite = elasticity(elas_base, "SUITE only", suite=True)
elas_nosu  = elasticity(elas_base, "NO SUITE",  suite=False)

# elasticity vs division
ed = elas_all.join(merged.select("nb", "div_rate"), on="nb", how="inner")
print(f"\nelasticity (all) vs division rate, {ed.height} nbhds: "
      f"corr = {ed.select(pl.corr('beta','div_rate')).item():.3f}")

# write per-neighbourhood betas + price + division for tomorrow's PyFixest
os.makedirs(os.path.dirname(OUT_BETAS), exist_ok=True)
(elas_all.select("Neighbourhood","nb","n","beta","median_price","pct_condo")
 .join(merged.select("nb","div_rate","permits","median_lot"), on="nb", how="left")
 .write_csv(OUT_BETAS))
print(f"\nwrote per-neighbourhood betas -> {OUT_BETAS}")


# ==============================================================================
# plotElasticity.py  -- price vs sqft, pooled 5 priciest nbhds, small lots
# points only, coloured by neighbourhood, log-log. One PNG.
# Assumes edmontonPriceDivision.py objects OR reloads standalone.
# ==============================================================================
import os
import polars as pl

DATA = os.path.expanduser("~/DropboxExternal/dataRaw/edmonton")
HIST = f"{DATA}/Property_Assessment_Data_(Historical)_20260611.csv"
CURR = f"{DATA}/Property_Information_(Current_Calendar_Year)_20260612.csv"
OUT  = os.path.expanduser("~/projects/infillPuzzle/figures/price_sqft_top5.png")

PRICE_YEAR = 2023
LOT_LO, LOT_HI = 300, 400          # lot-size band (m^2)

def num(col, dtype=pl.Float64):
    return (pl.col(col).str.strip_chars().str.replace_all(r"[$,]", "")
            .cast(dtype, strict=False))

hist = pl.read_csv(HIST, infer_schema_length=0)
curr = pl.read_csv(CURR, infer_schema_length=0)

# floor area from current roll, un-redeveloped only (sqft stable since 2023)
area = curr.select("Account Number",
                   gross=num("Total Gross Area"),
                   curr_yb=num("year_built", pl.Int64))

h = hist.with_columns(
    ayear=num("Assessment Year", pl.Int64),
    assessed=num("Assessed Value"),
    lot=num("Lot Size"),
    hist_yb=num("Actual Year Built", pl.Int64),
)

# 5 priciest neighbourhoods by 2023 median house price (clean houses)
top5 = (h.filter((pl.col("ayear")==PRICE_YEAR)
                 & (pl.col("Assessment Class 1")=="RESIDENTIAL")
                 & (pl.col("lot")>150) & (pl.col("assessed")>100_000))
        .group_by("Neighbourhood").agg(mp=pl.col("assessed").median(), n=pl.len())
        .filter(pl.col("n")>=25).sort("mp", descending=True)
        .head(5)["Neighbourhood"].to_list())
print("Top 5:", top5)

# pooled parcels: 2023 value x stable sqft, small-lot band, in the 5 nbhds
d = (h.filter((pl.col("ayear")==PRICE_YEAR)
              & (pl.col("Assessment Class 1")=="RESIDENTIAL")
              & (pl.col("assessed")>0)
              & pl.col("lot").is_between(LOT_LO, LOT_HI)
              & pl.col("Neighbourhood").is_in(top5))
     .join(area, on="Account Number", how="inner")
     .filter((pl.col("gross")>0) & (pl.col("hist_yb")<=PRICE_YEAR)
             & (pl.col("curr_yb")<=PRICE_YEAR)))
print(f"pooled parcels ({LOT_LO}-{LOT_HI} m2 lots): {d.height}")
print(d.group_by("Neighbourhood").agg(n=pl.len()).sort("n", descending=True))

# get housing stats (sqft) by nbhd
print(
    h.filter((pl.col("ayear")==PRICE_YEAR)
             & (pl.col("Assessment Class 1")=="RESIDENTIAL")
             & (pl.col("assessed")>100_000))
             #& pl.col("Neighbourhood").is_in(top5))
    .group_by("Neighbourhood")
    .agg(n=pl.len(),
         p10=pl.col("lot").quantile(.10),
         median=pl.col("lot").median(),
         p90=pl.col("lot").quantile(.90),
         mean=pl.col("lot").mean())
    .sort("median", descending=True)
)

# ---- plot: log-log scatter, coloured by neighbourhood ----
fig, ax = plt.subplots(figsize=(9, 7))
colors = plt.cm.tab10.colors
for i, nb in enumerate(top5):
    sub = d.filter(pl.col("Neighbourhood")==nb)
    ax.scatter(sub["gross"].to_numpy(), sub["assessed"].to_numpy(),
               s=22, alpha=0.6, color=colors[i], label=f"{nb} (n={sub.height})")

#ax.set_xscale("log"); ax.set_yscale("log")
ax.set_xlabel("Building floor area  (m$^2$, log)")
ax.set_ylabel("Assessed value 2023  ($, log)")
ax.set_title(f"Price vs floor area — 5 priciest neighbourhoods, {LOT_LO}-{LOT_HI} m$^2$ lots")
ax.legend(fontsize=8, framealpha=0.9)
ax.grid(True, which="both", alpha=0.2)
fig.tight_layout()
os.makedirs(os.path.dirname(OUT), exist_ok=True)
fig.savefig(OUT, dpi=140)
print(f"wrote {OUT}")

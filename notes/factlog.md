# Fact log

## 2026-11-15 — duplexStock2018 (text/duplexStock2018.tex) — OK

- Script: `scripts/duplexStock2018.R` (log `scripts/duplexStock2018.out`). Source: BCA REVD19 roll (2018 condition), COV jurisdiction 200.
- `\duplexBldgStockEighteen` = 3,152 duplex buildings (roll stems floor(roll/1000)); `\duplexFolioStockEighteen` = 5,292 folios with use "Duplex, …".
- Denominator for shares: 3,088 buildings with 2018 zoning (64 = 2.0% missing zoning, dropped from shares).
- `\duplexZoningCodesEighteen` = 57 codes; `\duplexTopFiveZoneShareEighteen` = 68% in RT2, RT5, RT8, RT7, RM4; `\duplexRTShareEighteen` = 81% RT; `\duplexRSShareEighteen` = 5% RS (151 bldgs); RM 12%, CD 1%.
- Checks: folio/rollNumber/inventory key uniqueness; folio→description merge lost 0%; duplex→inventory merge row count preserved; zoning missing 1.3% of folios. Stem structure: 2,138 strata stems with 2 folios, 29 strata stems with 1 duplex folio (partner coded "Section 19(8)" or SFD), 983 single non-strata, 2 stems with two non-strata folios (possible undercount of 2). 11 stems with conflicting zoning across folios (modal code used).
- Excluded: 37 "2 Acres Or More (Single Family Dwelling, Duplex)" folios (not clearly duplexes).
- Not done: no published City total of duplex buildings to sanity-check against (Census "duplex" counts include house-with-suite, not comparable). Not split by tract/neighbourhood/East-West — descriptive count by zoning only.
- Surprising: 151 duplexes already in RS zones before the 2018 reform (legacy/non-conforming or CD-adjacent coding).

## 2026-11-15 — institutionalFacts (text/institutionalFacts.tex) — CHECK

- Script: `scripts/institutionalFacts.R`. Values transcribed with source keys in `text/CLAUDE.bib`; m→ft conversions computed.
- Why CHECK: vancouver.ca and council.vancouver.ca block automated retrieval (Cloudflare 403). Only the April 2023 RS-1 district schedule was read in full (third-party mirror). Everything else comes from web-search summaries of City documents; spot-check against the PDFs before submission (bib entries marked "verify"). Save them to `data/policyDocs/` (<200 KB) or `~/DropboxExternal/dataRaw/policyDocs/`. RS-1 Apr 2023: PDF in Dropbox, text extract `data/policyDocs/rs1DistrictSchedule2023Apr.txt`.
- Values:
  - `\suitesAllRSYear` 2004 (council extended suites city-wide, April 2004).
  - `\suitesIllegalShareTwoThousandFour` 78% of suites in suite-eligible areas unauthorized (2004 report); `\suitesPermittedShareTwentyNineteen` 27% of `\suitesTotalTwentyNineteen` 34,593 (Sept 2019 memo, which says stock data are incomplete).
  - `\lanewayFirstYear` 2009, RS-1 & RS-5 = `\lanewayFirstLotShare` 94% of SF lots; `\lanewayAllRSYear` 2013 (June 11, 2013).
  - `\duplexCouncilMonth` September (public hearing Sept 18–19, 2018, 7–4 vote); `\duplexEnactedDate` Oct 30, 2018. Duplexes approved in ~99% of RS, not all — draft text changed "all" → "nearly all".
  - Laneway not permitted with duplex: confirmed (Dec 2018 admin report, per search summary).
  - RS-1 (Apr 2023, read directly): duplex 0.70 (s. 3.1.1.1); single detached 0.60, rising to 0.70 if above-grade ≤ 0.45 etc. (s. 3.2.1.1(c)).
  - R1-1: Council Sept 14, 2023; effective Oct 17, 2023. Single detached 0.60, duplex 0.70 (s. 3.2.1.1, **June 2026 consolidation** — Oct 2023 text not checked). "No conditional increase to 0.70" rests on that consolidation.
  - RT zones: not folded into R1-1; multiplexes extended to RT-7, RT-9, FSD in June 2024 (MountainMath).
  - Multiplex: 3–4 units at ≥10.0 m frontage & 306 m²; 5 units at 464 m² / 13.4 m (omitted from text); 6 strata / 8 rental at ≥15.1 m & 557 m². Base FSR 0.70, up to 1.00 with secured rental / below-market / amenity contribution; secondary sources say By-law 14747 (June 2026) made 1.00 outright. Your guess "1.0 in all cases" is not right for 2023–26.
  - "Anything over 100 feet deep": confirmed — 306 m² on a 33-ft lot needs ~100 ft depth (`\rOneFourUnitDepthThirtyThreeFt`).
- Supports draft: 2023 reform did lower the single-family max (0.70 conditional → 0.60) while duplex stayed at 0.70, so F(2) > F(1) after 2023.

## 2026-11-15 — text notes (no macro)

- "type 1 extreme value?": yes, i.i.d. T1EV errors give IIA, so the duplex/SF odds don't depend on the multiplex option. Removed the `\fact`.
- Parallel trends: replaced the note with one sentence stating the assumption. No pre-trend test run yet (the regression isn't specified).
- `\eqref{eq:profit}` points to a label that doesn't exist; left for Tom.
- Fixed `\addbibresource{text/infillPuzzle}` → `.bib`; biber couldn't find the file, so every citation was undefined.

## 2026-11-15 — multiplex share on 33-ft lots, 2024+ (not in paper; no macro) — CHECK

- Script: `scripts/multiplexLotWidth.R` (log `.out`). Permits refreshed 2026-11-15 (latest issue 2026-11-13). Lot width from 2018 SFD lots in R1-1 (BCA 2019 roll, `bca19VancouverSingles.rds`).
- Linking permit to 2018 lot, in priority order: (a) nearest 2026 BCA folio centroid ≤5 m → rollStart (Tom's method; built `dataProcessed/bca26FolioGeometryVancouver.rds` from the 2026-01-09 gpkg in the format `vancouverR1FSRDate.R` expects); (b) 2026 BCA civic address (number + street, nearest candidate ≤60 m, since BCA has no E/W) → rollStart; (c) 2019 address; (d) ≤5 m to 2018 centroid. Where methods overlap, agreement is 99.9–100% (N≈1,070).
- Multiplex definition (revised): "Multiple Dwelling" category alone includes apartments, townhouses, mixed-use and pre-R1-1 projects. Now multiplex = Multiple Dwelling + description says multiplex / "two-units front/rear building" + applied ≥ 2023-10-17: 615 of 798 Multiple Dwelling permits (609 geocoded). The other 182 are excluded ("otherMultiple"); only 11 of them sit on 2018 SFD lots.
- Street-side geocodes: most unlinked multiplex permits were 5–10 m outside any lot (on the street). Match 4 = nearest 2018 SFD lot polygon within 30 m whose 2019/2026 address is on the permit's street. Validation vs 2026-address links for off-centroid permits: 86% agree (N=139), 96% when the nearest lot is >2 m closer than the next.
- Result: 43.4% of 2024+ multiplex lots are 33-ft (152 / 350 lots; 573 of 609 multiplex permits linked = 94%). Without street-nearest links 41.3% (N=264); street-nearest only when gap >2 m: 40.7% (N=312). Street-nearest links alone: 50% (N=86). By year: 2024 54% (26), 2025 36% (176), 2026 50% (148). Baseline: 53.7% of 2018 R1-1 SFD lots are 33s.
- Multiplex share of new builds by width: 33s 23.9% (152/637); 37–41 33%; 42–50 30%; >50 32%.
- Why CHECK: 14% of street-nearest links are probably the wrong lot (mostly a neighbour on the same street). "Latest permit per lot" drops 17.5% of linked permits (multi-permit projects plus some street-nearest collisions).
- Bug found and fixed in diagnostics (not in the main script): spatial lookups after merge() used an sf object in the pre-merge row order. Earlier claims from those diagnostics (nearest polygon "0% agreement", new-stem bridging, "62 apartments in R1-1") were artifacts; corrected: only 19 of 182 non-multiplex Multiple Dwelling permits are in R1-1.
- Dead ends: City parcel polygons (2026) omit most redeveloped lots.

## 2026-11-15 — lotShareThirtyThreeRone, mplxShareThirtyThree / mplxShareWider / mplxShareGap (text/multiplexLotWidth.tex) — CHECK

- Script: `scripts/multiplexLotWidth.R`.
- `\lotShareThirtyThreeRone` = 44%: 2018 SFD lots in R1-1 (BCA 2019, width > 0, N = 58,004) with round(width) == 33. A looser 30–36 ft bin gives 54%, so the number depends on the definition.
- `\mplxShareThirtyThree` 23% (123/531) vs `\mplxShareWider` 32% (219/686, round(width) > 33): multiplex share of single/duplex/multiplex new-build permits issued 2024+, one (latest) permit per 2018 lot; gap 9 pp.
- Why CHECK: inherits the permit-to-lot linking caveats above (14% of street-side links probably the wrong lot).

## 2026-11-15 — ppsf33 (text/ppsf33.tex, text/ppsf33Tract.png, text/ppsf33Neighbourhood.png) — CHECK

- Script: `scripts/vancouverPPSF33.R`. Eq. ppsfHedonic: log(price / MB_total_finished_area) on sale-year FE + log(effective year) + area FE, with and without the effective-year term; heteroskedasticity-robust SEs. Area FE demeaned for plots; estimates also saved to `dataProcessed/ppsf33{Tract,Neighbourhood}.csv`.
- Sample: BCA 2019 sales, COV, 'Single Family Dwelling' / 'Residential Dwelling with Suite', 'Improved Single Property Transaction'. Drops: multi-folio conveyances (2), years outside 2012–2019 (83%), invalid fields (0.2%), sale before year built (15.3%; the 2019 inventory describes a later house), width not rounding to 33 (58%), no 2026 centroid (0.8%), not now R1-1 (14.5%), log ppsf outside 0.5–99.5 pctile (1.0%). Final 6,513 sales; median $675/sqft, 2,311 sqft, effective year 1994.
- Areas with ≥ 20 sales: 56 of 66 tracts (6,432 sales, 98.8%); 20 of 23 BCA neighbourhoods (6,508 sales).
- Log effective year coefficient 2.13 (SE 0.30) tract, 2.15 nbhd, i.e. about 0.1% per year. Within R² 0.01. With- vs without-control FE correlate 0.9999 (tract) and 0.9999 (nbhd), so the figures lie on the 45° line.
- West (lon < −123.101) minus East mean FE: +54% tract, +57% nbhd; cor(FE, −longitude) 0.93 / 0.94. Neighbourhood FE vs sales-weighted mean tract FE: cor 0.99 (19 nbhds).
- Why CHECK: (1) the draft said 2012–2021, but the 2019 extract ends in 2019, so the text now uses the macros. (2) The effective-year control is nearly irrelevant as specified (log of a calendar year ≈ linear year). Log age or log sqft would be a stronger control; not run, because it isn't the draft's specification. (3) No published City ppsf to sanity-check against.

## 2026-11-15 — ppsf33 revised: sales end before duplex approval — CHECK

- Per Tom: keep the 2019 extract, but don't overlap the duplex period. Sales now 2012 to 2018-09-17 (council approval 2018-09-18, the same cutoff as `D_REFORM` in vancouverPermitLogit.R). The tract plot has no labels.
- 5,822 sales (median $660/sqft); 5,750 in 56 tracts, 5,818 in 20 neighbourhoods. Log effective year 1.89 (SE 0.32) tract, 1.91 nbhd. With/without-control correlation still 1.00. West premium 56% tract, 59% nbhd; tract–nbhd cor 0.99.
- Other CHECK points as in the previous entry.

## 2026-11-15 — permit definitions; newBuildValueThreshold; mplxShare{East,West}{ThirtyThree,Fifty} (text/multiplexLotWidth.tex) — CHECK

- Script: `scripts/multiplexLotWidth.R` now links all permits issued 2017+ and saves `dataProcessed/permitLotLinks.rds`.
- Products per `vancouverPermitLogit.R`: drop categories containing infill/laneway (mainly "Infill Single Detached House", 176); single = single detached / one-family; duplex = duplex; multiple dwelling = multiplex only if the description says multiplex / "two-units front/rear" and it was applied ≥ 2023-10-17. Previously my rule used grepl("Single"), which wrongly counted infill single detached houses.
- New build (per Tom): typeofwork "New Building", or any other permit type with value ≥ `\newBuildValueThreshold` $340,000 = q0.95 of value among non-new-build principal permits (`\newBuildValueQuantile`). Adds 1,147 Addition/Alteration, 31 Demolition, 3 Temporary. Post-reform it adds 69 single-family lots (large renovations) and 1 duplex. Tom's older getRelevantPermits.R used $125k (q0.75 of alterations), which would mostly add ordinary renovations.
- Post-reform (applied ≥ 2023-10-17), latest permit per lot, share multiplex of single+duplex+multiplex: East 33-ft 33% (N=262), East 50-ft 41% (N=32), West 33-ft 18% (N=198), West 50-ft 32% (N=104). 50 = round(width) == 50; East/West by lot longitude vs Ontario St (−123.101).
- The earlier macros moved with the new definitions: mplxShareThirtyThree 22 (550), mplxShareWider 30 (732), gap 8.
- Why CHECK: East 50-ft N is small (32). Linking caveats as above.

## 2026-11-15 — permitTrends (text/permitTrends.png, text/permitTrends.tex) — CHECK

- Script: `scripts/vancouverPermitMap.R`. 33-ft lots only (round(width) == 33), plotted at the 2018 lot centroid, latest permit per lot within each era. Duplex era: applied 2018-09-18 to 2023-10-16 (1,068 lots). R1-1 era: applied ≥ 2023-10-17 (460 lots). Tiles: keyless OSM, faded (cartolight needs an API key under rosm 0.3.2).
- Duplex share of single+duplex: East 44% → 70%, West 21% → 66%. This supports the draft's claim that West duplex take-up rose relative to East.
- Why CHECK: R1-1-era permits are right-censored (applied but not yet issued by Nov 2026 are missing), and single-family counts are low in that era (52 East, 55 West).

## 2026-11-15 — new-build cutoff sensitivity (multiplexLotWidth.R; no new macros) — OK

- Main cutoff A = $340,000 (q95 of value among non-new-build principal permits). Alternatives: B = $1,200,000 (q95 of all principal permits, new builds included); C = $706,637 (q33 of new-build single + duplex values). Every principal permit is linked; the rule is applied afterwards; `newBuild` (rule A) is saved in permitLotLinks.rds and used by the map.
- Non-new-build permits counted (linked): A 294, B 28, C 82.
- Post-reform multiplex share, E33 / E50 / W33 / W50: A .332/.406/.182/.317; B .339/.406/.193/.347; C .336/.406/.188/.340.
- 2024+ multiplex share, 33 vs wider: A .224/.299; B .233/.316; C .229/.311.
- Map duplex share of single+duplex on 33s, East and West, duplex era → R1-1 era: A .44→.70 / .21→.66; B .45→.72 / .22→.71; C .45→.72 / .22→.69.
- Conclusions are not sensitive to the cutoff.

## 2026-11-15 — elasPremium; tables elasPpsfCor, elasPpsfDupCor (text/) — CHECK

- Script: `scripts/vancouverElasticityPremium.R`. Mirrors `vancouverPriceElasticity.R`: the elasticity is an area-specific slope of log price on log sqft, and the duplex premium is DUP − SFD area×product FE in a pooled log ppsf hedonic. Controls follow eq. ppsfHedonic (sale-year FE, log effective year). Same SFD sample as ppsf33 (now saved by vancouverPPSF33.R; area FE cor 1.0000 with ppsf33Tract.csv).
- Validation: simulated 40 areas with known slopes; cor(true, est) 0.977, MAE 0.042.
- Elasticity: mean 0.38 (tract, 56) / 0.36 (nbhd, 20); median SE 0.095 / 0.056. cor(lppsf, elas): tract 0.57 (West 0.73 N=17, East 0.10 N=39); nbhd 0.66 (West 0.82 N=9, East 0.46 N=11).
- Duplexes: strata duplex units only (non-strata records carry one unit's sqft at building price, avg $1,310/sqft vs ~$760–920 strata; 151 sales dropped). Any zone: 64% RT, R3-2 315, RM-4 64, R1-1 43. RT ∩ R1-1 polygon overlap 0.3 m² (slivers), so the zones are distinct. 1,658 duplex sales after filters; median $733/sqft, 1,428 sqft.
- Areas with ≥10 duplex and ≥20 SFD sales: 15 tracts (409 duplex sales; 3 West), 10 nbhds (1,318). Mean premium +13% tract, +9% nbhd.
- Correlations, tract: premium vs lppsf −0.80, vs elas −0.70, lppsf vs elas 0.54. Nbhd: −0.10, +0.34, 0.45.
- Why CHECK: tract and neighbourhood disagree. The tract result is mainly East vs 3 West tracts (East premiums ~+0.05 to +0.26, West −0.05 to +0.03). At neighbourhood level, Fraserview (20 SFD sales, elasticity 0.07, premium −0.15) and Kitsilano (+0.09) drive the weak correlations. Very few areas either way. Also, 409 of 1,658 duplex sales are in qualifying tracts, because duplexes sit in RT-heavy tracts with few 33-ft R1 sales.
- Also: multi-folio conveyances are now dropped against the whole sales table (no change to either sample).

## 2026-11-15 — elasPpsfDupCor: West Side indicator added — CHECK

- The table now includes a West Side dummy (lot longitude < −123.101). Tracts (15, 3 West): West vs lppsf 0.98, vs elas 0.48, vs premium −0.78. Nbhds (10, 2 West: Kitsilano, Marpole): 0.95, 0.37, −0.24. The tract premium correlation is essentially East vs West. Neighbourhoods: 10 have ≥10 duplex sales (8 have ≥45), but only 2 are West.

## 2026-11-16 — shareDiD (text/shareDiD.tex, text/shareDiDTable.tex; tab:ShareDiD) — CHECK

- Script: `scripts/permitShareDiD.R`. Eq. ShareDiD: duplex share of single+duplex new builds (rule A) on 33-ft 2018 SFD lots now R1-1, latest permit per lot × era (the same sample as the permitTrends map: 1,068 duplex era + 337 R1-1 era = 1,405). Cells = area × 12-month application period relative to 2023-10-17 (8 periods; the Sept 18 – Oct 16, 2018 fragment is merged into the first). ppsf_j = ppsf33 withEff FE. Area + period FE, weighted by permits per cell (equivalent to a permit-level LPM). Clustered by area; wild cluster restricted bootstrap p-values with Webb weights and 9,999 draws, computed by FWL demeaning (the coefficient matches feols to 1e-8).
- Validation: 1,500 simulated 20-cluster panels with β=0: WCR rejects 6.3% at 5%, CR1 clustered rejects 8.4%; with β=0.5, mean estimate 0.505.
- Tracts: β = 0.452 (SE 0.098, WCR p<0.001), 56 tracts, 1,389 permits (1.1% lost: tract has no ppsf33). Nbhd: 0.418 (0.098, WCR p<0.001), 20 nbhds, 1,404 permits.
- + ppsf × linear period trend: tract 0.430 (0.200, p=0.038), nbhd 0.474 (0.279, WCR p=0.155); trend coefficients ≈0 (0.006, −0.015).
- ±6 months: tract 0.337 (0.342, p=0.375; 29 identifying tracts, 128 permits); nbhd 0.200 (0.380, p=0.610; 16 nbhds, 161 permits). About half the window permits are in singleton areas.
- Unweighted cells: tract 0.509 (0.117), nbhd 0.589 (0.144).
- Cutoff sensitivity (base spec, tract / nbhd): A 0.452/0.418; B 0.469/0.458; C 0.448/0.442.
- West−East mean log ppsf gap: 0.41 tract, 0.38 nbhd → implied relative West increase of 19 / 16 pp.
- East/West 2×2, no controls: East 44.0% (N=704) → 70.3% (175); West 20.6% (364) → 66.0% (162). DiD +19.2 pp (SE 5.8, z 3.30, p 0.001); ratio of odds ratios 2.49, Woolf χ²(1) 10.78, p 0.001. DiD under B/C cutoffs: 21.5 / 20.6 pp.
- Duplex share by period: −5 .12, −4 .33, −3 .42, −2 .47, −1 .38, 0 .74, 1 .64, 2 .64. The dip in the last pre-reform year could reflect applicants rushing single-family permits ahead of the FSR cut; this would also affect the ±6-month window.
- Why CHECK: (1) the narrow-window estimates are positive but insignificant (few permits; possible anticipation). (2) The neighbourhood trend column is not significant. (3) The last period is right-censored (applied through 2026-09-08; unissued permits are missing). (4) The draft's "convergence in pricing" motivation for the trend term was not tested here; the trend coefficients are ≈0. (5) Inherits the permit-linking caveats.

## 2026-11-16 — shareDiD period length (text/shareDiDPeriods.tex; tab:ShareDiDPeriods; sdMonth*/sdYear*/sd*Range* macros) — OK

- Script: `scripts/permitShareDiD.R`. Periods are now `perOf(applied, L)` = L-month bins from 2023-10-17 (a first partial bin shorter than half a period joins the next). L = 12 reproduces the previous periods exactly; Table ShareDiD is unchanged.
- Weighted cell OLS = permit-level LPM with area + period FE for any L, so thin cells don't bias it. Tract β 0.450–0.464, SE 0.098–0.112; nbhd β 0.410–0.434, SE 0.098–0.106. Annual periods have the lowest SE and MDE (2.8 SE: 0.274 tract, 0.276 nbhd). Median permits per cell: tract 1 (monthly) to 3 (annual); nbhd 1 to 7. No tract-month cell has ≥5 permits.
- Unweighted cells are sensitive to L (tract 0.457–0.615), as expected.
- The share sample is saved to `dataProcessed/shareDiDPermits.rds` for permitLogitDiD.R.

## 2026-11-16 — logitDiD (text/logitDiD.tex, text/logitDiDTable.tex; tab:LogitDiD, eq:LogitDiD) — CHECK

- Script: `scripts/permitLogitDiD.R`. Permit level, same sample (1,389 tract / 1,404 nbhd permits), 12-month period FE, clustered by area.
- LPM: area FE 0.452 / 0.418 (same as the cell estimates; permit-level SEs differ in the third decimal from the small-sample adjustment). ppsf control instead of area FE: 0.458 (0.092) / 0.407 (0.096); WCR p ≤ 0.001.
- Logit, area FE: 2.387 (0.553) tract, 2.265 (0.506) nbhd (1 nbhd dropped, all one product; N = 1,400). ppsf control: 2.405 (0.440) / 2.171 (0.458); ppsf level −2.59 / −2.45. Conditional logit (exact, model-based SE; robust variance isn't available with exact): 2.305 (0.608) / 2.210 (0.590). Odds ratio at the W−E gap: 2.66 / 2.35, vs 2.49 for the unconditional 2×2.
- Monte Carlo (1,000 draws, actual design, area/period effects from the FE logit, true β = clogit estimate): FE logit mean 2.381 vs 2.305 (+3%) tract and 2.274 vs 2.210 (+3%) nbhd; clogit 2.300 / 2.217 (unbiased); ppsf-control logit 2.316 / 2.129 (−4% nbhd, attenuated by omitted area heterogeneity). MC SD of the FE logit is 0.655 / 0.600, vs clustered SEs of 0.553 / 0.506.
- Why CHECK: the clustered logit SEs understate sampling variability by about 15–20% on this design; there's no wild bootstrap for the logit (a score bootstrap would be the next step). Inherits the ShareDiD caveats (right-censored last period, linking).
- Refs added to CLAUDE.bib: NeymanScott1948, Chamberlain1980, Greene2004, HahnNewey2004, FernandezValWeidner2016, Katz2001, AiNorton2003, Puhani2012.

## 2026-11-17 — typeofwork restriction; bootstrap off (all permit macros rerun) — CHECK

- `multiplexLotWidth.R` now keeps only New Building and Addition / Alteration (drops Demolition 7,198, Salvage and Abatement 6,751, Temporary 502, Outdoor 62; 52,683 → 38,170, 27.5% lost). Cutoffs are saved to `dataProcessed/permitNewBuildCutoffs.rds` and read by `permitShareDiD.R` (no more hard-coded values).
- The main cutoff A (q95 of non-new-build principal permits) rises from $340,000 to $660,000: before, about 12.8k near-zero Demolition and Salvage permits pulled the quantile down. Before, 31 demolition permits ≥ $340k counted as "new builds". B: $1.2M → $1.61M. C is unchanged ($706,637). Non-new-build permits added to the new-build sample: A 294 → 90, B 28 → 15, C 82 → 81. A and C now give near-identical results.
- ShareDiD base: tract 0.452 → 0.449 (SE 0.098, clustered p < 0.001), nbhd 0.418 → 0.443 (0.102, p < 0.001). Trend: 0.405 (p = 0.048) / 0.476 (p = 0.106). Window: 0.223 (p = 0.53) / 0.158 (p = 0.69). Permits 1,389 → 1,343 (tract). E/W 2×2 DiD 19 → 21 pp, OR 2.49 → 2.59. Map shares E 45→72, W 22→69.
- LogitDiD: FE logit 2.311 (0.569) / 2.356 (0.558); clogit 2.228 / 2.294; MC FE-logit bias +5% / +4%.
- multiplex shares: 33 vs wider 23% / 31%; E33 .338, W33 .188, W50 .340.
- Bootstrap off (`BOOT <- FALSE` in permitShareDiD.R and permitLogitDiD.R): bootstrap table rows dropped; macros sdTractP, sdNbhdP, sd*TrendP, sd*WinP, lp*CtlP, sdNBoot print \fact{bootstrap off}. The draft's prose and captions still describe WCR p-values. Added \usepackage{xcolor} to main.tex (\fact needs \textcolor and had never been used).
- Why CHECK: the prose describing the bootstrap needs a decision (see the bootstrap note in the session). The logit Monte Carlo (not a bootstrap) is still on, and takes about 4.5 of the 7 minutes.

## 2026-11-17 — Addition / Alteration classification (scripts/vancouverPermitTypeOfWork.R; no macros) — CHECK

- Residential (propertyuse has Dwelling) Addition / Alteration permits: 12,302. Classes by description regex, exclusive in order: conversion that adds units 205 (1.7%; median $750k; East 103 / West 95; 131 typed Multiple Conversion Dwelling); added secondary suite 1,191 (9.7%); high value ≥ cutoff A 586 (4.8%; mostly envelope, roof and window jobs on apartment buildings, some rebuilds of houses); other 10,320 (84%).
- Saved to `dataProcessed/permitAddAltClass.rds`. Not used in any estimate yet.
- Why CHECK: the regexes were spot-checked on samples only. The suite class still includes some permits that only repair a house that already has a suite.

## 2026-11-20 — Wild bootstrap back on; Conley SEs in logitDiD (lgConley*, lp*FEConley, lg*FEConley*) — CHECK

- `BOOT <- TRUE` in permitShareDiD.R and permitLogitDiD.R (9,999 Webb draws); every \fact{bootstrap off} marker is resolved. WCR validation (1,500 null panels): 6.3% rejection at 5% vs 8.4% for CR1. ShareDiD p: base <0.001 / <0.001; trend 0.049 / 0.152; window 0.557 / 0.685. LogitDiD LPM, all four columns p < 0.001.
- Conley (1999) SEs for postX in the LPM and logit (panels A, B of tab:LogitDiD, in brackets; 2 km). Bartlett kernel in lot-to-lot UTM distance; own sandwich from the fixest scores and bread. fixest's `vcov_conley` uses a uniform kernel: it agrees with my uniform version, but it is not PSD at 5–8 km and its SE falls to 0.36 (logit) there, so it isn't used. Check: with K = same-area indicator, the function reproduces fixest's CR0 cluster SE to 1e-8 for all four models.
- Tract SEs (cluster / Bartlett 1, 2, 3, 5 km): LPM-FE 0.097 / 0.102 0.105 0.102 0.097; logit-FE 0.569 / 0.593 0.600 0.568 0.537; logit-ctl 0.463 / 0.513 0.506 0.484 0.508. Nbhd logit-FE 0.558 / 0.571 0.579 0.539 0.492. Conley at 1–3 km is 0–10% above clustered; inference is unchanged.
- Monte Carlo (400 draws, actual tract design and coordinates, LPM, rejection at 5%): iid errors: cluster 9%, Conley 1/2/3/5 km 9/11/13/17%. 0.3 km exponential field: iid 39%, cluster 12%, Conley 2 km 11%. 1 km field: cluster 37%, Conley 2 km 32%.
- Why CHECK: (1) Conley isn't a fix for few effective clusters. postX varies only at area × post, so long cutoffs over-reject even with iid errors, and if the errors are correlated across tracts, neither method reaches nominal size. (2) The 2 km cutoff is a judgment call; the table caption names it and the .out shows 1–5 km. (3) There's still no bootstrap for the logit (score bootstrap is the candidate).
- Caption of tab:LogitDiD: one sentence added for the bracketed SEs; Conley1999 added to CLAUDE.bib. No other prose changed.

## 2026-11-20 — ewPreYear, ewDup{East,West}{PreYear,Post} (text/shareDiD.tex) — OK

- Script: `scripts/permitShareDiD.R`, same sample as the E/W 2×2 (latest new-build permit per 33-ft lot × era, single + duplex). Duplex share by application year, 2022: East 58% (N=144), West 34% (N=59). R1-1 era (applied ≥ 2023-10-17): East 72% (172), West 69% (155). These match the numbers Tom had typed into the intro and the map macros dupVsSingle*RoneEra. The intro now uses the macros.

## 2026-11-20 — sqftPrePost (text/sqftPrePost.tex, text/sqftPrePostTable.tex; tab:sqftPrePost) — CHECK

- Script: `scripts/newBuildFSRPrePost.R`. Realized FSR = 2026 BCA finished floor area (sum over 2026 folios on the rollStart; single-folio duplex with a duplex use ×2) / 2018 lot area, on the 1,357 shareDiD permits.
- Flow: not the lot's latest principal permit −17 (1.3%); a 2026 folio without area −27 (2.0%); building not complete on the 2026 roll (max year built < max(2019, issue year)) −277 (21.1%); single-folio duplex with unknown unit count −6. In the table: 1,030 (856 duplex era, 174 R1-1 era). Coverage: duplex era 83%, R1-1 era 53% (issued 2024 89%, 2025 62%, 2026 0%).
- ×2 check (duplex-era duplexes, median FSR): multi-folio 0.725, single-folio ×2 0.715.
- Mean (SD, median): duplex era single 0.70 (0.05, 0.71) N=549, duplex 0.73 (0.05, 0.72) N=307. R1-1 era single 0.64 (0.06, 0.61) N=51, duplex 0.75 (0.05, 0.76) N=123. Duplex − single: 0.033 → 0.113. E/W within product are similar.
- Why CHECK: (1) The R1-1 era coverage is only 53% and skewed to early, fast builds. (2) R1-1 era singles are a mix: the median is 0.61 but p90 is 0.71, so some built at the old FSR (transition or vesting). (3) Duplexes were already 0.03 above singles in the duplex era. (4) FSR uses the 2018 lot area; it doesn't reflect lot consolidations or subdivisions.
- Added BCAssessment2026 to CLAUDE.bib.

## 2026-11-20 — shareDiDElasTable (tab:shareDiDElasTable; exploratory) — CHECK

- Script: `scripts/permitShareDiDElas.R`. Tract × 12-month cells as in permitShareDiD.R column (1), weighted by permits, period FE, no tract FE, SEs clustered by tract, WCR bootstrap p (9,999 Webb draws). Regressors: log ppsf and size elasticity (elasPremiumTract.csv) and their post-2023 interactions, each alone and together. 56 tracts, 368 cells, 1,343 permits (1,357 → 1,343, lost 1.0%: tract has no ppsf).
- Final spec (after the hedonic respec below): together, log ppsf −0.439 (0.100), elasticity −0.180 (0.183, p 0.36); post × ppsf 0.420 (0.129, p 0.006), post × elasticity 0.158 (0.209, p 0.46). Alone: post × ppsf 0.463, post × elasticity 0.516 (p 0.010).
- Why CHECK: elasticity is noisy (reliability ≈ 0.54 from its own SEs; correlation with ppsf 0.48), so measurement error biases the horse race toward ppsf. Tom wrote the paragraph and \input in main.tex.

## 2026-11-20 — Hedonic respec: sale-year + effective-year FE (ppsf33*, elasPremium*, elasPpsfCor, elasPpsfDupCor, ppsfEffBin*, ppsfAgeNonPos*; downstream shareDiD*, logitDiD*, sqftPrePost*) — CHECK

- eq. ppsfHedonic in vancouverPPSF33.R and vancouverElasticityPremium.R: log ppsf (and log price, for elasticity) on sale-year FE + effective-year FE (effective years ≤ 1965, the 10th percentile, pooled: 11% of sales; 54 bins, 2 with < 10 sales) + area FE. The log effective year control is gone; the \ppsfEffCoef* macros are removed (they were not used in the text).
- Sample: drop sales with effective year (2019 roll) > sale year, 118 of 5,882 (2.0%); duplexes 1,676 → 1,669. Keep new homes (effective year = sale year). Tract sample 5,750 → 5,638; neighbourhoods still 20.
- Tried and dropped (Tom): log effective age, log(1 + age), year-built FE. In-sample cor(ppsf, elasticity) for tract / nbhd: old 0.57 / 0.66; log age, age > 0: 0.38 / 0.27; this spec 0.48 / 0.48. About 60% of the cross-area covariance is between E/W (West mean elasticity 0.47 vs 0.37, tracts).
- Levels: cor(with, without control) 0.999 / 1.000. West premium 55% / 57% (was 56 / 59). Duplex premium: mean 9% / 5% (was 13 / 9); cor(premium, elasticity) −0.36 / −0.02 (was −0.70 / 0.34).
- Downstream: sdTractB 0.449 → 0.454, sdNbhdB 0.443 → 0.448; lgTractFE 2.311 → 2.332, lgNbhdFE 2.356 → 2.381; ORs 2.55 / 2.36.
- Figures fig:tractPPSF / fig:neighbourhoodPPSF now plot elasticity (95% CI) against log ppsf: text/elasPpsfTract.png, text/elasPpsfNeighbourhood.png, from vancouverElasticityPremium.R.
- Why CHECK: (1) The elasticity correlations moved a lot; the draft claim that the duplex premium "falls with elasticity" across tracts is now weak (−0.36). (2) Run environment: this R setup keeps fixest singletons, so the ±6-month window columns of tab:ShareDiD have higher counts (tract areas 29 → 52) than the earlier run; the coefficients are unaffected. The FSR table also matched 8 more R1-1-era duplexes (123 → 131) with unchanged inputs. (3) Your new paragraph has \ref{tab:shareDiD}, but the label is tab:ShareDiD.

## 2026-11-20 — elasPpsfDupCor: neighbourhood panel dropped; figure legend and titles removed — CHECK (REVISIT)

- Table 2 (tab:elasPpsfDup) now has census tracts only (15 tracts, 3 West Side, 409 duplex sales). Draft: the neighbourhood premium mean and the "Across neighbourhoods the correlations are weak" sentence are removed; the caption says census tracts. The \dupPremium*Nbhd and \dupAreasNbhd macros are still generated but not used.
- Figures elasPpsfTract.png / elasPpsfNeighbourhood.png: no plot titles (the captions carry them) and no longitude legend (the caption already says darker = further east).
- REVISIT (Tom to sleep on it). Problem: about 64% of strata duplex sales are in RT zones, but the single-family sample is 33' lots now R1-1. Tracts mostly separate RT from R1, so RT-heavy tracts drop out (tract panel 409 duplex sales). Neighbourhoods contain both, so the old nbhd panel (1,311 sales, 60% Kitsilano + Grandview) compared RT duplexes with R1 houses elsewhere in the same neighbourhood.
- Option tested (scratch, not in repo): single-family sample = 33' lots now R1-1 or RT, pre-2018 sales (6,322 SFD sales, 613 in RT). Tracts: 22 areas (5 West), 924 duplex sales; cor(premium, ppsf) −0.48, (premium, elasticity) −0.20, (premium, West) −0.52; mean premium 8.6%. Neighbourhoods: same 10 areas (2 West), cor(premium, ppsf) −0.05, still flat; Kitsilano premium +7%. Among Table 2 areas cor(ppsf, elasticity) is only 0.05 / 0.10 (selection: duplex markets lack the expensive West Side), vs 0.40 / 0.54 across all areas. Odd point: West tract 9330045.02 (all-RT, 56 SFD sales) has elasticity 0.01.
- Decide: (a) adopt R1 + RT for Table 2 and the premium paragraph only (permit analysis stays R1); (b) whether a neighbourhood panel returns; (c) a nearest-R1-neighbour matching design for the premium.

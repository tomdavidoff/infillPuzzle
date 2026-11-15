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

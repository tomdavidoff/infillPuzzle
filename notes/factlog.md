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

- Script: `scripts/multiplexLotWidth.R` (log `.out`). Permits refreshed 2026-11-15 (latest issue 2026-11-13); lot width from 2018 SFD lots in R1-1 (BCA 2019 roll).
- 42.6% of 2024+ multiplex new-build permits are on 33-ft lots (118 / 277 matched). By year: 2024 46% (N=24), 2025 38% (141), 2026 48% (112). Baseline: 53.7% of 2018 R1-1 SFD lots are 33s.
- Multiplex share of new builds by width: 33s 21.8% (118/541); 37–41 31%; 42–50 30%; >50 32%.
- Why CHECK: only 279 of 791 multiplex permits (35%) link to a 2018 SFD lot, vs 71% of singles and 63% of duplexes. Of 339 unmatched multiplexes inside R1-1, 283 have addresses absent from the 2019 roll (new civic numbers) and points 20–50 m from any 2018 lot centroid. 5 m spatial, parcel-polygon and address matches agree 100% where they overlap and none recover them. 173 more multiplex permits are outside R1-1 (RM-8A, C-2, RM-12…). If unmatched multiplexes are on wider lots or assemblies, the 33-ft share is overstated.
- Also lost: 8% of 2018 SFD lots have zero recorded width.

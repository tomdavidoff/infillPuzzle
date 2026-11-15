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

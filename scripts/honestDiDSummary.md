# HonestDiD results, permit event study (working notes, not for the paper) — 11/22/26

Model: LPM of duplex (vs single) on period x tract log ppsf, tract and period FE, SEs clustered by tract (56).
Relative magnitudes: post violations at most Mbar x the largest pre-period period-to-period change.
"Breakdown" = largest Mbar on the grid at which the robust 95% CI excludes zero.
Smoothness (Delta^SD, M = 0 is linear extrapolation of the pre-trend): the CI includes zero at M = 0 in **every** row.

| # | Periods / sample | Ref. period | Pre-trend p | Pooled DiD (se) | Largest pre change | First post: est. [95% CI] | Breakdown Mbar | Post average: est. [95% CI] | Breakdown Mbar |
|---|---|---|---|---|---|---|---|---|---|
| 1 | 12-mo from Oct 17 2023, all permits | Nov 22–Oct 23 | 0.05 | 0.454 (0.100) | 0.31 | 0.22 [−0.18, 0.63] | none | 0.19 [−0.07, 0.46] | none |
| 2 | same, anticipation dating | Oct 21–Oct 22 | — | 0.454 (0.100) | 0.27 | 0.31 [0.05, 0.58] | 0.18 | 0.46 [0.21, 0.70] | 0.24 |
| 3 | same, drop Apr 17–Oct 16 2023 | Oct 22–Apr 23 | 0.23 | 0.490 (0.102) | 0.27 | 0.24 [−0.30, 0.79] | none | 0.23 [−0.21, 0.66] | none |
| 4 | same, drop Jul 1–Oct 16 2023 | Oct 22–Jun 23 | 0.23 | 0.497 (0.097) | 0.27 | 0.41 [−0.00, 0.83] | 0 (lb 0.004) | 0.40 [0.08, 0.71] | 0.10–0.15 |
| 5 | calendar pre years, drop Jan 1–Oct 16 2023 | 2022 | 0.19 | 0.514 (0.100) | 0.28 | 0.43 [0.12, 0.75] | 0.3 (fails 0.5) | 0.42 [0.12, 0.71] | 0.1 (fails 0.2) |
| 6 | Sept 18 pre years, Oct 17 post years, drop Sept 18 2022–Oct 16 2023 | 2021/22 | 0.036 | 0.522 (0.106) | 0.29 | 0.53 [0.16, 0.91] | 0.5 (fails 1) | 0.52 [0.22, 0.82] | 0.3 (fails 0.5) |
| **7** | **Calendar years 2019–2022, 2024–2026; 2018 and 2023 excluded (current)** | **2022** | **0.27** | **0.561 (0.117)** | **0.28** | **0.38 [0.04, 0.73]** | **0.1 (fails 0.2)** | **0.40 [0.06, 0.73]** | **0 (fails 0.1)** |

Row 7 (11/22/26, current): pre 2019 0.01 (0.20), 2020 −0.17 (0.17), 2021 −0.28 (0.17); post 2024 0.38 (0.17), 2025 0.51 (0.18),
2026 0.30 (0.29, partial year). 2023 estimated separately in the figure: 0.32 (0.16).

Grids: rows 1–2 Mbar 0–1 by 0.1 (row 2 breakdowns refined on a 0.01 grid); rows 3–4 by 0.05; rows 5–7 Mbar in {0, .1, .2, .3, .5, 1}.
Pooled DiD in rows 1–4 from cell-weighted / permit-level fits with the same point estimate; row 1 SE is the cell version.

Row 6 detail (superseded by row 7): pre coefficients 2018/19 0.24 (0.17), 2019/20 0.04 (0.21),
2020/21 −0.25 (0.14); post 2023/24 0.53 (0.19), 2024/25 0.40 (0.17), 2025/26 0.63 (0.26). The pre-trend rejection (p = 0.036)
comes from a path that falls 2018/19–2020/21 (0.24 → 0.04 → −0.25) and then rises to the reference year (0). The long-run
decline would bias against the effect if continued; the last step is upward, which is why smoothness (linear extrapolation) fails.
In coefficient units, row 6 breakdowns: first post year robust up to 0.5 x 0.29 ≈ 0.15/yr violation; average up to ≈ 0.09/yr.

Superseded scripts for rows 3–5: permitHonestDiDDonut.R (rows 3–4), permitEventStudyDonut.R + permitHonestDiDJan23.R (row 5).

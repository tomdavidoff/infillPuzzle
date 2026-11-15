
# Vancouver infill puzzle paper ("PHAT")

Paper with Tsur Somerville and Bob Helsley on Vancouver infill / missing-middle
housing. Tom writes the prose and the model. Your job is mostly empirical:
produce the facts, tables and figures the draft asks for, reproducibly.

## Layout

- `text/main.tex` — the draft. Do not edit prose unless asked. Compile from the repo root.
- `scripts/<name>.R` — all code. Source other scripts by relative path.
- `text/` — all outputs that the paper reads: `text/<name>.png`, `text/<name>.tex`.
- Raw data: `~/DropboxExternal/dataRaw/` (outside git, never modify).
- Processed data: `~/DropboxExternal/dataProcessed/` (outside git).
- Old scripts may still point at `~/OneDrive/` or `~/OneDrive - UBC/`. Those
  paths are dead; update them to `~/DropboxExternal/`.
- Disposable caches can go in `~/Downloads`.

## Data

- Building permits: City of Vancouver open data. Cache the download in
  `~/DropboxExternal/dataRaw/`. Re-download only if the cached copy is older
  than ~48 hours.
- Assessment data: BC Assessment. The main price-level regressor is
  `logBcaLandPPSF`.
- Projected CRS: EPSG 26910 (UTM 10N), as used in existing Vancouver scripts.

## How facts get into the paper

The draft marks missing empirics with `\fact{description}` (prints in red).
For each one:

1. Write or extend a script in `scripts/` that computes it.
2. Have the script write the number(s) to `text/<name>.tex` as macros, e.g.
   `\newcommand{\duplexShareEast}{0.42}`. Figures go to `text/<name>.png`.
3. Replace the `\fact{}` in the draft with the macro, and `\input` the file.
4. Never type a number into the .tex by hand.
5. Also with the fact insert a reference to an external data source if appropriate, e.g. `\cite{CityOfVancouverOpenData}`. Create a CLAUDE.bib and update appropriately, in the text/ directory.

Report back: the value, the N and denominator behind it, and anything
surprising (dropped rows, unexpected signs, outliers). Don't massage a result
toward what the draft seems to expect; if it contradicts the text, say so.

## Checks (required)

- `stopifnot()` on row counts before/after every merge and join, and on key
  uniqueness.
- Print the share of rows lost at each filter step.
- Where an estimator is new, validate it on simulated data with a known
  parameter before running it on real data.
- Sanity-check aggregates against published City totals where possible.

## Code style

- R first: `data.table`, `fixest`, `sf`, `ggplot2`, DuckDB.
- Terse, modular, data.table-idiomatic.
- No decorative comments, no restructuring code you weren't asked to touch,
  no over-engineering.
- Do analysis at census tract level and compare to neighbourhood level and East/West dummy

## Git

- Work on a branch, not `main`.
- One commit per fact/table/figure, with a message naming the macro it produces.
- Never commit data. Never push without asking.
- Never change authorship, acknowledgements, or coauthor-written sections.

# Reporting

For every fact, append an entry to `notes/factlog.md`:
macro name, value, N/denominator, script, checks run, and a status:

- OK: all checks passed, nothing surprising.
- CHECK: result produced, but something deserves a look
  (rows dropped >5%, sign or magnitude differs from the draft, outliers).
- STOP: a check failed or the data contradicts an assumption.
  Do not work around it. Stop and ask.

Put the status in the commit message too, e.g. `[CHECK] duplexShareEast`.

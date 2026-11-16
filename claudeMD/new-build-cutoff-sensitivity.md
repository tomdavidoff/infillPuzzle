---
name: new-build-cutoff-sensitivity
description: "When reporting permit-based shares, Tom wants them shown under alternative \"high-value permit counts as new build\" cutoffs"
metadata:
  node_type: memory
  type: feedback
  originSessionId: 86e491a1-abad-4fe3-be58-8e25b4dd5498
  modified: 2026-11-16T01:01:52.246Z
---

When permit results depend on which permits count as new builds, show the key shares under each cutoff, not only the main one:
(A) q95 of value among non-new-build permits for single/duplex/multiple dwelling (main, $340k as of Nov 2026),
(B) q95 of value among all those permits, and (C) q33 of value among new-build single + duplex permits.
Any permit type (not only Addition / Alteration) at or above the cutoff counts as a new build.

**Why:** Tom asked for this after I reported results under one cutoff only (2026-11-15). The draft's footnote defines new builds by a value percentile, so the choice is discretionary.

**How to apply:** `scripts/multiplexLotWidth.R` links all principal permits and prints a cutoff sensitivity table. Extend that table whenever new permit-based shares are added. Related: [[vancouver-ca-blocked]].

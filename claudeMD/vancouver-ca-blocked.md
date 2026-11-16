---
name: vancouver-ca-blocked
description: vancouver.ca / council.vancouver.ca / bylaws.vancouver.ca return Cloudflare 403 to WebFetch and curl; policy facts need Tom to supply PDFs
metadata:
  node_type: memory
  type: reference
  originSessionId: 86e491a1-abad-4fe3-be58-8e25b4dd5498
  modified: 2026-11-15T22:00:23.133Z
---

City of Vancouver sites (vancouver.ca, council.vancouver.ca, bylaws.vancouver.ca) block automated fetches (Cloudflare 403, as of 2026-11-15). Wayback-proxy workaround was denied by the permission classifier; don't retry it. Policy facts sourced only from search summaries are marked CHECK in notes/factlog.md and "verify" in text/CLAUDE.bib. To verify, ask Tom to save the PDFs into data/policyDocs/ (repo, <200 KB) or ~/DropboxExternal/dataRaw/policyDocs/ (larger), then read them locally. A copy of the April 2023 RS-1 schedule was read from a third-party mirror.

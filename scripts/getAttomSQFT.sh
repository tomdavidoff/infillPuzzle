#!/bin/bash

BASEDIR=~/BigFilesExternal/attom
OUTFILE=~/DropboxExternal/dataProcessed/attomSQFT.tsv
TMPDIR="$BASEDIR/tmp_extract"

echo -e "ATTOM_ID\tAreaBuilding\tYearBuilt" > "$OUTFILE"

for z in "$BASEDIR"/UNI_OF_BC_TAXASSESSOR_INITIAL_*.zip; do
  echo "Processing $(basename "$z")..."
  mkdir -p "$TMPDIR"
  unzip -o "$z" -d "$TMPDIR"

  find "$TMPDIR" -name "*.txt" | while read f; do
    header=$(head -1 "$f")
    cols=$(echo "$header" | tr '\t' '\n' | grep -n "ATTOM ID\|AreaBuilding\|YearBuilt" | cut -d: -f1 | paste -sd, -)
    if [ -n "$cols" ]; then
      tail -n +2 "$f" | cut -d$'\t' -f"$cols" >> "$OUTFILE"
      echo "  Extracted from $(basename "$f"), columns: $cols"
    else
      echo "  WARNING: no matching columns in $(basename "$f")"
    fi
  done

  rm -rf "$TMPDIR"
  echo "  Cleaned up extracted files."
done

echo "Done. Output: $OUTFILE"

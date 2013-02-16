#!/bin/sh

if [ ! -f "$1" ]; then
  echo "USAGE: $0 [exam file]"
  exit 1
fi

exam_file="$1"
tmp=$(mktemp)
tmp=/tmp/a

# Convert to text
pdftotext "$exam_file" "$tmp"

# Pick out part I
sed -n -i '/^Directions.*50.*:/,/^P[aA][rR][tT] II$/ p' "$tmp"
./parse-part_ii.py "$tmp"

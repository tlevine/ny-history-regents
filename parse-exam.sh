#!/bin/sh
set -e

if [ ! -f "$1" ]; then
  echo "USAGE: $0 [exam file]"
  exit 1
fi

DB=/tmp/history-regents.db
exam_file="$1"
tmp=$(mktemp)

# Convert to text
pdftotext "$exam_file" "$tmp"

# Pick out part I
sed -n -i '/^Directions.*50.*:/,/^P[aA][rR][tT] II$/ p' "$tmp"

# Put in the database.
./parse-part_ii.py "$DB" "$tmp"
rm "$tmp"

# Put it in the right table.
sqlite3 "$DB" < schema.sql
sqlite3 "$DB" "
BEGIN TRANSACTION;
UPDATE current SET examfile = '$exam_file';
INSERT INTO question SELECT * FROM current;
DROP TABLE CURRENT;
COMMIT;"

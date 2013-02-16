#!/bin/sh
set -e

USAGE="USAGE: $0 [exam file] [scoring file]"

if !(test -f "$1" && test -f "$2"); then
  echo "$USAGE"
  exit 1
fi

# Input variables
exam_file="$1"
scoring_file="$2"

# Temporary files
DB=/tmp/global-history-regents.db
tmp_exam=$(mktemp)
tmp_scoring=$(mktemp)

# Convert to text
pdftotext "$exam_file" "$tmp_exam"
pdftotext "$exam_file" "$tmp_scoring"

# Pick out part I
sed -n -i '/^Directions.*50.*:/,/^P[aA][rR][tT] II$/ p' "$tmp_exam"

# Put the questions in the database.
./parse-part_ii.py "$DB" "$tmp_exam"
rm "$tmp_exam"

# Put the answers in the database.
sed -n '1,/^50/ p' "$tmp_scoring" | sed -n -e '/^[0-9].*[1-4]/p' | sed -e 's/[^0-9 ]//g' -e 's/  */ /' -e 's/ *$//' |sort -n |
  sed -n '/^[0-9]\{1,2\} [1-4]$/p' | 
  sed "s/^\([0-9]*\) \([1-4]\)/UPDATE current SET correct_answer = \2 WHERE \"number\" = \1;/" | sqlite3 "$DB"

# Put it in the right table.
sqlite3 "$DB" < schema.sql
sqlite3 "$DB" "
BEGIN TRANSACTION;
UPDATE current SET examfile = '$exam_file';
INSERT INTO question (examfile, \"number\", question, answer1, answer2, answer3, answer4, correct_answer)
  SELECT examfile, \"number\", question, answer1, answer2, answer3, answer4, correct_answer FROM current;
COMMIT;" || echo "There was an error in the question extraction, so I'm skipping this file.\n"
sqlite3 "$DB" 'DROP TABLE current;'

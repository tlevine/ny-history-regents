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
DB=/tmp/history-regents.db
tmp_exam=$(mktemp)
tmp_scoring=$(mktemp)

# Questons
pdftotext "$exam_file" "$tmp_exam"
sed -n -i '/^Directions.*50.*:/,/^P[aA][rR][tT] II$/ p' "$tmp_exam"
./parse-part_i.py "$DB" "$tmp_exam"
rm "$tmp_exam"

# Answers
pdftohtml -xml -l 1 "$scoring_file" "$tmp_scoring".xml
./parse-scoring.py  "$tmp_scoring".xml | sqlite3 "$DB"

# Put it in the right table.
sqlite3 "$DB" < schema.sql
echo $exam_file
sqlite3 "$DB" "
BEGIN TRANSACTION;
INSERT INTO question (examfile, \"number\", question, answer1, answer2, answer3, answer4, correct_choice)
  SELECT '$exam_file' AS examfile, \"number\", question, answer1, answer2, answer3, answer4, correct_choice FROM current;
COMMIT;" # || echo "There was an error in the question extraction, so I'm skipping this file.\n"
sqlite3 "$DB" 'DROP TABLE current;'

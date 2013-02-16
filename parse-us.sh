#!/bin/sh
set -e

for exam in $(ls downloads/www.nysedregents.org/USHistoryGov/Archive/*exam.pdf); do
  echo $exam
  ./parse-exam.sh "$exam"
done

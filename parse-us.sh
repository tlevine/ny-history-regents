#!/bin/sh

for exam in $(ls downloads/www.nysedregents.org/USHistoryGov/Archive/20080124exam.pdf); do
  ./parse-exam.sh "$exam"
done

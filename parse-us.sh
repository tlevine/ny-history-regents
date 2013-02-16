#!/bin/sh
set -e

exam() {
  exam="$1"
  exam=$(echo "$scoringkey" | sed 's/\(scoring\|key\).*$/exam.pdf/')
  echo $exam
  echo $scoringkey
  [ -f "$exam" ] && [ -f "$scoringkey" ] && ./parse-exam.sh "$exam" "$scoringkey"
  echo
}

for scoringkey in $(ls downloads/www.nysedregents.org/USHistoryGov/Archive/*scoringkeyIandII.pdf); do
  exam "$scoringkey"
done

for scoringkey in $(ls downloads/www.nysedregents.org/USHistoryGov/Archive/*scoringkey.pdf); do
  exam "$scoringkey"
done

for scoringkey in $(ls downloads/www.nysedregents.org/USHistoryGov/Archive/*key.pdf); do
  exam "$scoringkey"
done

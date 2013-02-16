#!/bin/sh

if [ ! -f "$1" ]; then
  echo "USAGE: $0 [exam file]"
  exit 1
fi

exam_file="$1"
tmp=$(mktemp)

# Convert to text
pdftotext "$exam_file" "$tmp"

# Pick out part I
sed -n -i '/^Answer all questions in this part.$/,/^Part II$/ p' "$tmp"


echo $tmp

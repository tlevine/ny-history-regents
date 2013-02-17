Download the history regents and convert them into a format useful for
prediction of answers.

Download

    ./download.py

Then parse it

    ./parse-exam.sh [exam file pdf]

Parse all of the US history regents.

    ./parse-us.sh

Then export to csv.

    sqlite3 -csv -header /tmp/history-regents.db 'select * from question;' > history-regents.csv


## Results
If I guess the answer that is Leveschtein-closest to the others,
I get 254 of 795 correct, or about one-fourth. Boring.

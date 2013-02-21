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

## Feature selection

### Neutral answers win.
I think that sentiment will tell us something. Question 43 from the January 2004 test is one question that makes me think so.

> 43 The changes shown in the graph support the recent concerns of Americans about the
>
> 1) future of Social Security and Medicare
> 2) return to an agrarian society
> 3) surplus of health care workers
> 4) shortage of schools and colleges

Without looking at the graph, I correctly guessed choice 1. The words "surplus" and "shortage" felt too strong,
so narrowed it down to the first two choices. A return to an agrarian society seemed unreasonable, so I said 1.

### Short answers win.
Short answers seem to be correct, or at least long answers seem to be wrong. Naively, we might look at

* Number of characters
* Number of words

It might be related to the linguistic complexity, so here are some other things worth considering

* Number of clauses
* How fancy the words are
* Number of adjectives

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
> 1. future of Social Security and Medicare
> 2. return to an agrarian society
> 3. surplus of health care workers
> 4. shortage of schools and colleges

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

### The conjunction "and" wins.

Somewhat contrary to my suspicion that short answers win, I also suspect that
the presense of the conjunction "and" normally occurs in correct answers. For
question 30 of June 2007, I first eliminated 1 and 3 because they both included
the same phraes "Latin American". Based on my shortness criterion, I would have
chosen choice 4, but that didn't feel right, so I correctly chose choice 2.
I still haven't looked at the map mentioned in the question.

> 30 The conclusion that can best be supported by the
> information on this map is that construction of
> the Panama Canal was motivated by the desire of
> the United States to
> 
> (1) raise the living standards of Latin American people
> (2) increase naval mobility and expand overseas markets
> (3) improve relations with Latin American and Asian nations
> (4) maintain a policy of collective security

### Qualitative, abstract, synthesized statements about graphs win

I correctly excluded answers 1 and 2 from question 45 of January 2004 because they sounded too quantitative.

> 45 Which statement is most clearly supported by the
> information in the graph?
> 
> 1. More children were under age 6 in 1990 than in 1950.
> 2. Since 1990, women have made up more than half of the workforce.
> 3. The gap between male and female incomes has declined.
> 4. Fewer women are staying home to raise their young children.

### Equivalent answers lose.

Without reading the three headlines, I correctly guessed choice 1 because the others were all equivalent.
As a side note, even after reading the headlines, I had no idea that these were talking about compromises.

> 2
>
> * "New Congress to Have Two Houses"
> * "Slaves to Count as Three-Fifths of a Person"
> * "President to be Chosen by Electoral Vote"
>
> Which conclusion about the Constitutional Convention is best supported by these headlines?
>
> 1. The framers of the Constitution were able to compromise on important issues.
> 2. States that were small in area would lose power in the new Constitution.
> 3. States with large populations controlled the outcome of the convention.
> 4. The president and Congress would have equal power under the new constitution.

## Questions whose answers I can't guess well

Question 20 of the January 2004 test

> 20 The common purpose of these legislative acts was to
>
> 1. protect the nation’s natural resources
> 2. improve conditions for recent immigrants to the United States
> 3. advance the growth of big business
> 4. promote the general welfare of the American public

Maybe a hint is that "resources" and "conditions" are used in the table of legislative acts.
I've generally seen that vocabularity is rarely repeated within an entire
test, so repeating vocabulary might be an attempt to trick students.

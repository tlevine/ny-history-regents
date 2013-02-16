#!/usr/bin/env python2
'Given the text of part II, convert it to a sqlite database.'
import re
import dumptruck

def questionize(f):
    'Parse the part II given the file handle.'
    questions = ['']
    for line in f:
        if re.match(r'[0-9]{1,2} .+', line):
            questions.append(line)
        else:
            questions[-1] += line
    return questions[1:]

def parse_question(question):
    row = {
        'number': int(question.split(' ')[0]),
        'answer1': '',
        'answer2': '',
        'answer3': '',
        'answer4': '',
    }
    for line in question.split('\n'):
        answer = None
        if re.match(r'^\([1234]\) .+', line):
            answer = line[1]
            row['answer' + answer] += line[4:]
        elif answer:
            answer['answer' + answer] += line

    return row

def main():
    import sys
    import os

    usage = "USAGE: %s [part II text file]" % sys.argv[0]
    if len(sys.argv) != 3:
        print usage
        exit(1)

    examfile = sys.argv[2]
    dbfile = sys.argv[1]

    if not os.path.isfile(examfile):
        print usage
        exit(2)

    # Get the questions
    questions = questionize(open(examfile))

    # Structure the questions
    d_messy = map(parse_question, questions)

    # Remove questions that aren't questions.
    d_clean = filter(lambda row: '' not in row.values(), d_messy)

    for row in d_clean:
        row['examfile'] = examfile

    dt = dumptruck.DumpTruck(dbname = dbfile)
    dt.create_table({'examfile': 'abc'}, 'current')
    dt.upsert(d_clean, 'current')

if __name__ == '__main__':
    main()

#!/usr/bin/env python2
'Given the text of part II, convert it to a sqlite database.'
import re
import dumptruck

def parse(f):
    'Parse the part II given the file handle.'
    questions = ['']
    for line in f:
        if line == '\n':
            questions.append('')
        elif re.match(r'[0-9]{1,2} .+', line):
            questions.append(line)
        else:
            questions[-1] += line
    return questions

def main():
    import sys
    import os

    usage = "USAGE: %s [part II text file]" % sys.argv[0]
    if len(sys.argv) != 2:
        print usage
        exit(1)
    elif not os.path.isfile(sys.argv[1]):
        print usage
        exit(2)

    d = parse(open(sys.argv[1]))

    print sys.argv[1]
    print d

if __name__ == '__main__':
    main()

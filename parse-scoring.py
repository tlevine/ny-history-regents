#!/usr/bin/env python2
import re
import lxml.etree

def parse(pdf2xml):
    return '\n'.join(map(sql, pdf2xml.xpath('//text[b]')))

def sql(text):
    try:
        number = int(re.sub(r'[^0-9]', '', text.xpath('text()')[0]))
        choice = int(text.xpath('b/text()')[0])
    except:
        return ''
    else:
        return 'UPDATE current SET correct_choice = %d WHERE "number" = %d;' % (choice, number)

def main():
    import sys
    pdf2xml = lxml.etree.parse(sys.argv[1])
    print parse(pdf2xml)

if __name__ == '__main__':
    main()

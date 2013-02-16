#!/usr/bin/env python2
'Download the tests and answers given the index file.'
import lxml.html
import os
from urllib import urlretrieve

def download_parse_index(index_url, index_filename):
    if not os.path.isfile(index_filename):
        urlretrieve(index_url, filename = index_filename)

    html = lxml.html.parse(index_filename)
    html.getroot().make_links_absolute(index_url)
    test_urls = filter(lambda href: href[-4:] in ('.pdf', '.xls'), html.xpath('//a/@href'))

def main():
    download_parse_index('http://www.nysedregents.org/USHistoryGov/home.html', 'us.html')
#   download_parse_index('http://www.nysedregents.org/GlobalHistoryGeography', 'global.html')

if __name__ == '__main__':
    main()

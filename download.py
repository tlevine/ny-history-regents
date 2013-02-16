#!/usr/bin/env python2
'Download the tests and answers given the index file.'
import lxml.html
import os
from urllib import urlretrieve
import h.hardhat as hardhat

def index_to_files(index_url):
    hardhat.get(index_url, 'downloads')
    html = lxml.html.parse(index_filename)
    html.getroot().make_links_absolute(index_url)
    test_urls = filter(lambda href: href[-4:] in ('.pdf', '.xls'), html.xpath('//a/@href'))
    return test_urls

def main():
    download_parse_index('http://www.nysedregents.org/USHistoryGov/home.html', 'us.html')
#   download_parse_index('http://www.nysedregents.org/GlobalHistoryGeography', 'global.html')

if __name__ == '__main__':
    main()

#!/usr/bin/env python2
'Download the tests and answers given the index file.'
import lxml.html
import os
from urllib import urlretrieve
import h.hardhat as hardhat

def index_to_files(index_url):
    raw = hardhat.get(index_url, 'downloads')
    html = lxml.html.fromstring(raw)
    html.make_links_absolute(index_url)
    test_urls = filter(lambda href: href[-4:] in ('.pdf', '.xls'), html.xpath('//a/@href'))
    return test_urls

def main():
    print index_to_files('http://www.nysedregents.org/USHistoryGov/home.html')
    print index_to_files('http://www.nysedregents.org/GlobalHistoryGeography')

if __name__ == '__main__':
    main()

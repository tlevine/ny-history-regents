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
    INDEX_URLS = [
        'http://www.nysedregents.org/USHistoryGov/home.html',
        'http://www.nysedregents.org/GlobalHistoryGeography',
    ]
    for index_url in INDEX_URLS:
        for test_url in index_to_files(index_url):
            hardhat.get(test_url)

if __name__ == '__main__':
    main()

#!/bin/sh

# recursively save a website (IE all the pages in the domain and directory you point it to

# m is mirror -- it gets the whole site, essentially
# k is --convert-links, meaning it turns links to pages that will be downloaded into links to the local file
wget -mk "$@"

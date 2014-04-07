#!/usr/bin/env python

import urllib
import sys

__author__ = "Anthony Scemama"
__date__ = "Mon Apr  7 15:30:29 CEST 2014"

"""Fetches a file from a given URL.
   syntax : %s <URL> <local_filename>"""%(sys.argv[0])

# Check command line
if len(sys.argv) < 3:
  print """
  usage:
    %s <URL> <local_filename>"""%(sys.argv[0])

URL = sys.argv[1]
filename = sys.argv[2]


webpage = urllib.urlopen(URL)
data = webpage.read()

if "404 - Not Found" in data:
  print "Error in URL"
  sys.exit(1)

file = open(filename,'w')
file.write(data)
file.close()



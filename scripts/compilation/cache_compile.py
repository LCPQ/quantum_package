#!/usr/bin/env python

import os
import sys
import shelve
import hashlib
import re

r = re.compile(ur'-c\s+(\S+\.[fF]90)\s+-o\s+(\S+\.o)')
p = re.compile(ur'-I IRPF90_temp/\S*\s+')
mod = re.compile(ur'module\s+(?P<mod>\S+).+end\s?module\s+(?P=mod)?', re.MULTILINE | re.IGNORECASE)

TMPDIR="/tmp/qp_compiler/"

def main():
    # Create temp directory
    if "qp_compiler" not in os.listdir("/tmp"):
      os.mkdir("/tmp/qp_compiler/")

    line = sys.argv[1:]
    command = " ".join(line)
    command_clean = p.sub('',command)

    try:
      match = r.search(command_clean)
      input  = match.group(1)
      output = match.group(2)
    except:
      os.system(command)
      return
    m = hashlib.md5()

    # Fread : read input
    with open(input,'r') as file:
      fread = file.read()
      m.update( " ".join( [ command, fread ] ))

    # Md5 Key containing command + content of Fread
    key = TMPDIR+m.hexdigest()
    try:
        # Try to return the content of the .o file
        with open(key,'r') as file:
            result = file.read()
    except IOError:
        # Compile the file -> .o
        os.system(command)
        # Read the .o
        with open(output,'r') as file:
            result = file.read()
        # Copy the .o in database
        if not mod.search(fread.replace('\n',' ')):
            with open(key,'w') as file:
                file.write(result)
        else:
            print input+' -> module'
    else:
        # Write the .o file
        with open(output,'w') as file:
            file.write(result)

if __name__ == '__main__':
    main()


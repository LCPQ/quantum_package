#!/usr/bin/env python2

import os

QP_ROOT=os.environ["QP_ROOT"]

name_to_elec = {}
with open(QP_ROOT+"/data/list_element.txt","r") as f:
  data = f.readlines()
  for line in data:
    b = line.split()
    name_to_elec[b[1]] = int(b[0])

if __name__ == '__main__':
  print name_to_elec

#!/usr/bin/env python

import time
import os
import sys

"""Follow the output files in standard output"""

class OutputFollower(object):

  def __init__(self,filename):
    self.filename = filename
    self.dir = filename+'/output/'
    self.last_time = {}
    self.last_line = {}
    self.data      = {}
    self.update_file_list()
    self.running = False

  def update_file_list(self):
    self.file_list = filter(lambda x: x.endswith('.rst'), os.listdir(self.dir) )
    for filename in self.file_list:
      filename = self.dir+'/'+filename
      statbuf = os.stat(filename)
      date = statbuf.st_mtime 
      if filename not in self.last_time:
        self.last_time[filename] = -1
        self.last_line[filename] = -1
        self.data[filename] = []
      if date > self.last_time[filename]:
          # Read file
          file = open(filename,'r')
          self.data[filename] = file.read().splitlines()
          file.close()
          # Print new lines
          output = self.data[filename]
          for line in output[self.last_line[filename]+1:]:
            print line
          self.last_time[filename] = date
          self.last_line[filename] = len(output)-1

  def start(self):
    self.running = True
    while self.running:
      time.sleep(1.)
      self.update_file_list()


def main():
  F = OutputFollower(sys.argv[1])

  # Handle signals
  import signal
  def handler(signum,frame):
    F.running = False

  for i in [2, 15]:
   try:
    signal.signal(i, handler)
   except:
    pass

  F.start()

if __name__ == '__main__':
  main()



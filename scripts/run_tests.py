#!/usr/bin/python

import unittest
import os
import subprocess
from math import *

env = os.environ
try:
  verbosity = int(sys.argv[1])
except:
  verbosity = 1

testfiles = []
for f in os.listdir(os.getcwd()):
  if f.endswith('.irp.f'):
    testfiles.append(f.replace('.irp.f',''))

def run_test(test_name,inp):
  command = './'+test_name+" ${QPACKAGE_ROOT}/data/inputs/"+inp
  result = subprocess.check_output(command, shell=True)
  return result


template = """
class $test(unittest.TestCase):
    
    execfile('$test.ref')

    def setUp(self):
       self.name = '$test'

    def _test_input(self,inp):
       output = run_test(self.name, inp)
       for line in output.splitlines():
          buffer = line.split(':')
          if len(buffer) == 1:
            continue
          l,r = buffer
          l,r = l.strip(), eval(r)
          if type(r) == float:
            self.assertAlmostEqual(self.data[inp][l], r, 
              places=abs(int(log10(self.precision[l]*max(abs(self.data[inp][l]),1.e-12)))), msg=None)
          else:
            self.assertEqual(self.data[inp][l], r, msg=None)

    t = "def test_$k(self): self._test_input('$i')"
    for i in data.keys():
       k = i
       k = k.replace('.ezfio','')
       k = k.replace('-','_m_')
       k = k.replace('+','_p_')
       exec t.replace('$i',i).replace('$k',k) in locals()
"""

for test in testfiles:
    exec template.replace('$test',test) in locals()

if __name__ == '__main__':
   unittest.main(verbosity=verbosity)

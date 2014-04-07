#!/usr/bin/python

import sys,os

def run_test(test_name,inp):
  command = './'+test_name+" ${QPACKAGE_ROOT}/data/inputs/"+inp
  result = subprocess.check_output(command, shell=True)
  return result

if __name__ == '__main__':

    import unittest
    import subprocess
    from math import *
    
    from multiprocessing import Pool
    env = os.environ
    
    verbosity = 1
    try:
      nproc = int(subprocess.check_output("cat /proc/cpuinfo | grep processor | wc -l", shell=True))
    except:
      nproc=4
    
    testfiles = []
    for f in os.listdir(os.getcwd()):
      if f.endswith('.irp.f'):
        testfiles.append(f.replace('.irp.f',''))
    
    # start worker processes
    pool = Pool(processes=nproc)
    
    template = """
class $test(unittest.TestCase):
    
    default_precision = 1.e-10

    execfile('$test.ref')

    name = '$test'
    tasks = {}

    def setUp(self):
       for d in self.data.keys():
         if d not in self.tasks:
           self.tasks[d] = pool.apply_async(run_test, [self.name, d])

    def _test_input(self,inp):
       output = self.tasks[inp].get()
       for line in output.splitlines():
          buffer = line.split(':')
          if len(buffer) == 1:
            continue
          l,r = buffer
          l,r = l.strip(), eval(r)
          if 'precision' in self.__dict__:
            precision = self.precision[l]
          else:
            precision = self.default_precision
          if type(r) == float:
            self.assertAlmostEqual(self.data[inp][l], r, 
              places=abs(int(log10(precision*max(abs(self.data[inp][l]),1.e-12)))), msg=None)
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

    unittest.main(verbosity=verbosity)

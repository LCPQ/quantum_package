#!/usr/bin/env python

__author__ = "Anthony Scemama and Applencourt for the amazing PEP8"
__date__ = "jeudi 26 mars 2015, 12:49:35 (UTC+0100)"

"""
Creates the provider of a variable that has to be
fetched from the EZFIO file.
"""

import sys
import os


class EZFIO_Provider(object):

    data = """BEGIN_PROVIDER [ %(type)s, %(name)s ]
  implicit none
  BEGIN_DOC
!  %(doc)s
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_%(ezfio_dir)s_%(ezfio_name)s(has)
  if (has) then
    call ezfio_get_%(ezfio_dir)s_%(ezfio_name)s(%(name)s)
  else
    %(default)s
  endif
  %(write)s

END_PROVIDER
"""

    write_correspondance = {"integer": "write_int",
                            "logical": "write_bool",
                            "double precision": "write_double"}

    def __init__(self):
        self.values = "type doc default name ezfio_dir ezfio_name write output".split()
        for v in self.values:
            exec "self.%s = None" % (v) in locals()

    def __repr__(self):
        self.get_default()
        self.set_write()
        for v in self.values:
            exec "test = self.%s is None" % (v) in locals()
            if test:
                msg = "Error : %s is not set in ezfio_with_default.py" % (v)
                print >>sys.stderr, msg
                for v in self.values:
                    exec "x = str(self.%s)" % (v) in locals()
                    print >>sys.stderr, "%s : %s" % (v, x)
                sys.exit(1)
        return self.data % self.__dict__

    def set_write(self):
        self.write = ""
        if self.type in self.write_correspondance:
            write = self.write_correspondance[self.type]
            output = self.output
            name = self.name
            self.write = """
  call write_time(%(output)s)
  call %(write)s(%(output)s, %(name)s, &
      '%(name)s')""" % locals()

    def set_type(self, t):
        self.type = t.lower()

    def set_doc(self, t):
        self.doc = t.replace('\n', '\n! ')

    def set_name(self, t):
        self.name = t

    def set_ezfio_dir(self, t):
        self.ezfio_dir = t.lower()

    def set_ezfio_name(self, t):
        self.ezfio_name = t.lower()

    def set_output(self, t):
        self.output = t

    def set_default(self, t):
        self.default = t

    def get_default(self):
        filename = '/'.join([os.environ['QPACKAGE_ROOT'],
                             'data',
                             'ezfio_defaults'])

        with open(filename, 'r') as f:
            lines = f.readlines()

        # Search directory
        for k, line in enumerate(lines):
            if line[0] != ' ':
                if line.strip().lower() == self.ezfio_dir:
                    break
        if k + 1 == len(lines):
            return
        # Search name
        while k < len(lines):
            k += 1
            buffer = lines[k].split()
            if len(buffer) == 0:
                return
            if buffer[0].lower() == self.ezfio_name:
                break
        v = buffer[1]
        name = self.name
        try:
            v_eval = eval(v)
            if isinstance(v_eval, bool):
                v = '.%s.' % (v)
            elif isinstance(v_eval, float):
                v = v.replace('e', 'd')
                v = v.replace('E', 'D')
            v = "%(name)s = %(v)s" % locals()
        except:
            v = "call ezfio_get_%(v)s(%(name)s)" % locals()
        self.default = v


def test_module():
    T = EZFIO_Provider()
    T.set_type("double precision")
    T.set_name("thresh_SCF")
    T.set_doc("Threshold on the convergence of the Hartree Fock energy")
    T.set_ezfio_dir("Hartree_Fock")
    T.set_ezfio_name("thresh_SCF")
    T.set_output("output_Hartree_Fock")
    print T


if __name__ == '__main__':
    test_module()

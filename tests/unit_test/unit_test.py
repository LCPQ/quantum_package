#!/usr/bin/env python
# -*- coding: utf-8 -*-
import unittest
import subprocess
import os

qpackage_root = os.environ['QPACKAGE_ROOT']

import sys
EZFIO = "{0}/EZFIO".format(qpackage_root)

sys.path = [EZFIO + "/Python"] + sys.path

from ezfio import ezfio

precision = 1.e-8

from collections import defaultdict


def init_folder(name, basis):

    cmd = "cp {0}/tests/{1}.xyz .".format(qpackage_root, name)
    subprocess.check_call([cmd], shell=True)

    cmd = "qp_create_ezfio_from_xyz -b {0} {1}.xyz".format(basis, name)
    subprocess.check_call([cmd], shell=True)


def run_hf(name, basis):

    ref_energy = defaultdict(dict)

    ref_energy["sto-3g"]["methane"] = -39.7267433402

    init_folder(name, basis)

    cmd = "qp_run SCF {0}.ezfio/".format(name)
    subprocess.check_call([cmd], shell=True)

    ezfio.set_file("{0}.ezfio".format(name))
    cur_e = ezfio.get_hartree_fock_energy()

    return abs(cur_e - ref_energy[basis][name]) <= precision


def run_full_ci_10k_pt2_end(name, basis):

    ref_energy_var = defaultdict(dict)
    ref_energy_pt2 = defaultdict(dict)

    ref_energy_var["sto-3g"]["methane"] = -0.398058753535695E+02
    ref_energy_pt2["sto-3g"]["methane"] = -0.398059182483741E+02

    ezfio.set_file("{0}.ezfio".format(name))

    ezfio.full_ci_do_pt2_end = True
    ezfio.full_ci_n_det_max_fci = 10000
    ezfio.full_ci_pt2_max = 1.e-8

    cmd = "qp_run full_ci {0}.ezfio/".format(name)
    subprocess.check_call([cmd], shell=True)

    cur_var = ezfio.get_full_ci_energy()
    cur_pt2 = ezfio.get_full_ci_energy_pt2()

    t = [abs(cur_var - ref_energy_var[basis][name]) <= precision,
         abs(cur_pt2 - ref_energy_pt2[basis][name]) <= precision]

    return all(t)


def run_big_test(name, basis):
    if run_hf(name, basis):
        return run_full_ci_10k_pt2_end(name, basis)
    else:
        raise ValueError("Fail un run_hf")


class SimplisticTest(unittest.TestCase):

    def test_full_ci_10k_pt2_end(self):
        self.assertTrue(run_big_test("methane", "sto-3g"))

if __name__ == '__main__':
    unittest.main()

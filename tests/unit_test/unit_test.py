#!/usr/bin/env python
# -*- coding: utf-8 -*-
import unittest
import subprocess
import os
import sys

qpackage_root = os.environ['QPACKAGE_ROOT']

EZFIO = "{0}/EZFIO".format(qpackage_root)
sys.path = [EZFIO + "/Python"] + sys.path

from ezfio import ezfio
from collections import defaultdict

# ~#~#~ #
# O p t #
# ~#~#~ #

precision = 5.e-8

# A test get a geo file and a basis file.
# A global dict containt the result for this test
# A test return True or Raise a error !
# More ezfio condition you set, beter it is


# You cannot order the test flow.
# So if you dont whant to remarque on test (for example the HF), set
# a global variable and check for it
global has_hf_alredy
has_hf_alredy = False


def init_folder(geo, basis, mult=1):
    '''
    Take a geo in arg (aka a existing geo.xyz in test/)
    And create the geo.ezfio with the adeguate basis and multipliciti
    DO NOT CHECK IS THE EZFIO FOLDER ALREADY EXIST
    '''

    cmd = "cp {0}/tests/{1}.xyz .".format(qpackage_root, geo)
    subprocess.check_call([cmd], shell=True)

    cmd = "qp_create_ezfio_from_xyz -b {0} -m {1} {2}.xyz".format(basis,
                                                                  mult,
                                                                  geo)
    subprocess.check_call([cmd], shell=True)


def get_error_message(l_exepected, l_cur):
    l_msg = ["Need {0} get {1} error is {2}".format(i, j, abs(i - j))
             for i, j in zip(l_exepected, l_cur)]
    return "\n".join(l_msg)


def run_hf(geo, basis):
    """
    Run a simle by default hf
    EZFIO path = geo.ezfio
    """
    # ~#~#~#~#~#~#~#~#~#~ #
    # R e f _ e n e r g y #
    # ~#~#~#~#~#~#~#~#~#~ #

    ref_energy = defaultdict(dict)

    ref_energy["sto-3g"]["methane"] = -39.7267433402

    # ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ #
    # G l o b a l _ v a r i a b l e #
    # ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ #

    global has_hf_alredy
    has_hf_alredy = True

    # ~#~#~#~ #
    # I n i t #
    # ~#~#~#~ #

    init_folder(geo, basis)
    ezfio.set_file("{0}.ezfio".format(geo))

    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #
    # S e t _ p a r a m e t e r #
    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #
    ezfio.bielec_integrals_direct = False
    ezfio.bielec_integrals_threshold_ao = 1.e-15
    ezfio.bielec_integrals_write_ao_integrals = False
    ezfio.bielec_integrals_read_ao_integrals = False

    ezfio.bielec_integrals_threshold_mo = 1.e-15
    ezfio.bielec_integrals_write_mo_integrals = False
    ezfio.bielec_integrals_read_mo_integrals = False

    ezfio.hartree_fock_mo_guess_type = "Huckel"
    ezfio.hartree_fock_thresh_scf = 1.e-10
    ezfio.hartree_fock_n_it_scf_max = 100

    # ~#~#~ #
    # R u n #
    # ~#~#~ #

    cmd = "qp_run SCF {0}.ezfio/".format(geo)
    subprocess.check_call([cmd], shell=True)

    # ~#~#~#~#~ #
    # C h e c k #
    # ~#~#~#~#~ #

    cur_e = ezfio.get_hartree_fock_energy()
    ref_e = ref_energy[basis][geo]

    if abs(cur_e - ref_e) <= precision:
        return True
    else:
        raise ValueError(get_error_message([ref_e], [cur_e]))


def run_full_ci_10k_pt2_end(geo, basis):
    """
    Run a Full_ci with 10k with the TruePT2
    EZFIO path = geo.ezfio
    """

    # ~#~#~#~#~#~#~#~#~#~ #
    # R e f _ e n e r g y #
    # ~#~#~#~#~#~#~#~#~#~ #

    ref_energy_var = defaultdict(dict)
    ref_energy_pt2 = defaultdict(dict)

    ref_energy_var["sto-3g"]["methane"] = -0.398058753535695E+02
    ref_energy_pt2["sto-3g"]["methane"] = -0.398059182483741E+02

    # ~#~#~#~ #
    # I n i t #
    # ~#~#~#~ #

    ezfio.set_file("{0}.ezfio".format(geo))

    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #
    # S e t _ p a r a m e t e r #
    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #

    ezfio.full_ci_do_pt2_end = True
    ezfio.full_ci_n_det_max_fci = 10000
    ezfio.full_ci_pt2_max = 1.e-8

    # ~#~#~ #
    # R u n #
    # ~#~#~ #

    cmd = "qp_run full_ci {0}.ezfio/".format(geo)
    subprocess.check_call([cmd], shell=True)

    # ~#~#~#~#~ #
    # C h e c k #
    # ~#~#~#~#~ #

    cur_var = ezfio.get_full_ci_energy()
    cur_pt2 = ezfio.get_full_ci_energy_pt2()

    ref_var = ref_energy_var[basis][geo]
    ref_pt2 = ref_energy_pt2[basis][geo]

    t = [abs(cur_var - ref_var) <= precision,
         abs(cur_pt2 - ref_pt2) <= precision]

    if all(t):
        return True
    else:
        raise ValueError(get_error_message([ref_var, ref_pt2],
                                           [cur_var, cur_pt2]))


def run_big_test(geo, basis):
    if not has_hf_alredy:
        run_hf(geo, basis)

    run_full_ci_10k_pt2_end(geo, basis)
    return True


class SimplisticTest(unittest.TestCase):

    def test_full_ci_10k_pt2_end(self):
        self.assertTrue(run_big_test("methane", "sto-3g"))

if __name__ == '__main__':
    unittest.main()

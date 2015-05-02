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

global filename_check


def init_folder(geo, basis, mult=1, ezfio_name=None):
    '''
    Take a geo in arg (aka a existing geo.xyz in test/)
    And create the geo.ezfio with the adeguate basis and multipliciti
    DO NOT CHECK IS THE EZFIO FOLDER ALREADY EXIST
    '''

    cmd = "cp {0}/tests/{1}.xyz .".format(qpackage_root, geo)
    subprocess.check_call([cmd], shell=True)

    if not ezfio_name:
        ezfio_name = geo

    cmd = "qp_create_ezfio_from_xyz -b {0} -m {1} {2}.xyz -o {3}.ezfio"
    subprocess.check_call([cmd.format(basis, mult, geo, ezfio_name)],
                          shell=True)


def get_error_message(l_exepected, l_cur):
    l_msg = ["Need {0} get {1} error is {2}".format(i, j, abs(i - j))
             for i, j in zip(l_exepected, l_cur)]
    return "\n".join(l_msg)


#  _
# /  |_   _   _ |    o ._  ._     _|_
# \_ | | (/_ (_ |<   | | | |_) |_| |_
#                          |
def check_disk_acess(geo, basis, mult=1):

    import uuid
    filename = str(uuid.uuid4())

    # ~#~#~#~ #
    # I n i t #
    # ~#~#~#~ #

    init_folder(geo, basis, mult, ezfio_name=filename)
    ezfio.set_file("{0}.ezfio".format(filename))

    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #
    # S e t _ p a r a m e t e r #
    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #

    # Test 1
    ezfio.bielec_integrals_disk_access_ao_integrals = "Write"
    cmd = "qp_edit -c {0}.ezfio".format(filename)
    subprocess.check_call([cmd], shell=True)

    # Test 2
    ezfio.bielec_integrals_disk_access_ao_integrals = "IculeAcess"
    cmd = "qp_edit -c {0}.ezfio".format(filename)

    try:
        subprocess.check_call([cmd], shell=True)
        return_code = False
    except subprocess.CalledProcessError:
        return_code = True

    # ~#~#~#~#~#~#~#~ #
    # F i n a l i z e #
    # ~#~#~#~#~#~#~#~ #

    if return_code:
        subprocess.call(["rm -R {0}.ezfio".format(filename)], shell=True)
    return return_code


def check_mo_guess(geo, basis, mult=1):

    import uuid
    filename = str(uuid.uuid4())

    # ~#~#~#~ #
    # I n i t #
    # ~#~#~#~ #

    init_folder(geo, basis, mult, ezfio_name=filename)
    ezfio.set_file("{0}.ezfio".format(filename))

    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #
    # S e t _ p a r a m e t e r #
    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #

    # Test 1
    ezfio.hartree_fock_mo_guess_type = "Huckel"
    cmd = "qp_edit -c {0}.ezfio".format(filename)
    subprocess.check_call([cmd], shell=True)

    # Test 2
    ezfio.hartree_fock_mo_guess_type = "IculeGuess"
    cmd = "qp_edit -c {0}.ezfio".format(filename)

    try:
        subprocess.check_call([cmd], shell=True)
        return_code = False
    except subprocess.CalledProcessError:
        return_code = True

    # ~#~#~#~#~#~#~#~ #
    # F i n a l i z e #
    # ~#~#~#~#~#~#~#~ #

    if return_code:
        subprocess.call(["rm -R {0}.ezfio".format(filename)], shell=True)
    return return_code


#  _
# /  |_   _   _ |        _. |      _   _
# \_ | | (/_ (_ |<   \/ (_| | |_| (/_ _>
#
def run_hf(geo, basis, mult=1):
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

    init_folder(geo, basis, mult)
    ezfio.set_file("{0}.ezfio".format(geo))

    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #
    # S e t _ p a r a m e t e r #
    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #
    ezfio.bielec_integrals_direct = False
    ezfio.bielec_integrals_threshold_ao = 1.e-15
    ezfio.bielec_integrals_disk_access_ao_integrals = "None"

    ezfio.bielec_integrals_threshold_mo = 1.e-15
    ezfio.bielec_integrals_disk_access_mo_integrals = "None"

    ezfio.hartree_fock_mo_guess_type = "Huckel"
    ezfio.hartree_fock_thresh_scf = 1.e-10
    ezfio.hartree_fock_n_it_scf_max = 100

    ezfio.pseudo_integrals_do_pseudo = False

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


def hf_then_10k_test(geo, basis):
    if not has_hf_alredy:
        run_hf(geo, basis)

    try:
        run_full_ci_10k_pt2_end(geo, basis)
        return_code = True
    except:
        return_code = False

    # ~#~#~#~#~#~#~#~ #
    # F i n a l i z e #
    # ~#~#~#~#~#~#~#~ #

    if return_code:
        subprocess.call(["rm -R {0}.ezfio".format(geo)], shell=True)
    return return_code


#  _
# /  |_   _   _ |     _. ._   _  _  ._      _  ._ _|_
# \_ | | (/_ (_ |<   (_| |_) (_ (_) | | \/ (/_ |   |_
#                      | | __
def check_convert(path_out):
    '''
    Path_out is the out_file
    '''

    # ~#~#~#~#~#~#~#~#~#~ #
    # R e f _ e n e r g y #
    # ~#~#~#~#~#~#~#~#~#~ #

    ref_energy = defaultdict(dict)

    ref_energy["HBO.out"] = -100.0185822589

    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #
    # S e t _ p a r a m e t e r #
    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #

    cmd = "cp {0}/tests/{1} .".format(qpackage_root, path_out)
    subprocess.check_call([cmd], shell=True)

    cmd = "qp_convert_output_to_ezfio.py {0}".format(path_out)
    subprocess.check_call([cmd], shell=True)

    # Test 2
    cmd = "qp_edit -c {0}.ezfio".format(path_out)
    subprocess.check_call([cmd], shell=True)

    cmd = "qp_run SCF {0}.ezfio".format(path_out)
    subprocess.check_call([cmd], shell=True)

    # ~#~#~#~#~ #
    # C h e c k #
    # ~#~#~#~#~ #

    ezfio.set_file("{0}.ezfio".format(path_out))

    cur_e = ezfio.get_hartree_fock_energy()
    ref_e = ref_energy[path_out]

    if abs(cur_e - ref_e) <= precision:
        subprocess.call(["rm {0}".format(path_out)], shell=True)
        subprocess.call(["rm -R {0}.ezfio".format(path_out)], shell=True)
        return True
    else:
        raise ValueError(get_error_message([ref_e], [cur_e]))

# ___
#  |  _   _ _|_
#  | (/_ _>  |_
#
class ValueTest(unittest.TestCase):

    def test_full_ci_10k_pt2_end(self):
        self.assertTrue(hf_then_10k_test("methane", "sto-3g"))

    def test_check_convert_hf_energy(self):
        self.assertTrue(check_convert("HBO.out"))


class InputTest(unittest.TestCase):

    def test_check_disk_acess(self):
        self.assertTrue(check_disk_acess("methane", "un-ccemd-ref"))

    def test_check_mo_guess(self):
        self.assertTrue(check_mo_guess("methane", "maug-cc-pVDZ"))


if __name__ == '__main__':
    unittest.main()

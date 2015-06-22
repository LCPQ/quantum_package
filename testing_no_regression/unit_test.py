#!/usr/bin/env python
# -*- coding: utf-8 -*-
import unittest
import subprocess
import os
import sys

qpackage_root = os.environ['QP_ROOT']

EZFIO = "{0}/install/EZFIO".format(qpackage_root)
sys.path = [EZFIO + "/Python"] + sys.path

from ezfio import ezfio
from collections import defaultdict
from collections import namedtuple

Energy = namedtuple('Energy', ['without_pseudo', 'with_pseudo'])

# ~#~#~ #
# O p t #
# ~#~#~ #

precision = 5.e-7

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


def init_folder(geo, basis, mult=1, pseudo=False, ezfio_name=None):
    '''
    Take a geo in arg (aka a existing geo.xyz in test/)
    And create the geo.ezfio with the adeguate basis and multipliciti
    DO NOT CHECK IS THE EZFIO FOLDER ALREADY EXIST
    '''

    if not ezfio_name:
        ezfio_name = geo

    if pseudo:
        cmd = "qp_create_ezfio_from_xyz -b {0} -m {1} {2}.xyz -p -o {3}.ezfio"
    else:
        cmd = "qp_create_ezfio_from_xyz -b {0} -m {1} {2}.xyz -o {3}.ezfio"

    subprocess.check_call([cmd.format(basis, mult, geo, ezfio_name)],
                          shell=True)

def get_error_message(l_exepected, l_cur):
    l_msg = ["Need {0} get {1} error is {2}".format(i, j, abs(i - j))
             for i, j in zip(l_exepected, l_cur)]
    return "\n" + "\n".join(l_msg)


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
    ezfio.integrals_bielec_disk_access_ao_integrals = "Write"
    cmd = "qp_edit -c {0}.ezfio".format(filename)
    subprocess.check_call([cmd], shell=True)

    # Test 2
    ezfio.integrals_bielec_disk_access_ao_integrals = "IculeAcess"
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
def run_hf(geo, basis, mult=1, pseudo=False, remove_after_sucess=True):
    """
    Run a simle by default hf
    EZFIO path = geo.ezfio
    """

    # ~#~#~#~#~#~#~#~#~#~ #
    # R e f _ e n e r g y #
    # ~#~#~#~#~#~#~#~#~#~ #

    ref_energy = defaultdict(defaultdict)

    ref_energy["sto-3g"]["methane"] = Energy(-39.7267433402, None)
    ref_energy["vdz"]["SO2"] = Energy(None, -41.48912297776174)
#   ref_energy["vdz"]["HBO"] = Energy(None, -19.1198231418)
    ref_energy["vdz"]["HBO"] = Energy(None, -19.1198254041)

    # ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ #
    # G l o b a l _ v a r i a b l e #
    # ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ #

    global has_hf_alredy
    has_hf_alredy = True

    # ~#~#~#~ #
    # I n i t #
    # ~#~#~#~ #

    init_folder(geo, basis, mult, pseudo)
    ezfio.set_file("{0}.ezfio".format(geo))

    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #
    # S e t _ p a r a m e t e r #
    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #
    ezfio.integrals_bielec_direct = False
    ezfio.integrals_bielec_threshold_ao = 1.e-15
    ezfio.integrals_bielec_disk_access_ao_integrals = "None"

    ezfio.integrals_bielec_threshold_mo = 1.e-15
    ezfio.integrals_bielec_disk_access_mo_integrals = "None"

    ezfio.hartree_fock_mo_guess_type = "Huckel"
    ezfio.hartree_fock_thresh_scf = 1.e-10
    ezfio.hartree_fock_n_it_scf_max = 100

    ezfio.pseudo_do_pseudo = pseudo

    # ~#~#~ #
    # R u n #
    # ~#~#~ #

#    cmd = "{0}/Hartree_Fock/SCF {1}.ezfio/".format(QP_src,geo)
    cmd = "qp_run SCF {0}.ezfio/".format(geo)
    subprocess.check_call([cmd], shell=True)

    # ~#~#~#~#~ #
    # C h e c k #
    # ~#~#~#~#~ #

    cur_e = ezfio.get_hartree_fock_energy()

    ref_e = ref_energy[basis][geo]
    if pseudo:
        ref_e = ref_e.with_pseudo
    else:
        ref_e = ref_e.without_pseudo

    if abs(cur_e - ref_e) <= precision:

        if remove_after_sucess:
            subprocess.call(["rm -R {0}.ezfio".format(geo)], shell=True)

        return True

    else:
        raise ValueError(get_error_message([ref_e], [cur_e]))


def run_full_ci_10k_pt2_end(geo, basis, pseudo):
    """
    Run a Full_ci with 10k with the TruePT2
    EZFIO path = geo.ezfio
    """

    # ~#~#~#~#~#~#~#~#~#~ #
    # R e f _ e n e r g y #
    # ~#~#~#~#~#~#~#~#~#~ #

    ref_energy_var = defaultdict(dict)
    ref_energy_pt2 = defaultdict(dict)

    ref_energy_var["sto-3g"]["methane"] = Energy(-39.8058687211, None)
    ref_energy_pt2["sto-3g"]["methane"] = Energy(-39.8059180427, None)

    # ~#~#~#~ #
    # I n i t #
    # ~#~#~#~ #

    ezfio.set_file("{0}.ezfio".format(geo))

    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #
    # S e t _ p a r a m e t e r #
    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #

    ezfio.determinants_n_det_max = 10000
    ezfio.determinants_n_det_max_jacobi = 10000
    ezfio.determinants_n_states = 1
    ezfio.determinants_read_wf = 1
    ezfio.determinants_s2_eig = False

    ezfio.determinants_threshold_generators = 0.99
    ezfio.determinants_threshold_selectors = 0.999

    ezfio.perturbation_do_pt2_end = True
    ezfio.perturbation_pt2_max = 1.e-4

    # ~#~#~ #
    # R u n #
    # ~#~#~ #

#    cmd = "{0}/Full_CI/full_ci {1}.ezfio/".format(QP_src,geo)
    cmd = "qp_run full_ci {0}.ezfio/".format(geo)
    subprocess.check_call([cmd], shell=True)

    # ~#~#~#~#~ #
    # C h e c k #
    # ~#~#~#~#~ #

    cur_var = ezfio.get_full_ci_energy()
    cur_pt2 = ezfio.get_full_ci_energy_pt2()

    ref_var = ref_energy_var[basis][geo]
    ref_pt2 = ref_energy_pt2[basis][geo]

    if pseudo:
        ref_var = ref_var.with_pseudo
        ref_pt2 = ref_pt2.with_pseudo
    else:
        ref_var = ref_var.without_pseudo
        ref_pt2 = ref_pt2.without_pseudo

    t = [abs(cur_var - ref_var) <= precision,
         abs(cur_pt2 - ref_pt2) <= precision]

    if all(t):
        return True
    else:
        raise ValueError(get_error_message([ref_var, ref_pt2],
                                           [cur_var, cur_pt2]))


def hf_then_10k_test(geo, basis, mult=1, pseudo=False):

    run_hf(geo, basis, mult, pseudo, remove_after_sucess=False)

    try:
        run_full_ci_10k_pt2_end(geo, basis, pseudo)
    except:
        raise
    else:
        return_code = True

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
        subprocess.call(["rm -R {0}.ezfio".format(path_out)], shell=True)
        return True
    else:
        raise ValueError(get_error_message([ref_e], [cur_e]))


# ___
#  |  _   _ _|_
#  | (/_ _>  |_
#
class ValueTest(unittest.TestCase):

    def test_hf_then_full_ci_10k_pt2_end(self):
        self.assertTrue(hf_then_10k_test(geo="methane",
                                         basis="sto-3g",
                                         mult=1,
                                         pseudo=False))

    def test_hf(self):
        self.assertTrue(run_hf(geo="HBO",
                               basis="vdz",
                               mult=1,
                               pseudo=True))


class ConvertTest(unittest.TestCase):
    def test_check_convert_hf_energy(self):
        self.assertTrue(check_convert("HBO.out"))


class InputTest(unittest.TestCase):

    def test_check_disk_acess(self):
        self.assertTrue(check_disk_acess(geo="methane",
                                         basis="un-ccemd-ref"))

    def test_check_mo_guess(self):
        self.assertTrue(check_mo_guess(geo="methane",
                                       basis="maug-cc-pVDZ"))

if __name__ == '__main__':
    unittest.main()

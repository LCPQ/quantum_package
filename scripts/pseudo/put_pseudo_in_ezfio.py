#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Create the pseudo potential for a given atom

Usage:
    put_pseudo_in_ezfio.py <ezfio_path> <pseudo_name>  [<db_dump_path>]

Help:
    atom is the Abreviation of the atom
"""


import os
import sys
from docopt import docopt

from subprocess import Popen, PIPE

qpackage_root = os.environ['QP_ROOT']

EZFIO = "{0}/install/EZFIO".format(qpackage_root)
sys.path = [EZFIO + "/Python"] + sys.path

from ezfio import ezfio

import re
p = re.compile(ur'\|(\d+)><\d+\|')


def get_pseudo_str(db_dump_path,pseudo_name,l_atom):
    """
    Run EMSL_local for getting the str of the pseudo potential

    str_ele :
        Element Symbol: Na
        Number of replaced protons: 10
        Number of projectors: 2

        Pseudopotential data:

        Local component:
        Coeff.      r^n Exp.
        1.00000000  -1  5.35838717
        5.35838717  1   3.67918975
        -2.07764789 0   1.60507673

        Non-local component:
        Coeff.      r^n Exp.        Proj.
        10.69640234 0   1.32389367  |0><0|
        10.11238853 0   1.14052020  |1><1|
    """

    EMSL_root = "{0}/install/emsl/".format(qpackage_root)
    EMSL_path = "{0}/EMSL_api.py".format(EMSL_root)

    str_ = ""

    for a in l_atom:

        if a is not 'X':
            l_cmd_atom = ["--atom", a]

            l_cmd_head = [EMSL_path, "get_basis_data",
                          "--db_dump_path", db_dump_path,
                          "--basis", pseudo_name]

            process = Popen(l_cmd_head + l_cmd_atom, stdout=PIPE, stderr=PIPE)

            stdout, _ = process.communicate()
            str_ += stdout.strip() + "\n"

        else: # Dummy atoms
            str_ += """Element Symbol: X
Number of replaced protons: 0
Number of projectors: 0

Pseudopotential data:

Local component: 
Coeff.		r^n	Exp.
0.0  -1   0.
0.0   1   0.
0.0   0   0.

Non-local component: 
Coeff.		r^n	Exp.		Proj.
"""
    return str_


def get_v_n_dz_local(str_ele):
    """
    From a str_ele of the pseudo (aka only one ele in the str)
    get the list ussefull for the Local potential : v_k n_k and dz_k
    """
    l_v_k = []
    l_n_k = []
    l_dz_k = []

    for l in str_ele.splitlines():
        try:
            v, n, dz = l.split()
            v = float(v)
            n = int(n)
            dz = float(dz)
        except ValueError:
            pass
        else:
            l_v_k.append(v)
            l_n_k.append(n)
            l_dz_k.append(dz)

    return l_v_k, l_n_k, l_dz_k


def get_v_n_dz_l_nonlocal(str_ele):
    """
    From a str_ele of the pseudo (aka only one ele in the str)
    get the list ussefull for the non Local potential
         v_kl (v, l)
         n_k (v, l)
        dz_k (dz ,l)
    """
    l_v_kl = []
    l_n_kl = []
    l_dz_kl = []

    for l in str_ele.splitlines():
        try:
            v, n, dz, proj = l.split()
            v = float(v)
            n = int(n)
            dz = float(dz)
            l = int(p.match(proj).group(1))

        except ValueError:
            pass
        else:
            l_v_kl.append([v])
            l_n_kl.append([n])
            l_dz_kl.append([dz])

    if not l_v_kl:
        l_v_kl.append([0.])
        l_n_kl.append([0])
        l_dz_kl.append([0.])

    return l_v_kl, l_n_kl, l_dz_kl


def get_zeff_alpha_beta(str_ele):
    """
    Return the the zeff, alpha num elec and beta num elec
        Assert ezfio_set_file alredy defined
    """

    import re

    # ___
    #  |  ._  o _|_
    # _|_ | | |  |_
    #

    # ~#~#~#~#~#~#~ #
    # s t r _ e l e #
    # ~#~#~#~#~#~#~ #

#    m = re.search('Element Symbol: ([a-zA-Z]+)', str_ele)
#    name = m.group(1).capitalize()
    name = str_ele.split("\n")[0].strip().capitalize()

    m = re.search('Number of replaced protons: (\d+)', str_ele)
    z_remove = int(m.group(1))

    #  _
    # |_) _. ._ _  _
    # |  (_| | _> (/_
    #

    from elts_num_ele import name_to_elec
    from math import ceil, floor
    z = name_to_elec[name]

    z_eff = z - z_remove

    alpha = int(ceil(z_remove / 2.))
    beta = int(floor(z_remove / 2.))

    # Remove more alpha, than beta

    #  _
    # |_)  _ _|_     ._ ._
    # | \ (/_ |_ |_| |  | |
    #

    return [z_remove, z_eff, alpha, beta]


def add_zero(array, size, type):
    for add in xrange(len(array), size):
        array.append([type(0)])

    return array


def make_it_square(matrix, dim, type=float):
    """
    matix the matrix to square
    dim array  [lmax, kmax]
    type the null value you want
        [[[28.59107316], [19.37583724]], [[50.25646328]]]
            =>
        [[[28.59107316], [19.37583724]], [[50.25646328], [0.0]]]
    """

    lmax = dim[0]
    kmax = dim[1]

    for l_list in matrix:

        l_list = add_zero(l_list, lmax, type)

        for k_list in list_:
            k_list = add_zero(k_list, kmax, type)

    return matrix

def full_path(path):
    path = os.path.expanduser(path)
    path = os.path.expandvars(path)
    path = os.path.abspath(path)
    return path

if __name__ == "__main__":
    arguments = docopt(__doc__)
    # ___
    #  |  ._  o _|_
    # _|_ | | |  |_
    #

    # ~#~#~#~#~ #
    # E Z F I O #
    # ~#~#~#~#~ #

    ezfio_path = full_path(arguments["<ezfio_path>"])
    ezfio.set_file("{0}".format(ezfio_path))

    # ~#~#~#~#~#~#~#~#~#~#~ #
    # P s e u d o _ d a t a #
    # ~#~#~#~#~#~#~#~#~#~#~ #

    if arguments["<db_dump_path>"]:
        db_dump_path = full_path(arguments["<db_dump_path>"])
    else:
        db_dump_path= full_path("{0}/data/BFD-Pseudo.dump".format(qpackage_root))

    pseudo_name = arguments["<pseudo_name>"]
    l_ele = ezfio.get_nuclei_nucl_label()

    str_ = get_pseudo_str(db_dump_path,pseudo_name,l_ele)

    #  _
    # |_) _. ._ _  _
    # |  (_| | _> (/_
    #

    l_str_ele = [str_ele for str_ele in str_.split("Element Symbol: ")
                 if str_ele]

    for i in "l_zeff l_remove v_k n_k dz_k v_kl n_kl dz_kl".split():
        exec("{0} = []".format(i))

    alpha_tot = 0
    beta_tot = 0

    for str_ele in l_str_ele:

        # ~#~#~#~#~ #
        # S p l i t #
        # ~#~#~#~#~ #

        l = str_ele.find("Local component:")
        nl = str_ele.find("Non-local component")

        # ~#~#~#~#~ #
        # L o c a l #
        # ~#~#~#~#~ #

        l_v, l_n, l_dz = get_v_n_dz_local(str_ele[l:nl])

        v_k.append(l_v)
        n_k.append(l_n)
        dz_k.append(l_dz)

        # ~#~#~#~#~#~#~#~#~ #
        # N o n _ L o c a l #
        # ~#~#~#~#~#~#~#~#~ #

        l_v_kl, l_n_kl, l_dz_kl = get_v_n_dz_l_nonlocal(str_ele[nl:])

        v_kl.append(l_v_kl)
        n_kl.append(l_n_kl)
        dz_kl.append(l_dz_kl)

        # ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ #
        # Z _ e f f , a l p h a / b e t a _ e l e c #
        # ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ #

        zremove, zeff, alpha, beta = get_zeff_alpha_beta(str_ele)

        alpha_tot += alpha
        beta_tot += beta
        l_zeff.append(zeff)
        l_remove.append(zremove)

    #                                  _
    #  /\   _|  _|   _|_  _     _  _ _|_ o  _
    # /--\ (_| (_|    |_ (_)   (/_ /_ |  | (_)
    #

    # ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ #
    # Z _ e f f , a l p h a / b e t a _ e l e c #
    # ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ #

    ezfio.nuclei_nucl_charge = l_zeff
    ezfio.pseudo_nucl_charge_remove = l_remove

    alpha_tot = ezfio.get_electrons_elec_alpha_num() - alpha_tot
    beta_tot = ezfio.get_electrons_elec_beta_num() - beta_tot

    ezfio.electrons_elec_alpha_num = alpha_tot
    ezfio.electrons_elec_beta_num = beta_tot

    # Change all the array 'cause EZFIO
    #   v_kl (v, l) => v_kl(l,v)
    #    v_kl => zip(*_v_kl)
    # [[7.0, 79.74474797, -49.45159098], [1.0, 5.41040609, -4.60151975]]
    # [(7.0, 1.0), (79.74474797, 5.41040609), (-49.45159098, -4.60151975)]

    # ~#~#~#~#~ #
    # L o c a l #
    # ~#~#~#~#~ #

    klocmax = max([len(i) for i in v_k])
    ezfio.pseudo_pseudo_klocmax = klocmax

    ezfio.pseudo_pseudo_v_k = zip(*v_k)
    ezfio.pseudo_pseudo_n_k = zip(*n_k)
    ezfio.pseudo_pseudo_dz_k = zip(*dz_k)

    # ~#~#~#~#~#~#~#~#~ #
    # N o n _ L o c a l #
    # ~#~#~#~#~#~#~#~#~ #

    lmax = max([len(i) for i in v_kl])
    kmax = max([len(sublist) for list_ in v_kl for sublist in list_])

    ezfio.pseudo_pseudo_lmax = lmax - 1
    ezfio.pseudo_pseudo_kmax = kmax

    v_kl = make_it_square(v_kl, [lmax, kmax])
    n_kl = make_it_square(n_kl, [lmax, kmax], int)
    dz_kl = make_it_square(dz_kl, [lmax, kmax])

    ezfio.pseudo_pseudo_v_kl = zip(*v_kl)
    ezfio.pseudo_pseudo_n_kl = zip(*n_kl)
    ezfio.pseudo_pseudo_dz_kl = zip(*dz_kl)

    ezfio.pseudo_do_pseudo = True

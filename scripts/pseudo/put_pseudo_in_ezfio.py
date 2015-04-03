#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Create the pseudo potential for a given atom

Usage:
    put_pseudo_in_ezfio.py --ezfio=<path>  --atom=<atom>...
"""


import os
import sys
from docopt import docopt

from subprocess import Popen, PIPE

qpackage_root = os.environ['QPACKAGE_ROOT']

EZFIO = "{0}/EZFIO".format(qpackage_root)
sys.path = [EZFIO + "/Python"] + sys.path

from ezfio import ezfio

import re
p = re.compile(ur'\|(\d+)><\d+\|')


def get_pseudo_str(l_atom):
    """
    Run EMSL_local for geting the str of the speudo potential
    """

    EMSL_root = "{0}/EMSL_Basis/".format(qpackage_root)
    EMSL_path = "{0}/EMSL_api.py".format(EMSL_root)
    db_path = "{0}/db/Pseudo.db".format(EMSL_root)

    l_cmd_atom = []
    for a in l_atom:
        l_cmd_atom += ["--atom", a]

    l_cmd_head = [EMSL_path, "get_basis_data",
                  "--db_path", db_path,
                  "--basis", "BFD-Pseudo"]

    process = Popen(l_cmd_head + l_cmd_atom, stdout=PIPE, stderr=PIPE)

    stdout, _ = process.communicate()
    return stdout.strip()


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
            l_v_kl.append((v, l))
            l_n_kl.append((n, l))
            l_dz_kl.append((dz, l))

    return l_v_kl, l_n_kl, l_dz_kl


if __name__ == "__main__":
    arguments = docopt(__doc__)

    # ___
    #  |  ._  o _|_
    # _|_ | | |  |_
    #

    # ~#~#~#~#~ #
    # E Z F I O #
    # ~#~#~#~#~ #

    ezfio_path = arguments["--ezfio"]
    ezfio_path = os.path.expanduser(ezfio_path)
    ezfio_path = os.path.expandvars(ezfio_path)
    ezfio_path = os.path.abspath(ezfio_path)

    ezfio.set_file("{0}".format(ezfio_path))

    # ~#~#~#~#~#~#~#~#~#~#~ #
    # P s e u d o _ d a t a #
    # ~#~#~#~#~#~#~#~#~#~#~ #

    l_ele = arguments["--atom"]
    str_ = get_pseudo_str(l_ele)

    #  _
    # |_) _. ._ _  _
    # |  (_| | _> (/_
    #

    l_str_ele = [str_ele for str_ele in str_.split("Element Symbol: ")
                 if str_ele]

    for str_ele in l_str_ele:

        # ~#~#~#~#~ #
        # S p l i t #
        # ~#~#~#~#~ #

        l = str_ele.find("Local component:")
        nl = str_ele.find("Non-local component")

        # ~#~#~#~#~ #
        # L o c a l #
        # ~#~#~#~#~ #

        print "local"

        l_v, l_n, l_dz = get_v_n_dz_local(str_ele[l:nl])

        print l_v
        print l_n
        print l_dz

        ezfio.pseudo_klocmax = len(l_v)
        ezfio.pseudo_v_k = l_v
        ezfio.pseudo_n_k = l_n
        ezfio.pseudo_dz_k = l_dz

        # ~#~#~#~#~#~#~#~#~ #
        # N o n _ L o c a l #
        # ~#~#~#~#~#~#~#~#~ #

        print "non local"

        l_v_kl, l_n_kl, l_dz_kl = get_v_n_dz_l_nonlocal(str_ele[nl:])

        print l_v_kl
        print l_n_kl
        print l_dz_kl

        if l_v_kl:
            ezfio.pseudo_lmax = max([i[1] for i in l_v_kl]) + 1
            ezfio.pseudo_kmax = len(l_v_kl)
            ezfio.pseudo_v_kl = l_v_kl
            ezfio.pseudo_n_kl = l_n_kl
            ezfio.pseudo_dz_kl = l_dz_kl

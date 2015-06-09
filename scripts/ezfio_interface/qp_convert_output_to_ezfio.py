#!/usr/bin/env python
"""
convert output of gamess/GAU$$IAN to ezfio

Usage:
    qp_convert_output_to_ezfio.py <file.out> [--ezfio=<folder.ezfio>]

Option:
    file.out is the file to check (like gamess.out)
    folder.ezfio is the name you whant for the ezfio
        (by default is file.out.ezfio)

"""


import sys
import os
from functools import reduce


# ~#~#~#~#~#~#~#~ #
# Add to the path #
# ~#~#~#~#~#~#~#~ #


try:
    QP_ROOT = os.environ["QP_ROOT"]
except:
    print "Error: QP_ROOT environment variable not found."
    sys.exit(1)
else:
    sys.path = [QP_ROOT + "/install/EZFIO/Python",
                QP_ROOT + "/resultsFile",
                QP_ROOT + "/scripts"] + sys.path

# ~#~#~#~#~#~ #
# I m p o r t #
# ~#~#~#~#~#~ #

from ezfio import ezfio


try:
    from resultsFile import *
except:
    print "Error: resultsFile Python library not installed"
    sys.exit(1)

from docopt import docopt

#  _
# |_    ._   _ _|_ o  _  ._
# | |_| | | (_  |_ | (_) | |
#


def write_ezfio(res, filename):

    res.clean_uncontractions()
    ezfio.set_file(filename)

    #  _
    # |_ |  _   _ _|_ ._ _  ._   _
    # |_ | (/_ (_  |_ | (_) | | _>
    #
    ezfio.set_electrons_elec_alpha_num(res.num_alpha)
    ezfio.set_electrons_elec_beta_num(res.num_beta)

    #
    # |\ |      _ |  _  o
    # | \| |_| (_ | (/_ |
    #

    # ~#~#~#~ #
    # I n i t #
    # ~#~#~#~ #

    charge = []
    coord_x = []
    coord_y = []
    coord_z = []

    # ~#~#~#~#~#~#~ #
    # P a r s i n g #
    # ~#~#~#~#~#~#~ #

    for a in res.geometry:
        charge.append(a.charge)
        if res.units == 'BOHR':
            coord_x.append(a.coord[0])
            coord_y.append(a.coord[1])
            coord_z.append(a.coord[2])
        else:
            coord_x.append(a.coord[0] / a0)
            coord_y.append(a.coord[1] / a0)
            coord_z.append(a.coord[2] / a0)

    # ~#~#~#~#~ #
    # W r i t e #
    # ~#~#~#~#~ #

    ezfio.set_nuclei_nucl_num(len(res.geometry))
    ezfio.set_nuclei_nucl_charge(charge)

    # Transformt H1 into H
    import re
    p = re.compile(ur'(\d*)$')
    label = [p.sub("", x.name).capitalize() for x in res.geometry]
    ezfio.set_nuclei_nucl_label(label)

    ezfio.set_nuclei_nucl_coord(coord_x + coord_y + coord_z)

    #                 _
    #   /\   _   _   |_)  _.  _ o  _
    #  /--\ (_) _>   |_) (_| _> | _>
    #

    # ~#~#~#~ #
    # I n i t #
    # ~#~#~#~ #

    import string
    at = []
    num_prim = []
    power_x = []
    power_y = []
    power_z = []
    coefficient = []
    exponent = []

    res.clean_contractions()
    res.convert_to_cartesian()

    # ~#~#~#~#~#~#~ #
    # P a r s i n g #
    # ~#~#~#~#~#~#~ #

    for b in res.basis:
        c = b.center
        for i, atom in enumerate(res.geometry):
            if atom.coord == c:
                at.append(i + 1)
        num_prim.append(len(b.prim))
        s = b.sym
        power_x.append(string.count(s, "x"))
        power_y.append(string.count(s, "y"))
        power_z.append(string.count(s, "z"))
        coefficient.append(b.coef)
        exponent.append([p.expo for p in b.prim])

    # ~#~#~#~#~ #
    # W r i t e #
    # ~#~#~#~#~ #

    ezfio.set_ao_basis_ao_num(len(res.basis))
    ezfio.set_ao_basis_ao_nucl(at)
    ezfio.set_ao_basis_ao_prim_num(num_prim)
    ezfio.set_ao_basis_ao_power(power_x + power_y + power_z)

    # ~#~#~#~#~#~#~ #
    # P a r s i n g #
    # ~#~#~#~#~#~#~ #

    prim_num_max = ezfio.get_ao_basis_ao_prim_num_max()

    for i in range(len(res.basis)):
        coefficient[
            i] += [0. for j in range(len(coefficient[i]), prim_num_max)]
        exponent[i] += [0. for j in range(len(exponent[i]), prim_num_max)]

    coefficient = reduce(lambda x, y: x + y, coefficient, [])
    exponent = reduce(lambda x, y: x + y, exponent, [])

    coef = []
    expo = []
    for i in range(prim_num_max):
        for j in range(i, len(coefficient), prim_num_max):
            coef.append(coefficient[j])
            expo.append(exponent[j])

    # ~#~#~#~#~ #
    # W r i t e #
    # ~#~#~#~#~ #

    ezfio.set_ao_basis_ao_coef(coef)
    ezfio.set_ao_basis_ao_expo(expo)
    ezfio.set_ao_basis_ao_basis("Read by resultsFile")

    #                _
    # |\/|  _   _   |_)  _.  _ o  _
    # |  | (_) _>   |_) (_| _> | _>
    #

    # ~#~#~#~ #
    # I n i t #
    # ~#~#~#~ #

    MoTag = res.determinants_mo_type
    ezfio.set_mo_basis_mo_label('Orthonormalized')
    MO_type = MoTag
    allMOs = res.mo_sets[MO_type]

    # ~#~#~#~#~#~#~ #
    # P a r s i n g #
    # ~#~#~#~#~#~#~ #

    try:
        closed = [(allMOs[i].eigenvalue, i) for i in res.closed_mos]
        active = [(allMOs[i].eigenvalue, i) for i in res.active_mos]
        virtual = [(allMOs[i].eigenvalue, i) for i in res.virtual_mos]
    except:
        closed = []
        virtual = []
        active = [(allMOs[i].eigenvalue, i) for i in range(len(allMOs))]

    closed = map(lambda x: x[1], closed)
    active = map(lambda x: x[1], active)
    virtual = map(lambda x: x[1], virtual)
    MOindices = closed + active + virtual

    MOs = []
    for i in MOindices:
        MOs.append(allMOs[i])

    mo_tot_num = len(MOs)
    while len(MOindices) < mo_tot_num:
        MOindices.append(len(MOindices))

    MOmap = list(MOindices)
    for i in range(len(MOindices)):
        MOmap[i] = MOindices.index(i)

    energies = []
    for i in xrange(mo_tot_num):
        energies.append(MOs[i].eigenvalue)

    if res.occ_num is not None:
        OccNum = []
        for i in MOindices:
            OccNum.append(res.occ_num[MO_type][i])

        while len(OccNum) < mo_tot_num:
            OccNum.append(0.)

    MoMatrix = []
    sym0 = [i.sym for i in res.mo_sets[MO_type]]
    sym = [i.sym for i in res.mo_sets[MO_type]]
    for i in xrange(len(sym)):
        sym[MOmap[i]] = sym0[i]

    MoMatrix = []
    for i in xrange(len(MOs)):
        m = MOs[i]
        for coef in m.vector:
            MoMatrix.append(coef)

    while len(MoMatrix) < len(MOs[0].vector) ** 2:
        MoMatrix.append(0.)

    # ~#~#~#~#~ #
    # W r i t e #
    # ~#~#~#~#~ #

    ezfio.set_mo_basis_mo_tot_num(mo_tot_num)
    ezfio.set_mo_basis_mo_occ(OccNum)
    ezfio.set_mo_basis_mo_coef(MoMatrix)

    # ______                   _
    # | ___ \                 | |
    # | |_/ /__  ___ _   _  __| | ___
    # |  __/ __|/ _ \ | | |/ _` |/ _ \
    # | |  \__ \  __/ |_| | (_| | (_) |
    # \_|  |___/\___|\__,_|\__,_|\___/
    #

    ezfio.set_pseudo_do_pseudo(False)


def get_full_path(file_path):
    file_path = os.path.expanduser(file_path)
    file_path = os.path.expandvars(file_path)
    file_path = os.path.abspath(file_path)
    return file_path

if __name__ == '__main__':
    arguments = docopt(__doc__)

    file_ = get_full_path(arguments['<file.out>'])

    if arguments["--ezfio"]:
        ezfio_file = get_full_path(arguments["--ezfio"])
    else:
        ezfio_file = "{0}.ezfio".format(file_)

    try:
        res_file = getFile(file_)
    except:
        raise
    else:
        print file_, 'recognized as', str(res_file).split('.')[-1].split()[0]

    write_ezfio(res_file, ezfio_file)

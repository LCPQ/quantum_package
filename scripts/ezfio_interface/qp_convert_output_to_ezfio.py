#!/usr/bin/env python
"""
convert output of gamess/GAU$$IAN to ezfio

Usage:
    qp_convert_output_to_ezfio.py <file.out> [-o <ezfio_directory>]

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
    sys.path = [ QP_ROOT + "/install/EZFIO/Python", 
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

    while len(MoMatrix) < len(MOs[0].vector)**2:
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

    # INPUT
    # {% for lanel,zcore, l_block in l_atom  $}
    #       #local l_block l=0}
    #       {label} GEN {zcore} {len(l_block)-1 #lmax_block}
    #       {% for l_param in l_block%}
    #                {len(l_param) # list of parameter aka n_max_bock_max(n)}
    #                {% for coef,n,zeta for l_param}
    #                    {coef,n, zeta}


    # OUTPUT

    # Local are 1 array padded by max(n_max_block) when l == 0  (output:k_loc_max)
    # v_k[n-2][atom] = value

    #Non Local are 2 array padded with max of lmax_block when l!=0 (output:lmax+1)  and max(n_max_block)whem l !=0 (kmax)
    # v_kl[l][n-2][atom] = value

    def pad(array, size, value=0):
        new_array = array
        for add in xrange(len(array), size):
            new_array.append(value)

        return new_array

    def parse_str(pseudo_str):
        '''Return 4d array  atom,l,n, attribute (attribute is coef, n, zeta)'''
        matrix = []
        array_l_max_block = []
        array_z_remove = []

        for block in [b for b in pseudo_str.split('\n\n') if b]:
            #First element is header, the rest are l_param 
            array_party = [i for i in re.split(r"\n\d+\n", block) if i]

            z_remove, l_max_block = map(int, array_party[0].split()[-2:])
            array_l_max_block.append(l_max_block)
            array_z_remove.append(z_remove)

            x = []
            for l in array_party[1:]:
              y = []
              for coef_n_zeta in l.split('\n'):
                z = coef_n_zeta.split()
                if z : y.append(z)
              x.append(y)
            matrix.append(x)
        return (matrix, array_l_max_block, array_z_remove)

    def get_local_stuff(matrix):

        matrix_local_unpad = [atom[0] for atom in matrix]
        k_loc_max = max(len(i) for i in matrix_local_unpad)

        matrix_local = [ pad(ll, k_loc_max, [0., 2, 0.]) for ll in matrix_local_unpad]
        m_coef = [[float(i[0]) for i in atom] for atom in matrix_local]
        m_n = [[int(i[1]) - 2 for i in atom] for atom in matrix_local]
        m_zeta = [[float(i[2]) for i in atom] for atom in matrix_local]
        return (k_loc_max, m_coef, m_n, m_zeta)

    def get_non_local_stuff(matrix):

        matrix_unlocal_unpad = [atom[1:] for atom in matrix]
        l_max_block = max(len(i) for i in matrix_unlocal_unpad)
        k_max = max([len(item) for row in matrix_unlocal_unpad for item in row])

        matrix_unlocal_semipaded = [[pad(item, k_max, [0., 2, 0.]) for item in row] for row in matrix_unlocal_unpad]

        empty_row = [[0., 2, 0.] for k in range(l_max_block)]
        matrix_unlocal = [  pad(ll, l_max_block, empty_row) for ll in matrix_unlocal_semipaded ]

        m_coef_noloc = [[[float(k[0]) for k in j] for j in i] for i in matrix_unlocal]
        m_n_noloc =    [[[int(k[1]) - 2 for k in j] for j in i]  for i in matrix_unlocal]
        m_zeta_noloc = [[[float(k[2]) for k in j] for j in i] for i in matrix_unlocal]

        return (l_max_block, k_max, m_coef_noloc, m_n_noloc, m_zeta_noloc)

    try:
        pseudo_str = []
        label = ezfio.get_nuclei_nucl_label()
        for ecp in res.pseudo:
          pseudo_str += [ "%(label)s GEN %(zcore)d %(lmax)d" % { "label": label[ ecp["atom"]-1 ],
                "zcore": ecp["zcore"], "lmax": ecp["lmax"] } ]
          lmax = ecp["lmax"]
          for l in [lmax] + list(range(0,lmax)):
            pseudo_str += [ "%d"%len(ecp[str(l)]) ]
            for t in ecp[str(l)]:
              pseudo_str += [ "%f  %d  %f"%t ]
          pseudo_str += [""]
        pseudo_str = "\n".join(pseudo_str)
             
        matrix, array_l_max_block, array_z_remove = parse_str(pseudo_str)
        array_z_remove = map(float,array_z_remove)
    except:
        ezfio.set_pseudo_do_pseudo(False)
    else:
        ezfio.set_pseudo_do_pseudo(True)
        
        # ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ #
        # Z _ e f f , a l p h a / b e t a _ e l e c #
        # ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ #

        ezfio.set_pseudo_nucl_charge_remove(array_z_remove)
        charge = ezfio.get_nuclei_nucl_charge()
        charge = [ i - j for i, j in zip(charge, array_z_remove) ] 
        ezfio.set_nuclei_nucl_charge (charge)

        import math
        num_elec_diff = sum(array_z_remove)/2
        nalpha = ezfio.get_electrons_elec_alpha_num() - num_elec_diff
        nbeta  = ezfio.get_electrons_elec_beta_num() - num_elec_diff

        ezfio.set_electrons_elec_alpha_num(nalpha)
        ezfio.set_electrons_elec_beta_num( nbeta )

        # Change all the array 'cause EZFIO
        #   v_kl (v, l) => v_kl(l,v)
        #    v_kl => zip(*_v_kl)
        # [[7.0, 79.74474797, -49.45159098], [1.0, 5.41040609, -4.60151975]]
        # [(7.0, 1.0), (79.74474797, 5.41040609), (-49.45159098, -4.60151975)]

        # ~#~#~#~#~ #
        # L o c a l #
        # ~#~#~#~#~ #

        klocmax, m_coef, m_n, m_zeta = get_local_stuff(matrix)
        ezfio.pseudo_pseudo_klocmax = klocmax

        ezfio.pseudo_pseudo_v_k  = zip(*m_coef)
        ezfio.pseudo_pseudo_n_k  = zip(*m_n)
        ezfio.pseudo_pseudo_dz_k = zip(*m_zeta)

        # ~#~#~#~#~#~#~#~#~ #
        # N o n _ L o c a l #
        # ~#~#~#~#~#~#~#~#~ #

        l_max_block, k_max, m_coef_noloc, m_n_noloc, m_zeta_noloc = get_non_local_stuff(
            matrix)

        ezfio.pseudo_pseudo_lmax = l_max_block - 1
        ezfio.pseudo_pseudo_kmax = k_max

        ezfio.pseudo_pseudo_v_kl = zip(*m_coef_noloc)
        ezfio.pseudo_pseudo_n_kl = zip(*m_n_noloc)
        ezfio.pseudo_pseudo_dz_kl = zip(*m_zeta_noloc)


def get_full_path(file_path):
    file_path = os.path.expanduser(file_path)
    file_path = os.path.expandvars(file_path)
    file_path = os.path.abspath(file_path)
    return file_path


if __name__ == '__main__':
    arguments = docopt(__doc__)

    file_ = get_full_path(arguments['<file.out>'])

    if arguments["-o"]:
        ezfio_file = get_full_path(arguments["<ezfio_directory>"])
    else:
        ezfio_file = "{0}.ezfio".format(file_)

    try:
        res_file = getFile(file_)
    except:
        raise
    else:
        print file_, 'recognized as', str(res_file).split('.')[-1].split()[0]

    write_ezfio(res_file, ezfio_file)
    if os.system("qp_run save_ortho_mos "+ezfio_file) != 0:
      print """Warning: You need to run 

         qp_run save_ortho_mos """+ezfio_file+"""

to be sure your MOs will be orthogonal, which is not the case when
the MOs are read from output files (not enough precision in output)."""



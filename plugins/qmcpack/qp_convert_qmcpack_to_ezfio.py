#!/usr/bin/python

print "#QP -> QMCPACK"

# ___           
#  |  ._  o _|_ 
# _|_ | | |  |_ 
#

from ezfio import ezfio

import os
import sys
ezfio_path = sys.argv[1]

ezfio.set_file(ezfio_path)

do_pseudo = ezfio.get_pseudo_do_pseudo()
if do_pseudo:
    print "do_pseudo True"
    from qp_path import QP_ROOT

    l_ele_path = os.path.join(QP_ROOT,"data","list_element.txt")
    with open(l_ele_path, "r") as f:
        data_raw = f.read()

    l_element_raw = data_raw.split("\n")
    l_element = [element_raw.split() for element_raw in l_element_raw]
    d_z = dict((abr, z) for (z, abr, ele) in l_element)
else:
    print "do_pseudo False"

try:
    n_det = ezfio.get_determinants_n_det()
except IOError:
    n_det = 1

if n_det == 1:
    print "multi_det False"
else:
    print "multi_det True"

#             
# |\/| o  _  _ 
# |  | | _> (_ 
#

def list_to_string(l):
    return " ".join(map(str, l))


ao_num = ezfio.get_ao_basis_ao_num()
print "ao_num", ao_num

mo_num = ezfio.get_mo_basis_mo_tot_num()
print "mo_num", mo_num

alpha = ezfio.get_electrons_elec_alpha_num()
beta = ezfio.get_electrons_elec_beta_num()
print "elec_alpha_num", alpha
print "elec_beta_num", beta
print "elec_tot_num", alpha + beta
print "spin_multiplicity", 2 * (alpha - beta) + 1

l_label = ezfio.get_nuclei_nucl_label()
l_charge = ezfio.get_nuclei_nucl_charge()
l_coord = ezfio.get_nuclei_nucl_coord()

l_coord_str = [list_to_string(i) for i in zip(*l_coord)]

print "nucl_num", len(l_label)

#  _               
# /   _   _  ._ _| 
# \_ (_) (_) | (_| 
#
print "Atomic coord in Bohr"

for i, t in enumerate(zip(l_label, l_charge, l_coord_str)):
    t_1 = d_z[t[0]] if do_pseudo else t[1]
    
    t_new = [t[0],t_1,t[2]]
    print list_to_string(t_new)

#
# Call externet process to get the sysmetry
#
import subprocess
process = subprocess.Popen(
    ['qp_print_basis', ezfio_path],
    stdout=subprocess.PIPE)
out, err = process.communicate()

basis_raw, sym_raw, _ , det_raw, _ = out.split("\n\n\n")

#  _                 __        
# |_)  _.  _ o  _   (_   _ _|_ 
# |_) (_| _> | _>   __) (/_ |_ 
#

basis_without_header = "\n".join(basis_raw.split("\n")[11:])

import re
l_basis_raw = re.split('\n\s*\n', basis_without_header)

a_already_print = []

l_basis_clean = []


for i, (a,b) in enumerate(zip(l_label,l_basis_raw)):

    if a not in a_already_print:
        l_basis_clean.append(b.replace('Atom {0}'.format(i + 1), a))
        a_already_print.append(a)
    else:
        continue

print "BEGIN_BASIS_SET\n"
print "\n\n".join(l_basis_clean)
print "END_BASIS_SET"

#       _     
# |\/| / \  _ 
# |  | \_/ _> 
#


#
# Function
#
def same_character(item1):
    return item1 == item1[0] * len(item1)


def compare_gamess_style(item1, item2):
    if len(item1) < len(item2):
        return -1
    elif len(item1) > len(item2):
        return 1
    elif same_character(item1) and same_character(item2):
        if item1 < item2:
            return -1
        else:
            return 1
    elif same_character(item1) and not same_character(item2):
        return -1
    elif not same_character(item1) and same_character(item2):
        return 1
    else:
        return compare_gamess_style(item1[:-1], item2[:-1])


def expend_sym_str(str_):
    #Expend x2 -> xx
    # yx2 -> xxy
    for i, c in enumerate(str_):
        try:
            n = int(c)
        except ValueError:
            pass
        else:
            str_ = str_[:i - 1] + str_[i - 1] * n + str_[i + 1:]

    #Order by frequency
    return "".join(sorted(str_, key=str_.count, reverse=True))


def expend_sym_l(l_l_sym):
    for l in l_l_sym:
        l[2] = expend_sym_str(l[2])

    return l_l_sym


def get_nb_permutation(str_):

    l = len(str_) - 1
    if l == 0:
        return 1
    else:
        return 2 * (2 * l + 1)


def order_l_l_sym(l_l_sym):

    l_order_mo = [i for i,_ in enumerate(l_l_sym)]

    n = 1
    for i in range(len(l_l_sym)):
        if n != 1:
            n += -1
            continue 

        l = l_l_sym[i]
        n = get_nb_permutation(l[2])

        l_l_sym[i:i + n], l_order_mo[i:i+n] = zip(*sorted(zip(l_l_sym[i:i + n],l_order_mo[i:i+n]),
                                              key=lambda x: x[0][2],
                                              cmp=compare_gamess_style))

    return l_l_sym, l_order_mo


#==========================
# We will order the symetry
#==========================

l_sym_without_header = sym_raw.split("\n")[3:-2]
l_l_sym_raw = [i.split() for i in l_sym_without_header]
print len(l_l_sym_raw)

l_l_sym_expend_sym = expend_sym_l(l_l_sym_raw)
print len(l_l_sym_expend_sym)

l_l_sym_ordered, l_order_mo = order_l_l_sym(l_l_sym_expend_sym)


#========
#MO COEF
#========
def order_phase(mo_coef):
    #Order
    mo_coef_phase = []
    import math

    for i in mo_coef:
        if abs(max(i)) > abs(min(i)):
            sign_max = math.copysign(1, max(i))
        else:
            sign_max = math.copysign(1, min(i))

        if sign_max == -1:
            ii = [-1 * l for l in i]
        else:
            ii = i

        mo_coef_phase.append(ii)
    return mo_coef_phase

def chunked(l, chunks_size):
    l_block = []
    for i in l:
        chunks = [i[x:x + chunks_size] for x in xrange(0, len(i), chunks_size)]
        l_block.append(chunks)
    return l_block


def print_mo_coef(mo_coef_block, l_l_sym):
    print ""
    print "BEGIN_MO"
    print ""
    len_block_curent = 0
    nb_block = len(mo_coef_block[0])
    for i_block in range(0, nb_block):
        a = [i[i_block] for i in mo_coef_block]
        r_ = range(len_block_curent, len_block_curent + len(a[0]))
        print " ".join([str(i + 1) for i in r_])

        len_block_curent += len(a[0])

        for l in l_l_sym:
            i = int(l[0]) - 1
            i_a = int(l[1]) - 1
            sym = l[2]

            print l_label[i_a], sym, " ".join('{: 3.8f}'.format(i)
                                              for i in a[i])

        if i_block != nb_block - 1:
            print ""
        else:
            print "END_MO"


mo_coef = ezfio.get_mo_basis_mo_coef()
mo_coef_transp = zip(*mo_coef)
mo_coef_block = chunked(mo_coef_transp, 4)
print_mo_coef(mo_coef_block, l_l_sym_ordered)

#  _                    
# |_) _  _       _|  _  
# |  _> (/_ |_| (_| (_) 
#
if do_pseudo:
    print ""
    print "BEGIN_PSEUDO"
    klocmax = ezfio.get_pseudo_pseudo_klocmax()
    kmax = ezfio.get_pseudo_pseudo_kmax()
    lmax = ezfio.get_pseudo_pseudo_lmax()

    n_k = ezfio.get_pseudo_pseudo_n_k()
    v_k = ezfio.get_pseudo_pseudo_v_k()
    dz_k = ezfio.get_pseudo_pseudo_dz_k()

    n_kl = ezfio.get_pseudo_pseudo_n_kl()
    v_kl = ezfio.get_pseudo_pseudo_v_kl()
    dz_kl = ezfio.get_pseudo_pseudo_dz_kl()

    for i, a in enumerate(l_label):

        l_str = []

        #Local
        l_dump = []
        for k in range(klocmax):
            if v_k[k][i]:
                l_ = list_to_string([v_k[k][i], n_k[k][i] + 2, dz_k[k][i]])
                l_dump.append(l_)

        l_str.append(l_dump)

        #Non local
        for l in range(lmax + 1):
            l_dump = []
            for k in range(kmax):
                if v_kl[l][k][i]:
                    l_ = list_to_string([v_kl[l][k][i], n_kl[l][k][i] + 2,
                                         dz_kl[l][k][i]])
                    l_dump.append(l_)
            if l_dump:
                l_str.append(l_dump)

        str_ = "PARAMETERS FOR {0} ON ATOM {1} WITH ZCORE {2} AND LMAX {3} ARE"
        print str_.format(a, i + 1, int(d_z[a])-int(l_charge[i]), int(len(l_str) - 1))

        for i, l in enumerate(l_str):
            str_ = "FOR L= {0} COEFF N ZETA"
            print str_.format(int(len(l_str) - i - 1))
            for ii, ll in enumerate(l):
                print " ", ii + 1, ll

    str_ = "THE ECP RUN REMOVES {0} CORE ELECTRONS, AND THE SAME NUMBER OF PROTONS."
    print str_.format(sum([int(d_z[a])-int(l_charge[i]) for i,a in enumerate(l_label)]))
    print "END_PSEUDO"

#  _         
# | \  _ _|_ 
# |_/ (/_ |_ 
#
print ""
print "BEGIN_DET"
print ""
print "mo_num", mo_num
print "det_num", n_det
print ""



token = "Determinants ::"
pos = det_raw.rfind(token) + len(token)

det_without_header = det_raw[pos+2::]

d_rep={"+":"1","-":"0"}

det_without_header = det_raw[pos+2::]


for line_raw in det_without_header.split("\n"):
    line = line_raw

    if line_raw:
        try:
            float(line)
        except ValueError:

            print line_raw.strip(), len(line_raw.strip())
            print l_order_mo, len(l_order_mo)

            line_order = [line_raw[i] for i in l_order_mo]
            line= "".join([d_rep[x] if x in d_rep else x for x in line_raw])

    print line.strip()

print "END_DET"


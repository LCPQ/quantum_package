#!/usr/bin/env python2

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
    d_z = dict((abr, z) for (z, abr, ele, _) in filter(lambda x: x != [], l_element) )
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
print "spin_multiplicity", (alpha - beta) + 1

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

basis_raw, sym_raw, _ = out.split("\n\n\n")

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
d_gms_order ={ 0:["s"],
     1:[ "x", "y", "z" ],
     2:[ "xx", "yy", "zz", "xy", "xz", "yz" ],
     3:[ "xxx", "yyy", "zzz", "xxy", "xxz", "yyx", "yyz", "zzx", "zzy", "xyz"],
     4:[ "xxxx", "yyyy", "zzzz", "xxxy", "xxxz", "yyyx", "yyyz", "zzzx", "zzzy", "xxyy", "xxzz", "yyzz", "xxyz", "yyxz", "zzxy"] }

def compare_gamess_style(item1, item2):
    n1,n2 = map(len,(item1,item2))
    assert (n1 == n2)
    try:
        l = d_gms_order[n1]
    except KeyError:
        return 0
#       raise (KeyError, "We dont handle L than 4")
    else:
        a = l.index(item1)
        b = l.index(item2)
        return cmp( a, b )

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


def n_orbital(n):
    if n==0:
        return 1
    elif n==1:
        return 3
    else:
        return 2*n_orbital(n-1)-n_orbital(n-2)+1

def get_nb_permutation(str_):
    if (str_) == 's': return 1
    else: return n_orbital(len(str_))

def order_l_l_sym(l_l_sym):
    n = 1
    iter_ = range(len(l_l_sym))
    for i in iter_:
        if n != 1:
            n += -1
            continue 

        l = l_l_sym[i]
        n = get_nb_permutation(l[2])

        l_l_sym[i:i + n] = sorted(l_l_sym[i:i + n],
                                  key=lambda x: x[2],
                                  cmp=compare_gamess_style)


    return l_l_sym


#==========================
# We will order the symetry
#==========================

l_sym_without_header = sym_raw.split("\n")[3:-2]
l_l_sym_raw = [i.split() for i in l_sym_without_header]
l_l_sym_expend_sym = expend_sym_l(l_l_sym_raw)
l_l_sym_ordered = order_l_l_sym(l_l_sym_expend_sym)

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

            print l_label[i_a], sym, " ".join('%20.15e'%i
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


psi_coef = ezfio.get_determinants_psi_coef()
psi_det = ezfio.get_determinants_psi_det()
bit_kind = ezfio.get_determinants_bit_kind()


nexcitedstate = ezfio.get_determinants_n_states()

print ""
print "BEGIN_DET"
print ""
print "mo_num", mo_num
print "det_num", n_det
print ""

if "QP_STATE" in os.environ:
  state = int(os.environ["QP_STATE"])-1
else:
  state = 0

psi_coef_small = psi_coef[state]




encode = 8*bit_kind

def bindigits(n, bits):
    s = bin(n & int("1"*bits, 2))[2:]
    return ("{0:0>%s}" % (bits)).format(s)

decode = lambda det: ''.join(bindigits(i,encode)[::-1] for i in det)[:mo_num]

MultiDetAlpha = []
MultiDetBeta = []
for coef, (det_a, det_b) in zip(psi_coef_small, psi_det):

        print coef
        MyDetA=decode(det_a)
        MyDetB=decode(det_b)
        print MyDetA 
        print MyDetB
        print ''
        MultiDetAlpha.append( det_a  )
	MultiDetBeta.append( det_b  )
print "END_DET"

import h5py
H5_qmcpack=h5py.File('MultiDet.h5','w')
groupMultiDet=H5_qmcpack.create_group("MultiDet")
groupMultiDet.create_dataset("NbDet",(1,),dtype="f8",data=len(psi_coef_small))

groupMultiDet.create_dataset("Coeff",(len(psi_coef_small),),dtype="f8",data=psi_coef)
groupMultiDet.create_dataset("nstate",(1,),dtype="i4",data=len(MyDetA))
groupMultiDet.create_dataset("nexcitedstate",(1,),dtype="i4",data=nexcitedstate)
groupMultiDet.create_dataset("Nbits",(1,),dtype="i4",data=len(det_a))

print "temp=",MultiDetAlpha[0]
mylen="S"+str(len(MyDetA))
groupMultiDet.create_dataset("CI_Alpha",(len(psi_coef_small),len(det_a)),dtype='i8',data=MultiDetAlpha)

mylen="S"+str(len(MyDetB))
groupMultiDet.create_dataset("CI_Beta",(len(psi_coef_small),len(det_b)),dtype='i8',data=MultiDetBeta)

H5_qmcpack.close()

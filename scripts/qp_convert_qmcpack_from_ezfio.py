#!/usr/bin/python

print "#QP -> QMCPACK"

from ezfio import ezfio

import sys
ezfio_path = sys.argv[1]

ezfio.set_file(ezfio_path)

ao_num = ezfio.get_ao_basis_ao_num()
print ao_num

mo_num = ezfio.get_mo_basis_mo_tot_num()
print mo_num


alpha = ezfio.get_electrons_elec_alpha_num()
beta = ezfio.get_electrons_elec_beta_num()
print "elec_alpha_num", alpha
print "elec_beta_num", beta
print "elec_tot_num",  alpha + beta
print "spin_multiplicity", 2*(alpha-beta)+1

print "Atomic coord in Bohr"
l_label = ezfio.get_nuclei_nucl_label()
l_charge = ezfio.get_nuclei_nucl_charge()
l_coord = ezfio.get_nuclei_nucl_coord()

l_coord_str = [" ".join(map(str,i)) for i in l_coord]

for i in zip(l_label,l_charge,l_coord_str):
	print " ".join(map(str,i))


import subprocess
process = subprocess.Popen(['qp_print_basis', ezfio_path], stdout=subprocess.PIPE)
out, err = process.communicate()

basis_raw, sym_raw, mo_raw = out.split("\n\n\n")

basis_without_header = "\n".join(basis_raw.split("\n")[7:])
for i,l in enumerate(l_label):
	basis_without_header=basis_without_header.replace('Atom {0}'.format(i+1),l)

print "BEGIN_BASIS_SET"
print basis_without_header
print "END_BASIS_SET"

#       _     
# |\/| / \  _ 
# |  | \_/ _> 
#             
def same_character(item1):
    return item1==item1[0]* len(item1)

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
    elif not same_character(item1) and  same_character(item2):
        return 1
    else:
        return compare_gamess_style(item1[:-1],item2[:-1])

def expend_and_order_sym(str_):
	#Expend
	for i,c in enumerate(str_):
		try:
			n = int(c)
		except ValueError: 
			pass
		else:
			str_ = str_[:i-1] + str_[i-1]*n + str_[i+1:]

	#Order by frequency
	return "".join(sorted(str_,key=str_.count,reverse=True))

def get_nb_permutation(str_):

	l = len(str_)-1
	if l==0:
		return 1
	else:
		return 2*(2*l + 1)

## We will order the symetry
l_sym_without_header = sym_raw.split("\n")[3:-2]

l_l_sym = [i.split() for i in l_sym_without_header]

for l in l_l_sym:
	l[2] = expend_and_order_sym(l[2])


print l_l_sym

l_l_sym_iter = iter(l_l_sym)
for i,l in enumerate(l_l_sym_iter):
	n = get_nb_permutation(l[2])
	if n !=1:
		l_l_sym[i:i+n] = sorted(l_l_sym[i:i+n],key=lambda x : x[2], cmp=compare_gamess_style)
		for next_ in range(n-1):
			next(l_l_sym_iter)

#Is orderd now

l_block = mo_raw.split("\n\n")[5:-1]


l_block_format=[]

print ""
print "BEGIN_MO"
for block in l_block:
	print ""
	l_ligne = block.split("\n")
	print l_ligne.pop(0)

	for l  in l_l_sym:
		i = int(l[0]) - 1
		i_a = int(l[1]) - 1
		sym = l[2]

		print l_label[i_a],sym,l_ligne[i]

print "END_MO"


print "BEGIN_PSEUDO"
do_pseudo = ezfio.get_pseudo_do_pseudo()

zcore = ezfio.get_pseudo_nucl_charge_remove()
klocmax = ezfio.get_pseudo_pseudo_klocmax()
kmax = ezfio.get_pseudo_pseudo_kmax()
lmax = ezfio.get_pseudo_pseudo_lmax()


n_k = ezfio.get_pseudo_pseudo_n_k()
v_k = ezfio.get_pseudo_pseudo_v_k()
dz_k = ezfio.get_pseudo_pseudo_dz_k()

n_kl = ezfio.get_pseudo_pseudo_n_kl()
v_kl = ezfio.get_pseudo_pseudo_v_kl()
dz_kl = ezfio.get_pseudo_pseudo_dz_kl()

def list_to_string(l):
	return " ".join(map(str,l))

for i,a in enumerate(l_label):

	l_str = []

	l_dump = []
	for k in range(klocmax):
			if v_k[k][i]:
				l_ = list_to_string([v_k[k][i], n_k[k][i]+2, dz_k[k][i]])
				l_dump.append(l_)

	l_str.append(l_dump)
	print "non loc"
	for l in range(lmax+1):
		l_dump = []
		for k in range(kmax):
			if v_kl[l][k][i]:
				l_ = list_to_string([v_kl[l][k][i], n_kl[l][k][i]+2, dz_kl[l][k][i]])
				l_dump.append(l_)
		if l_dump:
			l_str.append(l_dump)

	str_ = "PARAMETERS FOR {0} ON ATOM {1} WITH ZCORE {2} AND LMAX {3} ARE"
	print str_.format(a,i+1,zcore[i],len(l_str))

	for i, l in enumerate(l_str):
		str_ = "FOR L= {0} COEFF N ZETA"
		print str_.format(len(l_str)-i-1)
		for ii, ll in enumerate(l):
			print " ",ii+1, ll

str_ = "THE ECP RUN REMOVES {0} CORE ELECTRONS, AND THE SAME NUMBER OF PROTONS."
print str_.format(sum(zcore))
print "END_PSEUDO"

print "BEGIN_DET"
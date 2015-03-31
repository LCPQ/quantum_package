#!/bin/bash
#  Convert a old ezfio file (with option.irp.f ezfio_default)
# into a new EZFIO.cfg type

# Hartree Fock
# Changin the case, don't know if is needed or not 
mv $1/Hartree_Fock $1/hartree_fock
mv $1/hartree_Fock/thresh_SCF $1/hartree_fock/thresh_scf

mv $1/hartree_Fock/thresh_SCF $1/hartree_fock/thresh_scf

# BiInts
mv $1/bi_integrals $1/bielect_integrals
#!/bin/bash
#  Convert a old ezfio file (with option.irp.f ezfio_default)
# into a new EZFIO.cfg type
mv $1/Hartree_Fock $1/hartree_fock
mv $1/hartree_Fock/thresh_SCF $1/hartree_fock/thresh_scf

#!/bin/bash
#  Convert a old ezfio file (with option.irp.f ezfio_default)
# into a new EZFIO.cfg type

# Hartree Fock
# Changin the case, don't know if is needed or not 
mv $1/Hartree_Fock $1/hartree_fock 2> /dev/null

mv $1/hartree_Fock/thresh_SCF $1/hartree_fock/thresh_scf 2> /dev/null

# BiInts
mv $1/bi_integrals $1/bielect_integrals 2> /dev/null

if [ -f  $1/bielect_integrals/read_ao_integrals ]; then
    if [ `cat $1/bielect_integrals/read_ao_integrals` -eq "True" ]
    then
            echo "Read" > $1/bielect_integrals/disk_access_ao_integrals
    
    elif [ `cat bielect_integrals/write_ao_integrals` -eq "True" ]
    then
            echo "Write" > $1/bielect_integrals/disk_access_ao_integrals
    
    else
            echo "None" > $1/bielect_integrals/disk_access_ao_integrals
    
    fi
fi
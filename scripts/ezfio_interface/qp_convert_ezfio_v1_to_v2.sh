#!/bin/bash
#  Convert a old ezfio file (with option.irp.f ezfio_default)
# into a new EZFIO.cfg type

# Hartree Fock
# Changin the case, don't know if is needed or not 

echo "Will tranform qp_v1.*_ezfio to qp_v2.*_ezfio"
echo "All action are irrevocable! And is by choice"
echo "You need to stop to use a old version! Plz..."

echo "Change thresh_SCF > thresh_scf0"
mv $1/hartree_Fock/thresh_SCF $1/hartree_fock/thresh_scf 2> /dev/null

# Set disk_acess
echo "Change {read,write}_ao_integrals > disk_access_ao_integrals"

biint=$1/Integrals_bielec

if [[ -f  $biint/read_ao_integrals ]]; then
    if [[ `cat $1/Integrals_bielec/read_ao_integrals` -eq "T" ]]
    then
            echo "Read" > $biint/disk_access_ao_integrals
    
    elif [[ `cat $biint/write_ao_integrals` -eq "T" ]]
    then
            echo "Write" > $biint/disk_access_ao_integrals
    
    else
            echo "None" > $biint/disk_access_ao_integrals
    
    fi
    rm $biint/read_ao_integrals  $biint/write_ao_integrals  $biint/write_ao_intergals 2> /dev/null
fi

mv $1/MonoInts $1/Integrals_Monoelec

echo "Done"
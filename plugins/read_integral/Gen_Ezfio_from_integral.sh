#!/bin/bash

ezfio=$1
# Create the integral
echo 'Create Integral'

echo 'Create EZFIO'
read nel nmo natom <<< $(cat param) 
read e_nucl <<< $(cat e_nuc)
./create_ezfio.py $ezfio $nel $natom $nmo $e_nucl
#Handle the orbital consitensy check
qp_edit -c $ezfio &> /dev/null
cp $ezfio/{ao,mo}_basis/ao_md5 

#Read the integral
echo 'Read Integral'
qp_run read_integrals_mo $ezfio 

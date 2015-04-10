#!/bin/bash
# $1 name
# $2 mult

echo "name" $1
echo "mul"  $2
echo "\`get_basis.sh\` need to be changed"

rm -R $1.ezfio 
qp_create_ezfio_from_xyz $1.xyz -b "cc-pvdz" -m $2
~/quantum_package/scripts/pseudo/put_pseudo_in_ezfio.py --ezfio $1.ezfio/ --atom $1
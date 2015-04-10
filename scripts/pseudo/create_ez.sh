#!/bin/bash
# $1 name
# $2 mult

echo "name" $1
echo "basis" $2
echo "mul"  $3
echo "\`get_basis.sh\` need to be changed"

rm -R $1.ezfio 
qp_create_ezfio_from_xyz $1.xyz -b $2 -m $3
~/quantum_package/scripts/pseudo/put_pseudo_in_ezfio.py $1.ezfio
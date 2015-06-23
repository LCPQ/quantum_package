#!/bin/bash
# This script should be included

# pro:
# 
# solid way to compare fancy version strings:
# support any length of sub-parts (ie: 1.3alpha.2.dev2 > 1.1 ?)
# support alpha-betical sort (ie: 1.alpha < 1.beta2)
# support big size version (ie: 1.10003939209329320932 > 1.2039209378273789273 ?)
# can easily be modified to support n arguments. (leaved as an exercise ;) )
# usually very usefull with 3 arguments: (ie: 1.2 < my_version < 2.7 )
# cons:
# 
# uses a lot of various calls to different programs. So it's not that efficient.
# uses a pretty recent version of sort and it might not be available on your system. (check with man sort)

function version_gt() { test "$(echo "$@" | tr " " "\n" | sort -V | tail -n 1)" == "$1"; }

irp_cur_version=`irpf90 -v`
irp_need_version=1.6.7

if version_gt $irp_cur_version $irp_need_version; then
    echo "OK"
fi
echo "FAIL"
BEGIN_PROVIDER [ logical, read_ao_integrals_erf ]
&BEGIN_PROVIDER [ logical, read_mo_integrals_erf ]
&BEGIN_PROVIDER [ logical, write_ao_integrals_erf ]
&BEGIN_PROVIDER [ logical, write_mo_integrals_erf ]

 BEGIN_DOC
! One level of abstraction for disk_access_ao_integrals_erf and disk_access_mo_integrals_erf
 END_DOC
implicit none

    if (disk_access_ao_integrals_erf.EQ.'Read') then
        read_ao_integrals_erf =  .True.
        write_ao_integrals_erf = .False.

    else if  (disk_access_ao_integrals_erf.EQ.'Write') then
        read_ao_integrals_erf = .False.
        write_ao_integrals_erf =  .True.
    
    else if (disk_access_ao_integrals_erf.EQ.'None') then
        read_ao_integrals_erf = .False.
        write_ao_integrals_erf = .False.

    else
        print *, 'bielec_integrals_erf/disk_access_ao_integrals_erf has a wrong type'
        stop 1

    endif

    if (disk_access_mo_integrals_erf.EQ.'Read') then
        read_mo_integrals_erf =  .True.
        write_mo_integrals_erf = .False.

    else if  (disk_access_mo_integrals_erf.EQ.'Write') then
        read_mo_integrals_erf = .False.
        write_mo_integrals_erf =  .True.

    else if (disk_access_mo_integrals_erf.EQ.'None') then
        read_mo_integrals_erf = .False.
        write_mo_integrals_erf = .False.

    else
        print *, 'bielec_integrals_erf/disk_access_mo_integrals_erf has a wrong type'
        stop 1

    endif

END_PROVIDER

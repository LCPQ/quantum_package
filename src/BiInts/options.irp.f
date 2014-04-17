BEGIN_PROVIDER [ logical, write_mo_integrals ]
  implicit none
  BEGIN_DOC
  !  If true, write MO integrals in EZFIO
  END_DOC
  
  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_bielec_integrals_write_mo_integrals(has)
  if (has) then
    call ezfio_get_bielec_integrals_write_mo_integrals(write_mo_integrals)
  else
    write_mo_integrals = .False.
    call ezfio_set_bielec_integrals_write_mo_integrals(write_mo_integrals)
  endif
  
END_PROVIDER

BEGIN_PROVIDER [ logical, write_ao_integrals ]
  implicit none
  BEGIN_DOC
  !  If true, write AO integrals in EZFIO
  END_DOC
  
  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_bielec_integrals_write_ao_integrals(has)
  if (has) then
    call ezfio_get_bielec_integrals_write_ao_integrals(write_ao_integrals)
  else
    write_ao_integrals = .False.
    call ezfio_set_bielec_integrals_write_ao_integrals(write_ao_integrals)
  endif
  
END_PROVIDER

BEGIN_PROVIDER [ logical, read_mo_integrals ]
  implicit none
  BEGIN_DOC
  !  If true, read MO integrals in EZFIO
  END_DOC
  
  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_bielec_integrals_read_mo_integrals(has)
  if (has) then
    call ezfio_get_bielec_integrals_read_mo_integrals(read_mo_integrals)
  else
    read_mo_integrals = .False.
    call ezfio_set_bielec_integrals_read_mo_integrals(read_mo_integrals)
  endif
  
END_PROVIDER

BEGIN_PROVIDER [ logical, read_ao_integrals ]
  implicit none
  BEGIN_DOC
  !  If true, read AO integrals in EZFIO
  END_DOC
  
  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_bielec_integrals_read_ao_integrals(has)
  if (has) then
    call ezfio_get_bielec_integrals_read_ao_integrals(read_ao_integrals)
  else
    read_ao_integrals = .False.
    call ezfio_set_bielec_integrals_read_ao_integrals(read_ao_integrals)
  endif
  
END_PROVIDER

BEGIN_PROVIDER [ double precision, ao_integrals_threshold ]
  implicit none
  BEGIN_DOC
  !  If <pq|rs> < ao_integrals_threshold, <pq|rs> = 0
  END_DOC
  
  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_bielec_integrals_threshold_ao(has)
  if (has) then
    call ezfio_get_bielec_integrals_threshold_ao(ao_integrals_threshold)
  else
    ao_integrals_threshold = 1.d-15
    call ezfio_set_bielec_integrals_threshold_ao(ao_integrals_threshold)
  endif
  
END_PROVIDER


BEGIN_PROVIDER [ double precision, mo_integrals_threshold ]
  implicit none
  BEGIN_DOC
  !  If <ij|kl> < mo_integrals_threshold, <ij|kl> = 0
  END_DOC
  
  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_bielec_integrals_threshold_mo(has)
  if (has) then
    call ezfio_get_bielec_integrals_threshold_mo(mo_integrals_threshold)
  else
    mo_integrals_threshold = 1.d-11
    call ezfio_set_bielec_integrals_threshold_mo(mo_integrals_threshold)
  endif
  
END_PROVIDER


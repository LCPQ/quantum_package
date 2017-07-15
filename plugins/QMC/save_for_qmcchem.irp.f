program save_for_qmc

  integer                        :: iunit
  integer, external              :: get_unit_and_open
  logical                        :: exists
  double precision               :: e_ref
  
  ! Determinants
  read_wf = .True.
  TOUCH read_wf
  print *,  "N_det = ", N_det
  call write_spindeterminants
  
  ! Reference Energy
  if (do_pseudo) then
    call write_pseudopotential
  endif
  call system( &
   'mkdir -p '//trim(ezfio_filename)//'/simulation ;' // &
   'cp '//trim(ezfio_filename)//'/.version '//trim(ezfio_filename)//'/simulation/.version ; ' // &
   'mkdir -p '//trim(ezfio_filename)//'/properties ;' // &
   'cp '//trim(ezfio_filename)//'/.version '//trim(ezfio_filename)//'/properties/.version ; ' // &
   'echo T > '//trim(ezfio_filename)//'/properties/e_loc' &
  )
  iunit = 13
  open(unit=iunit,file=trim(ezfio_filename)//'/simulation/e_ref',action='write')
  call ezfio_has_full_ci_zmq_energy_pt2(exists)
  if (exists) then
    call ezfio_get_full_ci_zmq_energy_pt2(e_ref)
  else
    call ezfio_has_full_ci_zmq_energy(exists)
    if (exists) then
      call ezfio_get_full_ci_zmq_energy(e_ref)
    else
      call ezfio_has_hartree_fock_energy(exists)
      if (exists) then
        call ezfio_get_hartree_fock_energy(e_ref)
      else
        e_ref = 0.d0
      endif
    endif
  endif
  write(iunit,*) e_ref
  close(iunit)

end

use bitmasks

BEGIN_PROVIDER [ integer, N_det_generators_restart ]
 implicit none
 BEGIN_DOC
 ! Number of determinants in the wave function
 END_DOC
 logical                        :: exists
 character*64                   :: label
 integer, save :: ifirst = 0
!if(ifirst == 0)then
 PROVIDE ezfio_filename
   call ezfio_has_determinants_n_det(exists)
   print*,'exists = ',exists
   if(.not.exists)then
    print*,'The OSOCI needs a restart WF'
    print*,'There are none in the EZFIO file ...'
    print*,'Stopping ...'
    stop
   endif
   print*,'passed N_det_generators_restart'
   call ezfio_get_determinants_n_det(N_det_generators_restart)
 ASSERT (N_det_generators_restart > 0)
  ifirst = 1
!endif
END_PROVIDER


 BEGIN_PROVIDER [ integer(bit_kind), psi_det_generators_restart, (N_int,2,psi_det_size) ]
&BEGIN_PROVIDER [ integer(bit_kind), ref_generators_restart, (N_int,2) ]
 implicit none
 BEGIN_DOC
 ! The wave function determinants. Initialized with Hartree-Fock if the EZFIO file
 ! is empty
 END_DOC
  integer                        :: i
  logical                        :: exists
  character*64                   :: label
  
 integer, save :: ifirst = 0
!if(ifirst == 0)then
  provide N_det_generators_restart
  if(.True.)then
    call ezfio_has_determinants_N_int(exists)
    if (exists) then
     call ezfio_has_determinants_bit_kind(exists)
     if (exists) then
      call ezfio_has_determinants_N_det(exists)
      if (exists) then
       call ezfio_has_determinants_N_states(exists)
       if (exists) then
        call ezfio_has_determinants_psi_det(exists)
       endif
      endif
     endif
    endif

    if(.not.exists)then
     print*,'The OSOCI needs a restart WF'
     print*,'There are none in the EZFIO file ...'
     print*,'Stopping ...'
     stop
    endif
    print*,'passed psi_det_generators_restart'
   
      call read_dets(psi_det_generators_restart,N_int,N_det_generators_restart)
      do i = 1, N_int
        ref_generators_restart(i,1) = psi_det_generators_restart(i,1,1)
        ref_generators_restart(i,2) = psi_det_generators_restart(i,2,1)
      enddo
  endif
  ifirst = 1
!endif

END_PROVIDER



BEGIN_PROVIDER [ double precision, psi_coef_generators_restart, (psi_det_size,N_states_diag) ]
  implicit none
  BEGIN_DOC
  ! The wave function coefficients. Initialized with Hartree-Fock if the EZFIO file
  ! is empty
  END_DOC
  
  integer                        :: i,k, N_int2
  logical                        :: exists
  double precision, allocatable  :: psi_coef_read(:,:)
  character*(64)                 :: label

 integer, save :: ifirst = 0
!if(ifirst == 0)then
  psi_coef_generators_restart = 0.d0
  do i=1,N_states_diag
    psi_coef_generators_restart(i,i) = 1.d0
  enddo

    call ezfio_has_determinants_psi_coef(exists)
    
   if(.not.exists)then
    print*,'The OSOCI needs a restart WF'
    print*,'There are none in the EZFIO file ...'
    print*,'Stopping ...'
    stop
   endif
   print*,'passed  psi_coef_generators_restart'
    
    if (exists) then
      
      allocate (psi_coef_read(N_det_generators_restart,N_states))
      call ezfio_get_determinants_psi_coef(psi_coef_read)
      do k=1,N_states
        do i=1,N_det_generators_restart
          psi_coef_generators_restart(i,k) = psi_coef_read(i,k)
        enddo
      enddo
      deallocate(psi_coef_read)
      
    endif
  ifirst = 1
!endif
    
    
  
END_PROVIDER


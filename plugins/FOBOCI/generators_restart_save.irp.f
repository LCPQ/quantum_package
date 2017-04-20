
use bitmasks
 
BEGIN_PROVIDER [ integer, N_det_generators_restart ]
 implicit none
 BEGIN_DOC
 ! Read the wave function 
 END_DOC
 integer :: i
 integer, save :: ifirst = 0
 double precision :: norm
 print*, ' Providing N_det_generators_restart'
 if(ifirst == 0)then
  call ezfio_get_determinants_n_det(N_det_generators_restart)
  ifirst = 1
 else
  print*,'PB in generators_restart restart !!!'
 endif
 call write_int(output_determinants,N_det_generators_restart,'Number of generators_restart')
END_PROVIDER


 BEGIN_PROVIDER [ integer(bit_kind), psi_det_generators_restart, (N_int,2,N_det_generators_restart) ]
&BEGIN_PROVIDER [ integer(bit_kind), ref_generators_restart, (N_int,2) ]
&BEGIN_PROVIDER [ double precision, psi_coef_generators_restart, (N_det_generators_restart,N_states) ]
 implicit none
 BEGIN_DOC
 ! read wf
 ! 
 END_DOC
 integer                        :: i, k
 integer, save :: ifirst = 0
 double precision, allocatable  :: psi_coef_read(:,:)
 print*, ' Providing psi_det_generators_restart'
 if(ifirst == 0)then
  call read_dets(psi_det_generators_restart,N_int,N_det_generators_restart)
   do k = 1, N_int
    ref_generators_restart(k,1) = psi_det_generators_restart(k,1,1)
    ref_generators_restart(k,2) = psi_det_generators_restart(k,2,1)
   enddo
   allocate (psi_coef_read(N_det_generators_restart,N_states))
   call ezfio_get_determinants_psi_coef(psi_coef_read)
   do k = 1, N_states
    do i = 1, N_det_generators_restart
     psi_coef_generators_restart(i,k) = psi_coef_read(i,k)
    enddo
   enddo
  ifirst = 1
  deallocate(psi_coef_read)
 else 
  print*,'PB in generators_restart restart !!!'
 endif

END_PROVIDER

BEGIN_PROVIDER [ integer, size_select_max]
 implicit none
 BEGIN_DOC
 ! Size of the select_max array
 END_DOC
 size_select_max = 10000
END_PROVIDER

BEGIN_PROVIDER [ double precision, select_max, (size_select_max) ]
 implicit none
 BEGIN_DOC
 ! Memo to skip useless selectors
 END_DOC
 select_max = huge(1.d0)
END_PROVIDER

 BEGIN_PROVIDER [ integer, N_det_generators ]
&BEGIN_PROVIDER [ integer(bit_kind), psi_det_generators, (N_int,2,10000) ]
&BEGIN_PROVIDER [ double precision, psi_coef_generators, (10000,N_states) ]

END_PROVIDER

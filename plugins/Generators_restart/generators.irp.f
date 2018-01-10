use bitmasks
 
BEGIN_PROVIDER [ integer, N_det_generators ]
 implicit none
 BEGIN_DOC
 ! Read the wave function 
 END_DOC
 integer :: i
 integer, save :: ifirst = 0
 double precision :: norm
 if(ifirst == 0)then
  call ezfio_get_determinants_n_det(N_det_generators)
  ifirst = 1
 else
  print*,'PB in generators restart !!!'
 endif
 call write_int(6,N_det_generators,'Number of generators')
END_PROVIDER


 BEGIN_PROVIDER [ integer(bit_kind), psi_det_generators, (N_int,2,N_det_generators) ]
&BEGIN_PROVIDER [ double precision, psi_coef_generators, (N_det_generators,N_states) ]
 implicit none
 BEGIN_DOC
 ! read wf
 ! 
 END_DOC
 integer                        :: i, k
 integer, save :: ifirst = 0
 double precision, allocatable  :: psi_coef_read(:,:)
 if(ifirst == 0)then
  call read_dets(psi_det_generators,N_int,N_det_generators)
   allocate (psi_coef_read(N_det_generators,N_states))
   call ezfio_get_determinants_psi_coef(psi_coef_read)
   do k = 1, N_states
    do i = 1, N_det_generators
     psi_coef_generators(i,k) = psi_coef_read(i,k)
    enddo
   enddo
  ifirst = 1
  deallocate(psi_coef_read)
 else 
  print*,'PB in generators restart !!!'
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


use bitmasks

BEGIN_PROVIDER [ integer, N_det_generators ]
 implicit none
 BEGIN_DOC
 ! Read the wave function 
 END_DOC
 integer :: i
 integer, save :: ifirst = 0
 double precision :: norm
 read_wf = .True.
 if(ifirst == 0)then
  N_det_generators = N_det
  ifirst = 1
 endif
 call write_int(output_determinants,N_det_generators,'Number of generators')
END_PROVIDER


 BEGIN_PROVIDER [ integer(bit_kind), psi_det_generators, (N_int,2,psi_det_size) ]
&BEGIN_PROVIDER [ double precision, psi_coef_generators, (psi_det_size,N_states) ]
 implicit none
 BEGIN_DOC
 ! read wf
 ! 
 END_DOC
 integer                        :: i, k
 integer, save :: ifirst = 0
 if(ifirst == 0)then
  do i=1,N_det_generators
    do k=1,N_int
      psi_det_generators(k,1,i) = psi_det(k,1,i)
      psi_det_generators(k,2,i) = psi_det(k,2,i)
    enddo
   do k = 1, N_states
    psi_coef_generators(i,k) = psi_coef(i,k)
   enddo
  enddo
  ifirst = 1
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


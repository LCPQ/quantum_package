use bitmasks

BEGIN_PROVIDER [ integer, N_states ] 
 implicit none
 BEGIN_DOC
! Number of states to consider
 END_DOC
 logical                        :: exists
 PROVIDE ezfio_filename
 call ezfio_has_determinants_n_states(exists)
 if (exists) then
   call ezfio_get_determinants_n_states(N_states)
 else
   N_states = 1
   call ezfio_set_determinants_n_states(N_states)
 endif
 ASSERT (N_states > 0)
END_PROVIDER

BEGIN_PROVIDER [ integer, N_det ]
 implicit none
 BEGIN_DOC
 ! Number of determinants in the wave function
 END_DOC
 logical                        :: exists
 PROVIDE ezfio_filename
 call ezfio_has_determinants_n_det(exists)
 if (exists) then
   call ezfio_get_determinants_n_det(N_det)
 else
   N_det = 1
   call ezfio_set_determinants_n_det(N_det)
 endif
 ASSERT (N_det > 0)
END_PROVIDER


BEGIN_PROVIDER [ integer, psi_det_size ]
 implicit none
 BEGIN_DOC
 ! Size of the psi_det/psi_coef arrays
 END_DOC
 psi_det_size = 1000*N_states
END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), psi_det, (N_int,2,psi_det_size) ]
&BEGIN_PROVIDER [ double precision, psi_coef, (psi_det_size,N_states) ]
 implicit none
 BEGIN_DOC
 ! The wave function. Initialized with Hartree-Fock if the EZFIO file
 ! is empty
 END_DOC

 integer, save :: ifirst = 0

 if (ifirst == 0) then
    ifirst = 1
    psi_det = 0_bit_kind
    psi_coef = 0.d0

    integer :: i
    do i=1,N_int
      psi_det(i,1,1) = HF_bitmask(i,1)
      psi_det(i,2,1) = HF_bitmask(i,2)
    enddo

    do i=1,N_states
      psi_coef(i,i) = 1.d0
    enddo
 endif

END_PROVIDER



BEGIN_PROVIDER [ integer, N_det_reference ]
 implicit none
 BEGIN_DOC
 ! Number of determinants in the reference wave function
 END_DOC
   N_det_reference = N_det
 ASSERT (N_det_reference > 0)
END_PROVIDER

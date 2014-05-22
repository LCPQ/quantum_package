use bitmasks

BEGIN_PROVIDER [ integer, N_states ] 
 implicit none
 BEGIN_DOC
! Number of states to consider
 END_DOC
 N_states = 1
END_PROVIDER

BEGIN_PROVIDER [ integer, N_det ]
 implicit none
 BEGIN_DOC
 ! Number of determinants in the wave function
 END_DOC
 N_det = 1
END_PROVIDER

BEGIN_PROVIDER [ integer, psi_det_size ]
 implicit none
 BEGIN_DOC
 ! Size of the psi_det/psi_coef arrays
 END_DOC
 psi_det_size = 1000
END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), psi_det, (N_int,2,psi_det_size) ]
&BEGIN_PROVIDER [ double precision, psi_coef, (psi_det_size,N_states) ]
 implicit none
 BEGIN_DOC
 ! The wave function. Initialized with Hartree-Fock
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


BEGIN_PROVIDER [ integer, N_det_generators ]
 implicit none
 BEGIN_DOC
 ! Number of generator determinants in the wave function
 END_DOC
 N_det_generators = N_det
END_PROVIDER

BEGIN_PROVIDER [ integer(bit_kind), psi_generators, (N_int,2,psi_det_size) ]
 implicit none
 BEGIN_DOC
 ! Determinants on which H is applied
 END_DOC
 psi_generators = 0_bit_kind
 integer :: i

 do i=1,N_int
   psi_generators(i,1,1) = psi_det(i,1,1)
   psi_generators(i,2,1) = psi_det(i,1,1)
 enddo

END_PROVIDER

BEGIN_PROVIDER [ integer, psi_ref_size]
 implicit none
 BEGIN_DOC
 ! Number of generator determinants in the wave function
 END_DOC
 psi_det_size = N_det
END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), psi_ref, (N_int,2,psi_ref_size) ]
&BEGIN_PROVIDER [ double precision, psi_ref_coef, (psi_ref_size,N_states) ]
 implicit none
 BEGIN_DOC
 ! Determinants on which H is applied
 END_DOC
 integer :: i,k

 do k = 1, psi_ref_size
  do i=1,N_int
    psi_ref(i,1,k) = psi_det(i,1,k)
    psi_ref(i,2,k) = psi_det(i,1,k)
  enddo
  do i = 1, N_states
   psi_ref_coef(k,i) = psi_coef(k,i)
  enddo
 enddo

END_PROVIDER


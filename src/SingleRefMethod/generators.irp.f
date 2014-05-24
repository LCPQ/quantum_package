use bitmasks

BEGIN_PROVIDER [ integer, N_det_generators ]
 implicit none
 BEGIN_DOC
 ! For Single reference wave functions, the number of generators is 1 : the
 ! Hartree-Fock determinant
 END_DOC
 N_det_generators = 1
END_PROVIDER

BEGIN_PROVIDER [ integer(bit_kind), psi_generators, (N_int,2,1) ]
 implicit none
 BEGIN_DOC
 ! For Single reference wave functions, the generator is the
 ! Hartree-Fock determinant
 END_DOC
 psi_generators = 0_bit_kind
 integer :: i

 do i=1,N_int
   psi_generators(i,1,1) = HF_bitmask(i,1)
   psi_generators(i,2,1) = HF_bitmask(i,2)
 enddo

END_PROVIDER



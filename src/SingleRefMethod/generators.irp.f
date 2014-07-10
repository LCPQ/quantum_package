use bitmasks

BEGIN_PROVIDER [ integer, N_det_generators ]
 implicit none
 BEGIN_DOC
 ! For Single reference wave functions, the number of generators is 1 : the
 ! Hartree-Fock determinant
 END_DOC
 N_det_generators = 1
END_PROVIDER

BEGIN_PROVIDER [ integer(bit_kind), psi_generators, (N_int,2,psi_det_size) ]
 implicit none
 BEGIN_DOC
 ! For Single reference wave functions, the generator is the
 ! Hartree-Fock determinant
 END_DOC
 psi_generators = 0_bit_kind
 integer :: i,j,k
 integer :: degree

 do i=1,N_int
   psi_generators(i,1,1) = HF_bitmask(i,1)
   psi_generators(i,2,1) = HF_bitmask(i,2)
 enddo

 do j=1,N_det
   call get_excitation_degree(HF_bitmask,psi_det(1,1,j),degree,N_int)
   if (degree == 0) then
     k = j
     exit
   endif
 end do

 do j=2,k
   psi_generators(:,:,j) = psi_det(:,:,j-1)
 enddo
 do j=k+1,N_det
   psi_generators(:,:,j) = psi_det(:,:,j)
 enddo

END_PROVIDER

BEGIN_PROVIDER [ double precision, select_max, (1) ]
 implicit none
 BEGIN_DOC
 ! Memo to skip useless selectors
 END_DOC
 select_max(1) = huge(1.d0)
END_PROVIDER

BEGIN_PROVIDER [ integer, size_select_max ]
 implicit none
 BEGIN_DOC
 ! Size of select_max
 END_DOC
 size_select_max = 1

END_PROVIDER



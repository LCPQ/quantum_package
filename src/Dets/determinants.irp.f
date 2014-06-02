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


BEGIN_PROVIDER [ integer, N_det_max_jacobi ]
 implicit none
 BEGIN_DOC
 ! Maximum number of determinants diagonalized my jacobi
 END_DOC
 logical                        :: exists
 PROVIDE ezfio_filename
 call ezfio_has_determinants_n_det_max_jacobi(exists)
 if (exists) then
   call ezfio_get_determinants_n_det_max_jacobi(N_det_max_jacobi)
 else
   N_det_max_jacobi = 1500
   call ezfio_set_determinants_n_det_max_jacobi(N_det_max_jacobi)
 endif
 ASSERT (N_det_max_jacobi > 0)
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
 endif

 integer :: i
 do i=1,N_int
   psi_det(i,1,1) = HF_bitmask(i,1)
   psi_det(i,2,1) = HF_bitmask(i,2)
 enddo

 do i=1,N_states
   psi_coef(i,i) = 1.d0
 enddo

END_PROVIDER


BEGIN_PROVIDER [ double precision, psi_average_norm_contrib, (N_det) ]
 implicit none
 BEGIN_DOC
 ! Contribution of determinants to the state-averaged density
 END_DOC
 integer :: i,j,k
 double precision :: f
 f = 1.d0/dble(N_states)
 do i=1,N_det
   psi_average_norm_contrib(i) = psi_coef(i,1)*psi_coef(i,1)*f
 enddo
 do k=2,N_states
   do i=1,N_det
     psi_average_norm_contrib(i) = psi_average_norm_contrib(i) + &
       psi_coef(i,k)*psi_coef(i,k)*f
   enddo
 enddo
END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), psi_det_sorted, (N_int,2,N_det) ]
&BEGIN_PROVIDER [ double precision, psi_coef_sorted, (N_det,N_states) ]
&BEGIN_PROVIDER [ double precision, psi_average_norm_contrib_sorted, (N_det) ]
 implicit none
 BEGIN_DOC
 ! Wave function sorted by determinants contribution to the norm (state-averaged)
 END_DOC
 integer :: i,j,k
 integer, allocatable ::  iorder(:)
 allocate ( iorder(N_det) )
 do i=1,N_det
   psi_average_norm_contrib_sorted(i) = -psi_average_norm_contrib(i)
   iorder(i) = i
 enddo
 call dsort(psi_average_norm_contrib_sorted,iorder,N_det)
 !DIR$ IVDEP
 do i=1,N_det
  do j=1,N_int
    psi_det_sorted(j,1,i) = psi_det(j,1,iorder(i))
    psi_det_sorted(j,2,i) = psi_det(j,2,iorder(i))
  enddo
  do k=1,N_states
    psi_coef_sorted(i,k) = psi_coef(iorder(i),k)
  enddo
  psi_average_norm_contrib_sorted(i) = -psi_average_norm_contrib_sorted(i)
 enddo

 deallocate(iorder)

END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), psi_det_sorted_bit, (N_int,2,N_det) ]
&BEGIN_PROVIDER [ double precision, psi_coef_sorted_bit, (N_det,N_states) ]
 implicit none
 BEGIN_DOC
 ! Determinants on which we apply <i|H|psi> for perturbation.
 !o They are sorted by determinants interpreted as integers. Useful
 ! to accelerate the search of a determinant
 END_DOC
 integer :: i,j,k
 integer, allocatable ::  iorder(:)
 integer*8, allocatable :: bit_tmp(:)
 integer*8, external :: det_search_key

 allocate ( iorder(N_det), bit_tmp(N_det) )

 do i=1,N_det
   iorder(i) = i
   !$DIR FORCEINLINE
   bit_tmp(i) = det_search_key(psi_det(1,1,i),N_int)
 enddo
 call i8sort(bit_tmp,iorder,N_det)
 !DIR$ IVDEP
 do i=1,N_det
  do j=1,N_int
    psi_det_sorted_bit(j,1,i) = psi_det(j,1,iorder(i))
    psi_det_sorted_bit(j,2,i) = psi_det(j,2,iorder(i))
  enddo
  do k=1,N_states
    psi_coef_sorted_bit(i,k) = psi_coef(iorder(i),k)
  enddo
 enddo

 deallocate(iorder)

END_PROVIDER


integer*8 function det_search_key(det,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
! Return an integer*8 corresponding to a determinant index for searching
  END_DOC
  integer, intent(in) :: Nint
  integer(bit_kind), intent(in) :: det(Nint,2)
  integer :: i
  det_search_key = iand(det(1,1),det(1,2))
  do i=2,Nint
    det_search_key = ieor(det_search_key,iand(det(i,1),det(i,2)))
  enddo
end

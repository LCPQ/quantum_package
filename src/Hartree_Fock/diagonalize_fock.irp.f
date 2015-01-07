 BEGIN_PROVIDER [ double precision, diagonal_Fock_matrix_mo, (ao_num) ]
&BEGIN_PROVIDER [ double precision, eigenvectors_Fock_matrix_mo, (ao_num_align,mo_tot_num) ]
   implicit none
   BEGIN_DOC
   ! Diagonal Fock matrix in the MO basis
   END_DOC
   
   integer                        :: i,j
   integer                        :: liwork, lwork, n, info
   integer, allocatable           :: iwork(:)
   double precision, allocatable  :: work(:), F(:,:), S(:,:)
   
   allocate(F(ao_num_align,ao_num), S(ao_num_align,ao_num) )
   do j=1,ao_num
     do i=1,ao_num
       S(i,j) = ao_overlap(i,j)
       F(i,j) = Fock_matrix_ao(i,j)
     enddo
   enddo

   n = ao_num
   lwork = 1+6*n + 2*n*n
   liwork = 3 + 5*n
   
   allocate(work(lwork), iwork(liwork) )

   lwork = -1
   liwork = -1

   call dsygvd(1,'v','u',ao_num,F,size(F,1),S,size(S,1),&
     diagonal_Fock_matrix_mo, work, lwork, iwork, liwork, info)
!    call dsygv(1, 'v', 'u',ao_num,F,size(F,1),S,size(S,1),&
!     diagonal_Fock_matrix_mo, work, lwork, info)



   if (info /= 0) then
     print *,  irp_here//' failed : ', info
     stop 1
   endif
   lwork = int(work(1))
   liwork = iwork(1)
   deallocate(work,iwork)
   allocate(work(lwork), iwork(liwork) )
!   deallocate(work)
!   allocate(work(lwork))

   call dsygvd(1,'v','u',ao_num,F,size(F,1),S,size(S,1),&
     diagonal_Fock_matrix_mo, work, lwork, iwork, liwork, info)

!    call dsygv(1, 'v', 'u',ao_num,F,size(F,1),S,size(S,1),&
!     diagonal_Fock_matrix_mo, work, lwork, info)

   if (info /= 0) then
     print *,  irp_here//' failed : ', info
     stop 1
   endif
   do j=1,mo_tot_num
     do i=1,ao_num
       eigenvectors_Fock_matrix_mo(i,j) = F(i,j)
     enddo
   enddo

   deallocate(work, iwork, F, S)
END_PROVIDER
 
BEGIN_PROVIDER [double precision, diagonal_Fock_matrix_mo_sum, (mo_tot_num)]
 implicit none
 BEGIN_DOC
 ! diagonal element of the fock matrix calculated as the sum over all the interactions 
 ! with all the electrons in the RHF determinant
 ! diagonal_Fock_matrix_mo_sum(i) = sum_{j=1, N_elec} 2 J_ij -K_ij 
 END_DOC
 integer :: i,j
 double precision :: accu
 do i = 1,elec_alpha_num
  accu = 0.d0
  do j = 1, elec_alpha_num
   accu += 2.d0 * mo_bielec_integral_jj_from_ao(i,j) - mo_bielec_integral_jj_exchange_from_ao(i,j)
  enddo
  diagonal_Fock_matrix_mo_sum(i) = accu + mo_mono_elec_integral(i,i)
 enddo
 do i = elec_alpha_num+1,mo_tot_num
  accu = 0.d0
  do j = 1, elec_alpha_num
   accu += 2.d0 * mo_bielec_integral_jj_from_ao(i,j) - mo_bielec_integral_jj_exchange_from_ao(i,j)
  enddo
  diagonal_Fock_matrix_mo_sum(i) = accu + mo_mono_elec_integral(i,i)
 enddo

END_PROVIDER

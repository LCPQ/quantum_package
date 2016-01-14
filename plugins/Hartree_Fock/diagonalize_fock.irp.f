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
   

!   if (mo_tot_num == ao_num) then
!       ! Solve H.C = E.S.C in AO basis set
!
!       allocate(F(ao_num_align,ao_num), S(ao_num_align,ao_num) )
!       do j=1,ao_num
!         do i=1,ao_num
!           S(i,j) = ao_overlap(i,j)
!           F(i,j) = Fock_matrix_ao(i,j)
!         enddo
!       enddo
!
!       n = ao_num
!       lwork = 1+6*n + 2*n*n
!       liwork = 3 + 5*n
!       
!       allocate(work(lwork), iwork(liwork) )
!
!       lwork = -1
!       liwork = -1
!
!       call dsygvd(1,'v','u',ao_num,F,size(F,1),S,size(S,1),&
!         diagonal_Fock_matrix_mo, work, lwork, iwork, liwork, info)
!
!       if (info /= 0) then
!         print *,  irp_here//' failed : ', info
!         stop 1
!       endif
!       lwork = int(work(1))
!       liwork = iwork(1)
!       deallocate(work,iwork)
!       allocate(work(lwork), iwork(liwork) )
!
!       call dsygvd(1,'v','u',ao_num,F,size(F,1),S,size(S,1),&
!         diagonal_Fock_matrix_mo, work, lwork, iwork, liwork, info)
!
!       if (info /= 0) then
!         print *,  irp_here//' failed : ', info
!         stop 1
!       endif
!       do j=1,mo_tot_num
!         do i=1,ao_num
!           eigenvectors_Fock_matrix_mo(i,j) = F(i,j)
!         enddo
!       enddo
!
!       deallocate(work, iwork, F, S)
!
!  else 
!
       ! Solve H.C = E.C in MO basis set
       
       allocate( F(mo_tot_num_align,mo_tot_num) )
       do j=1,mo_tot_num
         do i=1,mo_tot_num
           F(i,j) = Fock_matrix_mo(i,j)
         enddo
       enddo
       

       ! Insert level shift here
       do i = elec_alpha_num+1, mo_tot_num
         Fock_matrix_mo(i,i) += level_shift
       enddo

       n = mo_tot_num
       lwork = 1+6*n + 2*n*n
       liwork = 3 + 5*n
       
       allocate(work(lwork), iwork(liwork) )
       
       lwork = -1
       liwork = -1
       
       call dsyevd( 'V', 'U', mo_tot_num, F,                         &
           size(F,1), diagonal_Fock_matrix_mo,                       &
           work, lwork, iwork, liwork, info)
       
       if (info /= 0) then
         print *,  irp_here//' failed : ', info
         stop 1
       endif
       lwork = int(work(1))
       liwork = iwork(1)
       deallocate(work,iwork)
       allocate(work(lwork), iwork(liwork) )
       
       call dsyevd( 'V', 'U', mo_tot_num, F,                         &
           size(F,1), diagonal_Fock_matrix_mo,                       &
           work, lwork, iwork, liwork, info)
       
       if (info /= 0) then
         print *,  irp_here//' failed : ', info
         stop 1
       endif

       call dgemm('N','N',ao_num,mo_tot_num,mo_tot_num, 1.d0, &
         mo_coef, size(mo_coef,1), F, size(F,1), &
         0.d0, eigenvectors_Fock_matrix_mo, size(eigenvectors_Fock_matrix_mo,1))
       deallocate(work, iwork, F)

       ! Remove level shift 
       do i = elec_alpha_num+1, mo_tot_num
         Fock_matrix_mo(i,i) -= level_shift
       enddo

!  endif

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
 do j = 1,elec_alpha_num
  accu = 0.d0
  do i = 1, elec_alpha_num
   accu += 2.d0 * mo_bielec_integral_jj_from_ao(i,j) - mo_bielec_integral_jj_exchange_from_ao(i,j)
  enddo
  diagonal_Fock_matrix_mo_sum(j) = accu + mo_mono_elec_integral(j,j)
 enddo
 do j = elec_alpha_num+1,mo_tot_num
  accu = 0.d0
  do i = 1, elec_alpha_num
   accu += 2.d0 * mo_bielec_integral_jj_from_ao(i,j) - mo_bielec_integral_jj_exchange_from_ao(i,j)
  enddo
  diagonal_Fock_matrix_mo_sum(j) = accu + mo_mono_elec_integral(j,j)
 enddo

END_PROVIDER

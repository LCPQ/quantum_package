 BEGIN_PROVIDER [ double precision, diagonal_Fock_matrix_mo, (ao_num) ]
&BEGIN_PROVIDER [ double precision, eigenvectors_Fock_matrix_mo, (ao_num,mo_tot_num) ]
   implicit none
   BEGIN_DOC
   ! Diagonal Fock matrix in the MO basis
   END_DOC
   
   integer                        :: i,j
   integer                        :: liwork, lwork, n, info
   integer, allocatable           :: iwork(:)
   double precision, allocatable  :: work(:), F(:,:), S(:,:)
   
   
   allocate( F(mo_tot_num,mo_tot_num) )
   do j=1,mo_tot_num
     do i=1,mo_tot_num
       F(i,j) = Fock_matrix_mo(i,j)
     enddo
   enddo
   if(no_oa_or_av_opt)then
     integer                        :: iorb,jorb
     do i = 1, n_act_orb
       iorb = list_act(i)
       do j = 1, n_inact_orb
         jorb = list_inact(j)
         F(iorb,jorb) = 0.d0
         F(jorb,iorb) = 0.d0
       enddo
       do j = 1, n_virt_orb
         jorb = list_virt(j)
         F(iorb,jorb) = 0.d0
         F(jorb,iorb) = 0.d0
       enddo
       do j = 1, n_core_orb
         jorb = list_core(j)
         F(iorb,jorb) = 0.d0
         F(jorb,iorb) = 0.d0
       enddo
     enddo
   endif
   
   
   
   
   ! Insert level shift here
   do i = elec_beta_num+1, elec_alpha_num
     F(i,i) += 0.5d0*level_shift
   enddo
   
   do i = elec_alpha_num+1, mo_tot_num
     F(i,i) += level_shift
   enddo
   
   n = mo_tot_num
   lwork = 1+6*n + 2*n*n
   liwork = 3 + 5*n
   
   allocate(work(lwork))
   allocate(iwork(liwork) )
   
   lwork = -1
   liwork = -1
   
   call dsyevd( 'V', 'U', mo_tot_num, F,                             &
       size(F,1), diagonal_Fock_matrix_mo,                           &
       work, lwork, iwork, liwork, info)
   
   if (info /= 0) then
     print *,  irp_here//' DSYEVD failed : ', info
     stop 1
   endif
   lwork = int(work(1))
   liwork = iwork(1)
   deallocate(iwork)
   deallocate(work)
   
   allocate(work(lwork))
   allocate(iwork(liwork) )
   call dsyevd( 'V', 'U', mo_tot_num, F,                             &
       size(F,1), diagonal_Fock_matrix_mo,                           &
       work, lwork, iwork, liwork, info)
   deallocate(iwork)
   
   
   if (info /= 0) then
     call dsyev( 'V', 'L', mo_tot_num, F,                            &
         size(F,1), diagonal_Fock_matrix_mo,                         &
         work, lwork, info)
     
     if (info /= 0) then
       print *,  irp_here//' DSYEV failed : ', info
       stop 1
     endif
   endif
   
   call dgemm('N','N',ao_num,mo_tot_num,mo_tot_num, 1.d0,            &
       mo_coef, size(mo_coef,1), F, size(F,1),                       &
       0.d0, eigenvectors_Fock_matrix_mo, size(eigenvectors_Fock_matrix_mo,1))
   deallocate(work, F)
   


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

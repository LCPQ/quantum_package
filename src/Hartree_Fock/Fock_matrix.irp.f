 BEGIN_PROVIDER [ double precision, Fock_matrix_mo, (mo_tot_num_align,mo_tot_num) ]
&BEGIN_PROVIDER [ double precision, Fock_matrix_diag_mo, (mo_tot_num)]
   implicit none
   BEGIN_DOC
   ! Fock matrix on the MO basis.
   ! For open shells, the ROHF Fock Matrix is
   !
   !  |   F-K    |  F + K/2  |    F     |
   !  |---------------------------------|
   !  | F + K/2  |     F     |  F - K/2 |
   !  |---------------------------------|
   !  |    F     |  F - K/2  |  F + K   |
   !
   ! F = 1/2 (Fa + Fb)
   !
   ! K = Fb - Fa
   !
   END_DOC
   integer                        :: i,j,n
   if (elec_alpha_num == elec_beta_num) then
     Fock_matrix_mo = Fock_matrix_alpha_mo
   else
     
     do j=1,elec_beta_num
       ! F-K
       do i=1,elec_beta_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_alpha_mo(i,j)+Fock_matrix_beta_mo(i,j))&
             - (Fock_matrix_beta_mo(i,j) - Fock_matrix_alpha_mo(i,j))
       enddo
       ! F+K/2
       do i=elec_beta_num+1,elec_alpha_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_alpha_mo(i,j)+Fock_matrix_beta_mo(i,j))&
             + 0.5d0*(Fock_matrix_beta_mo(i,j) - Fock_matrix_alpha_mo(i,j))
       enddo
       ! F
       do i=elec_alpha_num+1, mo_tot_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_alpha_mo(i,j)+Fock_matrix_beta_mo(i,j))
       enddo
     enddo

     do j=elec_beta_num+1,elec_alpha_num
       ! F+K/2
       do i=1,elec_beta_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_alpha_mo(i,j)+Fock_matrix_beta_mo(i,j))&
             + 0.5d0*(Fock_matrix_beta_mo(i,j) - Fock_matrix_alpha_mo(i,j))
       enddo
       ! F
       do i=elec_beta_num+1,elec_alpha_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_alpha_mo(i,j)+Fock_matrix_beta_mo(i,j))
       enddo
       ! F-K/2
       do i=elec_alpha_num+1, mo_tot_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_alpha_mo(i,j)+Fock_matrix_beta_mo(i,j))&
             - 0.5d0*(Fock_matrix_beta_mo(i,j) - Fock_matrix_alpha_mo(i,j))
       enddo
     enddo

     do j=elec_alpha_num+1, mo_tot_num
       ! F
       do i=1,elec_beta_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_alpha_mo(i,j)+Fock_matrix_beta_mo(i,j))
       enddo
       ! F-K/2
       do i=elec_beta_num+1,elec_alpha_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_alpha_mo(i,j)+Fock_matrix_beta_mo(i,j))&
             - 0.5d0*(Fock_matrix_beta_mo(i,j) - Fock_matrix_alpha_mo(i,j))
       enddo
       ! F+K
       do i=elec_alpha_num+1,mo_tot_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_alpha_mo(i,j)+Fock_matrix_beta_mo(i,j)) &
             + (Fock_matrix_beta_mo(i,j) - Fock_matrix_alpha_mo(i,j))
       enddo
     enddo
     
   endif
   do i = 1, mo_tot_num
     Fock_matrix_diag_mo(i) = Fock_matrix_mo(i,i)
   enddo
END_PROVIDER
 
 
 
 BEGIN_PROVIDER [ double precision, Fock_matrix_alpha_ao, (ao_num_align, ao_num) ]
&BEGIN_PROVIDER [ double precision, Fock_matrix_beta_ao,  (ao_num_align, ao_num) ]
 implicit none
 BEGIN_DOC
 ! Alpha Fock matrix in AO basis set
 END_DOC
 
 integer                        :: i,j,k,l,k1,kmax
 double precision, allocatable  :: ao_ints_val(:)
 integer, allocatable           :: ao_ints_idx(:)
 double precision               :: integral
 double precision               :: ao_bielec_integral
 !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: ao_ints_idx, ao_ints_val
 if (do_direct_integrals) then
   PROVIDE all_utils ao_overlap_abs ao_integrals_threshold
   PROVIDE HF_density_matrix_ao_alpha  HF_density_matrix_ao_beta
   !$OMP PARALLEL DEFAULT(NONE) &
   !$OMP PRIVATE(i,j,l,k1,k,integral) &
   !$OMP SHARED(ao_num,Fock_matrix_alpha_ao,ao_mono_elec_integral,&
   !$OMP  ao_num_align,Fock_matrix_beta_ao,HF_density_matrix_ao_alpha, &
   !$OMP  HF_density_matrix_ao_beta) 
   !$OMP DO SCHEDULE(GUIDED)
   do j=1,ao_num
     do i=1,j
       Fock_matrix_alpha_ao(i,j) = ao_mono_elec_integral(i,j)
       Fock_matrix_beta_ao (i,j) = ao_mono_elec_integral(i,j)
       do l=1,ao_num
         do k=1,ao_num
           if ((abs(HF_density_matrix_ao_alpha(k,l)) > 1.d-9).or.    &
                 (abs(HF_density_matrix_ao_beta (k,l)) > 1.d-9)) then
             integral = (HF_density_matrix_ao_alpha(k,l)+HF_density_matrix_ao_beta (k,l)) * ao_bielec_integral(k,l,i,j)
             Fock_matrix_alpha_ao(i,j) += integral
             Fock_matrix_beta_ao (i,j) += integral

             integral = ao_bielec_integral(k,j,i,l)
             Fock_matrix_alpha_ao(i,j) -= HF_density_matrix_ao_alpha(k,l)*integral
             Fock_matrix_beta_ao (i,j) -= HF_density_matrix_ao_beta (k,l)*integral
           endif
         enddo
       enddo
       Fock_matrix_alpha_ao(j,i) = Fock_matrix_alpha_ao(i,j)
       Fock_matrix_beta_ao (j,i) = Fock_matrix_beta_ao (i,j)
     enddo
   enddo
   !$OMP END DO NOWAIT
   !$OMP END PARALLEL

 else
   !$OMP PARALLEL DEFAULT(NONE) &
   !$OMP PRIVATE(i,j,l,k1,k,integral,ao_ints_val,ao_ints_idx,kmax) &
   !$OMP SHARED(ao_num,Fock_matrix_alpha_ao,ao_mono_elec_integral,&
   !$OMP  ao_num_align,Fock_matrix_beta_ao,HF_density_matrix_ao_alpha, &
   !$OMP  HF_density_matrix_ao_beta) 
   allocate(ao_ints_idx(ao_num_align),ao_ints_val(ao_num_align))
   !$OMP DO SCHEDULE(GUIDED)
   do j=1,ao_num
     !DIR$ VECTOR ALIGNED
     do i=1,ao_num
       Fock_matrix_alpha_ao(i,j) = ao_mono_elec_integral(i,j)
       Fock_matrix_beta_ao (i,j) = ao_mono_elec_integral(i,j)
     enddo
     do l=1,ao_num
       do i=1,ao_num
         call get_ao_bielec_integrals_non_zero(i,l,j,ao_num,ao_ints_val,ao_ints_idx,kmax)
         !DIR$ VECTOR ALIGNED
         do k1=1,kmax
           k = ao_ints_idx(k1)
           integral = (HF_density_matrix_ao_alpha(k,l)+HF_density_matrix_ao_beta(k,l)) * ao_ints_val(k1)
           Fock_matrix_alpha_ao(i,j) += integral
           Fock_matrix_beta_ao (i,j) += integral
           integral = ao_ints_val(k1)
           Fock_matrix_alpha_ao(l,j) -= HF_density_matrix_ao_alpha(k,i) * integral
           Fock_matrix_beta_ao (l,j) -= HF_density_matrix_ao_beta (k,i) * integral
         enddo
       enddo
     enddo
   enddo
   !$OMP END DO
   deallocate(ao_ints_val,ao_ints_idx)
   !$OMP END PARALLEL
 endif

END_PROVIDER






BEGIN_PROVIDER [ double precision, Fock_matrix_alpha_mo, (mo_tot_num_align,mo_tot_num) ]
   implicit none
   BEGIN_DOC
   ! Fock matrix on the MO basis
   END_DOC
   double precision, allocatable  :: T(:,:)
   allocate ( T(ao_num_align,mo_tot_num) )
   !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: T
   call dgemm('N','N', ao_num, mo_tot_num, ao_num,                   &
       1.d0, Fock_matrix_alpha_ao,size(Fock_matrix_alpha_ao,1),      &
       mo_coef, size(mo_coef,1),                                     &
       0.d0, T, ao_num_align)
   call dgemm('T','N', mo_tot_num, mo_tot_num, ao_num,               &
       1.d0, mo_coef,size(mo_coef,1),                                &
       T, size(T,1),                                                 &
       0.d0, Fock_matrix_alpha_mo, mo_tot_num_align)
   deallocate(T)
END_PROVIDER
 
 
BEGIN_PROVIDER [ double precision, Fock_matrix_beta_mo, (mo_tot_num_align,mo_tot_num) ]
   implicit none
   BEGIN_DOC
   ! Fock matrix on the MO basis
   END_DOC
   double precision, allocatable  :: T(:,:)
   allocate ( T(ao_num_align,mo_tot_num) )
   !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: T
   call dgemm('N','N', ao_num, mo_tot_num, ao_num,                   &
       1.d0, Fock_matrix_beta_ao,size(Fock_matrix_beta_ao,1),        &
       mo_coef, size(mo_coef,1),                                     &
       0.d0, T, ao_num_align)
   call dgemm('T','N', mo_tot_num, mo_tot_num, ao_num,               &
       1.d0, mo_coef,size(mo_coef,1),                                &
       T, size(T,1),                                                 &
       0.d0, Fock_matrix_beta_mo, mo_tot_num_align)
   deallocate(T)
END_PROVIDER
 
BEGIN_PROVIDER [ double precision, HF_energy ]
 implicit none
 BEGIN_DOC
 ! Hartree-Fock energy
 END_DOC
  HF_energy = nuclear_repulsion + ref_bitmask_energy
END_PROVIDER


BEGIN_PROVIDER [ double precision, Fock_matrix_ao, (ao_num_align, ao_num) ]
 implicit none
 BEGIN_DOC
 ! Fock matrix in AO basis set
 END_DOC
 
 if (elec_alpha_num == elec_beta_num) then
 integer :: i,j
   do j=1,ao_num
    !DIR$ VECTOR ALIGNED
    do i=1,ao_num_align
     Fock_matrix_ao(i,j) = Fock_matrix_alpha_ao(i,j)
    enddo
   enddo
 else
   double precision, allocatable :: T(:,:), M(:,:)
   ! F_ao = S C F_mo C^t S
   allocate (T(mo_tot_num_align,mo_tot_num),M(ao_num_align,mo_tot_num))
   call dgemm('N','N', ao_num,ao_num,ao_num, 1.d0,                   &
       ao_overlap, size(ao_overlap,1),                               &
       mo_coef, size(mo_coef,1),                                     &
       0.d0,                                                         &
       M, size(M,1))
   call dgemm('N','N', ao_num,mo_tot_num,mo_tot_num, 1.d0,           &
       M, size(M,1),                                                 &
       Fock_matrix_mo, size(Fock_matrix_mo,1),                       &
       0.d0,                                                         &
       T, size(T,1))
   call dgemm('N','T', mo_tot_num,ao_num,mo_tot_num, 1.d0,           &
       T, size(T,1),                                                 &
       mo_coef, size(mo_coef,1),                                     &
       0.d0,                                                         &
       M, size(M,1))
   call dgemm('N','N', ao_num,ao_num,ao_num, 1.d0,                   &
       M, size(M,1),                                                 &
       ao_overlap, size(ao_overlap,1),                               &
       0.d0,                                                         &
       Fock_matrix_ao, size(Fock_matrix_ao,1))

   deallocate(T)
 endif
END_PROVIDER


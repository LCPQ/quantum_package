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
 !DIR$ ATTRIBUTES ALIGN : 32    :: ao_ints_idx, ao_ints_val
 if (do_direct_SCF) then
   do j=1,ao_num
     do i=1,ao_num
       Fock_matrix_alpha_ao(i,j) = ao_mono_elec_integral(i,j)
       Fock_matrix_beta_ao (i,j) = ao_mono_elec_integral(i,j)
       do l=1,ao_num
         do k=1,ao_num
           if ((abs(HF_density_matrix_ao_alpha(k,l)) > 1.d-9).or.    &
                 (abs(HF_density_matrix_ao_beta (k,l)) > 1.d-9)) then
             integral = 2.d0*ao_bielec_integral(k,l,i,j)-ao_bielec_integral(k,j,i,l)
             Fock_matrix_alpha_ao(i,j) =Fock_matrix_alpha_ao(i,j) +( HF_density_matrix_ao_alpha(k,l) * integral)
             Fock_matrix_beta_ao (i,j) =Fock_matrix_beta_ao (i,j) +( HF_density_matrix_ao_beta (k,l) * integral)
           endif
         enddo
       enddo
     enddo
   enddo
 else
   !$OMP PARALLEL DEFAULT(NONE) &
   !$OMP PRIVATE(i,j,l,k1,k,integral,ao_ints_val,ao_ints_idx,kmax) &
   !$OMP SHARED(ao_num,Fock_matrix_alpha_ao,ao_mono_elec_integral,&
   !$OMP  ao_num_align,Fock_matrix_beta_ao,HF_density_matrix_ao_alpha, &
   !$OMP  HF_density_matrix_ao_beta) 
   allocate(ao_ints_idx(ao_num_align),ao_ints_val(ao_num_align))
   !$OMP DO
   do i=1,ao_num
     do j=1,ao_num
       Fock_matrix_alpha_ao(i,j) = ao_mono_elec_integral(i,j)
       Fock_matrix_beta_ao (i,j) = ao_mono_elec_integral(i,j)
     enddo
     do j=1,ao_num
       do l=1,ao_num
         call get_ao_bielec_integrals_non_zero(i,l,j,ao_num,ao_ints_val,ao_ints_idx,kmax)
         do k1=1,kmax
           k = ao_ints_idx(k1)
           integral = ao_ints_val(k1)+ao_ints_val(k1)
           Fock_matrix_alpha_ao(i,j) += HF_density_matrix_ao_alpha(k,l) * integral
           Fock_matrix_beta_ao (i,j) += HF_density_matrix_ao_beta (k,l) * integral
           integral = -ao_ints_val(k1)
           Fock_matrix_alpha_ao(i,l) += HF_density_matrix_ao_alpha(k,j) * integral
           Fock_matrix_beta_ao (i,l) += HF_density_matrix_ao_beta (k,j) * integral
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
 

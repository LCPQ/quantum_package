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
 
 integer                        :: i,j
 do j=1,ao_num
   !DIR$ VECTOR ALIGNED
   do i=1,ao_num
     Fock_matrix_alpha_ao(i,j) = ao_mono_elec_integral(i,j) + ao_bi_elec_integral_alpha(i,j)
     Fock_matrix_beta_ao (i,j) = ao_mono_elec_integral(i,j) + ao_bi_elec_integral_beta (i,j)
   enddo
 enddo

END_PROVIDER


 BEGIN_PROVIDER [ double precision, ao_bi_elec_integral_alpha, (ao_num_align, ao_num) ]
&BEGIN_PROVIDER [ double precision, ao_bi_elec_integral_beta ,  (ao_num_align, ao_num) ]
 use map_module
 implicit none
 BEGIN_DOC
 ! Alpha Fock matrix in AO basis set
 END_DOC
 
 integer                        :: i,j,k,l,k1,r,s
 integer*8                      :: p,q
 double precision               :: integral
 double precision               :: ao_bielec_integral
 if (do_direct_integrals) then
   PROVIDE all_utils ao_overlap_abs ao_integrals_threshold gauleg_t2
   PROVIDE HF_density_matrix_ao_alpha  HF_density_matrix_ao_beta
   PROVIDE ao_bi_elec_integral_alpha

   ao_bi_elec_integral_alpha = 0.d0
   ao_bi_elec_integral_beta  = 0.d0
   !$OMP PARALLEL DEFAULT(NONE)                                      &
       !$OMP PRIVATE(i,j,l,k1,k,integral,ii,jj,kk,ll,i8,keys,values,p,q,r,s)&
       !$OMP SHARED(ao_num,HF_density_matrix_ao_alpha,HF_density_matrix_ao_beta,&
       !$OMP ao_integrals_map,ao_integrals_threshold, ao_bielec_integral_schwartz, &
       !$OMP ao_overlap_abs) &
       !$OMP REDUCTION(+:ao_bi_elec_integral_alpha,ao_bi_elec_integral_beta)

   allocate(keys(1), values(1))

   q = ao_num*ao_num*ao_num*ao_num
   !$OMP DO SCHEDULE(dynamic)
   do p=1_8,q
           call bielec_integrals_index_reverse(kk,ii,ll,jj,p)
           if ( (kk(1)>ao_num).or. &
                (ii(1)>ao_num).or. &
                (jj(1)>ao_num).or. &
                (ll(1)>ao_num) ) then
                cycle
           endif
           k = kk(1)
           i = ii(1)
           l = ll(1)
           j = jj(1)

           if (ao_overlap_abs(k,l)*ao_overlap_abs(i,j)  &
              < ao_integrals_threshold) then
             cycle
           endif
           if (ao_bielec_integral_schwartz(k,l)*ao_bielec_integral_schwartz(i,j)  &
              < ao_integrals_threshold) then
             cycle
           endif
           values(1) = ao_bielec_integral(k,l,i,j)
           if (abs(values(1)) < ao_integrals_threshold) then
             cycle
           endif
           do k2=1,8
             if (kk(k2)==0) then
               cycle
             endif
             i = ii(k2)
             j = jj(k2)
             k = kk(k2)
             l = ll(k2)
             integral = (HF_density_matrix_ao_alpha(k,l)+HF_density_matrix_ao_beta(k,l)) * values(1)
             ao_bi_elec_integral_alpha(i,j) += integral
             ao_bi_elec_integral_beta (i,j) += integral
             integral = values(1)
             ao_bi_elec_integral_alpha(l,j) -= HF_density_matrix_ao_alpha(k,i) * integral
             ao_bi_elec_integral_beta (l,j) -= HF_density_matrix_ao_beta (k,i) * integral
           enddo
   enddo
   !$OMP END DO
   deallocate(keys,values)
   !$OMP END PARALLEL
 else
   PROVIDE ao_bielec_integrals_in_map 
           
   integer(omp_lock_kind) :: lck(ao_num)
   integer*8                      :: i8
   integer                        :: ii(8), jj(8), kk(8), ll(8), k2
   integer(cache_map_size_kind)   :: n_elements_max, n_elements
   integer(key_kind), allocatable :: keys(:)
   double precision, allocatable  :: values(:)

   ao_bi_elec_integral_alpha = 0.d0
   ao_bi_elec_integral_beta  = 0.d0
   !$OMP PARALLEL DEFAULT(NONE)                                      &
       !$OMP PRIVATE(i,j,l,k1,k,integral,ii,jj,kk,ll,i8,keys,values,n_elements_max,n_elements)&
       !$OMP SHARED(ao_num,HF_density_matrix_ao_alpha,HF_density_matrix_ao_beta,&
       !$OMP ao_integrals_map) &
       !$OMP REDUCTION(+:ao_bi_elec_integral_alpha,ao_bi_elec_integral_beta)

   call get_cache_map_n_elements_max(ao_integrals_map,n_elements_max)
   allocate(keys(n_elements_max), values(n_elements_max))

   !$OMP DO SCHEDULE(dynamic)
   do i8=0_8,ao_integrals_map%map_size
     n_elements = n_elements_max
     call get_cache_map(ao_integrals_map,i8,keys,values,n_elements)
     if (n_elements == 0) then
       cycle
     endif
     do k1=1,n_elements
       call bielec_integrals_index_reverse(kk,ii,ll,jj,keys(k1))
       do k2=1,8
         if (kk(k2)==0) then
           cycle
         endif
         i = ii(k2)
         j = jj(k2)
         k = kk(k2)
         l = ll(k2)
         integral = (HF_density_matrix_ao_alpha(k,l)+HF_density_matrix_ao_beta(k,l)) * values(k1)
         ao_bi_elec_integral_alpha(i,j) += integral
         ao_bi_elec_integral_beta (i,j) += integral
         integral = values(k1)
         ao_bi_elec_integral_alpha(l,j) -= HF_density_matrix_ao_alpha(k,i) * integral
         ao_bi_elec_integral_beta (l,j) -= HF_density_matrix_ao_beta (k,i) * integral
       enddo
     enddo
   enddo
   !$OMP END DO
   deallocate(keys,values)
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
   allocate (T(ao_num_align,ao_num),M(ao_num_align,ao_num))
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

subroutine Fock_mo_to_ao(FMO,LDFMO,FAO,LDFAO)
  implicit none
  integer, intent(in)            :: LDFMO ! size(FMO,1)
  integer, intent(in)            :: LDFAO ! size(FAO,1)
  double precision, intent(in)   :: FMO(LDFMO,*)
  double precision, intent(out)  :: FAO(LDFAO,*)
  
  double precision, allocatable  :: T(:,:), M(:,:)
  ! F_ao = S C F_mo C^t S
  allocate (T(ao_num_align,ao_num),M(ao_num_align,ao_num))
  call dgemm('N','N', ao_num,ao_num,ao_num, 1.d0,                    &
      ao_overlap, size(ao_overlap,1),                                &
      mo_coef, size(mo_coef,1),                                      &
      0.d0,                                                          &
      M, size(M,1))
  call dgemm('N','N', ao_num,mo_tot_num,mo_tot_num, 1.d0,            &
      M, size(M,1),                                                  &
      FMO, size(FMO,1),                                              &
      0.d0,                                                          &
      T, size(T,1))
  call dgemm('N','T', mo_tot_num,ao_num,mo_tot_num, 1.d0,            &
      T, size(T,1),                                                  &
      mo_coef, size(mo_coef,1),                                      &
      0.d0,                                                          &
      M, size(M,1))
  call dgemm('N','N', ao_num,ao_num,ao_num, 1.d0,                    &
      M, size(M,1),                                                  &
      ao_overlap, size(ao_overlap,1),                                &
      0.d0,                                                          &
      FAO, size(FAO,1))
  deallocate(T,M)
end


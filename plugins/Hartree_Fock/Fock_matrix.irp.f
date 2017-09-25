 BEGIN_PROVIDER [ double precision, Fock_matrix_mo, (mo_tot_num,mo_tot_num) ]
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
     Fock_matrix_mo = Fock_matrix_mo_alpha
   else
     
     do j=1,elec_beta_num
       ! F-K
       do i=1,elec_beta_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_mo_alpha(i,j)+Fock_matrix_mo_beta(i,j))&
             - (Fock_matrix_mo_beta(i,j) - Fock_matrix_mo_alpha(i,j))
       enddo
       ! F+K/2
       do i=elec_beta_num+1,elec_alpha_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_mo_alpha(i,j)+Fock_matrix_mo_beta(i,j))&
             + 0.5d0*(Fock_matrix_mo_beta(i,j) - Fock_matrix_mo_alpha(i,j))
       enddo
       ! F
       do i=elec_alpha_num+1, mo_tot_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_mo_alpha(i,j)+Fock_matrix_mo_beta(i,j))
       enddo
     enddo

     do j=elec_beta_num+1,elec_alpha_num
       ! F+K/2
       do i=1,elec_beta_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_mo_alpha(i,j)+Fock_matrix_mo_beta(i,j))&
             + 0.5d0*(Fock_matrix_mo_beta(i,j) - Fock_matrix_mo_alpha(i,j))
       enddo
       ! F
       do i=elec_beta_num+1,elec_alpha_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_mo_alpha(i,j)+Fock_matrix_mo_beta(i,j))
       enddo
       ! F-K/2
       do i=elec_alpha_num+1, mo_tot_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_mo_alpha(i,j)+Fock_matrix_mo_beta(i,j))&
             - 0.5d0*(Fock_matrix_mo_beta(i,j) - Fock_matrix_mo_alpha(i,j))
       enddo
     enddo

     do j=elec_alpha_num+1, mo_tot_num
       ! F
       do i=1,elec_beta_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_mo_alpha(i,j)+Fock_matrix_mo_beta(i,j))
       enddo
       ! F-K/2
       do i=elec_beta_num+1,elec_alpha_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_mo_alpha(i,j)+Fock_matrix_mo_beta(i,j))&
             - 0.5d0*(Fock_matrix_mo_beta(i,j) - Fock_matrix_mo_alpha(i,j))
       enddo
       ! F+K
       do i=elec_alpha_num+1,mo_tot_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_mo_alpha(i,j)+Fock_matrix_mo_beta(i,j)) &
             + (Fock_matrix_mo_beta(i,j) - Fock_matrix_mo_alpha(i,j))
       enddo
     enddo
     
   endif

   do i = 1, mo_tot_num
     Fock_matrix_diag_mo(i) = Fock_matrix_mo(i,i) 
   enddo
END_PROVIDER
 
 
 
 BEGIN_PROVIDER [ double precision, Fock_matrix_ao_alpha, (ao_num, ao_num) ]
&BEGIN_PROVIDER [ double precision, Fock_matrix_ao_beta,  (ao_num, ao_num) ]
 implicit none
 BEGIN_DOC
 ! Alpha Fock matrix in AO basis set
 END_DOC
 
 integer                        :: i,j
 do j=1,ao_num
   do i=1,ao_num
     Fock_matrix_ao_alpha(i,j) = ao_mono_elec_integral(i,j) + ao_bi_elec_integral_alpha(i,j)
     Fock_matrix_ao_beta (i,j) = ao_mono_elec_integral(i,j) + ao_bi_elec_integral_beta (i,j)
   enddo
 enddo

END_PROVIDER


 BEGIN_PROVIDER [ double precision, ao_bi_elec_integral_alpha, (ao_num, ao_num) ]
&BEGIN_PROVIDER [ double precision, ao_bi_elec_integral_beta ,  (ao_num, ao_num) ]
 use map_module
 implicit none
 BEGIN_DOC
 ! Alpha Fock matrix in AO basis set
 END_DOC
 
 integer                        :: i,j,k,l,k1,r,s
 integer                        :: i0,j0,k0,l0
 integer*8                      :: p,q
 double precision               :: integral, c0, c1, c2
 double precision               :: ao_bielec_integral, local_threshold
 double precision, allocatable  :: ao_bi_elec_integral_alpha_tmp(:,:)
 double precision, allocatable  :: ao_bi_elec_integral_beta_tmp(:,:)

 ao_bi_elec_integral_alpha = 0.d0
 ao_bi_elec_integral_beta  = 0.d0
 if (do_direct_integrals) then

   !$OMP PARALLEL DEFAULT(NONE)                                      &
       !$OMP PRIVATE(i,j,l,k1,k,integral,ii,jj,kk,ll,i8,keys,values,p,q,r,s,i0,j0,k0,l0, &
       !$OMP ao_bi_elec_integral_alpha_tmp,ao_bi_elec_integral_beta_tmp, c0, c1, c2, &
       !$OMP local_threshold)&
       !$OMP SHARED(ao_num,HF_density_matrix_ao_alpha,HF_density_matrix_ao_beta,&
       !$OMP ao_integrals_map,ao_integrals_threshold, ao_bielec_integral_schwartz, &
       !$OMP ao_overlap_abs, ao_bi_elec_integral_alpha, ao_bi_elec_integral_beta)

   allocate(keys(1), values(1))
   allocate(ao_bi_elec_integral_alpha_tmp(ao_num,ao_num), &
            ao_bi_elec_integral_beta_tmp(ao_num,ao_num))
   ao_bi_elec_integral_alpha_tmp = 0.d0
   ao_bi_elec_integral_beta_tmp  = 0.d0

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
           local_threshold = ao_bielec_integral_schwartz(k,l)*ao_bielec_integral_schwartz(i,j)
           if (local_threshold < ao_integrals_threshold) then
             cycle
           endif
           i0 = i
           j0 = j
           k0 = k
           l0 = l
           values(1) = 0.d0
           local_threshold = ao_integrals_threshold/local_threshold
           do k2=1,8
             if (kk(k2)==0) then
               cycle
             endif
             i = ii(k2)
             j = jj(k2)
             k = kk(k2)
             l = ll(k2)
             c0 = HF_density_matrix_ao_alpha(k,l)+HF_density_matrix_ao_beta(k,l)
             c1 = HF_density_matrix_ao_alpha(k,i)
             c2 = HF_density_matrix_ao_beta(k,i)
             if ( dabs(c0)+dabs(c1)+dabs(c2) < local_threshold) then
               cycle
             endif
             if (values(1) == 0.d0) then
               values(1) = ao_bielec_integral(k0,l0,i0,j0)
             endif
             integral = c0 * values(1)
             ao_bi_elec_integral_alpha_tmp(i,j) += integral
             ao_bi_elec_integral_beta_tmp (i,j) += integral
             integral = values(1)
             ao_bi_elec_integral_alpha_tmp(l,j) -= c1 * integral
             ao_bi_elec_integral_beta_tmp (l,j) -= c2 * integral
           enddo
   enddo
   !$OMP END DO NOWAIT
   !$OMP CRITICAL
   ao_bi_elec_integral_alpha += ao_bi_elec_integral_alpha_tmp
   !$OMP END CRITICAL
   !$OMP CRITICAL
   ao_bi_elec_integral_beta  += ao_bi_elec_integral_beta_tmp
   !$OMP END CRITICAL
   deallocate(keys,values,ao_bi_elec_integral_alpha_tmp,ao_bi_elec_integral_beta_tmp)
   !$OMP END PARALLEL
 else
   PROVIDE ao_bielec_integrals_in_map 
           
   integer(omp_lock_kind) :: lck(ao_num)
   integer*8                      :: i8
   integer                        :: ii(8), jj(8), kk(8), ll(8), k2
   integer(cache_map_size_kind)   :: n_elements_max, n_elements
   integer(key_kind), allocatable :: keys(:)
   double precision, allocatable  :: values(:)

   !$OMP PARALLEL DEFAULT(NONE)                                      &
       !$OMP PRIVATE(i,j,l,k1,k,integral,ii,jj,kk,ll,i8,keys,values,n_elements_max, &
       !$OMP  n_elements,ao_bi_elec_integral_alpha_tmp,ao_bi_elec_integral_beta_tmp)&
       !$OMP SHARED(ao_num,HF_density_matrix_ao_alpha,HF_density_matrix_ao_beta,&
       !$OMP  ao_integrals_map, ao_bi_elec_integral_alpha, ao_bi_elec_integral_beta) 

   call get_cache_map_n_elements_max(ao_integrals_map,n_elements_max)
   allocate(keys(n_elements_max), values(n_elements_max))
   allocate(ao_bi_elec_integral_alpha_tmp(ao_num,ao_num), &
            ao_bi_elec_integral_beta_tmp(ao_num,ao_num))
   ao_bi_elec_integral_alpha_tmp = 0.d0
   ao_bi_elec_integral_beta_tmp  = 0.d0

   !$OMP DO SCHEDULE(dynamic)
   !DIR$ NOVECTOR
   do i8=0_8,ao_integrals_map%map_size
     n_elements = n_elements_max
     call get_cache_map(ao_integrals_map,i8,keys,values,n_elements)
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
         ao_bi_elec_integral_alpha_tmp(i,j) += integral
         ao_bi_elec_integral_beta_tmp (i,j) += integral
         integral = values(k1)
         ao_bi_elec_integral_alpha_tmp(l,j) -= HF_density_matrix_ao_alpha(k,i) * integral
         ao_bi_elec_integral_beta_tmp (l,j) -= HF_density_matrix_ao_beta (k,i) * integral
       enddo
     enddo
   enddo
   !$OMP END DO NOWAIT
   !$OMP CRITICAL
   ao_bi_elec_integral_alpha += ao_bi_elec_integral_alpha_tmp
   !$OMP END CRITICAL
   !$OMP CRITICAL
   ao_bi_elec_integral_beta  += ao_bi_elec_integral_beta_tmp
   !$OMP END CRITICAL
   deallocate(keys,values,ao_bi_elec_integral_alpha_tmp,ao_bi_elec_integral_beta_tmp)
   !$OMP END PARALLEL

 endif

END_PROVIDER

BEGIN_PROVIDER [ double precision, Fock_matrix_mo_alpha, (mo_tot_num,mo_tot_num) ]
   implicit none
   BEGIN_DOC
   ! Fock matrix on the MO basis
   END_DOC
   call ao_to_mo(Fock_matrix_ao_alpha,size(Fock_matrix_ao_alpha,1), &
                 Fock_matrix_mo_alpha,size(Fock_matrix_mo_alpha,1))
END_PROVIDER
 
 
BEGIN_PROVIDER [ double precision, Fock_matrix_mo_beta, (mo_tot_num,mo_tot_num) ]
   implicit none
   BEGIN_DOC
   ! Fock matrix on the MO basis
   END_DOC
   call ao_to_mo(Fock_matrix_ao_beta,size(Fock_matrix_ao_beta,1), &
                 Fock_matrix_mo_beta,size(Fock_matrix_mo_beta,1))
END_PROVIDER
 
BEGIN_PROVIDER [ double precision, HF_energy ]
 implicit none
 BEGIN_DOC
 ! Hartree-Fock energy
 END_DOC
 HF_energy = nuclear_repulsion
 
 integer                        :: i,j
 do j=1,ao_num
   do i=1,ao_num
     HF_energy += 0.5d0 * (                                          &
         (ao_mono_elec_integral(i,j) + Fock_matrix_ao_alpha(i,j) ) *  HF_density_matrix_ao_alpha(i,j) +&
         (ao_mono_elec_integral(i,j) + Fock_matrix_ao_beta (i,j) ) *  HF_density_matrix_ao_beta (i,j) )
   enddo
 enddo
  
END_PROVIDER


BEGIN_PROVIDER [ double precision, Fock_matrix_ao, (ao_num, ao_num) ]
 implicit none
 BEGIN_DOC
 ! Fock matrix in AO basis set
 END_DOC
 
 if ( (elec_alpha_num == elec_beta_num).and. &
      (level_shift == 0.) ) &
 then
   integer                        :: i,j
   do j=1,ao_num
     do i=1,ao_num
       Fock_matrix_ao(i,j) = Fock_matrix_ao_alpha(i,j)
     enddo
   enddo
 else
   call mo_to_ao(Fock_matrix_mo,size(Fock_matrix_mo,1), &
      Fock_matrix_ao,size(Fock_matrix_ao,1)) 
 endif
END_PROVIDER



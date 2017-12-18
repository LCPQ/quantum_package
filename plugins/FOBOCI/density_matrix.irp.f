 BEGIN_PROVIDER [ double precision, one_body_dm_mo_alpha_generators_restart, (mo_tot_num,mo_tot_num) ]
&BEGIN_PROVIDER [ double precision, one_body_dm_mo_beta_generators_restart, (mo_tot_num,mo_tot_num) ]
&BEGIN_PROVIDER [ double precision, norm_generators_restart]
   implicit none
   BEGIN_DOC
   ! Alpha and beta one-body density matrix for the generators restart
   END_DOC

   integer                        :: j,k,l,m
   integer                        :: occ(N_int*bit_kind_size,2)
   double precision               :: ck, cl, ckl
   double precision               :: phase
   integer                        :: h1,h2,p1,p2,s1,s2, degree
   integer                        :: exc(0:2,2,2),n_occ_alpha
   double precision, allocatable  :: tmp_a(:,:), tmp_b(:,:)
   integer :: degree_respect_to_HF_k
   integer :: degree_respect_to_HF_l,index_ref_generators_restart
   double precision :: inv_coef_ref_generators_restart
   integer :: i

   do i = 1, N_det_generators_restart
    ! Find the reference determinant for intermediate normalization
    call get_excitation_degree(ref_generators_restart,psi_det_generators_restart(1,1,i),degree,N_int)   
    if(degree == 0)then
     index_ref_generators_restart = i
     inv_coef_ref_generators_restart = 1.d0/psi_coef_generators_restart(i,1)
     exit
    endif
   enddo
   norm_generators_restart = 0.d0
   do i = 1, N_det_generators_restart
    psi_coef_generators_restart(i,1) = psi_coef_generators_restart(i,1) * inv_coef_ref_generators_restart
    norm_generators_restart += psi_coef_generators_restart(i,1)**2
   enddo


     one_body_dm_mo_alpha_generators_restart = 0.d0
     one_body_dm_mo_beta_generators_restart  = 0.d0
     !$OMP PARALLEL DEFAULT(NONE)                                         &
        !$OMP PRIVATE(j,k,l,m,occ,ck, cl, ckl,phase,h1,h2,p1,p2,s1,s2, degree,exc, &
        !$OMP  tmp_a, tmp_b, n_occ_alpha)&
        !$OMP SHARED(psi_det_generators_restart,psi_coef_generators_restart,N_int,elec_alpha_num,&
        !$OMP  elec_beta_num,one_body_dm_mo_alpha_generators_restart,one_body_dm_mo_beta_generators_restart,N_det_generators_restart,&
        !$OMP  mo_tot_num,N_states, state_average_weight)
     allocate(tmp_a(mo_tot_num,mo_tot_num), tmp_b(mo_tot_num,mo_tot_num) )
     tmp_a = 0.d0
     tmp_b = 0.d0
     !$OMP DO SCHEDULE(dynamic)
     do k=1,N_det_generators_restart
       call bitstring_to_list(psi_det_generators_restart(1,1,k), occ(1,1), n_occ_alpha, N_int)
       call bitstring_to_list(psi_det_generators_restart(1,2,k), occ(1,2), n_occ_alpha, N_int)
       do m=1,N_states
         ck = psi_coef_generators_restart(k,m)*psi_coef_generators_restart(k,m) * state_average_weight(m)
         do l=1,elec_alpha_num
           j = occ(l,1)
           tmp_a(j,j) += ck
         enddo
         do l=1,elec_beta_num
           j = occ(l,2)
           tmp_b(j,j) += ck
         enddo
       enddo
       do l=1,k-1
         call get_excitation_degree(psi_det_generators_restart(1,1,k),psi_det_generators_restart(1,1,l),degree,N_int)
         if (degree /= 1) then
           cycle
         endif
         call get_mono_excitation(psi_det_generators_restart(1,1,k),psi_det_generators_restart(1,1,l),exc,phase,N_int)
         call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
         do m=1,N_states
           ckl = psi_coef_generators_restart(k,m) * psi_coef_generators_restart(l,m) * phase * state_average_weight(m)
           if (s1==1) then
             tmp_a(h1,p1) += ckl
             tmp_a(p1,h1) += ckl
           else
             tmp_b(h1,p1) += ckl
             tmp_b(p1,h1) += ckl
           endif
         enddo
       enddo
     enddo
     !$OMP END DO NOWAIT
     !$OMP CRITICAL
     one_body_dm_mo_alpha_generators_restart = one_body_dm_mo_alpha_generators_restart + tmp_a
     !$OMP END CRITICAL
     !$OMP CRITICAL
     one_body_dm_mo_beta_generators_restart  = one_body_dm_mo_beta_generators_restart  + tmp_b
     !$OMP END CRITICAL
     deallocate(tmp_a,tmp_b)
     !$OMP BARRIER
     !$OMP END PARALLEL

     do i = 1, mo_tot_num
      print*,'DM restat',i,one_body_dm_mo_beta_generators_restart(i,i) + one_body_dm_mo_alpha_generators_restart(i,i)
     enddo

END_PROVIDER



BEGIN_PROVIDER [ double precision, one_body_dm_mo_generators_restart, (mo_tot_num,mo_tot_num) ]
 implicit none
 BEGIN_DOC
 ! One-body density matrix for the generators_restart
 END_DOC
 one_body_dm_mo_generators_restart = one_body_dm_mo_alpha_generators_restart + one_body_dm_mo_beta_generators_restart
END_PROVIDER

BEGIN_PROVIDER [ double precision, one_body_spin_density_mo_generators_restart, (mo_tot_num,mo_tot_num) ]
 implicit none
 BEGIN_DOC
 ! rho(alpha) - rho(beta)
 END_DOC
 one_body_spin_density_mo_generators_restart = one_body_dm_mo_alpha_generators_restart - one_body_dm_mo_beta_generators_restart
END_PROVIDER


 BEGIN_PROVIDER [ double precision, one_body_dm_mo_alpha_osoci, (mo_tot_num,mo_tot_num) ]
&BEGIN_PROVIDER [ double precision, one_body_dm_mo_beta_osoci, (mo_tot_num,mo_tot_num) ]
   implicit none
   BEGIN_DOC
   ! Alpha and beta one-body density matrix that will be used for the OSOCI approach
   END_DOC
END_PROVIDER

 BEGIN_PROVIDER [ double precision, one_body_dm_mo_alpha_1h1p, (mo_tot_num,mo_tot_num) ]
&BEGIN_PROVIDER [ double precision, one_body_dm_mo_beta_1h1p, (mo_tot_num,mo_tot_num) ]
   implicit none
   BEGIN_DOC
   ! Alpha and beta one-body density matrix that will be used for the 1h1p approach
   END_DOC
END_PROVIDER


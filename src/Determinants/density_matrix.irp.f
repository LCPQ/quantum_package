 BEGIN_PROVIDER [ double precision, one_body_dm_mo_alpha, (mo_tot_num_align,mo_tot_num) ]
&BEGIN_PROVIDER [ double precision, one_body_dm_mo_beta, (mo_tot_num_align,mo_tot_num) ]
   implicit none
   BEGIN_DOC
   ! Alpha and beta one-body density matrix for each state
   END_DOC

   integer                        :: j,k,l,m
   integer                        :: occ(N_int*bit_kind_size,2)
   double precision               :: ck, cl, ckl
   double precision               :: phase
   integer                        :: h1,h2,p1,p2,s1,s2, degree
   integer                        :: exc(0:2,2,2),n_occ_alpha
   double precision, allocatable  :: tmp_a(:,:), tmp_b(:,:)

   if(only_single_double_dm)then
     print*,'ONLY DOUBLE DM'
     one_body_dm_mo_alpha = one_body_single_double_dm_mo_alpha
     one_body_dm_mo_beta  = one_body_single_double_dm_mo_beta 
   else 
     one_body_dm_mo_alpha = 0.d0
     one_body_dm_mo_beta  = 0.d0
     !$OMP PARALLEL DEFAULT(NONE)                                         &
        !$OMP PRIVATE(j,k,l,m,occ,ck, cl, ckl,phase,h1,h2,p1,p2,s1,s2, degree,exc, &
        !$OMP  tmp_a, tmp_b, n_occ_alpha)&
        !$OMP SHARED(psi_det,psi_coef,N_int,N_states,state_average_weight,elec_alpha_num,&
        !$OMP  elec_beta_num,one_body_dm_mo_alpha,one_body_dm_mo_beta,N_det,mo_tot_num_align,&
        !$OMP  mo_tot_num)
     allocate(tmp_a(mo_tot_num_align,mo_tot_num), tmp_b(mo_tot_num_align,mo_tot_num) )
     tmp_a = 0.d0
     tmp_b = 0.d0
     !$OMP DO SCHEDULE(dynamic)
     do k=1,N_det
       call bitstring_to_list(psi_det(1,1,k), occ(1,1), n_occ_alpha, N_int)
       call bitstring_to_list(psi_det(1,2,k), occ(1,2), n_occ_alpha, N_int)
       do m=1,N_states
         ck = psi_coef(k,m)*psi_coef(k,m) * state_average_weight(m)
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
         call get_excitation_degree(psi_det(1,1,k),psi_det(1,1,l),degree,N_int)
         if (degree /= 1) then
           cycle
         endif
         call get_mono_excitation(psi_det(1,1,k),psi_det(1,1,l),exc,phase,N_int)
         call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
         do m=1,N_states
           ckl = psi_coef(k,m) * psi_coef(l,m) * phase * state_average_weight(m)
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
     one_body_dm_mo_alpha = one_body_dm_mo_alpha + tmp_a
     !$OMP END CRITICAL
     !$OMP CRITICAL
     one_body_dm_mo_beta  = one_body_dm_mo_beta  + tmp_b
     !$OMP END CRITICAL
     deallocate(tmp_a,tmp_b)
     !$OMP END PARALLEL

   endif
END_PROVIDER

 BEGIN_PROVIDER [ double precision, one_body_single_double_dm_mo_alpha, (mo_tot_num_align,mo_tot_num) ]
&BEGIN_PROVIDER [ double precision, one_body_single_double_dm_mo_beta, (mo_tot_num_align,mo_tot_num) ]
   implicit none
   BEGIN_DOC
   ! Alpha and beta one-body density matrix for each state
   END_DOC

   integer                        :: j,k,l,m
   integer                        :: occ(N_int*bit_kind_size,2)
   double precision               :: ck, cl, ckl
   double precision               :: phase
   integer                        :: h1,h2,p1,p2,s1,s2, degree
   integer                        :: exc(0:2,2,2),n_occ_alpha
   double precision, allocatable  :: tmp_a(:,:), tmp_b(:,:)
   integer :: degree_respect_to_HF_k
   integer :: degree_respect_to_HF_l

   PROVIDE elec_alpha_num elec_beta_num

   one_body_single_double_dm_mo_alpha = 0.d0
   one_body_single_double_dm_mo_beta  = 0.d0
   !$OMP PARALLEL DEFAULT(NONE)                                         &
      !$OMP PRIVATE(j,k,l,m,occ,ck, cl, ckl,phase,h1,h2,p1,p2,s1,s2, degree,exc, &
      !$OMP  tmp_a, tmp_b, n_occ_alpha,degree_respect_to_HF_k,degree_respect_to_HF_l)&
      !$OMP SHARED(ref_bitmask,psi_det,psi_coef,N_int,N_states,state_average_weight,elec_alpha_num,&
      !$OMP  elec_beta_num,one_body_single_double_dm_mo_alpha,one_body_single_double_dm_mo_beta,N_det,mo_tot_num_align,&
      !$OMP  mo_tot_num)
   allocate(tmp_a(mo_tot_num_align,mo_tot_num), tmp_b(mo_tot_num_align,mo_tot_num) )
   tmp_a = 0.d0
   tmp_b = 0.d0
   !$OMP DO SCHEDULE(dynamic)
   do k=1,N_det
     call bitstring_to_list(psi_det(1,1,k), occ(1,1), n_occ_alpha, N_int)
     call bitstring_to_list(psi_det(1,2,k), occ(1,2), n_occ_alpha, N_int)
     call get_excitation_degree(ref_bitmask,psi_det(1,1,k),degree_respect_to_HF_k,N_int)
     
     do m=1,N_states
       ck = psi_coef(k,m)*psi_coef(k,m) * state_average_weight(m)
       call get_excitation_degree(ref_bitmask,psi_det(1,1,k),degree_respect_to_HF_l,N_int)
       if(degree_respect_to_HF_l.le.0)then
         do l=1,elec_alpha_num
           j = occ(l,1)
           tmp_a(j,j) += ck
         enddo
         do l=1,elec_beta_num
           j = occ(l,2)
           tmp_b(j,j) += ck
         enddo
       endif
     enddo
     do l=1,k-1
       call get_excitation_degree(ref_bitmask,psi_det(1,1,l),degree_respect_to_HF_l,N_int)
       if(degree_respect_to_HF_k.ne.0)cycle
       if(degree_respect_to_HF_l.eq.2.and.degree_respect_to_HF_k.ne.2)cycle
       call get_excitation_degree(psi_det(1,1,k),psi_det(1,1,l),degree,N_int)
       if (degree /= 1) then
         cycle
       endif
       call get_mono_excitation(psi_det(1,1,k),psi_det(1,1,l),exc,phase,N_int)
       call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
       do m=1,N_states
         ckl = psi_coef(k,m) * psi_coef(l,m) * phase * state_average_weight(m)
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
   one_body_single_double_dm_mo_alpha = one_body_single_double_dm_mo_alpha + tmp_a
   !$OMP END CRITICAL
   !$OMP CRITICAL
   one_body_single_double_dm_mo_beta  = one_body_single_double_dm_mo_beta  + tmp_b
   !$OMP END CRITICAL
   deallocate(tmp_a,tmp_b)
   !$OMP END PARALLEL
END_PROVIDER

BEGIN_PROVIDER [ double precision, one_body_dm_mo, (mo_tot_num_align,mo_tot_num) ]
 implicit none
 BEGIN_DOC
 ! One-body density matrix
 END_DOC
 one_body_dm_mo = one_body_dm_mo_alpha + one_body_dm_mo_beta
END_PROVIDER

BEGIN_PROVIDER [ double precision, one_body_spin_density_mo, (mo_tot_num_align,mo_tot_num) ]
 implicit none
 BEGIN_DOC
 ! rho(alpha) - rho(beta)
 END_DOC
 one_body_spin_density_mo = one_body_dm_mo_alpha - one_body_dm_mo_beta
END_PROVIDER

subroutine set_natural_mos
 implicit none
 BEGIN_DOC
 ! Set natural orbitals, obtained by diagonalization of the one-body density matrix in the MO basis
 END_DOC
 character*(64) :: label
 double precision, allocatable :: tmp(:,:)
 allocate(tmp(size(one_body_dm_mo,1),size(one_body_dm_mo,2)))

 ! Negation to have the occupied MOs first after the diagonalization
 tmp = -one_body_dm_mo
 label = "Natural"
 call mo_as_eigvectors_of_mo_matrix(tmp,size(tmp,1),size(tmp,2),label)
 deallocate(tmp)

end
subroutine save_natural_mos
 implicit none
 BEGIN_DOC
 ! Save natural orbitals, obtained by diagonalization of the one-body density matrix in the MO basis
 END_DOC
 call set_natural_mos
 call save_mos
 
end


BEGIN_PROVIDER [ double precision, state_average_weight, (N_states) ]
 implicit none
 BEGIN_DOC
 ! Weights in the state-average calculation of the density matrix
 END_DOC
 state_average_weight = 1.d0/dble(N_states)
END_PROVIDER


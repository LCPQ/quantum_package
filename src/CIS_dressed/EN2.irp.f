
 BEGIN_PROVIDER [double precision, EN2_corr_energy]
&BEGIN_PROVIDER [double precision, p_imp_EN,(elec_alpha_num+1:n_act_cis)]
&BEGIN_PROVIDER [double precision, h_imp_EN,(n_core_cis+1:elec_alpha_num)]
&BEGIN_PROVIDER [double precision, hp_imp_EN,(n_core_cis+1:elec_alpha_num,elec_alpha_num+1:n_act_cis)]

 BEGIN_DOC
 !Calculation of the EN2 correlation energy (EN2_corr_energy)
 !and calculation of the contribution of the disconnected Triples on the 
 !Singles, via the impossible (p_imp_EN, h_imp_EN, hp_imp_EN)
 END_DOC

 implicit none
 integer :: i,j,k,l !variables for going over the occupied (i,j) and virutal (k,l)
 double precision :: direct,exchg,hij !calculating direct, exchange and total contribution
 double precision :: get_mo_bielec_integral
 double precision :: e_i,e_k,e_j,e_l !epsilons of i,j,k,l
 double precision :: delta_e_ik,delta_e_ikj
 double precision :: delta_e !delta epsilons
 double precision :: delta_e_tmp,H_jj_total
 integer :: ispin1,ispin2,i_ok

 print*,'EN2_corr_energy'

 EN2_corr_energy=0.d0

 do i=n_core_cis+1,elec_alpha_num
  h_imp_EN(i)=0.d0
  do k =elec_beta_num + 1, n_act_cis
   p_imp_EN(k)=0.d0
   hp_imp_EN(i,k)=0.d0
  enddo
 enddo
 print*,'HF_energy = ',HF_energy
 print*,'1'

 if(EN_2_2)then
 do i=n_core_cis+1,elec_alpha_num
  h_imp_EN(i)=0.d0
 
  e_i=diagonal_Fock_matrix_mo(i)
 
  do k=elec_alpha_num+1,n_act_cis
   hp_imp_EN(i,k)=0.d0
  
   e_k=diagonal_Fock_matrix_mo(k)
   delta_e_ik=e_i-e_k

   !same spin contribution for EN2_corr_energy 
   do j=i+1,elec_alpha_num
    e_j=diagonal_Fock_matrix_mo(j)
    delta_e_ikj=delta_e_ik+e_j

    !same spin contribution for EN2_corr_energy and a part of the contribution to p_imp_EN,h_imp_EN
    do l=k+1,n_act_cis
     e_l=diagonal_Fock_matrix_mo(l)
     delta_e=delta_e_ikj-e_l
     delta_e=delta_e-mo_bielec_integral_jj_anti(i,j) - &
                     mo_bielec_integral_jj_anti(k,l)
     delta_e=delta_e+mo_bielec_integral_jj_anti(i,k) + &
                     mo_bielec_integral_jj_anti(i,l) + &
                     mo_bielec_integral_jj_anti(j,k) + &
                     mo_bielec_integral_jj_anti(j,l) 
 !                       
     direct=get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
     exchg=get_mo_bielec_integral(i,j,l,k,mo_integrals_map)

     hij=(direct-exchg)*(direct-exchg)
     hij=0.5d0*(-delta_e-dsqrt(delta_e*delta_e+4.d0*hij))

     EN2_corr_energy=EN2_corr_energy+2*hij
     p_imp_EN(k)=p_imp_EN(k)+hij
     h_imp_EN(i)=h_imp_EN(i)+hij

     p_imp_EN(l)=p_imp_EN(l)+hij
     h_imp_EN(j)=h_imp_EN(j)+hij

    enddo
   enddo

   !same spin contribution for hp_imp_EN

   do j=n_core_cis+1,elec_alpha_num
    if(j==i)cycle
    e_j=diagonal_Fock_matrix_mo(j)
    delta_e_ikj=delta_e_ik+e_j
 
    do l=elec_alpha_num+1,n_act_cis
     if(l==k)cycle
     e_l=diagonal_Fock_matrix_mo(l)
     delta_e=delta_e_ikj-e_l
     delta_e=delta_e-mo_bielec_integral_jj_anti(i,j) - &
                     mo_bielec_integral_jj_anti(k,l)
     delta_e=delta_e+mo_bielec_integral_jj_anti(i,k) + &
                     mo_bielec_integral_jj_anti(i,l) + &
                     mo_bielec_integral_jj_anti(j,k) + &
                     mo_bielec_integral_jj_anti(j,l) 

     direct=get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
     exchg=get_mo_bielec_integral(i,j,l,k,mo_integrals_map)

     hij=(direct-exchg)*(direct-exchg)
     hij = 0.5d0 * (-delta_e - dsqrt(delta_e * delta_e + 4.d0 * hij))

     hp_imp_EN(i,k)=hp_imp_EN(i,k)+hij

    enddo
   enddo

   !different spin contribution
   do j=n_core_cis+1,elec_beta_num
    e_j=diagonal_Fock_matrix_mo(j)
    delta_e_ikj=delta_e_ik+e_j

    do l=elec_beta_num+1,n_act_cis
     e_l=diagonal_Fock_matrix_mo(l)
     delta_e=delta_e_ikj-e_l 
     delta_e=delta_e-mo_bielec_integral_jj(i,j) - &
                     mo_bielec_integral_jj(k,l)
     delta_e=delta_e+mo_bielec_integral_jj_anti(i,k) + &
                     mo_bielec_integral_jj_anti(j,l) + &
                     mo_bielec_integral_jj(i,l) + &
                     mo_bielec_integral_jj(j,k)

     direct=get_mo_bielec_integral(i,j,k,l,mo_integrals_map)

     hij=direct*direct
     hij=0.5d0*(-delta_e-dsqrt(delta_e*delta_e+4.d0*hij))

     EN2_corr_energy=EN2_corr_energy+hij
     p_imp_EN(k)=p_imp_EN(k)+hij
     h_imp_EN(i)=h_imp_EN(i)+hij
     hp_imp_EN(i,k)=hp_imp_EN(i,k)+hij

    enddo
   enddo
  enddo
 enddo
 print*,'2'

 else
 do i=n_core_cis+1,elec_alpha_num
  h_imp_EN(i)=0.d0
 
  e_i=diagonal_Fock_matrix_mo(i)
 
  do k=elec_alpha_num+1,n_act_cis
   hp_imp_EN(i,k)=0.d0
  
   e_k=diagonal_Fock_matrix_mo(k)
   delta_e_ik=e_i-e_k

   !same spin contribution for EN2_corr_energy 
   do j=i+1,elec_alpha_num
    e_j=diagonal_Fock_matrix_mo(j)
    delta_e_ikj=delta_e_ik+e_j

    !same spin contribution for EN2_corr_energy and a part of the contribution to p_imp_EN,h_imp_EN
    do l=k+1,n_act_cis
     e_l=diagonal_Fock_matrix_mo(l)
     delta_e=delta_e_ikj-e_l
     delta_e=delta_e-mo_bielec_integral_jj_anti(i,j) - &
                     mo_bielec_integral_jj_anti(k,l)
     delta_e=delta_e+mo_bielec_integral_jj_anti(i,k) + &
                     mo_bielec_integral_jj_anti(i,l) + &
                     mo_bielec_integral_jj_anti(j,k) + &
                     mo_bielec_integral_jj_anti(j,l) 
 !                       
     direct=get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
     exchg=get_mo_bielec_integral(i,j,l,k,mo_integrals_map)

     hij=(direct-exchg)*(direct-exchg)
     hij = hij*hij/delta_e

     EN2_corr_energy=EN2_corr_energy+2*hij
     p_imp_EN(k)=p_imp_EN(k)+hij
     h_imp_EN(i)=h_imp_EN(i)+hij

     p_imp_EN(l)=p_imp_EN(l)+hij
     h_imp_EN(j)=h_imp_EN(j)+hij

    enddo
   enddo

   !same spin contribution for hp_imp_EN

   do j=n_core_cis+1,elec_alpha_num
    if(j==i)cycle
    e_j=diagonal_Fock_matrix_mo(j)
    delta_e_ikj=delta_e_ik+e_j
 
    do l=elec_alpha_num+1,n_act_cis
     if(l==k)cycle
     e_l=diagonal_Fock_matrix_mo(l)
     delta_e=delta_e_ikj-e_l
     delta_e=delta_e-mo_bielec_integral_jj_anti(i,j) - &
                     mo_bielec_integral_jj_anti(k,l)
     delta_e=delta_e+mo_bielec_integral_jj_anti(i,k) + &
                     mo_bielec_integral_jj_anti(i,l) + &
                     mo_bielec_integral_jj_anti(j,k) + &
                     mo_bielec_integral_jj_anti(j,l) 

     direct=get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
     exchg=get_mo_bielec_integral(i,j,l,k,mo_integrals_map)

     hij=(direct-exchg)*(direct-exchg)
     hij = hij*hij/delta_e

     hp_imp_EN(i,k)=hp_imp_EN(i,k)+hij

    enddo
   enddo

   !different spin contribution
   do j=n_core_cis+1,elec_beta_num
    e_j=diagonal_Fock_matrix_mo(j)
    delta_e_ikj=delta_e_ik+e_j

    do l=elec_beta_num+1,n_act_cis
     e_l=diagonal_Fock_matrix_mo(l)
     delta_e=delta_e_ikj-e_l 
     delta_e=delta_e-mo_bielec_integral_jj(i,j) - &
                     mo_bielec_integral_jj(k,l)
     delta_e=delta_e+mo_bielec_integral_jj_anti(i,k) + &
                     mo_bielec_integral_jj_anti(j,l) + &
                     mo_bielec_integral_jj(i,l) + &
                     mo_bielec_integral_jj(j,k)

     direct=get_mo_bielec_integral(i,j,k,l,mo_integrals_map)

     hij=direct*direct
     hij = hij*hij/delta_e

     EN2_corr_energy=EN2_corr_energy+hij
     p_imp_EN(k)=p_imp_EN(k)+hij
     h_imp_EN(i)=h_imp_EN(i)+hij
     hp_imp_EN(i,k)=hp_imp_EN(i,k)+hij

    enddo
   enddo
  enddo
 enddo

 endif


 print*,'EN correlation energy=',EN2_corr_energy
 print*,'EN correlation energy=',EN2_corr_energy

 END_PROVIDER



BEGIN_PROVIDER [double precision, MRMP2_density, (mo_tot_num_align, mo_tot_num)]
 implicit none
 integer :: i,j,k,l
 double precision :: accu, mp2_dm(mo_tot_num)
 MRMP2_density = one_body_dm_mo
 call give_2h2p_density(mp2_dm)
 accu = 0.d0
 do i = 1, n_virt_orb
  j = list_virt(i)
  accu += mp2_dm(j)
  MRMP2_density(j,j)+= mp2_dm(j)
 enddo

END_PROVIDER

subroutine give_2h2p_density(mp2_density_diag_alpha_beta)
 implicit none
 double precision, intent(out) :: mp2_density_diag_alpha_beta(mo_tot_num)
 integer :: i,j,k,l,m
 integer :: iorb,jorb,korb,lorb

 double precision               :: get_mo_bielec_integral
 double precision               :: direct_int
 double precision               :: coef_double

 mp2_density_diag_alpha_beta = 0.d0
   do k = 1, n_virt_orb
    korb = list_virt(k)
       do i = 1, n_inact_orb
        iorb = list_inact(i)
        do j = 1, n_inact_orb
         jorb = list_inact(j)
          do l = 1, n_virt_orb
              lorb = list_virt(l)
              direct_int = get_mo_bielec_integral(iorb,jorb,korb,lorb ,mo_integrals_map)
              coef_double = direct_int/(fock_core_inactive_total_spin_trace(iorb,1) + fock_core_inactive_total_spin_trace(jorb,1) &
                                     -fock_virt_total_spin_trace(korb,1) - fock_virt_total_spin_trace(lorb,1))
              mp2_density_diag_alpha_beta(korb) += coef_double * coef_double
          enddo
         enddo
        enddo
  print*, mp2_density_diag_alpha_beta(korb)
 enddo

end


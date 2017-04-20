BEGIN_PROVIDER [double precision, core_energy]
 implicit none
 integer :: i,j,k,l
 core_energy = 0.d0
 do i = 1, n_core_orb
  j = list_core(i)
  core_energy += 2.d0 * mo_mono_elec_integral(j,j) + mo_bielec_integral_jj(j,j)
  do k = i+1, n_core_orb
   l = list_core(k)
   core_energy += 2.d0 * (2.d0 * mo_bielec_integral_jj(j,l) - mo_bielec_integral_jj_exchange(j,l))
  enddo
 enddo
 core_energy += nuclear_repulsion

END_PROVIDER

BEGIN_PROVIDER [double precision, core_fock_operator, (mo_tot_num,mo_tot_num)]
 implicit none
 integer :: i,j,k,l,m,n
 double precision :: get_mo_bielec_integral
 core_fock_operator = 0.d0
 do i = 1, n_act_orb
  j = list_act(i)
  do k = 1, n_act_orb
   l = list_act(k)
   do m = 1, n_core_orb
    n = list_core(m)
    core_fock_operator(j,l) += 2.d0 * get_mo_bielec_integral(j,n,l,n,mo_integrals_map) - get_mo_bielec_integral(j,n,n,l,mo_integrals_map)
   enddo
  enddo
 enddo
END_PROVIDER

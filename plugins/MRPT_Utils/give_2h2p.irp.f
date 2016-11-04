subroutine give_2h2p(contrib_2h2p)
 implicit none
 double precision, intent(out) :: contrib_2h2p(N_states)
 integer :: i,j,k,l,m
 integer :: iorb,jorb,korb,lorb

 double precision               :: get_mo_bielec_integral
 double precision               :: direct_int,exchange_int
 double precision               :: numerator,denominator(N_states)

 contrib_2h2p = 0.d0
 do i = 1, n_inact_orb
  iorb = list_inact(i)
  do j = 1, n_inact_orb
   jorb = list_inact(j)
   do k = 1, n_virt_orb
    korb = list_virt(k)
    do l = 1, n_virt_orb
     lorb = list_virt(l)
     direct_int = get_mo_bielec_integral(iorb,jorb,korb,lorb ,mo_integrals_map)
     exchange_int = get_mo_bielec_integral(iorb,jorb,lorb,korb ,mo_integrals_map)
     numerator = 3.d0 * direct_int*direct_int + exchange_int*exchange_int -2.d0 * exchange_int * direct_int
     do m = 1, N_states
     denominator(m) = fock_core_inactive_total_spin_trace(iorb,m) + fock_core_inactive_total_spin_trace(jorb,m) &
                     -fock_virt_total_spin_trace(korb,m) - fock_virt_total_spin_trace(lorb,m)
     contrib_2h2p(m) += numerator / denominator(m)
     enddo
    enddo
   enddo
  enddo
 enddo
 contrib_2h2p = contrib_2h2p*0.5d0

end


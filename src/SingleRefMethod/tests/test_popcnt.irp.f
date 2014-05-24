program test_popcnt
 implicit none

 integer :: i,n_a,n_b
 
 n_a = 0
 n_b = 0
 do i=1,N_int
   n_a = n_a+popcnt(psi_generators(i,1,1))
   n_b = n_b+popcnt(psi_generators(i,2,1))
 enddo
 if (n_a == elec_alpha_num) then
   print *,  'alpha :', 1
 else
   print *,  'alpha : ', 0, n_a, elec_alpha_num
 endif
 if (n_b == elec_beta_num) then
   print *,  'beta  : ', 1
 else
   print *,  'beta  : ', 0,  n_b, elec_beta_num
 endif
end


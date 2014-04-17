program test_hf
 implicit none
 character*(300) :: h
 print *, 'mo_tot_num : ', mo_tot_num
 print *, 'alpha : ', elec_alpha_num
 call bitstring_to_hexa( h, HF_bitmask(1,1), N_int )
 print *, 'HF_alpha : 0x'//trim(h)
 print *, 'beta : ', elec_beta_num
 call bitstring_to_hexa( h, HF_bitmask(1,2), N_int )
 print *, 'HF_beta : 0x'//trim(h)

end

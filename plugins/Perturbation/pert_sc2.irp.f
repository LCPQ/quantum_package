double precision function repeat_all_e_corr(key_in)
 implicit none
 integer(bit_kind), intent(in)  :: key_in(N_int,2)
 integer :: i,degree
 double precision :: accu
 use bitmasks
 accu = 0.d0
 call get_excitation_degree(key_in,ref_bitmask,degree,N_int)
 if(degree==2)then
  do i = 1, N_det_selectors
   call get_excitation_degree(ref_bitmask,psi_selectors(1,1,i),degree,N_int)
   if(degree.ne.2)cycle
    call get_excitation_degree(key_in,psi_selectors(1,1,i),degree,N_int)
    if (degree<=3)cycle
    accu += E_corr_per_selectors(i)
  enddo
 elseif(degree==1)then
  do i = 1, N_det_selectors
   call get_excitation_degree(ref_bitmask,psi_selectors(1,1,i),degree,N_int)
   if(degree.ne.2)cycle
    call get_excitation_degree(key_in,psi_selectors(1,1,i),degree,N_int)
    if (degree<=2)cycle
    accu += E_corr_per_selectors(i)
  enddo
 endif
 repeat_all_e_corr = accu

end


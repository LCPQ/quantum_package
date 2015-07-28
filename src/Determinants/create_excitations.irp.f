subroutine do_mono_excitation(key_in,i_hole,i_particle,ispin,i_ok)
 implicit none
 BEGIN_DOC
 ! Apply the mono excitation operator : a^{dager}_(i_particle) a_(i_hole) of spin = ispin
 ! on key_in 
 ! ispin = 1  == alpha
 ! ispin = 2  == beta
 ! i_ok = 1  == the excitation is possible
 ! i_ok = -1 == the excitation is not possible
 END_DOC
 integer, intent(in) :: i_hole,i_particle,ispin
 integer(bit_kind), intent(inout) :: key_in(N_int,2)
 integer, intent(out) :: i_ok
 integer :: k,j,i
 use bitmasks
 ASSERT (i_hole > 0 )
 ASSERT (i_particle <= mo_tot_num)
 i_ok = 1
 ! hole
 k = ishft(i_hole-1,-bit_kind_shift)+1
 j = i_hole-ishft(k-1,bit_kind_shift)-1
 key_in(k,ispin) = ibclr(key_in(k,ispin),j)

 ! particle
 k = ishft(i_particle-1,-bit_kind_shift)+1
 j = i_particle-ishft(k-1,bit_kind_shift)-1
 key_in(k,ispin) = ibset(key_in(k,ispin),j)
 integer :: n_elec_tmp
 n_elec_tmp = 0    
 do i = 1, N_int
  n_elec_tmp += popcnt(key_in(i,1)) + popcnt(key_in(i,2))
 enddo
 if(n_elec_tmp .ne. elec_num)then
  i_ok = -1
 endif
end

subroutine set_bite_to_integer(i_physical,key,Nint)
 use bitmasks
 implicit none
 integer, intent(in) :: i_physical,Nint
 integer(bit_kind), intent(inout) :: key(Nint)
 integer :: k,j,i
 k = ishft(i_physical-1,-bit_kind_shift)+1
 j = i_physical-ishft(k-1,bit_kind_shift)-1
 key(k) = ibset(key(k),j)
end

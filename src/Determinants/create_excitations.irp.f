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
 !print*, n_elec_tmp,elec_num
 !call debug_det(key_in,N_int)
  i_ok = -1
 endif
end

subroutine do_spin_flip(key_in,i_flip,ispin,i_ok)
 implicit none
 BEGIN_DOC
 ! flip the spin ispin in the orbital i_flip
 ! on key_in 
 ! ispin = 1  == alpha
 ! ispin = 2  == beta
 ! i_ok = 1  == the flip is possible
 ! i_ok = -1 == the flip is not possible
 END_DOC
 integer, intent(in) :: i_flip,ispin
 integer(bit_kind), intent(inout) :: key_in(N_int,2)
 integer, intent(out) :: i_ok
 integer :: k,j,i
 integer(bit_kind) :: key_tmp(N_int,2)
 i_ok = -1
 key_tmp = 0_bit_kind
 k = ishft(i_flip-1,-bit_kind_shift)+1
 j = i_flip-ishft(k-1,bit_kind_shift)-1
 key_tmp(k,1) = ibset(key_tmp(k,1),j)
 integer :: other_spin(2)
 other_spin(1) = 2
 other_spin(2) = 1
 if(popcnt(iand(key_tmp(k,1),key_in(k,ispin))) == 1 .and. popcnt(iand(key_tmp(k,1),key_in(k,other_spin(ispin)))) == 0  )then
   ! There is a spin "ispin" in the orbital i_flip   AND  There is no electron of opposit spin in the same orbital "i_flip"
  key_in(k,ispin) = ibclr(key_in(k,ispin),j)  ! destroy the electron ispin in the orbital i_flip
  key_in(k,other_spin(ispin)) = ibset(key_in(k,other_spin(ispin)),j)  ! create an electron of spin other_spin in the same orbital
  i_ok = 1
 else 
  return
 endif
 


end

logical function is_spin_flip_possible(key_in,i_flip,ispin)
 implicit none
 BEGIN_DOC
 ! returns .True. if the spin-flip of spin ispin in the orbital i_flip is possible
 ! on key_in 
 END_DOC
 integer, intent(in) :: i_flip,ispin
 integer(bit_kind), intent(in) :: key_in(N_int,2)
 integer :: k,j,i
 integer(bit_kind) :: key_tmp(N_int,2)
 is_spin_flip_possible = .False. 
 key_tmp = 0_bit_kind
 k = ishft(i_flip-1,-bit_kind_shift)+1
 j = i_flip-ishft(k-1,bit_kind_shift)-1
 key_tmp(k,1) = ibset(key_tmp(k,1),j)
 integer :: other_spin(2)
 other_spin(1) = 2
 other_spin(2) = 1
 if(popcnt(iand(key_tmp(k,1),key_in(k,ispin))) == 1 .and. popcnt(iand(key_tmp(k,1),key_in(k,other_spin(ispin)))) == 0  )then
   ! There is a spin "ispin" in the orbital i_flip   AND  There is no electron of opposit spin in the same orbital "i_flip"
  is_spin_flip_possible = .True.
  return
 else 
  return
 endif
 


end

subroutine set_bit_to_integer(i_physical,key,Nint)
 use bitmasks
 implicit none
 integer, intent(in) :: i_physical,Nint
 integer(bit_kind), intent(inout) :: key(Nint)
 integer :: k,j,i
 k = ishft(i_physical-1,-bit_kind_shift)+1
 j = i_physical-ishft(k-1,bit_kind_shift)-1
 key(k) = ibset(key(k),j)
end


subroutine clear_bit_to_integer(i_physical,key,Nint)
 use bitmasks
 implicit none
 integer, intent(in) :: i_physical,Nint
 integer(bit_kind), intent(inout) :: key(Nint)
 integer :: k,j,i
 k = ishft(i_physical-1,-bit_kind_shift)+1
 j = i_physical-ishft(k-1,bit_kind_shift)-1
 key(k) = ibclr(key(k),j)
end


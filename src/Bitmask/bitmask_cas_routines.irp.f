integer function number_of_holes(key_in)
 ! function that returns the number of holes in the inact space 
 implicit none
 integer(bit_kind), intent(in) :: key_in(N_int,2)
 integer :: i
 number_of_holes = 0
 if(N_int == 1)then
    number_of_holes = number_of_holes  & 
   + popcnt( xor( iand(inact_bitmask(1,1), xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1)))), inact_bitmask(1,1)) )&   
   + popcnt( xor( iand(inact_bitmask(1,2), xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1)))), inact_bitmask(1,2)) )
 else if(N_int == 2)then
    number_of_holes = number_of_holes  & 
   + popcnt( xor( iand(inact_bitmask(1,1), xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1)))), inact_bitmask(1,1)) )&   
   + popcnt( xor( iand(inact_bitmask(1,2), xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1)))), inact_bitmask(1,2)) )&
   + popcnt( xor( iand(inact_bitmask(2,1), xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1)))), inact_bitmask(2,1)) )&   
   + popcnt( xor( iand(inact_bitmask(2,2), xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1)))), inact_bitmask(2,2)) ) 
 else if(N_int == 3)then
    number_of_holes = number_of_holes  & 
   + popcnt( xor( iand(inact_bitmask(1,1), xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1)))), inact_bitmask(1,1)) )&   
   + popcnt( xor( iand(inact_bitmask(1,2), xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1)))), inact_bitmask(1,2)) )&
   + popcnt( xor( iand(inact_bitmask(2,1), xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1)))), inact_bitmask(2,1)) )&   
   + popcnt( xor( iand(inact_bitmask(2,2), xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1)))), inact_bitmask(2,2)) )& 
   + popcnt( xor( iand(inact_bitmask(3,1), xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1)))), inact_bitmask(3,1)) )&   
   + popcnt( xor( iand(inact_bitmask(3,2), xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1)))), inact_bitmask(3,2)) ) 
 else if(N_int == 4)then
    number_of_holes = number_of_holes  & 
   + popcnt( xor( iand(inact_bitmask(1,1), xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1)))), inact_bitmask(1,1)) )&   
   + popcnt( xor( iand(inact_bitmask(1,2), xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1)))), inact_bitmask(1,2)) )&
   + popcnt( xor( iand(inact_bitmask(2,1), xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1)))), inact_bitmask(2,1)) )&   
   + popcnt( xor( iand(inact_bitmask(2,2), xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1)))), inact_bitmask(2,2)) )& 
   + popcnt( xor( iand(inact_bitmask(3,1), xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1)))), inact_bitmask(3,1)) )&   
   + popcnt( xor( iand(inact_bitmask(3,2), xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1)))), inact_bitmask(3,2)) )&
   + popcnt( xor( iand(inact_bitmask(4,1), xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1)))), inact_bitmask(4,1)) )&   
   + popcnt( xor( iand(inact_bitmask(4,2), xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1)))), inact_bitmask(4,2)) ) 
 else if(N_int == 5)then
    number_of_holes = number_of_holes  & 
   + popcnt( xor( iand(inact_bitmask(1,1), xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1)))), inact_bitmask(1,1)) )&   
   + popcnt( xor( iand(inact_bitmask(1,2), xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1)))), inact_bitmask(1,2)) )&
   + popcnt( xor( iand(inact_bitmask(2,1), xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1)))), inact_bitmask(2,1)) )&   
   + popcnt( xor( iand(inact_bitmask(2,2), xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1)))), inact_bitmask(2,2)) )& 
   + popcnt( xor( iand(inact_bitmask(3,1), xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1)))), inact_bitmask(3,1)) )&   
   + popcnt( xor( iand(inact_bitmask(3,2), xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1)))), inact_bitmask(3,2)) )&
   + popcnt( xor( iand(inact_bitmask(4,1), xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1)))), inact_bitmask(4,1)) )&   
   + popcnt( xor( iand(inact_bitmask(4,2), xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1)))), inact_bitmask(4,2)) )& 
   + popcnt( xor( iand(inact_bitmask(5,1), xor(key_in(5,1),iand(key_in(5,1),cas_bitmask(5,1,1)))), inact_bitmask(5,1)) )&   
   + popcnt( xor( iand(inact_bitmask(5,2), xor(key_in(5,2),iand(key_in(5,2),cas_bitmask(5,2,1)))), inact_bitmask(5,2)) ) 
 else if(N_int == 6)then
    number_of_holes = number_of_holes  & 
   + popcnt( xor( iand(inact_bitmask(1,1), xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1)))), inact_bitmask(1,1)) )&   
   + popcnt( xor( iand(inact_bitmask(1,2), xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1)))), inact_bitmask(1,2)) )&
   + popcnt( xor( iand(inact_bitmask(2,1), xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1)))), inact_bitmask(2,1)) )&   
   + popcnt( xor( iand(inact_bitmask(2,2), xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1)))), inact_bitmask(2,2)) )& 
   + popcnt( xor( iand(inact_bitmask(3,1), xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1)))), inact_bitmask(3,1)) )&   
   + popcnt( xor( iand(inact_bitmask(3,2), xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1)))), inact_bitmask(3,2)) )&
   + popcnt( xor( iand(inact_bitmask(4,1), xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1)))), inact_bitmask(4,1)) )&   
   + popcnt( xor( iand(inact_bitmask(4,2), xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1)))), inact_bitmask(4,2)) )&
   + popcnt( xor( iand(inact_bitmask(5,1), xor(key_in(5,1),iand(key_in(5,1),cas_bitmask(5,1,1)))), inact_bitmask(5,1)) )&   
   + popcnt( xor( iand(inact_bitmask(5,2), xor(key_in(5,2),iand(key_in(5,2),cas_bitmask(5,2,1)))), inact_bitmask(5,2)) )&
   + popcnt( xor( iand(inact_bitmask(6,1), xor(key_in(6,1),iand(key_in(6,1),cas_bitmask(6,1,1)))), inact_bitmask(6,1)) )&   
   + popcnt( xor( iand(inact_bitmask(6,2), xor(key_in(6,2),iand(key_in(6,2),cas_bitmask(6,2,1)))), inact_bitmask(6,2)) ) 
 else if(N_int == 7)then
    number_of_holes = number_of_holes  & 
   + popcnt( xor( iand(inact_bitmask(1,1), xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1)))), inact_bitmask(1,1)) )&   
   + popcnt( xor( iand(inact_bitmask(1,2), xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1)))), inact_bitmask(1,2)) )&
   + popcnt( xor( iand(inact_bitmask(2,1), xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1)))), inact_bitmask(2,1)) )&   
   + popcnt( xor( iand(inact_bitmask(2,2), xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1)))), inact_bitmask(2,2)) )& 
   + popcnt( xor( iand(inact_bitmask(3,1), xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1)))), inact_bitmask(3,1)) )&   
   + popcnt( xor( iand(inact_bitmask(3,2), xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1)))), inact_bitmask(3,2)) )&
   + popcnt( xor( iand(inact_bitmask(4,1), xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1)))), inact_bitmask(4,1)) )&   
   + popcnt( xor( iand(inact_bitmask(4,2), xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1)))), inact_bitmask(4,2)) )&
   + popcnt( xor( iand(inact_bitmask(5,1), xor(key_in(5,1),iand(key_in(5,1),cas_bitmask(5,1,1)))), inact_bitmask(5,1)) )&   
   + popcnt( xor( iand(inact_bitmask(5,2), xor(key_in(5,2),iand(key_in(5,2),cas_bitmask(5,2,1)))), inact_bitmask(5,2)) )&
   + popcnt( xor( iand(inact_bitmask(6,1), xor(key_in(6,1),iand(key_in(6,1),cas_bitmask(6,1,1)))), inact_bitmask(6,1)) )&   
   + popcnt( xor( iand(inact_bitmask(6,2), xor(key_in(6,2),iand(key_in(6,2),cas_bitmask(6,2,1)))), inact_bitmask(6,2)) )&
   + popcnt( xor( iand(inact_bitmask(7,1), xor(key_in(7,1),iand(key_in(7,1),cas_bitmask(7,1,1)))), inact_bitmask(7,1)) )&   
   + popcnt( xor( iand(inact_bitmask(7,2), xor(key_in(7,2),iand(key_in(7,2),cas_bitmask(7,2,1)))), inact_bitmask(7,2)) ) 
 else if(N_int == 8)then
    number_of_holes = number_of_holes  & 
   + popcnt( xor( iand(inact_bitmask(1,1), xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1)))), inact_bitmask(1,1)) )&   
   + popcnt( xor( iand(inact_bitmask(1,2), xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1)))), inact_bitmask(1,2)) )&
   + popcnt( xor( iand(inact_bitmask(2,1), xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1)))), inact_bitmask(2,1)) )&   
   + popcnt( xor( iand(inact_bitmask(2,2), xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1)))), inact_bitmask(2,2)) )& 
   + popcnt( xor( iand(inact_bitmask(3,1), xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1)))), inact_bitmask(3,1)) )&   
   + popcnt( xor( iand(inact_bitmask(3,2), xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1)))), inact_bitmask(3,2)) )&
   + popcnt( xor( iand(inact_bitmask(4,1), xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1)))), inact_bitmask(4,1)) )&   
   + popcnt( xor( iand(inact_bitmask(4,2), xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1)))), inact_bitmask(4,2)) )&
   + popcnt( xor( iand(inact_bitmask(5,1), xor(key_in(5,1),iand(key_in(5,1),cas_bitmask(5,1,1)))), inact_bitmask(5,1)) )&   
   + popcnt( xor( iand(inact_bitmask(5,2), xor(key_in(5,2),iand(key_in(5,2),cas_bitmask(5,2,1)))), inact_bitmask(5,2)) )&
   + popcnt( xor( iand(inact_bitmask(6,1), xor(key_in(6,1),iand(key_in(6,1),cas_bitmask(6,1,1)))), inact_bitmask(6,1)) )&   
   + popcnt( xor( iand(inact_bitmask(6,2), xor(key_in(6,2),iand(key_in(6,2),cas_bitmask(6,2,1)))), inact_bitmask(6,2)) )&
   + popcnt( xor( iand(inact_bitmask(8,1), xor(key_in(8,1),iand(key_in(8,1),cas_bitmask(8,1,1)))), inact_bitmask(8,1)) )&   
   + popcnt( xor( iand(inact_bitmask(8,2), xor(key_in(8,2),iand(key_in(8,2),cas_bitmask(8,2,1)))), inact_bitmask(8,2)) ) 
 else
   do i = 1, N_int
    number_of_holes = number_of_holes  & 
   + popcnt( xor( iand(inact_bitmask(i,1), xor(key_in(i,1),iand(key_in(i,1),cas_bitmask(i,1,1)))), inact_bitmask(i,1)) )&   
   + popcnt( xor( iand(inact_bitmask(i,2), xor(key_in(i,2),iand(key_in(i,2),cas_bitmask(i,2,1)))), inact_bitmask(i,2)) )
   enddo
 endif
end


integer function number_of_particles(key_in)
 ! function that returns the number of particles in the virtual space 
 implicit none
 integer(bit_kind), intent(in) :: key_in(N_int,2)
 integer :: i
 number_of_particles= 0
 if(N_int == 1)then
   number_of_particles= number_of_particles                                                                                  & 
      + popcnt( iand( iand( xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1))), virt_bitmask(1,1) ), virt_bitmask(1,1)) ) & 
      + popcnt( iand( iand( xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1))), virt_bitmask(1,2) ), virt_bitmask(1,2)) ) 
 else if(N_int == 2)then
   number_of_particles= number_of_particles                                                                                  & 
      + popcnt( iand( iand( xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1))), virt_bitmask(1,1) ), virt_bitmask(1,1)) ) & 
      + popcnt( iand( iand( xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1))), virt_bitmask(1,2) ), virt_bitmask(1,2)) ) &
      + popcnt( iand( iand( xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1))), virt_bitmask(2,1) ), virt_bitmask(2,1)) ) & 
      + popcnt( iand( iand( xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1))), virt_bitmask(2,2) ), virt_bitmask(2,2)) ) 
 else if(N_int == 3)then
   number_of_particles= number_of_particles                                                                                  & 
      + popcnt( iand( iand( xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1))), virt_bitmask(1,1) ), virt_bitmask(1,1)) ) & 
      + popcnt( iand( iand( xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1))), virt_bitmask(1,2) ), virt_bitmask(1,2)) ) &
      + popcnt( iand( iand( xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1))), virt_bitmask(2,1) ), virt_bitmask(2,1)) ) & 
      + popcnt( iand( iand( xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1))), virt_bitmask(2,2) ), virt_bitmask(2,2)) ) &
      + popcnt( iand( iand( xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1))), virt_bitmask(3,1) ), virt_bitmask(3,1)) ) & 
      + popcnt( iand( iand( xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1))), virt_bitmask(3,2) ), virt_bitmask(3,2)) ) 
 else if(N_int == 4)then
   number_of_particles= number_of_particles                                                                                  & 
      + popcnt( iand( iand( xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1))), virt_bitmask(1,1) ), virt_bitmask(1,1)) ) & 
      + popcnt( iand( iand( xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1))), virt_bitmask(1,2) ), virt_bitmask(1,2)) ) &
      + popcnt( iand( iand( xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1))), virt_bitmask(2,1) ), virt_bitmask(2,1)) ) & 
      + popcnt( iand( iand( xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1))), virt_bitmask(2,2) ), virt_bitmask(2,2)) ) &
      + popcnt( iand( iand( xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1))), virt_bitmask(3,1) ), virt_bitmask(3,1)) ) & 
      + popcnt( iand( iand( xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1))), virt_bitmask(3,2) ), virt_bitmask(3,2)) ) &
      + popcnt( iand( iand( xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1))), virt_bitmask(4,1) ), virt_bitmask(4,1)) ) & 
      + popcnt( iand( iand( xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1))), virt_bitmask(4,2) ), virt_bitmask(4,2)) ) 
 else if(N_int == 5)then
   number_of_particles= number_of_particles                                                                                  & 
      + popcnt( iand( iand( xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1))), virt_bitmask(1,1) ), virt_bitmask(1,1)) ) & 
      + popcnt( iand( iand( xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1))), virt_bitmask(1,2) ), virt_bitmask(1,2)) ) &
      + popcnt( iand( iand( xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1))), virt_bitmask(2,1) ), virt_bitmask(2,1)) ) & 
      + popcnt( iand( iand( xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1))), virt_bitmask(2,2) ), virt_bitmask(2,2)) ) &
      + popcnt( iand( iand( xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1))), virt_bitmask(3,1) ), virt_bitmask(3,1)) ) & 
      + popcnt( iand( iand( xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1))), virt_bitmask(3,2) ), virt_bitmask(3,2)) ) &
      + popcnt( iand( iand( xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1))), virt_bitmask(4,1) ), virt_bitmask(4,1)) ) & 
      + popcnt( iand( iand( xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1))), virt_bitmask(4,2) ), virt_bitmask(4,2)) ) &
      + popcnt( iand( iand( xor(key_in(5,1),iand(key_in(5,1),cas_bitmask(5,1,1))), virt_bitmask(5,1) ), virt_bitmask(5,1)) ) & 
      + popcnt( iand( iand( xor(key_in(5,2),iand(key_in(5,2),cas_bitmask(5,2,1))), virt_bitmask(5,2) ), virt_bitmask(5,2)) ) 
 else if(N_int == 6)then
   number_of_particles= number_of_particles                                                                                  & 
      + popcnt( iand( iand( xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1))), virt_bitmask(1,1) ), virt_bitmask(1,1)) ) & 
      + popcnt( iand( iand( xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1))), virt_bitmask(1,2) ), virt_bitmask(1,2)) ) &
      + popcnt( iand( iand( xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1))), virt_bitmask(2,1) ), virt_bitmask(2,1)) ) & 
      + popcnt( iand( iand( xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1))), virt_bitmask(2,2) ), virt_bitmask(2,2)) ) &
      + popcnt( iand( iand( xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1))), virt_bitmask(3,1) ), virt_bitmask(3,1)) ) & 
      + popcnt( iand( iand( xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1))), virt_bitmask(3,2) ), virt_bitmask(3,2)) ) &
      + popcnt( iand( iand( xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1))), virt_bitmask(4,1) ), virt_bitmask(4,1)) ) & 
      + popcnt( iand( iand( xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1))), virt_bitmask(4,2) ), virt_bitmask(4,2)) ) &
      + popcnt( iand( iand( xor(key_in(5,1),iand(key_in(5,1),cas_bitmask(5,1,1))), virt_bitmask(5,1) ), virt_bitmask(5,1)) ) & 
      + popcnt( iand( iand( xor(key_in(5,2),iand(key_in(5,2),cas_bitmask(5,2,1))), virt_bitmask(5,2) ), virt_bitmask(5,2)) ) &
      + popcnt( iand( iand( xor(key_in(6,1),iand(key_in(6,1),cas_bitmask(6,1,1))), virt_bitmask(6,1) ), virt_bitmask(6,1)) ) & 
      + popcnt( iand( iand( xor(key_in(6,2),iand(key_in(6,2),cas_bitmask(6,2,1))), virt_bitmask(6,2) ), virt_bitmask(6,2)) ) 
 else if(N_int == 7)then
   number_of_particles= number_of_particles                                                                                  & 
      + popcnt( iand( iand( xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1))), virt_bitmask(1,1) ), virt_bitmask(1,1)) ) & 
      + popcnt( iand( iand( xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1))), virt_bitmask(1,2) ), virt_bitmask(1,2)) ) &
      + popcnt( iand( iand( xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1))), virt_bitmask(2,1) ), virt_bitmask(2,1)) ) & 
      + popcnt( iand( iand( xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1))), virt_bitmask(2,2) ), virt_bitmask(2,2)) ) &
      + popcnt( iand( iand( xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1))), virt_bitmask(3,1) ), virt_bitmask(3,1)) ) & 
      + popcnt( iand( iand( xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1))), virt_bitmask(3,2) ), virt_bitmask(3,2)) ) &
      + popcnt( iand( iand( xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1))), virt_bitmask(4,1) ), virt_bitmask(4,1)) ) & 
      + popcnt( iand( iand( xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1))), virt_bitmask(4,2) ), virt_bitmask(4,2)) ) &
      + popcnt( iand( iand( xor(key_in(5,1),iand(key_in(5,1),cas_bitmask(5,1,1))), virt_bitmask(5,1) ), virt_bitmask(5,1)) ) & 
      + popcnt( iand( iand( xor(key_in(5,2),iand(key_in(5,2),cas_bitmask(5,2,1))), virt_bitmask(5,2) ), virt_bitmask(5,2)) ) &
      + popcnt( iand( iand( xor(key_in(6,1),iand(key_in(6,1),cas_bitmask(6,1,1))), virt_bitmask(6,1) ), virt_bitmask(6,1)) ) & 
      + popcnt( iand( iand( xor(key_in(6,2),iand(key_in(6,2),cas_bitmask(6,2,1))), virt_bitmask(6,2) ), virt_bitmask(6,2)) ) &
      + popcnt( iand( iand( xor(key_in(7,1),iand(key_in(7,1),cas_bitmask(7,1,1))), virt_bitmask(7,1) ), virt_bitmask(7,1)) ) & 
      + popcnt( iand( iand( xor(key_in(7,2),iand(key_in(7,2),cas_bitmask(7,2,1))), virt_bitmask(7,2) ), virt_bitmask(7,2)) )  
 else if(N_int == 8)then
   number_of_particles= number_of_particles                                                                                  & 
      + popcnt( iand( iand( xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1))), virt_bitmask(1,1) ), virt_bitmask(1,1)) ) & 
      + popcnt( iand( iand( xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1))), virt_bitmask(1,2) ), virt_bitmask(1,2)) ) &
      + popcnt( iand( iand( xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1))), virt_bitmask(2,1) ), virt_bitmask(2,1)) ) & 
      + popcnt( iand( iand( xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1))), virt_bitmask(2,2) ), virt_bitmask(2,2)) ) &
      + popcnt( iand( iand( xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1))), virt_bitmask(3,1) ), virt_bitmask(3,1)) ) & 
      + popcnt( iand( iand( xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1))), virt_bitmask(3,2) ), virt_bitmask(3,2)) ) &
      + popcnt( iand( iand( xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1))), virt_bitmask(4,1) ), virt_bitmask(4,1)) ) & 
      + popcnt( iand( iand( xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1))), virt_bitmask(4,2) ), virt_bitmask(4,2)) ) &
      + popcnt( iand( iand( xor(key_in(5,1),iand(key_in(5,1),cas_bitmask(5,1,1))), virt_bitmask(5,1) ), virt_bitmask(5,1)) ) & 
      + popcnt( iand( iand( xor(key_in(5,2),iand(key_in(5,2),cas_bitmask(5,2,1))), virt_bitmask(5,2) ), virt_bitmask(5,2)) ) &
      + popcnt( iand( iand( xor(key_in(6,1),iand(key_in(6,1),cas_bitmask(6,1,1))), virt_bitmask(6,1) ), virt_bitmask(6,1)) ) & 
      + popcnt( iand( iand( xor(key_in(6,2),iand(key_in(6,2),cas_bitmask(6,2,1))), virt_bitmask(6,2) ), virt_bitmask(6,2)) ) &
      + popcnt( iand( iand( xor(key_in(7,1),iand(key_in(7,1),cas_bitmask(7,1,1))), virt_bitmask(7,1) ), virt_bitmask(7,1)) ) & 
      + popcnt( iand( iand( xor(key_in(7,2),iand(key_in(7,2),cas_bitmask(7,2,1))), virt_bitmask(7,2) ), virt_bitmask(7,2)) ) & 
      + popcnt( iand( iand( xor(key_in(8,1),iand(key_in(8,1),cas_bitmask(8,1,1))), virt_bitmask(8,1) ), virt_bitmask(8,1)) ) & 
      + popcnt( iand( iand( xor(key_in(8,2),iand(key_in(8,2),cas_bitmask(8,2,1))), virt_bitmask(8,2) ), virt_bitmask(8,2)) )  
 else
   do i = 1, N_int
   number_of_particles= number_of_particles                                                                                  & 
      + popcnt( iand( iand( xor(key_in(i,1),iand(key_in(i,1),cas_bitmask(i,1,1))), virt_bitmask(i,1) ), virt_bitmask(i,1)) ) & 
      + popcnt( iand( iand( xor(key_in(i,2),iand(key_in(i,2),cas_bitmask(i,2,1))), virt_bitmask(i,2) ), virt_bitmask(i,2)) ) 
   enddo
 endif
end

logical function is_a_two_holes_two_particles(key_in)
 ! logical function that returns True if the determinant 'key_in' 
 ! belongs to the 2h-2p excitation class of the DDCI space 
 ! this is calculated using the CAS_bitmask that defines the active 
 ! orbital space, the inact_bitmasl that defines the inactive oribital space  
 ! and the virt_bitmask that defines the virtual orbital space 
 implicit none
 integer(bit_kind), intent(in) :: key_in(N_int,2)
 integer :: i,i_diff
 i_diff = 0
 if(N_int == 1)then
   i_diff = i_diff  &
    + popcnt( xor( iand(inact_bitmask(1,1), xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1)))), inact_bitmask(1,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(1,2), xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1)))), inact_bitmask(1,2)) )  &
    + popcnt( iand( iand( xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1))), virt_bitmask(1,1) ), virt_bitmask(1,1)) ) & 
    + popcnt( iand( iand( xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1))), virt_bitmask(1,2) ), virt_bitmask(1,2)) ) 
 else if(N_int == 2)then
   i_diff = i_diff  &
    + popcnt( xor( iand(inact_bitmask(1,1), xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1)))), inact_bitmask(1,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(1,2), xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1)))), inact_bitmask(1,2)) )  &
    + popcnt( iand( iand( xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1))), virt_bitmask(1,1) ), virt_bitmask(1,1)) ) & 
    + popcnt( iand( iand( xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1))), virt_bitmask(1,2) ), virt_bitmask(1,2)) ) &
    + popcnt( xor( iand(inact_bitmask(2,1), xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1)))), inact_bitmask(2,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(2,2), xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1)))), inact_bitmask(2,2)) )  &
    + popcnt( iand( iand( xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1))), virt_bitmask(2,1) ), virt_bitmask(2,1)) ) & 
    + popcnt( iand( iand( xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1))), virt_bitmask(2,2) ), virt_bitmask(2,2)) ) 

 else if(N_int == 3)then
   i_diff = i_diff  &
    + popcnt( xor( iand(inact_bitmask(1,1), xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1)))), inact_bitmask(1,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(1,2), xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1)))), inact_bitmask(1,2)) )  &
    + popcnt( iand( iand( xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1))), virt_bitmask(1,1) ), virt_bitmask(1,1)) ) & 
    + popcnt( iand( iand( xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1))), virt_bitmask(1,2) ), virt_bitmask(1,2)) ) &
    + popcnt( xor( iand(inact_bitmask(2,1), xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1)))), inact_bitmask(2,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(2,2), xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1)))), inact_bitmask(2,2)) )  &
    + popcnt( iand( iand( xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1))), virt_bitmask(2,1) ), virt_bitmask(2,1)) ) & 
    + popcnt( iand( iand( xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1))), virt_bitmask(2,2) ), virt_bitmask(2,2)) ) &
    + popcnt( xor( iand(inact_bitmask(3,1), xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1)))), inact_bitmask(3,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(3,2), xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1)))), inact_bitmask(3,2)) )  &
    + popcnt( iand( iand( xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1))), virt_bitmask(3,1) ), virt_bitmask(3,1)) ) & 
    + popcnt( iand( iand( xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1))), virt_bitmask(3,2) ), virt_bitmask(3,2)) ) 
 else if(N_int == 4)then
   i_diff = i_diff  &
    + popcnt( xor( iand(inact_bitmask(1,1), xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1)))), inact_bitmask(1,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(1,2), xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1)))), inact_bitmask(1,2)) )  &
    + popcnt( iand( iand( xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1))), virt_bitmask(1,1) ), virt_bitmask(1,1)) ) & 
    + popcnt( iand( iand( xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1))), virt_bitmask(1,2) ), virt_bitmask(1,2)) ) &
    + popcnt( xor( iand(inact_bitmask(2,1), xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1)))), inact_bitmask(2,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(2,2), xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1)))), inact_bitmask(2,2)) )  &
    + popcnt( iand( iand( xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1))), virt_bitmask(2,1) ), virt_bitmask(2,1)) ) & 
    + popcnt( iand( iand( xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1))), virt_bitmask(2,2) ), virt_bitmask(2,2)) ) &
    + popcnt( xor( iand(inact_bitmask(3,1), xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1)))), inact_bitmask(3,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(3,2), xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1)))), inact_bitmask(3,2)) )  &
    + popcnt( iand( iand( xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1))), virt_bitmask(3,1) ), virt_bitmask(3,1)) ) & 
    + popcnt( iand( iand( xor(key_in(4,2),iand(key_in(3,2),cas_bitmask(3,2,1))), virt_bitmask(3,2) ), virt_bitmask(3,2)) ) &
    + popcnt( xor( iand(inact_bitmask(4,1), xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1)))), inact_bitmask(4,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(4,2), xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1)))), inact_bitmask(4,2)) )  &
    + popcnt( iand( iand( xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1))), virt_bitmask(4,1) ), virt_bitmask(4,1)) ) & 
    + popcnt( iand( iand( xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1))), virt_bitmask(4,2) ), virt_bitmask(4,2)) ) 
 else if(N_int == 5)then
   i_diff = i_diff  &
    + popcnt( xor( iand(inact_bitmask(1,1), xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1)))), inact_bitmask(1,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(1,2), xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1)))), inact_bitmask(1,2)) )  &
    + popcnt( iand( iand( xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1))), virt_bitmask(1,1) ), virt_bitmask(1,1)) ) & 
    + popcnt( iand( iand( xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1))), virt_bitmask(1,2) ), virt_bitmask(1,2)) ) &
    + popcnt( xor( iand(inact_bitmask(2,1), xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1)))), inact_bitmask(2,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(2,2), xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1)))), inact_bitmask(2,2)) )  &
    + popcnt( iand( iand( xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1))), virt_bitmask(2,1) ), virt_bitmask(2,1)) ) & 
    + popcnt( iand( iand( xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1))), virt_bitmask(2,2) ), virt_bitmask(2,2)) ) &
    + popcnt( xor( iand(inact_bitmask(3,1), xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1)))), inact_bitmask(3,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(3,2), xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1)))), inact_bitmask(3,2)) )  &
    + popcnt( iand( iand( xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1))), virt_bitmask(3,1) ), virt_bitmask(3,1)) ) & 
    + popcnt( iand( iand( xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1))), virt_bitmask(3,2) ), virt_bitmask(3,2)) ) &
    + popcnt( xor( iand(inact_bitmask(4,1), xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1)))), inact_bitmask(4,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(4,2), xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1)))), inact_bitmask(4,2)) )  &
    + popcnt( iand( iand( xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1))), virt_bitmask(4,1) ), virt_bitmask(4,1)) ) & 
    + popcnt( iand( iand( xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1))), virt_bitmask(4,2) ), virt_bitmask(4,2)) ) &
    + popcnt( xor( iand(inact_bitmask(5,1), xor(key_in(5,1),iand(key_in(5,1),cas_bitmask(5,1,1)))), inact_bitmask(5,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(5,2), xor(key_in(5,2),iand(key_in(5,2),cas_bitmask(5,2,1)))), inact_bitmask(5,2)) )  &
    + popcnt( iand( iand( xor(key_in(5,1),iand(key_in(5,1),cas_bitmask(5,1,1))), virt_bitmask(5,1) ), virt_bitmask(5,1)) ) & 
    + popcnt( iand( iand( xor(key_in(5,2),iand(key_in(5,2),cas_bitmask(5,2,1))), virt_bitmask(5,2) ), virt_bitmask(5,2)) ) 
 else if(N_int == 6)then
   i_diff = i_diff  &
    + popcnt( xor( iand(inact_bitmask(1,1), xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1)))), inact_bitmask(1,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(1,2), xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1)))), inact_bitmask(1,2)) )  &
    + popcnt( iand( iand( xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1))), virt_bitmask(1,1) ), virt_bitmask(1,1)) ) & 
    + popcnt( iand( iand( xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1))), virt_bitmask(1,2) ), virt_bitmask(1,2)) ) &
    + popcnt( xor( iand(inact_bitmask(2,1), xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1)))), inact_bitmask(2,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(2,2), xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1)))), inact_bitmask(2,2)) )  &
    + popcnt( iand( iand( xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1))), virt_bitmask(2,1) ), virt_bitmask(2,1)) ) & 
    + popcnt( iand( iand( xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1))), virt_bitmask(2,2) ), virt_bitmask(2,2)) ) &
    + popcnt( xor( iand(inact_bitmask(3,1), xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1)))), inact_bitmask(3,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(3,2), xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1)))), inact_bitmask(3,2)) )  &
    + popcnt( iand( iand( xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1))), virt_bitmask(3,1) ), virt_bitmask(3,1)) ) & 
    + popcnt( iand( iand( xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1))), virt_bitmask(3,2) ), virt_bitmask(3,2)) ) &
    + popcnt( xor( iand(inact_bitmask(4,1), xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1)))), inact_bitmask(4,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(4,2), xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1)))), inact_bitmask(4,2)) )  &
    + popcnt( iand( iand( xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1))), virt_bitmask(4,1) ), virt_bitmask(4,1)) ) & 
    + popcnt( iand( iand( xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1))), virt_bitmask(4,2) ), virt_bitmask(4,2)) ) &
    + popcnt( xor( iand(inact_bitmask(5,1), xor(key_in(5,1),iand(key_in(5,1),cas_bitmask(5,1,1)))), inact_bitmask(5,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(5,2), xor(key_in(5,2),iand(key_in(5,2),cas_bitmask(5,2,1)))), inact_bitmask(5,2)) )  &
    + popcnt( iand( iand( xor(key_in(5,1),iand(key_in(5,1),cas_bitmask(5,1,1))), virt_bitmask(5,1) ), virt_bitmask(5,1)) ) & 
    + popcnt( iand( iand( xor(key_in(5,2),iand(key_in(5,2),cas_bitmask(5,2,1))), virt_bitmask(5,2) ), virt_bitmask(5,2)) ) &
    + popcnt( xor( iand(inact_bitmask(6,1), xor(key_in(6,1),iand(key_in(6,1),cas_bitmask(6,1,1)))), inact_bitmask(6,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(6,2), xor(key_in(6,2),iand(key_in(6,2),cas_bitmask(6,2,1)))), inact_bitmask(6,2)) )  &
    + popcnt( iand( iand( xor(key_in(6,1),iand(key_in(6,1),cas_bitmask(6,1,1))), virt_bitmask(6,1) ), virt_bitmask(6,1)) ) & 
    + popcnt( iand( iand( xor(key_in(6,2),iand(key_in(6,2),cas_bitmask(6,2,1))), virt_bitmask(6,2) ), virt_bitmask(6,2)) ) 
 else if(N_int == 7)then
   i_diff = i_diff  &
    + popcnt( xor( iand(inact_bitmask(1,1), xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1)))), inact_bitmask(1,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(1,2), xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1)))), inact_bitmask(1,2)) )  &
    + popcnt( iand( iand( xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1))), virt_bitmask(1,1) ), virt_bitmask(1,1)) ) & 
    + popcnt( iand( iand( xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1))), virt_bitmask(1,2) ), virt_bitmask(1,2)) ) &
    + popcnt( xor( iand(inact_bitmask(2,1), xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1)))), inact_bitmask(2,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(2,2), xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1)))), inact_bitmask(2,2)) )  &
    + popcnt( iand( iand( xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1))), virt_bitmask(2,1) ), virt_bitmask(2,1)) ) & 
    + popcnt( iand( iand( xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1))), virt_bitmask(2,2) ), virt_bitmask(2,2)) ) &
    + popcnt( xor( iand(inact_bitmask(3,1), xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1)))), inact_bitmask(3,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(3,2), xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1)))), inact_bitmask(3,2)) )  &
    + popcnt( iand( iand( xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1))), virt_bitmask(3,1) ), virt_bitmask(3,1)) ) & 
    + popcnt( iand( iand( xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1))), virt_bitmask(3,2) ), virt_bitmask(3,2)) ) &
    + popcnt( xor( iand(inact_bitmask(4,1), xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1)))), inact_bitmask(4,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(4,2), xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1)))), inact_bitmask(4,2)) )  &
    + popcnt( iand( iand( xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1))), virt_bitmask(4,1) ), virt_bitmask(4,1)) ) & 
    + popcnt( iand( iand( xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1))), virt_bitmask(4,2) ), virt_bitmask(4,2)) ) &
    + popcnt( xor( iand(inact_bitmask(5,1), xor(key_in(5,1),iand(key_in(5,1),cas_bitmask(5,1,1)))), inact_bitmask(5,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(5,2), xor(key_in(5,2),iand(key_in(5,2),cas_bitmask(5,2,1)))), inact_bitmask(5,2)) )  &
    + popcnt( iand( iand( xor(key_in(5,1),iand(key_in(5,1),cas_bitmask(5,1,1))), virt_bitmask(5,1) ), virt_bitmask(5,1)) ) & 
    + popcnt( iand( iand( xor(key_in(5,2),iand(key_in(5,2),cas_bitmask(5,2,1))), virt_bitmask(5,2) ), virt_bitmask(5,2)) ) &
    + popcnt( xor( iand(inact_bitmask(6,1), xor(key_in(6,1),iand(key_in(6,1),cas_bitmask(6,1,1)))), inact_bitmask(6,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(6,2), xor(key_in(6,2),iand(key_in(6,2),cas_bitmask(6,2,1)))), inact_bitmask(6,2)) )  &
    + popcnt( iand( iand( xor(key_in(6,1),iand(key_in(6,1),cas_bitmask(6,1,1))), virt_bitmask(6,1) ), virt_bitmask(6,1)) ) & 
    + popcnt( iand( iand( xor(key_in(6,2),iand(key_in(6,2),cas_bitmask(6,2,1))), virt_bitmask(6,2) ), virt_bitmask(6,2)) ) &
    + popcnt( xor( iand(inact_bitmask(7,1), xor(key_in(7,1),iand(key_in(7,1),cas_bitmask(7,1,1)))), inact_bitmask(7,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(7,2), xor(key_in(7,2),iand(key_in(7,2),cas_bitmask(7,2,1)))), inact_bitmask(7,2)) )  &
    + popcnt( iand( iand( xor(key_in(7,1),iand(key_in(7,1),cas_bitmask(7,1,1))), virt_bitmask(7,1) ), virt_bitmask(7,1)) ) & 
    + popcnt( iand( iand( xor(key_in(7,2),iand(key_in(7,2),cas_bitmask(7,2,1))), virt_bitmask(7,2) ), virt_bitmask(7,2)) ) 
 else if(N_int == 8)then
   i_diff = i_diff  &
    + popcnt( xor( iand(inact_bitmask(1,1), xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1)))), inact_bitmask(1,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(1,2), xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1)))), inact_bitmask(1,2)) )  &
    + popcnt( iand( iand( xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1))), virt_bitmask(1,1) ), virt_bitmask(1,1)) ) & 
    + popcnt( iand( iand( xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1))), virt_bitmask(1,2) ), virt_bitmask(1,2)) ) &
    + popcnt( xor( iand(inact_bitmask(2,1), xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1)))), inact_bitmask(2,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(2,2), xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1)))), inact_bitmask(2,2)) )  &
    + popcnt( iand( iand( xor(key_in(2,1),iand(key_in(2,1),cas_bitmask(2,1,1))), virt_bitmask(2,1) ), virt_bitmask(2,1)) ) & 
    + popcnt( iand( iand( xor(key_in(2,2),iand(key_in(2,2),cas_bitmask(2,2,1))), virt_bitmask(2,2) ), virt_bitmask(2,2)) ) &
    + popcnt( xor( iand(inact_bitmask(3,1), xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1)))), inact_bitmask(3,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(3,2), xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1)))), inact_bitmask(3,2)) )  &
    + popcnt( iand( iand( xor(key_in(3,1),iand(key_in(3,1),cas_bitmask(3,1,1))), virt_bitmask(3,1) ), virt_bitmask(3,1)) ) & 
    + popcnt( iand( iand( xor(key_in(3,2),iand(key_in(3,2),cas_bitmask(3,2,1))), virt_bitmask(3,2) ), virt_bitmask(3,2)) ) &
    + popcnt( xor( iand(inact_bitmask(4,1), xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1)))), inact_bitmask(4,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(4,2), xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1)))), inact_bitmask(4,2)) )  &
    + popcnt( iand( iand( xor(key_in(4,1),iand(key_in(4,1),cas_bitmask(4,1,1))), virt_bitmask(4,1) ), virt_bitmask(4,1)) ) & 
    + popcnt( iand( iand( xor(key_in(4,2),iand(key_in(4,2),cas_bitmask(4,2,1))), virt_bitmask(4,2) ), virt_bitmask(4,2)) ) &
    + popcnt( xor( iand(inact_bitmask(5,1), xor(key_in(5,1),iand(key_in(5,1),cas_bitmask(5,1,1)))), inact_bitmask(5,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(5,2), xor(key_in(5,2),iand(key_in(5,2),cas_bitmask(5,2,1)))), inact_bitmask(5,2)) )  &
    + popcnt( iand( iand( xor(key_in(5,1),iand(key_in(5,1),cas_bitmask(5,1,1))), virt_bitmask(5,1) ), virt_bitmask(5,1)) ) & 
    + popcnt( iand( iand( xor(key_in(5,2),iand(key_in(5,2),cas_bitmask(5,2,1))), virt_bitmask(5,2) ), virt_bitmask(5,2)) ) &
    + popcnt( xor( iand(inact_bitmask(6,1), xor(key_in(6,1),iand(key_in(6,1),cas_bitmask(6,1,1)))), inact_bitmask(6,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(6,2), xor(key_in(6,2),iand(key_in(6,2),cas_bitmask(6,2,1)))), inact_bitmask(6,2)) )  &
    + popcnt( iand( iand( xor(key_in(6,1),iand(key_in(6,1),cas_bitmask(6,1,1))), virt_bitmask(6,1) ), virt_bitmask(6,1)) ) & 
    + popcnt( iand( iand( xor(key_in(6,2),iand(key_in(6,2),cas_bitmask(6,2,1))), virt_bitmask(6,2) ), virt_bitmask(6,2)) ) &
    + popcnt( xor( iand(inact_bitmask(7,1), xor(key_in(7,1),iand(key_in(7,1),cas_bitmask(7,1,1)))), inact_bitmask(7,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(7,2), xor(key_in(7,2),iand(key_in(7,2),cas_bitmask(7,2,1)))), inact_bitmask(7,2)) )  &
    + popcnt( iand( iand( xor(key_in(7,1),iand(key_in(7,1),cas_bitmask(7,1,1))), virt_bitmask(7,1) ), virt_bitmask(7,1)) ) & 
    + popcnt( iand( iand( xor(key_in(7,2),iand(key_in(7,2),cas_bitmask(7,2,1))), virt_bitmask(7,2) ), virt_bitmask(7,2)) ) &
    + popcnt( xor( iand(inact_bitmask(8,1), xor(key_in(8,1),iand(key_in(8,1),cas_bitmask(8,1,1)))), inact_bitmask(8,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(8,2), xor(key_in(8,2),iand(key_in(8,2),cas_bitmask(8,2,1)))), inact_bitmask(8,2)) )  &
    + popcnt( iand( iand( xor(key_in(8,1),iand(key_in(8,1),cas_bitmask(8,1,1))), virt_bitmask(8,1) ), virt_bitmask(8,1)) ) & 
    + popcnt( iand( iand( xor(key_in(8,2),iand(key_in(8,2),cas_bitmask(8,2,1))), virt_bitmask(8,2) ), virt_bitmask(8,2)) ) 

 else

  do i = 1, N_int 
   i_diff = i_diff  &
    + popcnt( xor( iand(inact_bitmask(i,1), xor(key_in(i,1),iand(key_in(i,1),cas_bitmask(i,1,1)))), inact_bitmask(i,1)) )  &   
    + popcnt( xor( iand(inact_bitmask(i,2), xor(key_in(i,2),iand(key_in(i,2),cas_bitmask(i,2,1)))), inact_bitmask(i,2)) )  &
    + popcnt( iand( iand( xor(key_in(i,1),iand(key_in(i,1),cas_bitmask(i,1,1))), virt_bitmask(i,1) ), virt_bitmask(i,1)) ) & 
    + popcnt( iand( iand( xor(key_in(i,2),iand(key_in(i,2),cas_bitmask(i,2,1))), virt_bitmask(i,2) ), virt_bitmask(i,2)) ) 
  enddo
 endif
  is_a_two_holes_two_particles = (i_diff >3)
 end




integer function number_of_holes_verbose(key_in)
 ! function that returns the number of holes in the inact space 
 implicit none
 integer(bit_kind), intent(in) :: key_in(N_int,2)
 integer :: i
 integer(bit_kind) :: key_tmp(N_int,2)
 print*,'HOLES '
 print*,'jey_in = '
 call debug_det(key_in,N_int)
 number_of_holes_verbose = 0
   key_tmp(1,1) = xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1)))
   key_tmp(1,2) = xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,1,1)))
   call debug_det(key_tmp,N_int)
   key_tmp(1,1) = iand(key_tmp(1,1),inact_bitmask(1,1))
   key_tmp(1,2) = iand(key_tmp(1,2),inact_bitmask(1,2))
   call debug_det(key_tmp,N_int)
   key_tmp(1,1) = xor(key_tmp(1,1),inact_bitmask(1,1))
   key_tmp(1,2) = xor(key_tmp(1,2),inact_bitmask(1,2))
   call debug_det(key_tmp,N_int)
!  number_of_holes_verbose = number_of_holes_verbose + popcnt(key_tmp(1,1))   &
!                                                    + popcnt(key_tmp(1,2))
   number_of_holes_verbose = number_of_holes_verbose & 
   + popcnt( xor( iand(inact_bitmask(1,1), xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1)))), inact_bitmask(1,1)) )&   
   + popcnt( xor( iand(inact_bitmask(1,2), xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1)))), inact_bitmask(1,2)) )
  print*,'----------------------'
end


integer function number_of_particles_verbose(key_in)
 ! function that returns the number of particles in the inact space 
 implicit none
 integer(bit_kind), intent(in) :: key_in(N_int,2)
 integer :: i
 integer(bit_kind) :: key_tmp(N_int,2)
 print*,'PARTICLES '
 print*,'jey_in = '
 call debug_det(key_in,N_int)
 number_of_particles_verbose = 0
   key_tmp(1,1) = xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,1,1)))
   key_tmp(1,2) = xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,1,1)))
   call debug_det(key_tmp,N_int)
   key_tmp(1,1) = iand(key_tmp(1,2),virt_bitmask(1,2))
   key_tmp(1,2) = iand(key_tmp(1,2),virt_bitmask(1,2))
   call debug_det(key_tmp,N_int)
   key_tmp(1,1) = iand(key_tmp(1,1),virt_bitmask(1,1))
   key_tmp(1,2) = iand(key_tmp(1,2),virt_bitmask(1,2))
   call debug_det(key_tmp,N_int)
!  number_of_particles_verbose = number_of_particles_verbose + popcnt(key_tmp(1,1))   &
!                                                    + popcnt(key_tmp(1,2))
   number_of_particles_verbose = number_of_particles_verbose & 
      + popcnt( iand( iand( xor(key_in(1,1),iand(key_in(1,1),cas_bitmask(1,1,1))), virt_bitmask(1,1) ), virt_bitmask(1,1)) ) & 
      + popcnt( iand( iand( xor(key_in(1,2),iand(key_in(1,2),cas_bitmask(1,2,1))), virt_bitmask(1,2) ), virt_bitmask(1,2)) ) 
end


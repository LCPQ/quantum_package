integer function number_of_holes(key_in)
 ! function that returns the number of holes in the inact space 
 implicit none
 integer(bit_kind), intent(in) :: key_in(N_int,2)
 integer :: i
 number_of_holes = 0
 if(N_int == 1)then
   number_of_holes = number_of_holes + popcnt(iand(iand(key_in(1,1),inact_bitmask(1,1)),inact_bitmask(1,1))) & 
                                     + popcnt(iand(iand(key_in(1,2),inact_bitmask(1,1)),inact_bitmask(1,1))) 
  return
 else if(N_int == 2)then
   number_of_holes = number_of_holes + popcnt(iand(iand(key_in(1,1),inact_bitmask(1,1)),inact_bitmask(1,1))) & 
                                     + popcnt(iand(iand(key_in(1,2),inact_bitmask(1,1)),inact_bitmask(1,1))) &
                                     + popcnt(iand(iand(key_in(2,1),inact_bitmask(2,1)),inact_bitmask(2,1))) & 
                                     + popcnt(iand(iand(key_in(2,2),inact_bitmask(2,1)),inact_bitmask(2,1))) 
  return
 else if(N_int == 3)then
   number_of_holes = number_of_holes + popcnt(iand(iand(key_in(1,1),inact_bitmask(1,1)),inact_bitmask(1,1))) & 
                                     + popcnt(iand(iand(key_in(1,2),inact_bitmask(1,1)),inact_bitmask(1,1))) &
                                     + popcnt(iand(iand(key_in(2,1),inact_bitmask(2,1)),inact_bitmask(2,1))) & 
                                     + popcnt(iand(iand(key_in(2,2),inact_bitmask(2,1)),inact_bitmask(2,1))) &
                                     + popcnt(iand(iand(key_in(3,1),inact_bitmask(3,1)),inact_bitmask(3,1))) & 
                                     + popcnt(iand(iand(key_in(3,2),inact_bitmask(3,1)),inact_bitmask(3,1))) 
  return
 else if (N_int == 4)then 
   number_of_holes = number_of_holes + popcnt(iand(iand(key_in(1,1),inact_bitmask(1,1)),inact_bitmask(1,1))) & 
                                     + popcnt(iand(iand(key_in(1,2),inact_bitmask(1,1)),inact_bitmask(1,1))) &
                                     + popcnt(iand(iand(key_in(2,1),inact_bitmask(2,1)),inact_bitmask(2,1))) & 
                                     + popcnt(iand(iand(key_in(2,2),inact_bitmask(2,1)),inact_bitmask(2,1))) &
                                     + popcnt(iand(iand(key_in(3,1),inact_bitmask(3,1)),inact_bitmask(3,1))) & 
                                     + popcnt(iand(iand(key_in(3,2),inact_bitmask(3,1)),inact_bitmask(3,1))) &
                                     + popcnt(iand(iand(key_in(4,1),inact_bitmask(4,1)),inact_bitmask(4,1))) & 
                                     + popcnt(iand(iand(key_in(4,2),inact_bitmask(4,1)),inact_bitmask(4,1))) 
  return
 else if (N_int == 5)then 
   number_of_holes = number_of_holes + popcnt(iand(iand(key_in(1,1),inact_bitmask(1,1)),inact_bitmask(1,1))) & 
                                     + popcnt(iand(iand(key_in(1,2),inact_bitmask(1,1)),inact_bitmask(1,1))) &
                                     + popcnt(iand(iand(key_in(2,1),inact_bitmask(2,1)),inact_bitmask(2,1))) & 
                                     + popcnt(iand(iand(key_in(2,2),inact_bitmask(2,1)),inact_bitmask(2,1))) &
                                     + popcnt(iand(iand(key_in(3,1),inact_bitmask(3,1)),inact_bitmask(3,1))) & 
                                     + popcnt(iand(iand(key_in(3,2),inact_bitmask(3,1)),inact_bitmask(3,1))) &
                                     + popcnt(iand(iand(key_in(4,1),inact_bitmask(4,1)),inact_bitmask(4,1))) & 
                                     + popcnt(iand(iand(key_in(4,2),inact_bitmask(4,1)),inact_bitmask(4,1))) &
                                     + popcnt(iand(iand(key_in(5,1),inact_bitmask(5,1)),inact_bitmask(5,1))) & 
                                     + popcnt(iand(iand(key_in(5,2),inact_bitmask(5,1)),inact_bitmask(5,1))) 
  return
 else if (N_int == 6)then 
   number_of_holes = number_of_holes + popcnt(iand(iand(key_in(1,1),inact_bitmask(1,1)),inact_bitmask(1,1))) & 
                                     + popcnt(iand(iand(key_in(1,2),inact_bitmask(1,1)),inact_bitmask(1,1))) &
                                     + popcnt(iand(iand(key_in(2,1),inact_bitmask(2,1)),inact_bitmask(2,1))) & 
                                     + popcnt(iand(iand(key_in(2,2),inact_bitmask(2,1)),inact_bitmask(2,1))) &
                                     + popcnt(iand(iand(key_in(3,1),inact_bitmask(3,1)),inact_bitmask(3,1))) & 
                                     + popcnt(iand(iand(key_in(3,2),inact_bitmask(3,1)),inact_bitmask(3,1))) &
                                     + popcnt(iand(iand(key_in(4,1),inact_bitmask(4,1)),inact_bitmask(4,1))) & 
                                     + popcnt(iand(iand(key_in(4,2),inact_bitmask(4,1)),inact_bitmask(4,1))) &
                                     + popcnt(iand(iand(key_in(5,1),inact_bitmask(5,1)),inact_bitmask(5,1))) & 
                                     + popcnt(iand(iand(key_in(5,2),inact_bitmask(5,1)),inact_bitmask(5,1))) &
                                     + popcnt(iand(iand(key_in(6,1),inact_bitmask(6,1)),inact_bitmask(6,1))) & 
                                     + popcnt(iand(iand(key_in(6,2),inact_bitmask(6,1)),inact_bitmask(6,1))) 
  return
 else if (N_int == 7)then 
   number_of_holes = number_of_holes + popcnt(iand(iand(key_in(1,1),inact_bitmask(1,1)),inact_bitmask(1,1))) & 
                                     + popcnt(iand(iand(key_in(1,2),inact_bitmask(1,1)),inact_bitmask(1,1))) &
                                     + popcnt(iand(iand(key_in(2,1),inact_bitmask(2,1)),inact_bitmask(2,1))) & 
                                     + popcnt(iand(iand(key_in(2,2),inact_bitmask(2,1)),inact_bitmask(2,1))) &
                                     + popcnt(iand(iand(key_in(3,1),inact_bitmask(3,1)),inact_bitmask(3,1))) & 
                                     + popcnt(iand(iand(key_in(3,2),inact_bitmask(3,1)),inact_bitmask(3,1))) &
                                     + popcnt(iand(iand(key_in(4,1),inact_bitmask(4,1)),inact_bitmask(4,1))) & 
                                     + popcnt(iand(iand(key_in(4,2),inact_bitmask(4,1)),inact_bitmask(4,1))) &
                                     + popcnt(iand(iand(key_in(5,1),inact_bitmask(5,1)),inact_bitmask(5,1))) & 
                                     + popcnt(iand(iand(key_in(5,2),inact_bitmask(5,1)),inact_bitmask(5,1))) &
                                     + popcnt(iand(iand(key_in(6,1),inact_bitmask(6,1)),inact_bitmask(6,1))) & 
                                     + popcnt(iand(iand(key_in(6,2),inact_bitmask(6,1)),inact_bitmask(6,1))) &
                                     + popcnt(iand(iand(key_in(7,1),inact_bitmask(7,1)),inact_bitmask(7,1))) & 
                                     + popcnt(iand(iand(key_in(7,2),inact_bitmask(7,1)),inact_bitmask(7,1))) 
  return
 else if (N_int == 8)then 
   number_of_holes = number_of_holes + popcnt(iand(iand(key_in(1,1),inact_bitmask(1,1)),inact_bitmask(1,1))) & 
                                     + popcnt(iand(iand(key_in(1,2),inact_bitmask(1,1)),inact_bitmask(1,1))) &
                                     + popcnt(iand(iand(key_in(2,1),inact_bitmask(2,1)),inact_bitmask(2,1))) & 
                                     + popcnt(iand(iand(key_in(2,2),inact_bitmask(2,1)),inact_bitmask(2,1))) &
                                     + popcnt(iand(iand(key_in(3,1),inact_bitmask(3,1)),inact_bitmask(3,1))) & 
                                     + popcnt(iand(iand(key_in(3,2),inact_bitmask(3,1)),inact_bitmask(3,1))) &
                                     + popcnt(iand(iand(key_in(4,1),inact_bitmask(4,1)),inact_bitmask(4,1))) & 
                                     + popcnt(iand(iand(key_in(4,2),inact_bitmask(4,1)),inact_bitmask(4,1))) &
                                     + popcnt(iand(iand(key_in(5,1),inact_bitmask(5,1)),inact_bitmask(5,1))) & 
                                     + popcnt(iand(iand(key_in(5,2),inact_bitmask(5,1)),inact_bitmask(5,1))) &
                                     + popcnt(iand(iand(key_in(6,1),inact_bitmask(6,1)),inact_bitmask(6,1))) & 
                                     + popcnt(iand(iand(key_in(6,2),inact_bitmask(6,1)),inact_bitmask(6,1))) &
                                     + popcnt(iand(iand(key_in(7,1),inact_bitmask(7,1)),inact_bitmask(7,1))) & 
                                     + popcnt(iand(iand(key_in(7,2),inact_bitmask(7,1)),inact_bitmask(7,1))) &
                                     + popcnt(iand(iand(key_in(8,2),inact_bitmask(8,1)),inact_bitmask(8,1))) &
                                     + popcnt(iand(iand(key_in(8,2),inact_bitmask(8,1)),inact_bitmask(8,1))) 
  return
 else 
   do i = 1, N_int
    number_of_holes = number_of_holes + popcnt(iand(iand(key_in(i,1),inact_bitmask(i,1)),inact_bitmask(i,1))) & 
                                      + popcnt(iand(iand(key_in(i,2),inact_bitmask(i,1)),inact_bitmask(i,1))) 
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
   number_of_particles = number_of_particles + popcnt(iand(iand(key_in(1,1),virt_bitmask(1,1)),virt_bitmask(1,1))) & 
                                             + popcnt(iand(iand(key_in(1,2),virt_bitmask(1,1)),virt_bitmask(1,1))) 
   return
 else if(N_int == 2)then 
   number_of_particles = number_of_particles + popcnt(iand(iand(key_in(1,1),virt_bitmask(1,1)),virt_bitmask(1,1))) & 
                                             + popcnt(iand(iand(key_in(1,2),virt_bitmask(1,1)),virt_bitmask(1,1))) &
                                             + popcnt(iand(iand(key_in(2,1),virt_bitmask(2,1)),virt_bitmask(2,1))) & 
                                             + popcnt(iand(iand(key_in(2,2),virt_bitmask(2,1)),virt_bitmask(2,1))) 
   return
 else if(N_int == 3)then 
   number_of_particles = number_of_particles + popcnt(iand(iand(key_in(1,1),virt_bitmask(1,1)),virt_bitmask(1,1))) & 
                                             + popcnt(iand(iand(key_in(1,2),virt_bitmask(1,1)),virt_bitmask(1,1))) &
                                             + popcnt(iand(iand(key_in(2,1),virt_bitmask(2,1)),virt_bitmask(2,1))) & 
                                             + popcnt(iand(iand(key_in(2,2),virt_bitmask(2,1)),virt_bitmask(2,1))) &
                                             + popcnt(iand(iand(key_in(3,1),virt_bitmask(3,1)),virt_bitmask(3,1))) & 
                                             + popcnt(iand(iand(key_in(3,2),virt_bitmask(3,1)),virt_bitmask(3,1))) 
   return
 else if(N_int == 4)then 
   number_of_particles = number_of_particles + popcnt(iand(iand(key_in(1,1),virt_bitmask(1,1)),virt_bitmask(1,1))) & 
                                             + popcnt(iand(iand(key_in(1,2),virt_bitmask(1,1)),virt_bitmask(1,1))) &
                                             + popcnt(iand(iand(key_in(2,1),virt_bitmask(2,1)),virt_bitmask(2,1))) & 
                                             + popcnt(iand(iand(key_in(2,2),virt_bitmask(2,1)),virt_bitmask(2,1))) &
                                             + popcnt(iand(iand(key_in(3,1),virt_bitmask(3,1)),virt_bitmask(3,1))) & 
                                             + popcnt(iand(iand(key_in(3,2),virt_bitmask(3,1)),virt_bitmask(3,1))) &
                                             + popcnt(iand(iand(key_in(4,1),virt_bitmask(4,1)),virt_bitmask(4,1))) & 
                                             + popcnt(iand(iand(key_in(4,2),virt_bitmask(4,1)),virt_bitmask(4,1)))  
   return
 else if(N_int == 5)then 
   number_of_particles = number_of_particles + popcnt(iand(iand(key_in(1,1),virt_bitmask(1,1)),virt_bitmask(1,1))) & 
                                             + popcnt(iand(iand(key_in(1,2),virt_bitmask(1,1)),virt_bitmask(1,1))) &
                                             + popcnt(iand(iand(key_in(2,1),virt_bitmask(2,1)),virt_bitmask(2,1))) & 
                                             + popcnt(iand(iand(key_in(2,2),virt_bitmask(2,1)),virt_bitmask(2,1))) &
                                             + popcnt(iand(iand(key_in(3,1),virt_bitmask(3,1)),virt_bitmask(3,1))) & 
                                             + popcnt(iand(iand(key_in(3,2),virt_bitmask(3,1)),virt_bitmask(3,1))) &
                                             + popcnt(iand(iand(key_in(4,1),virt_bitmask(4,1)),virt_bitmask(4,1))) & 
                                             + popcnt(iand(iand(key_in(4,2),virt_bitmask(4,1)),virt_bitmask(4,1))) &
                                             + popcnt(iand(iand(key_in(5,1),virt_bitmask(5,1)),virt_bitmask(5,1))) & 
                                             + popcnt(iand(iand(key_in(5,2),virt_bitmask(5,1)),virt_bitmask(5,1)))  
   return
 else if(N_int == 6)then 
   number_of_particles = number_of_particles + popcnt(iand(iand(key_in(1,1),virt_bitmask(1,1)),virt_bitmask(1,1))) & 
                                             + popcnt(iand(iand(key_in(1,2),virt_bitmask(1,1)),virt_bitmask(1,1))) &
                                             + popcnt(iand(iand(key_in(2,1),virt_bitmask(2,1)),virt_bitmask(2,1))) & 
                                             + popcnt(iand(iand(key_in(2,2),virt_bitmask(2,1)),virt_bitmask(2,1))) &
                                             + popcnt(iand(iand(key_in(3,1),virt_bitmask(3,1)),virt_bitmask(3,1))) & 
                                             + popcnt(iand(iand(key_in(3,2),virt_bitmask(3,1)),virt_bitmask(3,1))) &
                                             + popcnt(iand(iand(key_in(4,1),virt_bitmask(4,1)),virt_bitmask(4,1))) & 
                                             + popcnt(iand(iand(key_in(4,2),virt_bitmask(4,1)),virt_bitmask(4,1))) &
                                             + popcnt(iand(iand(key_in(5,1),virt_bitmask(5,1)),virt_bitmask(5,1))) & 
                                             + popcnt(iand(iand(key_in(5,2),virt_bitmask(5,1)),virt_bitmask(5,1))) & 
                                             + popcnt(iand(iand(key_in(6,1),virt_bitmask(6,1)),virt_bitmask(6,1))) & 
                                             + popcnt(iand(iand(key_in(6,2),virt_bitmask(6,1)),virt_bitmask(6,1)))  
   return
 else if(N_int == 7)then 
   number_of_particles = number_of_particles + popcnt(iand(iand(key_in(1,1),virt_bitmask(1,1)),virt_bitmask(1,1))) & 
                                             + popcnt(iand(iand(key_in(1,2),virt_bitmask(1,1)),virt_bitmask(1,1))) &
                                             + popcnt(iand(iand(key_in(2,1),virt_bitmask(2,1)),virt_bitmask(2,1))) & 
                                             + popcnt(iand(iand(key_in(2,2),virt_bitmask(2,1)),virt_bitmask(2,1))) &
                                             + popcnt(iand(iand(key_in(3,1),virt_bitmask(3,1)),virt_bitmask(3,1))) & 
                                             + popcnt(iand(iand(key_in(3,2),virt_bitmask(3,1)),virt_bitmask(3,1))) &
                                             + popcnt(iand(iand(key_in(4,1),virt_bitmask(4,1)),virt_bitmask(4,1))) & 
                                             + popcnt(iand(iand(key_in(4,2),virt_bitmask(4,1)),virt_bitmask(4,1))) &
                                             + popcnt(iand(iand(key_in(5,1),virt_bitmask(5,1)),virt_bitmask(5,1))) & 
                                             + popcnt(iand(iand(key_in(5,2),virt_bitmask(5,1)),virt_bitmask(5,1))) & 
                                             + popcnt(iand(iand(key_in(6,1),virt_bitmask(6,1)),virt_bitmask(6,1))) & 
                                             + popcnt(iand(iand(key_in(6,2),virt_bitmask(6,1)),virt_bitmask(6,1))) & 
                                             + popcnt(iand(iand(key_in(7,1),virt_bitmask(7,1)),virt_bitmask(7,1))) & 
                                             + popcnt(iand(iand(key_in(7,2),virt_bitmask(7,1)),virt_bitmask(7,1)))  
   return
 else if(N_int == 8)then 
   number_of_particles = number_of_particles + popcnt(iand(iand(key_in(1,1),virt_bitmask(1,1)),virt_bitmask(1,1))) & 
                                             + popcnt(iand(iand(key_in(1,2),virt_bitmask(1,1)),virt_bitmask(1,1))) &
                                             + popcnt(iand(iand(key_in(2,1),virt_bitmask(2,1)),virt_bitmask(2,1))) & 
                                             + popcnt(iand(iand(key_in(2,2),virt_bitmask(2,1)),virt_bitmask(2,1))) &
                                             + popcnt(iand(iand(key_in(3,1),virt_bitmask(3,1)),virt_bitmask(3,1))) & 
                                             + popcnt(iand(iand(key_in(3,2),virt_bitmask(3,1)),virt_bitmask(3,1))) &
                                             + popcnt(iand(iand(key_in(4,1),virt_bitmask(4,1)),virt_bitmask(4,1))) & 
                                             + popcnt(iand(iand(key_in(4,2),virt_bitmask(4,1)),virt_bitmask(4,1))) &
                                             + popcnt(iand(iand(key_in(5,1),virt_bitmask(5,1)),virt_bitmask(5,1))) & 
                                             + popcnt(iand(iand(key_in(5,2),virt_bitmask(5,1)),virt_bitmask(5,1))) & 
                                             + popcnt(iand(iand(key_in(6,1),virt_bitmask(6,1)),virt_bitmask(6,1))) & 
                                             + popcnt(iand(iand(key_in(6,2),virt_bitmask(6,1)),virt_bitmask(6,1))) & 
                                             + popcnt(iand(iand(key_in(7,1),virt_bitmask(7,1)),virt_bitmask(7,1))) & 
                                             + popcnt(iand(iand(key_in(7,2),virt_bitmask(7,1)),virt_bitmask(7,1))) & 
                                             + popcnt(iand(iand(key_in(8,1),virt_bitmask(8,1)),virt_bitmask(8,1))) & 
                                             + popcnt(iand(iand(key_in(8,2),virt_bitmask(8,1)),virt_bitmask(8,1)))  
   return
 else
  do i = 1, N_int
   number_of_particles = number_of_particles + popcnt(iand(iand(key_in(i,1),virt_bitmask(i,1)),virt_bitmask(i,1))) & 
                                             + popcnt(iand(iand(key_in(i,2),virt_bitmask(i,1)),virt_bitmask(i,1))) 
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
   i_diff = i_diff                  + popcnt(iand(iand(key_in(1,1),inact_bitmask(1,1)),inact_bitmask(1,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(1,2),inact_bitmask(1,1)),inact_bitmask(1,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(1,1),virt_bitmask(1,1)),virt_bitmask(1,1)))   &   ! particles alpha
                                    + popcnt(iand(iand(key_in(1,2),virt_bitmask(1,1)),virt_bitmask(1,1)))       ! particles beta
  is_a_two_holes_two_particles = (i_diff >3)
 else if(N_int == 2)then
   i_diff = i_diff                  + popcnt(iand(iand(key_in(1,1),inact_bitmask(1,1)),inact_bitmask(1,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(1,2),inact_bitmask(1,1)),inact_bitmask(1,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(1,1),virt_bitmask(1,1)),  virt_bitmask(1,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(1,2),virt_bitmask(1,1)),  virt_bitmask(1,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(2,1),inact_bitmask(2,1)),inact_bitmask(2,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(2,2),inact_bitmask(2,1)),inact_bitmask(2,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(2,1), virt_bitmask(2,1)), virt_bitmask(2,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(2,2), virt_bitmask(2,1)), virt_bitmask(2,1)))     ! particles beta
  is_a_two_holes_two_particles = (i_diff >3)
 else if(N_int == 3)then
   i_diff = i_diff                  + popcnt(iand(iand(key_in(1,1),inact_bitmask(1,1)),inact_bitmask(1,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(1,2),inact_bitmask(1,1)),inact_bitmask(1,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(1,1),virt_bitmask(1,1)),  virt_bitmask(1,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(1,2),virt_bitmask(1,1)),  virt_bitmask(1,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(2,1),inact_bitmask(2,1)),inact_bitmask(2,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(2,2),inact_bitmask(2,1)),inact_bitmask(2,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(2,1), virt_bitmask(2,1)), virt_bitmask(2,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(2,2), virt_bitmask(2,1)), virt_bitmask(2,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(3,1),inact_bitmask(3,1)),inact_bitmask(3,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(3,2),inact_bitmask(3,1)),inact_bitmask(3,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(3,1), virt_bitmask(3,1)), virt_bitmask(3,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(3,2), virt_bitmask(3,1)), virt_bitmask(3,1)))     ! particles beta
  is_a_two_holes_two_particles = (i_diff >3)
 else if(N_int == 4)then
   i_diff = i_diff                  + popcnt(iand(iand(key_in(1,1),inact_bitmask(1,1)),inact_bitmask(1,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(1,2),inact_bitmask(1,1)),inact_bitmask(1,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(1,1),virt_bitmask(1,1)),  virt_bitmask(1,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(1,2),virt_bitmask(1,1)),  virt_bitmask(1,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(2,1),inact_bitmask(2,1)),inact_bitmask(2,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(2,2),inact_bitmask(2,1)),inact_bitmask(2,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(2,1), virt_bitmask(2,1)), virt_bitmask(2,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(2,2), virt_bitmask(2,1)), virt_bitmask(2,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(3,1),inact_bitmask(3,1)),inact_bitmask(3,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(3,2),inact_bitmask(3,1)),inact_bitmask(3,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(3,1), virt_bitmask(3,1)), virt_bitmask(3,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(3,2), virt_bitmask(3,1)), virt_bitmask(3,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(4,1),inact_bitmask(4,1)),inact_bitmask(4,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(4,2),inact_bitmask(4,1)),inact_bitmask(4,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(4,1), virt_bitmask(4,1)), virt_bitmask(4,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(4,2), virt_bitmask(4,1)), virt_bitmask(4,1)))     ! particles beta
  is_a_two_holes_two_particles = (i_diff >3)
 else if(N_int == 5)then
   i_diff = i_diff                  + popcnt(iand(iand(key_in(1,1),inact_bitmask(1,1)),inact_bitmask(1,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(1,2),inact_bitmask(1,1)),inact_bitmask(1,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(1,1),virt_bitmask(1,1)),  virt_bitmask(1,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(1,2),virt_bitmask(1,1)),  virt_bitmask(1,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(2,1),inact_bitmask(2,1)),inact_bitmask(2,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(2,2),inact_bitmask(2,1)),inact_bitmask(2,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(2,1), virt_bitmask(2,1)), virt_bitmask(2,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(2,2), virt_bitmask(2,1)), virt_bitmask(2,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(3,1),inact_bitmask(3,1)),inact_bitmask(3,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(3,2),inact_bitmask(3,1)),inact_bitmask(3,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(3,1), virt_bitmask(3,1)), virt_bitmask(3,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(3,2), virt_bitmask(3,1)), virt_bitmask(3,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(4,1),inact_bitmask(4,1)),inact_bitmask(4,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(4,2),inact_bitmask(4,1)),inact_bitmask(4,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(4,1), virt_bitmask(4,1)), virt_bitmask(4,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(4,2), virt_bitmask(4,1)), virt_bitmask(4,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(5,1),inact_bitmask(5,1)),inact_bitmask(5,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(5,2),inact_bitmask(5,1)),inact_bitmask(5,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(5,1), virt_bitmask(5,1)), virt_bitmask(5,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(5,2), virt_bitmask(5,1)), virt_bitmask(5,1)))     ! particles beta
  is_a_two_holes_two_particles = (i_diff >3)
 else if(N_int == 6)then
   i_diff = i_diff                  + popcnt(iand(iand(key_in(1,1),inact_bitmask(1,1)),inact_bitmask(1,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(1,2),inact_bitmask(1,1)),inact_bitmask(1,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(1,1),virt_bitmask(1,1)),  virt_bitmask(1,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(1,2),virt_bitmask(1,1)),  virt_bitmask(1,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(2,1),inact_bitmask(2,1)),inact_bitmask(2,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(2,2),inact_bitmask(2,1)),inact_bitmask(2,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(2,1), virt_bitmask(2,1)), virt_bitmask(2,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(2,2), virt_bitmask(2,1)), virt_bitmask(2,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(3,1),inact_bitmask(3,1)),inact_bitmask(3,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(3,2),inact_bitmask(3,1)),inact_bitmask(3,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(3,1), virt_bitmask(3,1)), virt_bitmask(3,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(3,2), virt_bitmask(3,1)), virt_bitmask(3,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(4,1),inact_bitmask(4,1)),inact_bitmask(4,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(4,2),inact_bitmask(4,1)),inact_bitmask(4,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(4,1), virt_bitmask(4,1)), virt_bitmask(4,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(4,2), virt_bitmask(4,1)), virt_bitmask(4,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(5,1),inact_bitmask(5,1)),inact_bitmask(5,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(5,2),inact_bitmask(5,1)),inact_bitmask(5,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(5,1), virt_bitmask(5,1)), virt_bitmask(5,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(5,2), virt_bitmask(5,1)), virt_bitmask(5,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(6,1),inact_bitmask(6,1)),inact_bitmask(6,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(6,2),inact_bitmask(6,1)),inact_bitmask(6,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(6,1), virt_bitmask(6,1)), virt_bitmask(6,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(6,2), virt_bitmask(6,1)), virt_bitmask(6,1)))     ! particles beta
  is_a_two_holes_two_particles = (i_diff >3)
 else if(N_int == 7)then
   i_diff = i_diff                  + popcnt(iand(iand(key_in(1,1),inact_bitmask(1,1)),inact_bitmask(1,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(1,2),inact_bitmask(1,1)),inact_bitmask(1,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(1,1),virt_bitmask(1,1)),  virt_bitmask(1,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(1,2),virt_bitmask(1,1)),  virt_bitmask(1,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(2,1),inact_bitmask(2,1)),inact_bitmask(2,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(2,2),inact_bitmask(2,1)),inact_bitmask(2,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(2,1), virt_bitmask(2,1)), virt_bitmask(2,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(2,2), virt_bitmask(2,1)), virt_bitmask(2,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(3,1),inact_bitmask(3,1)),inact_bitmask(3,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(3,2),inact_bitmask(3,1)),inact_bitmask(3,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(3,1), virt_bitmask(3,1)), virt_bitmask(3,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(3,2), virt_bitmask(3,1)), virt_bitmask(3,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(4,1),inact_bitmask(4,1)),inact_bitmask(4,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(4,2),inact_bitmask(4,1)),inact_bitmask(4,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(4,1), virt_bitmask(4,1)), virt_bitmask(4,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(4,2), virt_bitmask(4,1)), virt_bitmask(4,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(5,1),inact_bitmask(5,1)),inact_bitmask(5,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(5,2),inact_bitmask(5,1)),inact_bitmask(5,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(5,1), virt_bitmask(5,1)), virt_bitmask(5,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(5,2), virt_bitmask(5,1)), virt_bitmask(5,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(6,1),inact_bitmask(6,1)),inact_bitmask(6,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(6,2),inact_bitmask(6,1)),inact_bitmask(6,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(6,1), virt_bitmask(6,1)), virt_bitmask(6,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(6,2), virt_bitmask(6,1)), virt_bitmask(6,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(7,1),inact_bitmask(7,1)),inact_bitmask(7,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(7,2),inact_bitmask(7,1)),inact_bitmask(7,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(7,1), virt_bitmask(7,1)), virt_bitmask(7,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(7,2), virt_bitmask(7,1)), virt_bitmask(7,1)))     ! particles beta
  is_a_two_holes_two_particles = (i_diff >3)
 else if(N_int == 8)then
   i_diff = i_diff                  + popcnt(iand(iand(key_in(1,1),inact_bitmask(1,1)),inact_bitmask(1,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(1,2),inact_bitmask(1,1)),inact_bitmask(1,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(1,1),virt_bitmask(1,1)),  virt_bitmask(1,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(1,2),virt_bitmask(1,1)),  virt_bitmask(1,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(2,1),inact_bitmask(2,1)),inact_bitmask(2,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(2,2),inact_bitmask(2,1)),inact_bitmask(2,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(2,1), virt_bitmask(2,1)), virt_bitmask(2,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(2,2), virt_bitmask(2,1)), virt_bitmask(2,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(3,1),inact_bitmask(3,1)),inact_bitmask(3,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(3,2),inact_bitmask(3,1)),inact_bitmask(3,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(3,1), virt_bitmask(3,1)), virt_bitmask(3,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(3,2), virt_bitmask(3,1)), virt_bitmask(3,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(4,1),inact_bitmask(4,1)),inact_bitmask(4,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(4,2),inact_bitmask(4,1)),inact_bitmask(4,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(4,1), virt_bitmask(4,1)), virt_bitmask(4,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(4,2), virt_bitmask(4,1)), virt_bitmask(4,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(5,1),inact_bitmask(5,1)),inact_bitmask(5,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(5,2),inact_bitmask(5,1)),inact_bitmask(5,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(5,1), virt_bitmask(5,1)), virt_bitmask(5,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(5,2), virt_bitmask(5,1)), virt_bitmask(5,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(6,1),inact_bitmask(6,1)),inact_bitmask(6,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(6,2),inact_bitmask(6,1)),inact_bitmask(6,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(6,1), virt_bitmask(6,1)), virt_bitmask(6,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(6,2), virt_bitmask(6,1)), virt_bitmask(6,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(7,1),inact_bitmask(7,1)),inact_bitmask(7,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(7,2),inact_bitmask(7,1)),inact_bitmask(7,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(7,1), virt_bitmask(7,1)), virt_bitmask(7,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(7,2), virt_bitmask(7,1)), virt_bitmask(7,1))) &   ! particles beta
                                    + popcnt(iand(iand(key_in(8,1),inact_bitmask(8,1)),inact_bitmask(8,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(8,2),inact_bitmask(8,1)),inact_bitmask(8,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(8,1), virt_bitmask(8,1)), virt_bitmask(8,1))) &   ! particles alpha
                                    + popcnt(iand(iand(key_in(8,2), virt_bitmask(8,1)), virt_bitmask(8,1)))     ! particles beta
  is_a_two_holes_two_particles = (i_diff >3)
 else 
  do i = 1, N_int
   i_diff = i_diff                  + popcnt(iand(iand(key_in(i,1),inact_bitmask(i,1)),inact_bitmask(i,1))) &   ! holes alpha
                                    + popcnt(iand(iand(key_in(i,2),inact_bitmask(i,1)),inact_bitmask(i,1))) &   ! holes beta
                                    + popcnt(iand(iand(key_in(i,1),virt_bitmask(i,1)),virt_bitmask(i,1))) &     ! particles alpha
                                    + popcnt(iand(iand(key_in(i,2),virt_bitmask(i,1)),virt_bitmask(i,1)))       ! particles beta
  enddo
  is_a_two_holes_two_particles = (i_diff >3)
 endif
 end

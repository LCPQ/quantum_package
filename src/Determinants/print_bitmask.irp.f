program print_bitmask
 implicit none
 print*,'core'
 call debug_det(core_bitmask,N_int) 
 print*,'inact'
 call debug_det(inact_bitmask,N_int) 
 print*,'virt'
 call debug_det(virt_bitmask,N_int) 


end

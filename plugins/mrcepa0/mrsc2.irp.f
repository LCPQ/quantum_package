program mrsc2
  implicit none
  double precision, allocatable  :: energy(:)
  allocate (energy(N_states))
  
  !mrmode : 1=mrcepa0, 2=mrsc2 add, 3=mrcc
  mrmode = 2
  read_wf = .True.
  SOFT_TOUCH read_wf
  call print_cas_coefs
  call set_generators_bitmasks_as_holes_and_particles
  call run(N_states,energy)
  if(do_pt2_end)then
    call run_pt2(N_states,energy)
  endif
  deallocate(energy)
end



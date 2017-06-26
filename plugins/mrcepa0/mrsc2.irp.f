program mrsc2
  implicit none
  double precision, allocatable  :: energy(:)
  allocate (energy(N_states))
  
  !mrmode : 1=mrcepa0, 2=mrsc2 add, 3=mrcc
  mrmode = 2
  read_wf = .True.
  SOFT_TOUCH read_wf
  call set_generators_bitmasks_as_holes_and_particles
  if (.True.) then
    integer :: i,j
    do j=1,N_states
      do i=1,N_det
        psi_coef(i,j) = CI_eigenvectors(i,j)
      enddo
    enddo
    TOUCH psi_coef 
  endif
  call run(N_states,energy)
  if(do_pt2)then
    call run_pt2(N_states,energy)
  endif
  deallocate(energy)
end



program pouet
 implicit none
 read_wf = .True.
 touch read_wf
 call routine
end

subroutine routine
 implicit none
 integer :: i,j,number_of_holes,number_of_particles
 integer :: n_h,n_p
 do i = 1, N_det
  n_h = number_of_holes(psi_det(1,1,i))
  n_p = number_of_particles(psi_det(1,1,i))
  if(n_h == 0 .and. n_p == 0)then
   print*,'CAS'
  else if(n_h == 1 .and. n_p ==0)then
   print*,'1h'
  else if(n_h == 0 .and. n_p ==1)then
   print*,'1p'
  else if(n_h == 1 .and. n_p ==1)then
   print*,'1h1p'
  else if(n_h == 2 .and. n_p ==1)then
   print*,'2h1p'
  else if(n_h == 1 .and. n_p ==2)then
   print*,'1h2p'
  else 
    print*,'PB !! '
    call debug_det(psi_det(1,1,i),  N_int)
    stop
  endif
 enddo



end

subroutine set_generators_to_psi_det
 implicit none
 BEGIN_DOC
! subroutines that sets psi_det_generators to 
! the current psi_det
 END_DOC
 N_det_generators = N_det
 integer :: i,k 
 do i=1,N_det_generators
   do k=1,N_int
     psi_det_generators(k,1,i) = psi_det(k,1,i)
     psi_det_generators(k,2,i) = psi_det(k,2,i)
   enddo
  do k = 1, N_states
   psi_coef_generators(i,k) = psi_coef(i,k)
  enddo
 enddo

 touch N_det_generators psi_coef_generators psi_det_generators

end

subroutine set_generators_as_input_psi(ndet_input,psi_det_input,psi_coef_input)
 implicit none
 integer, intent(in) :: ndet_input 
 integer(bit_kind), intent(in) :: psi_det_input(N_int,2,ndet_input)
 double precision, intent(in) :: psi_coef_input(ndet_input,N_states) 
 BEGIN_DOC
! subroutines that sets psi_det_generators to 
! the current psi_det
 END_DOC
 N_det_generators = ndet_input
 integer :: i,k 
 do i=1,N_det_generators
   do k=1,N_int
     psi_det_generators(k,1,i) = psi_det_input(k,1,i)
     psi_det_generators(k,2,i) = psi_det_input(k,2,i)
   enddo
  do k = 1, N_states
   psi_coef_generators(i,k) = psi_coef_input(i,k)
  enddo
 enddo

 touch N_det_generators psi_coef_generators psi_det_generators

end

subroutine set_psi_det_as_input_psi(ndet_input,psi_det_input,psi_coef_input)
 implicit none
 integer, intent(in) :: ndet_input 
 integer(bit_kind), intent(in) :: psi_det_input(N_int,2,ndet_input)
 double precision, intent(in) :: psi_coef_input(ndet_input,N_states) 
 BEGIN_DOC
! subroutines that sets psi_det_generators to 
! the current psi_det
 END_DOC
 N_det= ndet_input
 if (psi_det_size < N_det) then
   psi_det_size = N_det
   TOUCH psi_det_size
 endif

 integer :: i,k 
 do i=1,N_det
   do k=1,N_int
     psi_det(k,1,i) = psi_det_input(k,1,i)
     psi_det(k,2,i) = psi_det_input(k,2,i)
   enddo
  do k = 1, N_states
   psi_coef(i,k) = psi_coef_input(i,k)
  enddo
 enddo

 soft_touch N_det psi_coef psi_det

end


subroutine set_psi_det_to_generators
 implicit none
 BEGIN_DOC
! subroutines that sets psi_det_generators to 
! the current psi_det
 END_DOC
 N_det= N_det_generators 
 integer :: i,k 
 do i = 1, psi_det_size
  do k=1,N_int
    psi_det(k,1,i) =  0_bit_kind
    psi_det(k,2,i) =  0_bit_kind
  enddo
  do k = 1, N_states
   psi_coef(i,k) = 0.d0
  enddo
 enddo
 do i=1,N_det_generators
   do k=1,N_int
     psi_det(k,1,i) =  psi_det_generators(k,1,i)
     psi_det(k,2,i) =  psi_det_generators(k,2,i)
   enddo
  do k = 1, N_states
   psi_coef(i,k) = psi_coef_generators(i,k)
  enddo
 enddo

 touch N_det psi_coef psi_det

end



subroutine set_generators_to_generators_restart
 implicit none
 BEGIN_DOC
! subroutines that sets psi_det_generators to 
! the current psi_det
 END_DOC
 N_det_generators = N_det_generators_restart
 integer :: i,k 
 do i=1,N_det_generators
   do k=1,N_int
     psi_det_generators(k,1,i) = psi_det_generators_restart(k,1,i)
     psi_det_generators(k,2,i) = psi_det_generators_restart(k,2,i)
   enddo
  do k = 1, N_states
   psi_coef_generators(i,k) = psi_coef_generators_restart(i,k)
  enddo
 enddo

 touch N_det_generators psi_coef_generators psi_det_generators

end


subroutine set_psi_det_to_generators_restart
 implicit none
 BEGIN_DOC
! subroutines that sets psi_det_generators to 
! the current psi_det
 END_DOC
 N_det = N_det_generators_restart
 integer :: i,k 
 do i=1,N_det_generators
   do k=1,N_int
     psi_det(k,1,i) = psi_det_generators_restart(k,1,i)
     psi_det(k,2,i) = psi_det_generators_restart(k,2,i)
   enddo
  do k = 1, N_states
   psi_coef(i,k) = psi_coef_generators_restart(i,k)
  enddo
 enddo

 touch N_det psi_coef psi_det

end



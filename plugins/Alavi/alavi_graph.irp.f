program alavi_graph
  implicit none
  integer :: exc(0:2,2,2),h1,p1,h2,p2,s1,s2
  double precision :: phase

  read_wf = .True.
  touch read_wf

  integer :: k,degree
  double precision :: hii

  do k=1,N_det
      call get_excitation_degree(psi_det(1,1,1),psi_det(1,1,k),degree,N_int)
      call  i_H_j(psi_det(1,1,k),psi_det(1,1,k),N_int,hii) 
      print*, k,abs(psi_coef(k,1)), hii,degree

!      if (degree == 2) then
!      	call get_excitation(psi_det(1,1,1),psi_det(1,1,k),exc,degree,phase,N_int)
!      	call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
!		print*, h1,h2,hii, abs(psi_coef(k,1))
!      endif
!      


  enddo
end

!plot "test.dat" u (abs($2)):(abs($3)):4  w p palette
subroutine pt2_moller_plesset(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,n_st)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint,ndet,n_st
  integer(bit_kind), intent(in)  :: det_pert(Nint,2)
  double precision , intent(out) :: c_pert(n_st),e_2_pert(n_st),H_pert_diag(N_st)
  double precision               :: i_H_psi_array(N_st)
  
  BEGIN_DOC
  ! compute the standard Moller-Plesset perturbative first order coefficient and second order energetic contribution
  !
  ! for the various n_st states.
  !
  ! c_pert(i) = <psi(i)|H|det_pert>/(difference of orbital energies) 
  !
  ! e_2_pert(i) = <psi(i)|H|det_pert>^2/(difference of orbital energies) 
  !
  END_DOC
  
  integer                        :: i,j
  double precision               :: diag_H_mat_elem
  integer                        :: exc(0:2,2,2)
  integer                        :: degree
  double precision               :: phase,delta_e,h
  integer                        :: h1,h2,p1,p2,s1,s2
  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)
  call get_excitation(ref_bitmask,det_pert,exc,degree,phase,Nint)
  call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
  delta_e = Fock_matrix_diag_mo(h1) + Fock_matrix_diag_mo(h2) - &
           (Fock_matrix_diag_mo(p1) + Fock_matrix_diag_mo(p2))
  delta_e = 1.d0/delta_e

  call i_H_psi(det_pert,psi_selectors,psi_selectors_coef,Nint,N_det,psi_selectors_size,n_st,i_H_psi_array)
  h = diag_H_mat_elem(det_pert,Nint)
  do i =1,n_st
    H_pert_diag(i) = h
    c_pert(i) = i_H_psi_array(i) *delta_e
    e_2_pert(i) = c_pert(i) * i_H_psi_array(i)
  enddo
  
end

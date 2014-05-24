subroutine pt2_epstein_nesbet(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,n_st)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint,ndet,n_st
  integer(bit_kind), intent(in)  :: det_pert(Nint,2)
  double precision , intent(out) :: c_pert(n_st),e_2_pert(n_st),H_pert_diag
  double precision               :: i_H_psi_array(N_st)
  
  BEGIN_DOC
  ! compute the standard Epstein-Nesbet perturbative first order coefficient and second order energetic contribution
  !
  ! for the various n_st states.
  !
  ! c_pert(i) = <psi(i)|H|det_pert>/( E(i) - <det_pert|H|det_pert> )
  !
  ! e_2_pert(i) = <psi(i)|H|det_pert>^2/( E(i) - <det_pert|H|det_pert> )
  !
  END_DOC
  
  integer                        :: i,j
  double precision               :: diag_H_mat_elem
  double precision, parameter    :: eps = tiny(1.d0)
  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)
  call i_H_psi(det_pert,psi_ref,psi_ref_coef,Nint,ndet,psi_ref_size,n_st,i_H_psi_array)
  H_pert_diag = diag_H_mat_elem(det_pert,Nint)
  do i =1,n_st
    c_pert(i) = i_H_psi_array(i) / (reference_energy(i) - H_pert_diag + eps)
    e_2_pert(i) = c_pert(i) * i_H_psi_array(i)
  enddo
  
end

subroutine pt2_epstein_nesbet_2x2(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,n_st)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint,ndet,n_st
  integer(bit_kind), intent(in)  :: det_pert(Nint,2)
  double precision , intent(out) :: c_pert(n_st),e_2_pert(n_st),H_pert_diag
  double precision               :: i_H_psi_array(N_st)
  
  BEGIN_DOC
  ! compute the Epstein-Nesbet 2x2 diagonalization coefficient and energetic contribution
  !
  ! for the various n_st states.
  !
  ! e_2_pert(i) = 0.5 * (( <det_pert|H|det_pert> -  E(i) )  - sqrt( ( <det_pert|H|det_pert> -  E(i)) ^2 + 4 <psi(i)|H|det_pert>^2  )
  !
  ! c_pert(i) = e_2_pert(i)/ <psi(i)|H|det_pert>
  !
  END_DOC
  
  integer                        :: i,j
  double precision               :: diag_H_mat_elem,delta_e
  double precision, parameter    :: eps = tiny(1.d0)
  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)
  call i_H_psi(det_pert,psi_ref,psi_ref_coef,Nint,N_det_ref,psi_ref_size,n_st,i_H_psi_array)
  H_pert_diag = diag_H_mat_elem(det_pert,Nint)
  do i =1,n_st
    delta_e = H_pert_diag - reference_energy(i)
    e_2_pert(i) = 0.5d0 * (delta_e - dsqrt(delta_e * delta_e + 4.d0 * i_H_psi_array(i) * i_H_psi_array(i)))
    c_pert(i) = e_2_pert(i)/(i_H_psi_array(i)+eps)
  enddo

end

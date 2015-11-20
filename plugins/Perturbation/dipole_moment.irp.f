subroutine pt2_dipole_moment_z(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,n_st,minilist,idx_minilist,N_minilist)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint,ndet,n_st
  integer(bit_kind), intent(in)  :: det_pert(Nint,2)
  double precision , intent(out) :: c_pert(n_st),e_2_pert(n_st),H_pert_diag(N_st)
  double precision               :: i_O1_psi_array(N_st)
  double precision               :: i_H_psi_array(N_st)
  
  integer, intent(in)            :: N_minilist
  integer, intent(in)            :: idx_minilist(0:N_det_selectors)
  integer(bit_kind), intent(in)  :: minilist(Nint,2,N_det_selectors)
  
  BEGIN_DOC
  ! compute the perturbatibe contribution to the dipole moment of one determinant
  !
  ! for the various n_st states, at various level of theory.
  !
  ! c_pert(i) = <psi(i)|H|det_pert>/(<psi(i)|H|psi(i)> - <det_pert|H|det_pert>)
  !
  ! e_2_pert(i) = c_pert(i) * <det_pert|Z|psi(i)>
  !
  ! H_pert_diag(i) = c_pert(i)^2 * <det_pert|Z|det_pert>
  !
  ! To get the contribution of the first order : 
  !
  ! <Z_1> = sum(over i)  e_2_pert(i) 
  !
  ! To get the contribution of the diagonal elements of the second order : 
  !
  ! [ <Z_0> + <Z_1> + sum(over i)  H_pert_diag(i) ] / [1. + sum(over i) c_pert(i) **2]
  !
  END_DOC
  
  integer                        :: i,j
  double precision               :: diag_H_mat_elem
  integer                        :: exc(0:2,2,2)
  integer                        :: degree
  double precision               :: phase,delta_e,h,oii,diag_o1_mat_elem
  integer                        :: h1,h2,p1,p2,s1,s2
  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)

! call get_excitation_degree(HF_bitmask,det_pert,degree,N_int)
! if(degree.gt.degree_max_generators+1)then
!  H_pert_diag = 0.d0
!  e_2_pert = 0.d0
!  c_pert = 0.d0
!  return
! endif

  call i_O1_psi(mo_dipole_z,det_pert,psi_selectors,psi_selectors_coef,Nint,N_det_selectors,psi_selectors_size,N_st,i_O1_psi_array)
  !call i_H_psi(det_pert,psi_selectors,psi_selectors_coef,Nint,N_det_selectors,psi_selectors_size,N_st,i_H_psi_array)
  call i_H_psi_minilist(det_pert,minilist,idx_minilist,N_minilist,psi_selectors_coef,Nint,N_minilist,psi_selectors_size,N_st,i_H_psi_array)
  
  h = diag_H_mat_elem(det_pert,Nint)
  oii = diag_O1_mat_elem(mo_dipole_z,det_pert,N_int)


  do i =1,N_st
    if(CI_electronic_energy(i)>h.and.CI_electronic_energy(i).ne.0.d0)then
      c_pert(i) = -1.d0
      e_2_pert(i) = selection_criterion*selection_criterion_factor*2.d0
    else if  (dabs(CI_electronic_energy(i) - h) > 1.d-6) then
        c_pert(i) = i_H_psi_array(i) / (CI_electronic_energy(i) - h)
        e_2_pert(i) = c_pert(i) * (i_O1_psi_array(i)+i_O1_psi_array(i) ) 
        H_pert_diag(i) = e_2_pert(i) + c_pert(i) * c_pert(i) * oii
    else
      c_pert(i) = -1.d0
      e_2_pert(i) = -dabs(i_H_psi_array(i))
      H_pert_diag(i) = c_pert(i) * i_O1_psi_array(i)
    endif
  enddo
end


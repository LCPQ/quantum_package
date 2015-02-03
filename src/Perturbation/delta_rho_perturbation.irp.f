subroutine pt2_delta_rho_one_point(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,n_st)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint,ndet,n_st
  integer(bit_kind), intent(in)  :: det_pert(Nint,2)
  double precision , intent(out) :: c_pert(n_st),e_2_pert(n_st),H_pert_diag(N_st)
  double precision               :: i_O1_psi_array(N_st)
  double precision               :: i_H_psi_array(N_st)
  
  BEGIN_DOC
  ! compute the perturbatibe contribution to the dipole moment of one determinant
  !
  ! for the various n_st states.
  !
  ! c_pert(i) = <psi(i)|H|det_pert>/(<psi(i)|H|psi(i)> - <det_pert|H|det_pert>)
  !
  ! e_2_pert(i) = |c_pert(i) * <det_pert|X|psi(i)>|
  !
  ! H_pert_diag(i) = c_pert(i) * <det_pert|X|psi(i)>
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
  call get_excitation_degree(HF_bitmask,det_pert,degree,N_int)
  if(degree.gt.max_degree_exc+1)then
   H_pert_diag = 0.d0
   e_2_pert = 0.d0
   c_pert = 0.d0
   return
  endif
  call i_O1_psi_alpha_beta(mo_integrated_delta_rho_one_point,det_pert,psi_selectors,psi_selectors_coef,Nint,N_det_selectors,psi_selectors_size,N_st,i_O1_psi_array)
  if(dabs(i_O1_psi_array(1)).le.1.d-9)then
   H_pert_diag = 0.d0
   e_2_pert = 0.d0
   c_pert = 0.d0
   return
  endif

  call i_H_psi(det_pert,psi_selectors,psi_selectors_coef,Nint,N_det_selectors,psi_selectors_size,N_st,i_H_psi_array)
! call filter_connected_i_H_psi0_print(psi_det,det_pert,N_int,N_det,idx)
  integer ::  idx(0:N_det)

  logical :: is_in_wavefunction,is_wf
  is_wf = is_in_wavefunction(det_pert,N_int,N_det)
  if(is_wf)then
   print*,'PB !!!!!!!!!!!!'
   call debug_det(det_pert,N_int)
   stop
  endif
  h = diag_H_mat_elem(det_pert,Nint)
! oii = diag_O1_mat_elem(mo_dipole_z,det_pert,N_int)


  do i =1,N_st
    if(CI_electronic_energy(i)>h.and.CI_electronic_energy(i).ne.0.d0)then
      c_pert(i) = -1.d0
      e_2_pert(i) = selection_criterion*selection_criterion_factor*2.d0
    else if  (dabs(CI_electronic_energy(i) - h) > 1.d-6) then
        c_pert(i) = i_H_psi_array(i) / (CI_electronic_energy(i) - h)
        H_pert_diag(i) = c_pert(i) * (i_O1_psi_array(i) + i_O1_psi_array(i) ) 
!       e_2_pert(i) = -dabs(c_pert(i) * (i_O1_psi_array(i)+i_O1_psi_array(i) ) + c_pert(i) * c_pert(i) * oii)
        e_2_pert(i) = -dabs(c_pert(i) * (i_O1_psi_array(i)+i_O1_psi_array(i) ) )
    else
      c_pert(i) = -1.d0
      e_2_pert(i) = -dabs(i_H_psi_array(i))
      H_pert_diag(i) = c_pert(i) * i_O1_psi_array(i)
    endif
  enddo


end

subroutine pt2_delta_rho_one_point_no_selection(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,n_st)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint,ndet,n_st
  integer(bit_kind), intent(in)  :: det_pert(Nint,2)
  double precision , intent(out) :: c_pert(n_st),e_2_pert(n_st),H_pert_diag(N_st)
  double precision               :: i_O1_psi_array(N_st)
  double precision               :: i_H_psi_array(N_st)
  
  BEGIN_DOC
  ! compute the perturbatibe contribution to the dipole moment of one determinant
  !
  ! for the various n_st states.
  !
  ! c_pert(i) = <psi(i)|H|det_pert>/(<psi(i)|H|psi(i)> - <det_pert|H|det_pert>)
  !
  ! e_2_pert(i) = |c_pert(i) * <det_pert|X|psi(i)>|
  !
  ! H_pert_diag(i) = c_pert(i) * <det_pert|X|psi(i)>
  !
  END_DOC
  
  integer                        :: i,j
  double precision               :: diag_H_mat_elem
  integer                        :: exc(0:2,2,2)
  integer                        :: degree
  double precision               :: phase,delta_e,h,oii,diag_o1_mat_elem,diag_O1_mat_elem_alpha_beta
  integer                        :: h1,h2,p1,p2,s1,s2
  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)
  call get_excitation_degree(HF_bitmask,det_pert,degree,N_int)
!  print*,'coucou'
! if(degree.gt.max_degree_exc+1)then
!  H_pert_diag = 0.d0
!  e_2_pert = 0.d0
!  c_pert = 0.d0
!  return
! endif
  call i_O1_psi_alpha_beta(mo_integrated_delta_rho_one_point,det_pert,psi_selectors,psi_selectors_coef,Nint,N_det_selectors,psi_selectors_size,N_st,i_O1_psi_array)
! if(dabs(i_O1_psi_array(1)).le.1.d-9)then
!  H_pert_diag = 0.d0
!  e_2_pert = 0.d0
!  c_pert = 0.d0
!  return
! endif

  call i_H_psi(det_pert,psi_selectors,psi_selectors_coef,Nint,N_det_selectors,psi_selectors_size,N_st,i_H_psi_array)
  h = diag_H_mat_elem(det_pert,Nint)
  oii = diag_O1_mat_elem_alpha_beta(mo_integrated_delta_rho_one_point,det_pert,N_int)


  do i =1,N_st
    if(CI_electronic_energy(i)>h.and.CI_electronic_energy(i).ne.0.d0)then
      c_pert(i) = -1.d0
      e_2_pert(i) = selection_criterion*selection_criterion_factor*2.d0
    else if  (dabs(CI_electronic_energy(i) - h) > 1.d-6) then
        c_pert(i) = i_H_psi_array(i) / (CI_electronic_energy(i) - h)
        H_pert_diag(i) = c_pert(i) * (i_O1_psi_array(i) + i_O1_psi_array(i) ) 
!       e_2_pert(i) = -dabs(c_pert(i) * (i_O1_psi_array(i)+i_O1_psi_array(i) ) + )
        e_2_pert(i) = c_pert(i) * (i_O1_psi_array(i)+i_O1_psi_array(i) ) + c_pert(i) * c_pert(i) * oii
    else
      c_pert(i) = -1.d0
      e_2_pert(i) = -dabs(i_H_psi_array(i))
      H_pert_diag(i) = c_pert(i) * i_O1_psi_array(i)
    endif
  enddo


end

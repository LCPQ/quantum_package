subroutine pt2_h_core(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,N_st,minilist,idx_minilist,N_minilist)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint,ndet,N_st
  integer(bit_kind), intent(in)  :: det_pert(Nint,2)
  double precision , intent(out) :: c_pert(N_st),e_2_pert(N_st),H_pert_diag(N_st)
  double precision               :: i_H_psi_array(N_st)
  
  integer, intent(in)            :: N_minilist
  integer, intent(in)            :: idx_minilist(0:N_det_selectors)
  integer(bit_kind), intent(in)  :: minilist(Nint,2,N_det_selectors)
  
  BEGIN_DOC
  ! compute the standard Epstein-Nesbet perturbative first order coefficient and second order energetic contribution
  !
  ! for the various N_st states.
  !
  ! c_pert(i) = <psi(i)|H|det_pert>/( E(i) - <det_pert|H|det_pert> )
  !
  ! e_2_pert(i) = <psi(i)|H|det_pert>^2/( E(i) - <det_pert|H|det_pert> )
  !
  END_DOC
  
  integer                        :: i,j
  double precision               :: diag_H_mat_elem, h

  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)

 integer           :: exc(0:2,2,2)
 integer           :: degree
 double precision  :: phase
 call get_excitation(ref_bitmask,det_pert,exc,degree,phase,N_int)
 h = diag_H_mat_elem(det_pert,N_int)
 print*,'delta E = ',h-ref_bitmask_energy
 if(h<ref_bitmask_energy)then
  c_pert = 0.d0
  e_2_pert = 0.d0
  H_pert_diag = 0.d0
  return
 endif
 if(degree>1)then
  c_pert = 0.d0
  e_2_pert = 0.d0
  H_pert_diag = 0.d0
  return
 endif
 integer :: h1,p1,h2,p2,s1,s2
 call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
 c_pert = phase * mo_mono_elec_integral(h1,p1)
 e_2_pert = -dabs(mo_mono_elec_integral(h1,p1)+1.d0)
  
end


subroutine pt2_epstein_nesbet_SC2_projected(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,N_st)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint,ndet,N_st
  integer(bit_kind), intent(in)  :: det_pert(Nint,2)
  double precision , intent(out) :: c_pert(N_st),e_2_pert(N_st),H_pert_diag(N_st)
  double precision               :: i_H_psi_array(N_st)
  integer                        :: idx_repeat(0:ndet)
  
  BEGIN_DOC
  ! compute the Epstein-Nesbet perturbative first order coefficient and second order energetic contribution
  !
  ! for the various N_st states, 
  ! 
  ! but  with the correction in the denominator 
  ! 
  ! comming from the interaction of that determinant with all the others determinants 
  ! 
  ! that can be repeated by repeating all the double excitations
  !
  ! : you repeat all the correlation energy already taken into account in CI_electronic_energy(1)
  ! 
  ! that could be repeated to this determinant.
  !
  ! In addition, for the perturbative energetic contribution you have the standard second order
  !
  ! e_2_pert = <psi_i|H|det_pert>^2/(Delta_E)
  !
  ! and also the purely projected contribution 
  !
  ! H_pert_diag = <HF|H|det_pert> c_pert
  END_DOC
  
  integer                        :: i,j,degree,l
  double precision               :: diag_H_mat_elem,accu_e_corr,hij,h0j,h,delta_E
  double precision               :: repeat_all_e_corr,accu_e_corr_tmp,e_2_pert_fonda

  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)

  call i_H_psi_SC2(det_pert,psi_selectors,psi_selectors_coef,Nint,N_det_selectors,psi_selectors_size,N_st,i_H_psi_array,idx_repeat)
  accu_e_corr = 0.d0
  !$IVDEP
  do i = 1, idx_repeat(0)
   accu_e_corr = accu_e_corr + E_corr_per_selectors(idx_repeat(i))
  enddo
  h =  diag_H_mat_elem(det_pert,Nint) + accu_e_corr
  delta_E = 1.d0/(CI_SC2_electronic_energy(1) - h)


  c_pert(1) = i_H_psi_array(1) /(CI_SC2_electronic_energy(1) - h)
  e_2_pert(1) = i_H_psi_array(1) * c_pert(1)

  do i =2,N_st
    H_pert_diag(i) = h
    if  (dabs(CI_SC2_electronic_energy(i) - h) > 1.d-6) then
      c_pert(i) = i_H_psi_array(i) / (-dabs(CI_SC2_electronic_energy(i) - h))
      e_2_pert(i) = (c_pert(i) * i_H_psi_array(i))
    else
      c_pert(i) = i_H_psi_array(i)
      e_2_pert(i) = -dabs(i_H_psi_array(i))
    endif
  enddo

  degree = popcnt(xor( ref_bitmask(1,1), det_pert(1,1))) +                      &
           popcnt(xor( ref_bitmask(1,2), det_pert(1,2)))
  !DEC$ NOUNROLL
  do l=2,Nint
    degree = degree+ popcnt(xor( ref_bitmask(l,1), det_pert(l,1))) +            &
                     popcnt(xor( ref_bitmask(l,2), det_pert(l,2)))
  enddo
  if(degree==4)then
  ! <psi|delta_H|psi>
   e_2_pert_fonda = e_2_pert(1) 
   H_pert_diag(1) = e_2_pert(1) * c_pert(1) * c_pert(1)
   do i = 1, N_st
    do j = 1, idx_repeat(0)
     e_2_pert(i) += e_2_pert_fonda * psi_selectors_coef(idx_repeat(j),i) * psi_selectors_coef(idx_repeat(j),i)
    enddo
   enddo
  endif
  
end


subroutine pt2_epstein_nesbet_SC2_no_projected(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,N_st)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint,ndet,N_st
  integer(bit_kind), intent(in)  :: det_pert(Nint,2)
  double precision , intent(out) :: c_pert(N_st),e_2_pert(N_st),H_pert_diag(N_st)
  double precision               :: i_H_psi_array(N_st)
  integer                        :: idx_repeat(0:ndet)
  
  BEGIN_DOC
  ! compute the Epstein-Nesbet perturbative first order coefficient and second order energetic contribution
  !
  ! for the various N_st states, 
  ! 
  ! but  with the correction in the denominator 
  ! 
  ! comming from the interaction of that determinant with all the others determinants 
  ! 
  ! that can be repeated by repeating all the double excitations
  !
  ! : you repeat all the correlation energy already taken into account in CI_electronic_energy(1)
  ! 
  ! that could be repeated to this determinant.
  !
  ! In addition, for the perturbative energetic contribution you have the standard second order
  !
  ! e_2_pert = <psi_i|H|det_pert>^2/(Delta_E)
  !
  ! and also the purely projected contribution 
  !
  ! H_pert_diag = <HF|H|det_pert> c_pert
  END_DOC
  
  integer                        :: i,j,degree,l
  double precision               :: diag_H_mat_elem,accu_e_corr,hij,h0j,h,delta_E
  double precision               :: repeat_all_e_corr,accu_e_corr_tmp,e_2_pert_fonda

  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)

  call i_H_psi_SC2(det_pert,psi_selectors,psi_selectors_coef,Nint,N_det_selectors,psi_selectors_size,N_st,i_H_psi_array,idx_repeat)
  accu_e_corr = 0.d0
  !$IVDEP
  do i = 1, idx_repeat(0)
   accu_e_corr = accu_e_corr + E_corr_per_selectors(idx_repeat(i))
  enddo
  h =  diag_H_mat_elem(det_pert,Nint) + accu_e_corr
  delta_E = 1.d0/(CI_SC2_electronic_energy(1) - h)


  c_pert(1) = i_H_psi_array(1) /(CI_SC2_electronic_energy(1) - h)
  e_2_pert(1) = i_H_psi_array(1) * c_pert(1)

  do i =2,N_st
    H_pert_diag(i) = h
    if  (dabs(CI_SC2_electronic_energy(i) - h) > 1.d-6) then
      c_pert(i) = i_H_psi_array(i) / (-dabs(CI_SC2_electronic_energy(i) - h))
      e_2_pert(i) = (c_pert(i) * i_H_psi_array(i))
    else
      c_pert(i) = i_H_psi_array(i)
      e_2_pert(i) = -dabs(i_H_psi_array(i))
    endif
  enddo
end





double precision function repeat_all_e_corr(key_in)
 implicit none
 integer(bit_kind), intent(in)  :: key_in(N_int,2)
 integer :: i,degree
 double precision :: accu
 use bitmasks
 accu = 0.d0
 call get_excitation_degree(key_in,ref_bitmask,degree,N_int)
 if(degree==2)then
  do i = 1, N_det_selectors
   call get_excitation_degree(ref_bitmask,psi_selectors(1,1,i),degree,N_int)
   if(degree.ne.2)cycle
    call get_excitation_degree(key_in,psi_selectors(1,1,i),degree,N_int)
    if (degree<=3)cycle
    accu += E_corr_per_selectors(i)
  enddo
 elseif(degree==1)then
  do i = 1, N_det_selectors
   call get_excitation_degree(ref_bitmask,psi_selectors(1,1,i),degree,N_int)
   if(degree.ne.2)cycle
    call get_excitation_degree(key_in,psi_selectors(1,1,i),degree,N_int)
    if (degree<=2)cycle
    accu += E_corr_per_selectors(i)
  enddo
 endif
 repeat_all_e_corr = accu

end


subroutine pt2_epstein_nesbet_sc2(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,N_st)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint,ndet,N_st
  integer(bit_kind), intent(in)  :: det_pert(Nint,2)
  double precision , intent(out) :: c_pert(N_st),e_2_pert(N_st),H_pert_diag(N_st)
  double precision               :: i_H_psi_array(N_st)
  
  BEGIN_DOC
  ! compute the standard Epstein-Nesbet perturbative first order coefficient and second order energetic contribution
  !
  ! for the various N_st states, but with the CISD_SC2 energies and coefficients
  !
  ! c_pert(i) = <psi(i)|H|det_pert>/( E(i) - <det_pert|H|det_pert> )
  !
  ! e_2_pert(i) = <psi(i)|H|det_pert>^2/( E(i) - <det_pert|H|det_pert> )
  !
  END_DOC
  
  integer                        :: i,j
  double precision               :: diag_H_mat_elem, h
  PROVIDE  selection_criterion

  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)
  call i_H_psi(det_pert,psi_selectors,psi_selectors_coef,Nint,N_det_selectors,psi_selectors_size,N_st,i_H_psi_array)
  h = diag_H_mat_elem(det_pert,Nint)
  do i =1,N_st
    if(CI_SC2_electronic_energy(i)>h.and.CI_SC2_electronic_energy(i).ne.0.d0)then
      c_pert(i) = -1.d0
      e_2_pert(i) = selection_criterion*selection_criterion_factor*2.d0
    else if  (dabs(CI_SC2_electronic_energy(i) - h) > 1.d-6) then
        c_pert(i) = i_H_psi_array(i) / (CI_SC2_electronic_energy(i) - h)
        H_pert_diag(i) = h*c_pert(i)*c_pert(i)
        e_2_pert(i) = c_pert(i) * i_H_psi_array(i)
    else
      c_pert(i) = -1.d0
      e_2_pert(i) = -dabs(i_H_psi_array(i))
      H_pert_diag(i) = h
    endif
  enddo
  
end

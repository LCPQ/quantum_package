subroutine pt2_epstein_nesbet(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,N_st)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint,ndet,N_st
  integer(bit_kind), intent(in)  :: det_pert(Nint,2)
  double precision , intent(out) :: c_pert(N_st),e_2_pert(N_st),H_pert_diag(N_st)
  double precision               :: i_H_psi_array(N_st)
  
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
  call i_H_psi(det_pert,psi_selectors,psi_selectors_coef,Nint,N_det_selectors,psi_selectors_size,N_st,i_H_psi_array)
  h = diag_H_mat_elem(det_pert,Nint)
  do i =1,N_st
    H_pert_diag(i) = h
    if (dabs(CI_electronic_energy(i) - h) > 1.d-6) then
        c_pert(i) = i_H_psi_array(i) / (CI_electronic_energy(i) - h)
        e_2_pert(i) = c_pert(i) * i_H_psi_array(i)
    else
      c_pert(i) = 0.d0
      e_2_pert(i) = 0.d0
    endif
  enddo
  
end

subroutine pt2_epstein_nesbet_2x2(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,N_st)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint,ndet,N_st
  integer(bit_kind), intent(in)  :: det_pert(Nint,2)
  double precision , intent(out) :: c_pert(N_st),e_2_pert(N_st),H_pert_diag(N_st)
  double precision               :: i_H_psi_array(N_st)
  
  BEGIN_DOC
  ! compute the Epstein-Nesbet 2x2 diagonalization coefficient and energetic contribution
  !
  ! for the various N_st states.
  !
  ! e_2_pert(i) = 0.5 * (( <det_pert|H|det_pert> -  E(i) )  - sqrt( ( <det_pert|H|det_pert> -  E(i)) ^2 + 4 <psi(i)|H|det_pert>^2  )
  !
  ! c_pert(i) = e_2_pert(i)/ <psi(i)|H|det_pert>
  !
  END_DOC
  
  integer                        :: i,j
  double precision               :: diag_H_mat_elem,delta_e, h
  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)
  call i_H_psi(det_pert,psi_selectors,psi_selectors_coef,Nint,N_det_selectors,psi_selectors_size,N_st,i_H_psi_array)
  h = diag_H_mat_elem(det_pert,Nint)
  do i =1,N_st
    H_pert_diag(i) = h
    delta_e = h - CI_electronic_energy(i)
    e_2_pert(i) = 0.5d0 * (delta_e - dsqrt(delta_e * delta_e + 4.d0 * i_H_psi_array(i) * i_H_psi_array(i)))
    if (dabs(i_H_psi_array(i)) > 1.d-6) then
      c_pert(i) = e_2_pert(i)/i_H_psi_array(i)
    else
      c_pert(i) = 0.d0
    endif
  enddo

end

subroutine pt2_epstein_nesbet_SC2(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,N_st)
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
  ! <det_pert|H|det_pert> --->  <det_pert|H|det_pert> + delta_e_corr
  !
  ! c_pert(i) = <psi(i)|H|det_pert>/( E(i) - (<det_pert|H|det_pert> ) )
  !
  ! e_2_pert(i) = <psi(i)|H|det_pert>^2/( E(i) - (<det_pert|H|det_pert> ) )
  !
  END_DOC
  
  integer                        :: i,j
  double precision               :: diag_H_mat_elem,accu_e_corr,hij,h
  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)
  call i_H_psi_SC2(det_pert,psi_selectors,psi_selectors_coef,Nint,N_det_selectors,psi_selectors_size,N_st,i_H_psi_array,idx_repeat)
  accu_e_corr = 0.d0
  do i = 1, idx_repeat(0)
   call i_H_j(psi_selectors(1,1,idx_repeat(i)),det_pert,Nint,hij)
   accu_e_corr = accu_e_corr + hij * psi_selectors_coef(idx_repeat(i),1)
  enddo
  accu_e_corr = accu_e_corr / psi_selectors_coef(1,1)
  h = diag_H_mat_elem(det_pert,Nint) + accu_e_corr
  do i =1,N_st
    H_pert_diag(i) = h
    e_2_pert(i) = c_pert(i) * i_H_psi_array(i)
    if (dabs(CI_electronic_energy(i) - h) > 1.d-6) then
      c_pert(i) = i_H_psi_array(i) / (CI_electronic_energy(i) - h)
    else
      c_pert(i) = 0.d0
    endif
  enddo
  
end
subroutine pt2_epstein_nesbet_2x2_SC2(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,N_st)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint,ndet,N_st
  integer(bit_kind), intent(in)  :: det_pert(Nint,2)
  double precision , intent(out) :: c_pert(N_st),e_2_pert(N_st),H_pert_diag(N_st)
  double precision               :: i_H_psi_array(N_st)
  integer                        :: idx_repeat(0:ndet)

  BEGIN_DOC
  ! compute the Epstein-Nesbet 2x2 diagonalization coefficient and energetic contribution
  !
  ! for the various N_st states.
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
  ! <det_pert|H|det_pert> --->  <det_pert|H|det_pert> + delta_e_corr
  !
  ! e_2_pert(i) = 0.5 * (( <det_pert|H|det_pert> -  E(i) )  - sqrt( ( <det_pert|H|det_pert> -  E(i)) ^2 + 4 <psi(i)|H|det_pert>^2  )
  !
  ! c_pert(i) = e_2_pert(i)/ <psi(i)|H|det_pert>
  !
  END_DOC
  
  integer                        :: i,j
  double precision               :: diag_H_mat_elem,accu_e_corr,hij,delta_e,h
  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)
  call i_H_psi_SC2(det_pert,psi_selectors,psi_selectors_coef,Nint,N_det_selectors,psi_selectors_size,N_st,i_H_psi_array,idx_repeat)
  accu_e_corr = 0.d0
  do i = 1, idx_repeat(0)
   call i_H_j(psi_selectors(1,1,idx_repeat(i)),det_pert,Nint,hij)
   accu_e_corr = accu_e_corr + hij * psi_selectors_coef(idx_repeat(i),1)
  enddo
  accu_e_corr = accu_e_corr / psi_selectors_coef(1,1)
  h = diag_H_mat_elem(det_pert,Nint) + accu_e_corr
  do i =1,N_st
    H_pert_diag(i) = h
    delta_e = h - CI_electronic_energy(i)
    e_2_pert(i) = 0.5d0 * (delta_e - dsqrt(delta_e * delta_e + 4.d0 * i_H_psi_array(i) * i_H_psi_array(i)))
    if (dabs(i_H_psi_array(i)) > 1.d-6) then
      c_pert(i) = e_2_pert(i)/i_H_psi_array(i)
    else
      c_pert(i) = 0.d0
    endif
  enddo
  
end

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
  ! BUT on the contrary with ""pt2_epstein_nesbet_SC2"", you compute the energy by projection 
  !
  ! <det_pert|H|det_pert> --->  <det_pert|H|det_pert> + delta_e_corr
  !
  ! c_pert(1) = 1/c_HF <psi(i)|H|det_pert>/( E(i) - (<det_pert|H|det_pert> ) )
  !
  ! e_2_pert(1) = <HF|H|det_pert> c_pert(1)
  !
  ! NOTE :::: if you satisfy Brillouin Theorem, the singles don't contribute !!
  !
  END_DOC
  
  integer                        :: i,j
  double precision               :: diag_H_mat_elem,accu_e_corr,hij,h0j,h
  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)
  call i_H_psi_SC2(det_pert,psi_selectors,psi_selectors_coef,Nint,N_det_selectors,psi_selectors_size,N_st,i_H_psi_array,idx_repeat)
  accu_e_corr = 0.d0
  call i_H_j(ref_bitmask,det_pert,Nint,h0j)
  do i = 1, idx_repeat(0)
   call i_H_j(psi_selectors(1,1,idx_repeat(i)),det_pert,Nint,hij)
   accu_e_corr = accu_e_corr + hij * psi_selectors_coef(idx_repeat(i),1)
  enddo
  accu_e_corr = accu_e_corr / psi_selectors_coef(1,1)
  h = diag_H_mat_elem(det_pert,Nint) + accu_e_corr

  c_pert(1) = 1.d0/psi_selectors_coef(1,1) * i_H_psi_array(1) / (CI_electronic_energy(i) - h)
  e_2_pert(1) = c_pert(i) * h0j
  do i =2,N_st
    H_pert_diag(i) = h
    if (dabs(CI_electronic_energy(i) - h) > 1.d-6) then
      c_pert(i) = i_H_psi_array(i) / (CI_electronic_energy(i) - h)
    else
      c_pert(i) = 0.d0
    endif
    e_2_pert(i) = c_pert(i) * i_H_psi_array(i)
  enddo
  
end

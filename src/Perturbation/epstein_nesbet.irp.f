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
    print *,  H_pert_diag
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

subroutine pt2_epstein_nesbet_SC2(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,n_st)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint,ndet,n_st
  integer(bit_kind), intent(in)  :: det_pert(Nint,2)
  double precision , intent(out) :: c_pert(n_st),e_2_pert(n_st),H_pert_diag
  double precision               :: i_H_psi_array(N_st)
  integer                        :: idx_repeat(0:ndet)
  
  BEGIN_DOC
  ! compute the Epstein-Nesbet perturbative first order coefficient and second order energetic contribution
  !
  ! for the various n_st states, 
  ! 
  ! but  with the correction in the denominator 
  ! 
  ! comming from the interaction of that determinant with all the others determinants 
  ! 
  ! that can be repeated by repeating all the double excitations
  !
  ! : you repeat all the correlation energy already taken into account in reference_energy(1)
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
  double precision               :: diag_H_mat_elem,accu_e_corr,hij
  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)
  call i_H_psi_SC2(det_pert,psi_ref,psi_ref_coef,Nint,ndet,psi_ref_size,n_st,i_H_psi_array,idx_repeat)
  accu_e_corr = 0.d0
  do i = 1, idx_repeat(0)
   call i_H_j(psi_ref(1,1,idx_repeat(i)),det_pert,Nint,hij)
   accu_e_corr = accu_e_corr + hij * psi_ref_coef(idx_repeat(i),1)
  enddo
  accu_e_corr = accu_e_corr / psi_ref_coef(1,1)
  H_pert_diag = diag_H_mat_elem(det_pert,Nint) + accu_e_corr
  do i =1,n_st
    c_pert(i) = i_H_psi_array(i) / (reference_energy(i) - H_pert_diag)
    e_2_pert(i) = c_pert(i) * i_H_psi_array(i)
  enddo
  
end
subroutine pt2_epstein_nesbet_2x2_SC2(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,n_st)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint,ndet,n_st
  integer(bit_kind), intent(in)  :: det_pert(Nint,2)
  double precision , intent(out) :: c_pert(n_st),e_2_pert(n_st),H_pert_diag
  double precision               :: i_H_psi_array(N_st)
  integer                        :: idx_repeat(0:ndet)

  BEGIN_DOC
  ! compute the Epstein-Nesbet 2x2 diagonalization coefficient and energetic contribution
  !
  ! for the various n_st states.
  ! 
  ! but  with the correction in the denominator 
  ! 
  ! comming from the interaction of that determinant with all the others determinants 
  ! 
  ! that can be repeated by repeating all the double excitations
  !
  ! : you repeat all the correlation energy already taken into account in reference_energy(1)
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
  double precision               :: diag_H_mat_elem,accu_e_corr,hij,delta_e
  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)
  call i_H_psi_SC2(det_pert,psi_ref,psi_ref_coef,Nint,ndet,psi_ref_size,n_st,i_H_psi_array,idx_repeat)
  accu_e_corr = 0.d0
  do i = 1, idx_repeat(0)
   call i_H_j(psi_ref(1,1,idx_repeat(i)),det_pert,Nint,hij)
   accu_e_corr = accu_e_corr + hij * psi_ref_coef(idx_repeat(i),1)
  enddo
  accu_e_corr = accu_e_corr / psi_ref_coef(1,1)
  H_pert_diag = diag_H_mat_elem(det_pert,Nint) + accu_e_corr
  do i =1,n_st
    delta_e = H_pert_diag - reference_energy(i)
    e_2_pert(i) = 0.5d0 * (delta_e - dsqrt(delta_e * delta_e + 4.d0 * i_H_psi_array(i) * i_H_psi_array(i)))
    c_pert(i) = e_2_pert(i)/i_H_psi_array(i)
  enddo
  
end

subroutine pt2_epstein_nesbet_SC2_projected(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,n_st)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint,ndet,n_st
  integer(bit_kind), intent(in)  :: det_pert(Nint,2)
  double precision , intent(out) :: c_pert(n_st),e_2_pert(n_st),H_pert_diag
  double precision               :: i_H_psi_array(N_st)
  integer                        :: idx_repeat(0:ndet)
  
  BEGIN_DOC
  ! compute the Epstein-Nesbet perturbative first order coefficient and second order energetic contribution
  !
  ! for the various n_st states, 
  ! 
  ! but  with the correction in the denominator 
  ! 
  ! comming from the interaction of that determinant with all the others determinants 
  ! 
  ! that can be repeated by repeating all the double excitations
  !
  ! : you repeat all the correlation energy already taken into account in reference_energy(1)
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
  double precision               :: diag_H_mat_elem,accu_e_corr,hij,h0j
  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)
  call i_H_psi_SC2(det_pert,psi_ref,psi_ref_coef,Nint,ndet,psi_ref_size,n_st,i_H_psi_array,idx_repeat)
  accu_e_corr = 0.d0
  call i_H_j(ref_bitmask,det_pert,Nint,h0j)
  do i = 1, idx_repeat(0)
   call i_H_j(psi_ref(1,1,idx_repeat(i)),det_pert,Nint,hij)
   accu_e_corr = accu_e_corr + hij * psi_ref_coef(idx_repeat(i),1)
  enddo
  accu_e_corr = accu_e_corr / psi_ref_coef(1,1)
  H_pert_diag = diag_H_mat_elem(det_pert,Nint) + accu_e_corr

  c_pert(1) = 1.d0/psi_ref_coef(1,1) * i_H_psi_array(1) / (reference_energy(i) - H_pert_diag)
  e_2_pert(1) = c_pert(i) * h0j
  do i =2,n_st
    c_pert(i) = i_H_psi_array(i) / (reference_energy(i) - H_pert_diag)
    e_2_pert(i) = c_pert(i) * i_H_psi_array(i)
  enddo
  
end

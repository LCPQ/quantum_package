BEGIN_TEMPLATE

subroutine pt2_epstein_nesbet ($arguments)
  use bitmasks
  implicit none
  $declarations
  
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
  double precision               :: i_H_psi_array(N_st)
  PROVIDE  selection_criterion

  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)
  !call i_H_psi(det_pert,psi_selectors,psi_selectors_coef,Nint,N_det_selectors,psi_selectors_size,N_st,i_H_psi_array)
  call i_H_psi_minilist(det_pert,minilist,idx_minilist,N_minilist,psi_selectors_coef,Nint,N_minilist,psi_selectors_size,N_st,i_H_psi_array)
  
  
  h = diag_H_mat_elem(det_pert,Nint)
  do i =1,N_st
    if(CI_electronic_energy(i)>h.and.CI_electronic_energy(i).ne.0.d0)then
      c_pert(i) = -1.d0
      e_2_pert(i) = selection_criterion*selection_criterion_factor*2.d0
    else if  (dabs(CI_electronic_energy(i) - h) > 1.d-6) then
        c_pert(i) = i_H_psi_array(i) / (CI_electronic_energy(i) - h)
        H_pert_diag(i) = h*c_pert(i)*c_pert(i)
        e_2_pert(i) = c_pert(i) * i_H_psi_array(i)
    else
      c_pert(i) = -1.d0
      e_2_pert(i) = -dabs(i_H_psi_array(i))
      H_pert_diag(i) = h
    endif
  enddo
  
end

subroutine pt2_epstein_nesbet_2x2 ($arguments)
  use bitmasks
  implicit none
  $declarations
  
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
  double precision               :: i_H_psi_array(N_st)
  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)
  PROVIDE CI_electronic_energy

  !call i_H_psi(det_pert,psi_selectors,psi_selectors_coef,Nint,N_det_selectors,psi_selectors_size,N_st,i_H_psi_array)
  call i_H_psi_minilist(det_pert,minilist,idx_minilist,N_minilist,psi_selectors_coef,Nint,N_minilist,psi_selectors_size,N_st,i_H_psi_array)
  
  h = diag_H_mat_elem(det_pert,Nint)
  do i =1,N_st
    if (i_H_psi_array(i) /= 0.d0) then
      delta_e = h - CI_electronic_energy(i)
      if (delta_e > 0.d0) then
        e_2_pert(i) = 0.5d0 * (delta_e - dsqrt(delta_e * delta_e + 4.d0 * i_H_psi_array(i) * i_H_psi_array(i)))
      else
        e_2_pert(i) = 0.5d0 * (delta_e + dsqrt(delta_e * delta_e + 4.d0 * i_H_psi_array(i) * i_H_psi_array(i)))
      endif
      if (dabs(i_H_psi_array(i)) > 1.d-6) then
        c_pert(i) = e_2_pert(i)/i_H_psi_array(i)
      else
        c_pert(i) = 0.d0
      endif
      H_pert_diag(i) = h*c_pert(i)*c_pert(i)
    else
      e_2_pert(i) = 0.d0
      c_pert(i) = 0.d0
      H_pert_diag(i) = 0.d0
    endif
  enddo

end

subroutine pt2_moller_plesset ($arguments)
  use bitmasks
  implicit none
  $declarations
  
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
  double precision               :: i_H_psi_array(N_st)
  integer                        :: h1,h2,p1,p2,s1,s2
  ASSERT (Nint == N_int)
  ASSERT (Nint > 0)
  call get_excitation(ref_bitmask,det_pert,exc,degree,phase,Nint)
  if (degree == 2) then
    call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
    delta_e = Fock_matrix_diag_mo(h1) + Fock_matrix_diag_mo(h2) - &
            (Fock_matrix_diag_mo(p1) + Fock_matrix_diag_mo(p2))
    delta_e = 1.d0/delta_e
  else if (degree == 1) then
    call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
    delta_e = Fock_matrix_diag_mo(h1) - Fock_matrix_diag_mo(p1) 
    delta_e = 1.d0/delta_e
  else
    delta_e = 0.d0
  endif

  call i_H_psi(det_pert,psi_selectors,psi_selectors_coef,Nint,N_det,psi_selectors_size,n_st,i_H_psi_array)
  h = diag_H_mat_elem(det_pert,Nint)
  do i =1,n_st
    H_pert_diag(i) = h
    c_pert(i) = i_H_psi_array(i) *delta_e
    e_2_pert(i) = c_pert(i) * i_H_psi_array(i)
  enddo
  
end
SUBST [ arguments, declarations ]

det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,N_st,minilist,idx_minilist,N_minilist ;

    integer, intent(in)             :: Nint
    integer, intent(in)             :: ndet
    integer, intent(in)             :: N_st
    integer, intent(in)             :: N_minilist
    integer(bit_kind), intent(in)   :: det_pert(Nint,2)
    double precision , intent(out)  :: c_pert(N_st)
    double precision , intent(out)  :: e_2_pert(N_st)
    double precision, intent(out)   :: H_pert_diag(N_st)
    integer, intent(in)             :: idx_minilist(0:N_det_selectors)
    integer(bit_kind), intent(in)   :: minilist(Nint,2,N_det_selectors)
;;


END_TEMPLATE


subroutine huckel_guess
  implicit none
  BEGIN_DOC
! Build the MOs using the extended Huckel model
  END_DOC
  integer                        :: i,j
  double precision               :: accu
  double precision               :: c
  character*(64)                 :: label

  label = "Guess"
  call mo_as_eigvectors_of_mo_matrix(mo_mono_elec_integral,          &
                                     size(mo_mono_elec_integral,1),  &
                                     size(mo_mono_elec_integral,2),label,1,.false.)
  TOUCH mo_coef

  c = 0.5d0 * 1.75d0

  do j=1,ao_num
    do i=1,ao_num
      Fock_matrix_ao_alpha(i,j) = c*ao_overlap(i,j)*(ao_mono_elec_integral_diag(i) + &
                                                  ao_mono_elec_integral_diag(j))
      Fock_matrix_ao_beta (i,j) = Fock_matrix_ao_alpha(i,j)
    enddo
    Fock_matrix_ao_alpha(j,j) = ao_mono_elec_integral(j,j) + ao_bi_elec_integral_alpha(j,j)
    Fock_matrix_ao_beta (j,j) = Fock_matrix_ao_alpha(j,j)
  enddo
  TOUCH Fock_matrix_ao_alpha Fock_matrix_ao_beta
  mo_coef = eigenvectors_fock_matrix_mo
  SOFT_TOUCH mo_coef
  call save_mos
  print *,  'E=', HF_energy

end

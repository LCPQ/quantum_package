subroutine huckel_guess
  implicit none
  BEGIN_DOC
! Build the MOs using the extended Huckel model
  END_DOC
  integer                        :: i,j
  double precision               :: tmp_matrix(ao_num_align,ao_num),accu
  double precision               :: c
  character*(64)                 :: label

  mo_coef = ao_ortho_lowdin_coef
  TOUCH mo_coef
  label = "Guess"
  call mo_as_eigvectors_of_mo_matrix(mo_mono_elec_integral,          &
                                     size(mo_mono_elec_integral,1),  &
                                     size(mo_mono_elec_integral,2),label)
  TOUCH mo_coef

  c = 0.5d0 * 1.75d0

  do j=1,ao_num
    do i=1,ao_num
      if (i.ne.j) then
        Fock_matrix_ao(i,j) = c*ao_overlap(i,j)*(ao_mono_elec_integral(i,i) + &
                                                 ao_mono_elec_integral(j,j))
      else
        Fock_matrix_ao(i,j) = Fock_matrix_alpha_ao(i,j)
      endif
    enddo
  enddo
  TOUCH Fock_matrix_ao
  mo_coef = eigenvectors_fock_matrix_mo
  SOFT_TOUCH mo_coef
  call save_mos

end


program scf
  call create_guess
  call orthonormalize_mos
  call run
end

subroutine create_guess
  implicit none
  BEGIN_DOC
! Create an H_core guess if no MOs are present in the EZFIO directory
  END_DOC
  logical                        :: exists
  PROVIDE ezfio_filename
  call ezfio_has_mo_basis_mo_coef(exists)
  if (.not.exists) then
    mo_coef = ao_ortho_lowdin_coef
    TOUCH mo_coef
    mo_label = 'Guess'
    call mo_as_eigvectors_of_mo_matrix(mo_mono_elec_integral,size(mo_mono_elec_integral,1),size(mo_mono_elec_integral,2),mo_label)
    SOFT_TOUCH mo_coef mo_label
  endif
end


subroutine run

  use bitmasks
  implicit none
  BEGIN_DOC
! Run SCF calculation
  END_DOC
  double precision               :: SCF_energy_before,SCF_energy_after,diag_H_mat_elem,get_mo_bielec_integral
  double precision               :: E0
  integer                        :: i_it, i, j, k
   
  E0 = HF_energy 

  thresh_SCF = 1.d-10
  call damping_SCF
  mo_label = "Canonical"
  TOUCH mo_label mo_coef
  call save_mos
  
end

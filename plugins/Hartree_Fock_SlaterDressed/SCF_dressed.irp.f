program scf
  BEGIN_DOC
! Produce `Hartree_Fock` MO orbital with Slater cusp dressing
! output: mo_basis.mo_tot_num mo_basis.mo_label mo_basis.ao_md5 mo_basis.mo_coef mo_basis.mo_occ
! output: hartree_fock.energy
! optional: mo_basis.mo_coef
  END_DOC
  call check_mos
  call debug
  call run
end

subroutine check_mos
  implicit none
  BEGIN_DOC
!   Create a MO guess if no MOs are present in the EZFIO directory
  END_DOC
  logical                        :: exists
  PROVIDE ezfio_filename
  call ezfio_has_mo_basis_mo_coef(exists)
  if (.not.exists) then
    print *,  'Please run SCF first'
    stop
  endif
end

subroutine debug
  implicit none
  integer                        :: i
  print *,  'A'
  do i=1,nucl_num
    print *,  i, cusp_A(1:nucl_num, i)
  enddo
  print *,  'B'
  do i=1,mo_tot_num
    print *,  i, cusp_B(1:nucl_num, i)
  enddo
  print *,  'C'
  do i=1,mo_tot_num
    print *,  i, cusp_C(1:nucl_num, i)
  enddo
end

subroutine run

  BEGIN_DOC
!   Run SCF calculation
  END_DOC

  use bitmasks
  implicit none

  double precision               :: SCF_energy_before,SCF_energy_after,diag_H_mat_elem
  double precision               :: EHF
  integer                        :: i_it, i, j, k
   
  EHF = HF_energy 

  mo_label = "CuspDressed"

  call ezfio_set_Hartree_Fock_SlaterDressed_slater_coef_ezfio(cusp_B)
! Choose SCF algorithm


!  call Roothaan_Hall_SCF
  
end



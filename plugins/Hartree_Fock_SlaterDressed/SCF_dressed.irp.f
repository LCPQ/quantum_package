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
  integer                        :: i,j,k
  print *,  'A'
  do i=1,nucl_num
    print *,  i, cusp_A(1:nucl_num, i)
  enddo
  print *,  'B'
  do i=1,mo_tot_num
    print *,  i, cusp_B(1:nucl_num, i)
  enddo
  print *,  'X'
  do i=1,mo_tot_num
    print *,  i, cusp_C(1:nucl_num, i)
  enddo
  print *,  '-----'
  return
  do k=-100,100
    double precision :: x, y, z
    x = 0.01d0 * k
    y = 0.d0
    do i=1,ao_num
      z = 0.d0
      do j=1,ao_prim_num(i)
        z += ao_coef_normalized_ordered_transp(j,i) * dexp(-ao_expo_ordered_transp(j,i) * x**2)
      enddo
      y += mo_coef(i,1) * z
      y += exp(-slater_expo(1)*dabs(x)) * slater_coef(1,1)
      z = 0.d0
      do j=1,ao_prim_num(i)
        z += ao_coef_normalized_ordered_transp(j,i) * dexp(-ao_expo_ordered_transp(j,i) * x**2) 
      enddo
      y -= z * GauSlaOverlap_matrix(i,1)* slater_coef(1,1)
    enddo
    print *,  x, y
  enddo
  print *,  '-----'
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
   
  mo_label = 'None'


!  print *,  HF_energy

  do i=1,ao_num
    print *,  mo_coef(i,1), cusp_corrected_mos(i,1)
  enddo
  mo_coef(1:ao_num,1:mo_tot_num) = cusp_corrected_mos(1:ao_num,1:mo_tot_num)
  SOFT_TOUCH mo_coef slater_coef
  call ezfio_set_Hartree_Fock_SlaterDressed_slater_coef_ezfio(slater_coef)
  call ezfio_set_Hartree_Fock_SlaterDressed_projector(ao_ortho_canonical_coef(1:ao_num,1:ao_num))
  call ezfio_set_Hartree_Fock_SlaterDressed_ao_orthoSlaOverlap(AO_orthoSlaOverlap_matrix)
  call save_mos
  print *,  'ci'
  print *, mo_coef(1:ao_num,1)
  print *,  'cAi'
  print *, slater_coef


!  EHF = HF_energy 
!  print *,  HF_energy
!  call Roothaan_Hall_SCF
  
end



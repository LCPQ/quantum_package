BEGIN_PROVIDER [ integer, mo_tot_num ]
  implicit none
  BEGIN_DOC
  ! Total number of molecular orbitals and the size of the keys corresponding
  END_DOC
  PROVIDE ezfio_filename
  call ezfio_get_mo_basis_mo_tot_num(mo_tot_num)
  ASSERT (mo_tot_num > 0)
END_PROVIDER


BEGIN_PROVIDER [ integer, mo_tot_num_align ]
  implicit none
  BEGIN_DOC
  ! Aligned variable for dimensioning of arrays
  END_DOC
  integer                        :: align_double
  mo_tot_num_align = align_double(mo_tot_num)
END_PROVIDER


 BEGIN_PROVIDER [ double precision, mo_coef, (ao_num_align,mo_tot_num) ]
&BEGIN_PROVIDER [ character*(64), mo_label ]
  implicit none
  BEGIN_DOC
  ! Molecular orbital coefficients on AO basis set
  ! mo_coef(i,j) = coefficient of the ith ao on the jth mo
  ! mo_label : Label characterizing the MOS (local, canonical, natural, etc)
  END_DOC
  integer                        :: i, j
  double precision, allocatable  :: buffer(:,:)
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: buffer
  logical                        :: exists
  PROVIDE ezfio_filename
  
  !Label
  call ezfio_has_mo_basis_mo_label(exists)
  if (exists) then
    call ezfio_get_mo_basis_mo_label(mo_label)
  else
    mo_label = 'no_label'
  endif
  
  ! Coefs
  allocate(buffer(ao_num,mo_tot_num))
  buffer = 0.d0
  call ezfio_get_mo_basis_mo_coef(buffer)
  do i=1,mo_tot_num
    do j=1,ao_num
      mo_coef(j,i) = buffer(j,i)
    enddo
    do j=ao_num+1,ao_num_align
      mo_coef(j,i) = 0.d0
    enddo
  enddo
  deallocate(buffer)
  
END_PROVIDER

BEGIN_PROVIDER [ double precision, mo_coef_transp, (mo_tot_num_align,ao_num) ]
  implicit none
  BEGIN_DOC
  ! Molecular orbital coefficients on AO basis set
  END_DOC
  integer                        :: i, j
  
  do j=1,ao_num
    do i=1,mo_tot_num
      mo_coef_transp(i,j) = mo_coef(j,i)
    enddo
    do i=mo_tot_num+1,mo_tot_num_align
      mo_coef_transp(i,j) = 0.d0
    enddo
  enddo
  
END_PROVIDER

BEGIN_PROVIDER [ double precision, mo_occ, (mo_tot_num) ]
  implicit none
  BEGIN_DOC
  ! MO occupation numbers
  END_DOC
  PROVIDE ezfio_filename
  logical :: exists
  call ezfio_has_mo_basis_mo_occ(exists)
  if (exists) then
    call ezfio_get_mo_basis_mo_occ(mo_occ)
  else
    mo_occ = 0.d0
    integer :: i
    do i=1,elec_beta_num
      mo_occ(i) = 2.d0
    enddo
    do i=elec_beta_num+1,elec_alpha_num
      mo_occ(i) = 1.d0
    enddo
  endif
END_PROVIDER


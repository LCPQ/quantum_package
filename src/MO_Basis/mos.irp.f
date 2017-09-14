BEGIN_PROVIDER [ integer, mo_tot_num ]
  implicit none
  BEGIN_DOC
  ! Total number of molecular orbitals and the size of the keys corresponding
  END_DOC
  PROVIDE ezfio_filename
  logical                        :: exists
  call ezfio_has_mo_basis_mo_tot_num(exists)
  if (exists) then
    call ezfio_get_mo_basis_mo_tot_num(mo_tot_num)
  else
    mo_tot_num = ao_ortho_canonical_num
  endif
  call write_int(6,mo_tot_num,'mo_tot_num')
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
  implicit none
  BEGIN_DOC
  ! Molecular orbital coefficients on AO basis set
  ! mo_coef(i,j) = coefficient of the ith ao on the jth mo
  ! mo_label : Label characterizing the MOS (local, canonical, natural, etc)
  END_DOC
  integer                        :: i, j
  double precision, allocatable  :: buffer(:,:)
  logical                        :: exists
  PROVIDE ezfio_filename
  

  ! Coefs
  call ezfio_has_mo_basis_mo_coef(exists)
  if (exists) then
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
  else
    ! Orthonormalized AO basis
    do i=1,mo_tot_num
      do j=1,ao_num
        mo_coef(j,i) = ao_ortho_canonical_coef(j,i)
      enddo
      do j=ao_num+1,ao_num_align
        mo_coef(j,i) = 0.d0
      enddo
    enddo
  endif
END_PROVIDER

BEGIN_PROVIDER [ double precision, mo_coef_in_ao_ortho_basis, (ao_num_align, mo_tot_num) ]
 implicit none
 BEGIN_DOC
 ! MO coefficients in orthogonalized AO basis
 !
 ! C^(-1).C_mo
 END_DOC
 call dgemm('N','N',ao_num,mo_tot_num,ao_num,1.d0,                   &
     ao_ortho_canonical_coef_inv, size(ao_ortho_canonical_coef_inv,1),&
     mo_coef, size(mo_coef,1), 0.d0,                                 &
     mo_coef_in_ao_ortho_basis, size(mo_coef_in_ao_ortho_basis,1))

END_PROVIDER

BEGIN_PROVIDER [ character*(64), mo_label ]
  implicit none
  BEGIN_DOC
  ! Molecular orbital coefficients on AO basis set
  ! mo_coef(i,j) = coefficient of the ith ao on the jth mo
  ! mo_label : Label characterizing the MOS (local, canonical, natural, etc)
  END_DOC

  logical                        :: exists
  PROVIDE ezfio_filename
  call ezfio_has_mo_basis_mo_label(exists)
  if (exists) then
    call ezfio_get_mo_basis_mo_label(mo_label)
  else
    mo_label = 'no_label'
  endif
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

BEGIN_PROVIDER [ double precision, S_mo_coef, (ao_num_align, mo_tot_num) ]
 implicit none
 BEGIN_DOC
 ! Product S.C where S is the overlap matrix in the AO basis and C the mo_coef matrix.
 END_DOC

 call dgemm('N','N', ao_num, mo_tot_num, ao_num,                   &
     1.d0, ao_overlap,size(ao_overlap,1),      &
     mo_coef, size(mo_coef,1),                                     &
     0.d0, S_mo_coef, size(S_mo_coef,1))

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


subroutine ao_to_mo(A_ao,LDA_ao,A_mo,LDA_mo)
  implicit none
  BEGIN_DOC
  ! Transform A from the AO basis to the MO basis
  !
  ! Ct.A_ao.C
  END_DOC
  integer, intent(in)            :: LDA_ao,LDA_mo
  double precision, intent(in)   :: A_ao(LDA_ao,ao_num)
  double precision, intent(out)  :: A_mo(LDA_mo,mo_tot_num)
  double precision, allocatable  :: T(:,:)
  
  allocate ( T(ao_num_align,mo_tot_num) )
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: T
  
  call dgemm('N','N', ao_num, mo_tot_num, ao_num,                    &
      1.d0, A_ao,LDA_ao,                                             &
      mo_coef, size(mo_coef,1),                                      &
      0.d0, T, ao_num_align)
  
  call dgemm('T','N', mo_tot_num, mo_tot_num, ao_num,                &
      1.d0, mo_coef,size(mo_coef,1),                                 &
      T, ao_num_align,                                               &
      0.d0, A_mo, LDA_mo)
  
  deallocate(T)
end

subroutine mo_to_ao(A_mo,LDA_mo,A_ao,LDA_ao)
  implicit none
  BEGIN_DOC
  ! Transform A from the MO basis to the AO basis
  !
  ! (S.C).A_mo.(S.C)t
  END_DOC
  integer, intent(in)            :: LDA_ao,LDA_mo
  double precision, intent(in)   :: A_mo(LDA_mo,mo_tot_num)
  double precision, intent(out)  :: A_ao(LDA_ao,ao_num)
  double precision, allocatable  :: T(:,:)
  
  allocate ( T(mo_tot_num_align,ao_num) )
  
  call dgemm('N','T', mo_tot_num, ao_num, mo_tot_num,                &
      1.d0, A_mo,size(A_mo,1),                                       &
      S_mo_coef, size(S_mo_coef,1),                                  &
      0.d0, T, size(T,1))
  
  call dgemm('N','N', ao_num, ao_num, mo_tot_num,                    &
      1.d0, S_mo_coef, size(S_mo_coef,1),                            &
      T, size(T,1),                                                  &
      0.d0, A_ao, size(A_ao,1))
  
  deallocate(T)
end

subroutine mo_to_ao_no_overlap(A_mo,LDA_mo,A_ao,LDA_ao)
  implicit none
  BEGIN_DOC
  ! Transform A from the MO basis to the S^-1 AO basis
  ! Useful for density matrix
  END_DOC
  integer, intent(in)            :: LDA_ao,LDA_mo
  double precision, intent(in)   :: A_mo(LDA_mo,mo_tot_num)
  double precision, intent(out)  :: A_ao(LDA_ao,ao_num)
  double precision, allocatable  :: T(:,:)
  
  allocate ( T(mo_tot_num_align,ao_num) )
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: T
  
  call dgemm('N','T', mo_tot_num, ao_num, mo_tot_num,                &
      1.d0, A_mo,size(A_mo,1),                                       &
      mo_coef, size(mo_coef,1),                                      &
      0.d0, T, size(T,1))
  
  call dgemm('N','N', ao_num, ao_num, mo_tot_num,                    &
      1.d0, mo_coef,size(mo_coef,1),                                 &
      T, size(T,1),                                                  &
      0.d0, A_ao, size(A_ao,1))
  
  deallocate(T)
end

subroutine mix_mo_jk(j,k)
 implicit none
 integer, intent(in) :: j,k
 integer :: i,i_plus,i_minus
 BEGIN_DOC
! subroutine that rotates the jth MO with the kth MO
! to give two new MO's that are 
!         '+' = 1/sqrt(2) (|j> + |k>) 
!         '-' = 1/sqrt(2) (|j> - |k>)
! by convention, the '+' MO is in the lower index (min(j,k))
! by convention, the '-' MO is in the greater index (max(j,k))
 END_DOC
 double precision :: array_tmp(ao_num,2),dsqrt_2
 if(j==k)then
  print*,'You want to mix two orbitals that are the same !'
  print*,'It does not make sense ... '
  print*,'Stopping ...'
  stop
 endif
 array_tmp = 0.d0
 dsqrt_2 = 1.d0/dsqrt(2.d0)
 do i = 1, ao_num
  array_tmp(i,1) = dsqrt_2 * (mo_coef(i,j) + mo_coef(i,k))
  array_tmp(i,2) = dsqrt_2 * (mo_coef(i,j) - mo_coef(i,k))
 enddo
 i_plus = min(j,k)
 i_minus = max(j,k)
 do i = 1, ao_num
  mo_coef(i,i_plus) = array_tmp(i,1)
  mo_coef(i,i_minus) = array_tmp(i,2)
 enddo

end

subroutine ao_ortho_cano_to_ao(A_ao,LDA_ao,A,LDA)
  implicit none
  BEGIN_DOC
  ! Transform A from the AO basis to the orthogonal AO basis
  !
  ! C^(-1).A_ao.Ct^(-1)
  END_DOC
  integer, intent(in)            :: LDA_ao,LDA
  double precision, intent(in)   :: A_ao(LDA_ao,*)
  double precision, intent(out)  :: A(LDA,*)
  double precision, allocatable  :: T(:,:)
  
  allocate ( T(ao_num_align,ao_num) )
  
  call dgemm('T','N', ao_num, ao_num, ao_num,                        &
      1.d0,                                                          &
      ao_ortho_canonical_coef_inv, size(ao_ortho_canonical_coef_inv,1),&
      A_ao,size(A_ao,1),                                             &
      0.d0, T, size(T,1))
  
  call dgemm('N','N', ao_num, ao_num, ao_num, 1.d0,                  &
      T, size(T,1),                                                  &
      ao_ortho_canonical_coef_inv,size(ao_ortho_canonical_coef_inv,1),&
      0.d0, A, size(A,1))
  
  deallocate(T)
end


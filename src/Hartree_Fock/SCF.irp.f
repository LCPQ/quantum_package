BEGIN_PROVIDER [ integer, it_scf ]
  implicit none
  BEGIN_DOC
  ! Number of the current SCF iteration
  END_DOC
  it_scf = 0
END_PROVIDER

 BEGIN_PROVIDER [ double precision, SCF_density_matrices, (ao_num_align,ao_num,2,n_it_scf_max) ]
&BEGIN_PROVIDER [ double precision, SCF_energies, (n_it_scf_max) ]
  implicit none
  BEGIN_DOC
  ! Density matrices at every SCF iteration
  END_DOC
  SCF_density_matrices = 0.d0
  SCF_energies = 0.d0
END_PROVIDER

subroutine insert_new_SCF_density_matrix
  implicit none
  integer                        :: i,j
  do j=1,ao_num
    do i=1,ao_num
      SCF_density_matrices(i,j,1,it_scf) = HF_density_matrix_ao_alpha(i,j)
      SCF_density_matrices(i,j,2,it_scf) = HF_density_matrix_ao_beta(i,j)
    enddo
  enddo
  SCF_energies(it_scf) = HF_energy
end

subroutine Fock_mo_to_ao(FMO,LDFMO,FAO,LDFAO)
  implicit none
  integer, intent(in)            :: LDFMO ! size(FMO,1)
  integer, intent(in)            :: LDFAO ! size(FAO,1)
  double precision, intent(in)   :: FMO(LDFMO,*)
  double precision, intent(out)  :: FAO(LDFAO,*)
  
  double precision, allocatable  :: T(:,:), M(:,:)
  ! F_ao = S C F_mo C^t S
  allocate (T(mo_tot_num_align,mo_tot_num),M(ao_num_align,mo_tot_num))
  call dgemm('N','N', ao_num,ao_num,ao_num, 1.d0,                    &
      ao_overlap, size(ao_overlap,1),                                &
      mo_coef, size(mo_coef,1),                                      &
      0.d0,                                                          &
      M, size(M,1))
  call dgemm('N','N', ao_num,mo_tot_num,mo_tot_num, 1.d0,            &
      M, size(M,1),                                                  &
      FMO, size(FMO,1),                                              &
      0.d0,                                                          &
      T, size(T,1))
  call dgemm('N','T', mo_tot_num,ao_num,mo_tot_num, 1.d0,            &
      T, size(T,1),                                                  &
      mo_coef, size(mo_coef,1),                                      &
      0.d0,                                                          &
      M, size(M,1))
  call dgemm('N','N', ao_num,ao_num,ao_num, 1.d0,                    &
      M, size(M,1),                                                  &
      ao_overlap, size(ao_overlap,1),                                &
      0.d0,                                                          &
      FAO, size(FAO,1))
  deallocate(T,M)
end

subroutine SCF_interpolation_step
  implicit none
  integer                        :: i,j
  double precision               :: c
  
  if (it_scf == 1) then
    return
  endif
  call random_number(c)
  do j=1,ao_num
    do i=1,ao_num
      HF_density_matrix_ao_alpha(i,j) = c*SCF_density_matrices(i,j,1,it_scf)+SCF_density_matrices(i,j,1,it_scf-1) * (1.d0 - c)
      HF_density_matrix_ao_beta (i,j) = c*SCF_density_matrices(i,j,2,it_scf)+SCF_density_matrices(i,j,2,it_scf-1) * (1.d0 - c)
    enddo
  enddo
  TOUCH HF_density_matrix_ao_alpha HF_density_matrix_ao_beta
  
  ! call Fock_mo_to_ao(Fock_matrix_mo_alpha, size(Fock_matrix_mo_alpha,1),&
      !      Fock_matrix_alpha_ao, size(Fock_matrix_alpha_ao,1) )
  ! call Fock_mo_to_ao(Fock_matrix_mo_beta, size(Fock_matrix_mo_beta,1),&
      !      Fock_matrix_beta_ao, size(Fock_matrix_beta_ao,1) )
  ! SOFT_TOUCH Fock_matrix_alpha_ao Fock_matrix_beta_ao Fock_matrix_mo_alpha Fock_matrix_mo_beta
end

subroutine scf_iterations
  implicit none
  integer                        :: i,j
  do i=1,n_it_scf_max
    it_scf += 1
    SOFT_TOUCH it_scf
    mo_coef = eigenvectors_Fock_matrix_mo
    TOUCH mo_coef
    call insert_new_SCF_density_matrix
    print *,  HF_energy
    if (SCF_energies(it_scf)>SCF_energies(it_scf-1)) then
      call SCF_interpolation_step
    endif
    if (it_scf>1 ) then
      if (dabs(SCF_energies(it_scf)-SCF_energies(it_scf-1)) < thresh_SCF) then
        exit
      endif
    endif
  enddo
end

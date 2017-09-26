subroutine damping_SCF
  implicit none
  double precision               :: E
  double precision, allocatable  :: D_alpha(:,:), D_beta(:,:)
  double precision               :: E_new
  double precision, allocatable  :: D_new_alpha(:,:), D_new_beta(:,:), F_new(:,:)
  double precision, allocatable  :: delta_alpha(:,:), delta_beta(:,:)
  double precision               :: lambda, E_half, a, b, delta_D, delta_E, E_min
  
  integer                        :: i,j,k
  logical                        :: saving
  character                      :: save_char

  allocate(                                                          &
      D_alpha( ao_num, ao_num ),                               &
      D_beta( ao_num, ao_num ),                                &
      F_new( ao_num, ao_num ),                                 &
      D_new_alpha( ao_num, ao_num ),                           &
      D_new_beta( ao_num, ao_num ),                            &
      delta_alpha( ao_num, ao_num ),                           &
      delta_beta( ao_num, ao_num ))
  
  do j=1,ao_num
    do i=1,ao_num
      D_alpha(i,j) = HF_density_matrix_ao_alpha(i,j)
      D_beta (i,j) = HF_density_matrix_ao_beta (i,j)
    enddo
  enddo
  
  
  call write_time(output_hartree_fock)

  write(output_hartree_fock,'(A4,1X,A16, 1X, A16, 1X, A16, 1X, A4 )')  &
    '====','================','================','================', '===='
  write(output_hartree_fock,'(A4,1X,A16, 1X, A16, 1X, A16, 1X, A4 )')  &
    '  N ', 'Energy  ', 'Energy diff  ', 'Density diff  ', 'Save'
  write(output_hartree_fock,'(A4,1X,A16, 1X, A16, 1X, A16, 1X, A4 )')  &
    '====','================','================','================', '===='

  E = HF_energy + 1.d0
  E_min = HF_energy
  delta_D = 0.d0
  do k=1,n_it_scf_max
    
    delta_E = HF_energy - E
    E = HF_energy
    
    if ( (delta_E < 0.d0).and.(dabs(delta_E) < thresh_scf) ) then
      exit
    endif

    saving = E < E_min
    if (saving) then
      call save_mos
      save_char = 'X'
      E_min = E
    else
      save_char = ' '
    endif

    write(output_hartree_fock,'(I4,1X,F16.10, 1X, F16.10, 1X, F16.10, 3X, A )')  &
      k, E, delta_E, delta_D, save_char
    
    D_alpha = HF_density_matrix_ao_alpha
    D_beta  = HF_density_matrix_ao_beta 
    mo_coef = eigenvectors_fock_matrix_mo
    TOUCH mo_coef
    
    D_new_alpha = HF_density_matrix_ao_alpha
    D_new_beta  = HF_density_matrix_ao_beta 
    F_new = Fock_matrix_ao
    E_new = HF_energy

    delta_alpha = D_new_alpha - D_alpha
    delta_beta  = D_new_beta  - D_beta 
    
    lambda = .5d0
    E_half = 0.d0
    do while (E_half > E)
      HF_density_matrix_ao_alpha = D_alpha + lambda * delta_alpha
      HF_density_matrix_ao_beta  = D_beta  + lambda * delta_beta
      TOUCH HF_density_matrix_ao_alpha HF_density_matrix_ao_beta
      mo_coef = eigenvectors_fock_matrix_mo
      TOUCH mo_coef
      E_half = HF_energy
      if ((E_half > E).and.(E_new < E)) then
        lambda = 1.d0
        exit
      else if ((E_half > E).and.(lambda > 5.d-4)) then
        lambda = 0.5d0 * lambda
        E_new = E_half
      else
        exit
      endif
    enddo

    a = (E_new + E - 2.d0*E_half)*2.d0
    b = -E_new - 3.d0*E + 4.d0*E_half
    lambda = -lambda*b/(a+1.d-16)
    D_alpha = (1.d0-lambda) * D_alpha + lambda * D_new_alpha
    D_beta  = (1.d0-lambda) * D_beta  + lambda * D_new_beta 
    delta_E = HF_energy - E
    do j=1,ao_num
      do i=1,ao_num
        delta_D = delta_D + &
        (D_alpha(i,j) - HF_density_matrix_ao_alpha(i,j))*(D_alpha(i,j) - HF_density_matrix_ao_alpha(i,j)) + &
        (D_beta (i,j) - HF_density_matrix_ao_beta (i,j))*(D_beta (i,j) - HF_density_matrix_ao_beta (i,j))
      enddo
    enddo
    delta_D = dsqrt(delta_D/dble(ao_num)**2)
    HF_density_matrix_ao_alpha = D_alpha
    HF_density_matrix_ao_beta  = D_beta
    TOUCH HF_density_matrix_ao_alpha HF_density_matrix_ao_beta
    mo_coef = eigenvectors_fock_matrix_mo
    TOUCH mo_coef

  enddo
  write(output_hartree_fock,'(A4,1X,A16, 1X, A16, 1X, A16, 1X, A4 )')  '====','================','================','================', '===='
  write(output_hartree_fock,*)
  
  if(.not.no_oa_or_av_opt)then
   call mo_as_eigvectors_of_mo_matrix(Fock_matrix_mo,size(Fock_matrix_mo,1),size(Fock_matrix_mo,2),mo_label,1,.true.)
  endif

  call write_double(output_hartree_fock, E_min, 'Hartree-Fock energy')
  call ezfio_set_hartree_fock_energy(E_min)

  call write_time(output_hartree_fock)

  deallocate(D_alpha,D_beta,F_new,D_new_alpha,D_new_beta,delta_alpha,delta_beta)
end

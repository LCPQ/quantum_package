program cis
  implicit none
  integer                        :: i
  
  call super_CI
  
end

subroutine super_CI
  implicit none
  double precision               :: E, delta_E, delta_D, E_min
  integer                        :: k
  character                      :: save_char

  call write_time(6)
  write(6,'(A4,X,A16, X, A16, X, A16 )')           &
      '====','================','================','================'
  write(6,'(A4,X,A16, X, A16, X, A16 )')           &
      '  N ', 'Energy  ', 'Energy diff  ', 'Save  '
  write(6,'(A4,X,A16, X, A16, X, A16 )')           &
      '====','================','================','================'
  
  E = HF_energy + 1.d0
  delta_D = 0.d0
  E_min = HF_energy
  FREE psi_det psi_coef
  call clear_mo_map
  N_det = 1
  SOFT_TOUCH N_det
  mo_coef = eigenvectors_fock_matrix_mo
  TOUCH mo_coef
  do k=1,n_it_scf_max
    delta_E = HF_energy - E
    E = HF_energy
    if (E < E_min) then
      call save_mos
      save_char = 'X'
    else
      save_char = ' '
    endif
    E_min = min(E,E_min)
    write(6,'(I4,X,F16.10, X, F16.10, X, A8 )') &
        k, E, delta_E, save_char
    if ( (delta_E < 0.d0).and.(dabs(delta_E) < thresh_scf) ) then
      exit
    endif
    call H_apply_cis
    call diagonalize_CI
    call set_natural_mos
    FREE psi_det psi_coef
    call clear_mo_map
    N_det = 1
    SOFT_TOUCH N_det
    mo_coef = eigenvectors_fock_matrix_mo
    TOUCH mo_coef
  enddo
  
  write(6,'(A4,X,A16, X, A16, X, A16 )')           &
      '====','================','================','================'
  call write_time(6)
end
    

program dressed_dmc
  implicit none
  double precision               :: E0, hij
  double precision, allocatable  :: H_jj(:), energies(:), delta_jj(:), cj(:), hj(:)
  integer                        :: i
  double precision, external     :: diag_h_mat_elem

  if (.not.read_wf) then
    stop 'read_wf should be true'
  endif

  PROVIDE mo_bielec_integrals_in_map
  allocate ( H_jj(N_det), delta_jj(N_det), hj(N_det), cj(N_det), energies(N_states) )

  ! Read <i|\Phi_0>
  ! -=-=-=-==-=-=-=

  character*(32) :: w, w2
  integer :: k
  do while (.True.)
    read(*,*) w
    if ( trim(w) == 'Ci_h_psidet' ) then
      exit
    endif
  enddo
  do i=1,N_det
    read(*,*) k, w, hj(i)
  enddo

  do while (.True.)
    read(*,*) w
    if ( trim(w) == 'Ci_overlap_psidet' ) then
      exit
    endif
  enddo
  do i=1,N_det
    read(*,*) k, w, cj(i)
  enddo

  read(*,*)
  read(*,*) w, w2, E0
  print *,  'E0=', E0
  print *,  'Ndet = ', N_det

  ! Compute delta_ii
  ! -=-=-=-==-=-=-=-

  do i=1,N_det
   call i_H_psi(psi_det(1,1,i),psi_det,cj,N_int,N_det,size(psi_coef,1),N_states,energies)
   if (dabs(cj(i)) < 1.d-6) then
      delta_jj(i) = 0.d0
   else
      delta_jj(i) = (hj(i) - energies(1))/cj(i)
   endif
   H_jj(i) = diag_h_mat_elem(psi_det(1,1,i),N_int) + delta_jj(i)
   print *,  'Delta_jj(',i,') = ', Delta_jj(i), H_jj(i)
  enddo


  call davidson_diag_hjj(psi_det,psi_coef,H_jj,energies,size(psi_coef,1),N_det,N_states,N_states_diag,N_int,6)

  call save_wavefunction
  call write_spindeterminants

  E0 = 0.d0
  do i=1,N_det
   call i_H_psi(psi_det(1,1,i),psi_det,psi_coef(1,1),N_int,N_det,size(psi_coef,1),N_states,energies)
   E0 += psi_coef(i,1) * energies(1)
  enddo
  print *,  'Trial energy: ', E0 + nuclear_repulsion

  deallocate (H_jj, delta_jj, energies, cj)
end

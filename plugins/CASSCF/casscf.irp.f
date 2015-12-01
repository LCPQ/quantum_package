program casscf
  implicit none
  BEGIN_DOC
! Optimize MOs and CI coefficients of the CAS
  END_DOC
  
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer(bit_kind), allocatable  :: generators_bitmask_save(:,:,:,:) 

  integer                        :: degree, N_generators_bitmask_save, N_det_ci
  double precision               :: E_old, E_CI
  double precision               :: selection_criterion_save, selection_criterion_min_save
  
  integer                        :: N_det_old
  integer                        :: i, j, k, l
  integer                        :: i_bit, j_bit, i_int, j_int
  integer(bit_kind), allocatable :: bit_tmp(:), cas_bm(:)
  character*(64) :: label

  allocate( pt2(N_states), norm_pert(N_states),H_pert_diag(N_states) )
  allocate( generators_bitmask_save(N_int,2,6,N_generators_bitmask) )
  allocate( bit_tmp(N_int), cas_bm(N_int) )

  PROVIDE N_det_cas
  N_det_old = 0
  pt2 = 1.d0
  E_CI = 1.d0
  E_old = 0.d0
  diag_algorithm = "Lapack"
  selection_criterion_save = selection_criterion
  selection_criterion_min_save = selection_criterion_min


  cas_bm = 0_bit_kind
  do i=1,N_cas_bitmask
    do j=1,N_int
      cas_bm(j) = ior(cas_bm(j), cas_bitmask(j,1,i))
      cas_bm(j) = ior(cas_bm(j), cas_bitmask(j,2,i))
    enddo
  enddo

  ! Save CAS-SD bitmask
  generators_bitmask_save = generators_bitmask
  N_generators_bitmask_save = N_generators_bitmask

  ! Set the CAS bitmask 
  do i=1,6
    generators_bitmask(:,:,i,:) = cas_bitmask
  enddo
  N_generators_bitmask = N_cas_bitmask
  SOFT_TOUCH generators_bitmask N_generators_bitmask


  ! If the number of dets already in the file is larger than the requested
  ! number of determinants, truncate the wf
  if (N_det > N_det_max) then
    call diagonalize_CI
    call save_wavefunction
    psi_det = psi_det_sorted
    psi_coef = psi_coef_sorted
    N_det = N_det_max
    soft_touch N_det psi_det psi_coef
    call diagonalize_CI
    call save_wavefunction
    print *,  'N_det    = ', N_det
    print *,  'N_states = ', N_states
    print *,  'PT2      = ', pt2
    print *,  'E        = ', CI_energy
    print *,  'E+PT2    = ', CI_energy+pt2
    print *,  '-----'
  endif

  ! Start MCSCF iteration

  ! CAS-CI
  ! ------

  E_old = E_CI

  ! Reset the selection criterion
  selection_criterion     = selection_criterion_save
  selection_criterion_min = selection_criterion_min_save
  SOFT_TOUCH selection_criterion_min selection_criterion selection_criterion_factor

  ! Set the CAS bitmask 
  do i=1,6
    generators_bitmask(:,:,i,:) = cas_bitmask
  enddo
  N_generators_bitmask = N_cas_bitmask
  SOFT_TOUCH generators_bitmask N_generators_bitmask

  do while (N_det < N_det_max.and.maxval(abs(pt2(1:N_states))) > pt2_max)
    N_det_old = N_det
    call H_apply_CAS_SD_selected_no_skip(pt2, norm_pert, H_pert_diag,  N_states)

    PROVIDE  psi_coef
    PROVIDE  psi_det
    PROVIDE  psi_det_sorted

    if (N_det > N_det_max) then
      psi_det = psi_det_sorted
      psi_coef = psi_coef_sorted
      N_det = N_det_max
      soft_touch N_det psi_det psi_coef
    endif
    call diagonalize_CI
    call save_wavefunction
    print *,  '======'
    print *,  'CAS-CI'
    print *,  '======'
    print *,  ''
    print *,  'N_det      = ', N_det
    print *,  'N_states   = ', N_states
    print *,  'PT2        = ', pt2
    print *,  'E(CAS)     = ', CI_energy
    print *,  'E(CAS)+PT2 = ', CI_energy+pt2
    print *,  '-----'
    print *,  ''
    E_CI = sum(CI_energy(1:N_states)+pt2(1:N_states))/dble(N_states)

    call ezfio_set_casscf_energy(CI_energy(1))
    if (abort_all) then
      exit
    endif
    if (N_det == N_det_old) then
      exit
    endif

  enddo

  ! Super-CI
  ! --------

  selection_criterion_min = 1.d-12
  selection_criterion     = 1.d-12

  ! Set the CAS bitmask
  generators_bitmask = generators_bitmask_save
  N_generators_bitmask = N_generators_bitmask_save
  SOFT_TOUCH generators_bitmask N_generators_bitmask selection_criterion selection_criterion_min selection_criterion_factor

  N_det_ci = N_det

  call H_apply_CAS_SD_selected(pt2, norm_pert, H_pert_diag,  N_states)
  

  do i=1,mo_tot_num
   i_int = ishft(i-1,-bit_kind_shift)+1
   i_bit = j-ishft(i_int-1,bit_kind_shift)-1
   bit_tmp(:) = 0_bit_kind
   bit_tmp(i_int) = ibset(0_bit_kind,i_bit)
   if (iand(bit_tmp(i_int), cas_bm(i_int)) == 0_bit_kind) then
     ! Not a CAS MO
     cycle
   endif

   do j=1,mo_tot_num
     if (j == i) then
       cycle
     endif
     j_int = ishft(j-1,-bit_kind_shift)+1
     j_bit = j-ishft(j_int-1,bit_kind_shift)-1
     bit_tmp(:) = 0_bit_kind
     bit_tmp(j_int) = ibset(0_bit_kind,j_bit)
     if (iand(bit_tmp(j_int), cas_bm(j_int)) == 0_bit_kind) then
       ! Not a CAS MO
       cycle
     endif
     ! Now, both i and j are MOs of the CAS. De-couple them in the DM
     one_body_dm_mo(i,j) = 0.d0
   enddo

  enddo

  SOFT_TOUCH one_body_dm_mo

  double precision               :: mx, ov
  double precision, allocatable  :: mo_coef_old(:,:)
  integer, allocatable           :: iorder(:)
  logical, allocatable           :: selected(:)
  allocate( mo_coef_old(size(mo_coef,1), size(mo_coef,2)), iorder(mo_tot_num), selected(mo_tot_num) )
  mo_coef_old = mo_coef
  label = "Canonical"
  call mo_as_eigvectors_of_mo_matrix(one_body_dm_mo,size(one_body_dm_mo,1),size(one_body_dm_mo,2),label,-1)
  selected = .False.
  do j=1,mo_tot_num
   mx = -1.d0
   iorder(j) = j
   do i=1,mo_tot_num
    if (selected(i)) then
      cycle
    endif
    ov = 0.d0
    do l=1,ao_num
     do k=1,ao_num
      ov = ov + mo_coef_old(k,j) * ao_overlap(k,l) * mo_coef(l,i)
     enddo
    enddo
    ov= dabs(ov)
    if (ov > mx) then
      mx = ov
      iorder(j) = i
    endif
   enddo
   selected( iorder(j) ) = .True.
  enddo
  mo_coef_old = mo_coef
  do i=1,mo_tot_num
    mo_coef(:,i) = mo_coef_old(:,iorder(i))
  enddo

  call save_mos

  call write_double(6,E_CI,"Energy(CAS)")

  deallocate( mo_coef_old )
  deallocate( pt2, norm_pert,H_pert_diag )
  deallocate( generators_bitmask_save )
  deallocate( bit_tmp, cas_bm, iorder )
end

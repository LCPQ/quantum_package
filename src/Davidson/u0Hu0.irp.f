subroutine u_0_H_u_0(e_0,u_0,n,keys_tmp,Nint,N_st,sze_8)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes e_0 = <u_0|H|u_0>/<u_0|u_0>
  !
  ! n : number of determinants
  !
  END_DOC
  integer, intent(in)            :: n,Nint, N_st, sze_8
  double precision, intent(out)  :: e_0(N_st)
  double precision, intent(inout):: u_0(sze_8,N_st)
  integer(bit_kind),intent(in)   :: keys_tmp(Nint,2,n)
  
  double precision, allocatable  :: v_0(:,:), s_0(:,:)
  double precision               :: u_dot_u,u_dot_v,diag_H_mat_elem
  integer :: i,j
  allocate (v_0(sze_8,N_st),s_0(sze_8,N_st))
  call H_S2_u_0_nstates_openmp(v_0,s_0,u_0,N_st,sze_8)
  do i=1,N_st
    e_0(i) = u_dot_v(v_0(1,i),u_0(1,i),n)/u_dot_u(u_0(1,i),n)
  enddo
  deallocate (s_0, v_0)
end

BEGIN_PROVIDER [ double precision, psi_energy, (N_states) ]
  implicit none
  BEGIN_DOC
! Energy of the current wave function
  END_DOC
  call u_0_H_u_0(psi_energy,psi_coef,N_det,psi_det,N_int,N_states,psi_det_size)
END_PROVIDER



subroutine H_S2_u_0_nstates_openmp(v_0,s_0,u_0,N_st,sze_8)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes v_0 = H|u_0> and s_0 = S^2 |u_0>
  !
  ! Assumes that the determinants are in psi_det
  !
  ! istart, iend, ishift, istep are used in ZMQ parallelization.
  END_DOC
  integer, intent(in)            :: N_st,sze_8
  double precision, intent(inout)  :: v_0(sze_8,N_st), s_0(sze_8,N_st), u_0(sze_8,N_st)
  integer :: k
  double precision, allocatable  :: u_t(:,:)
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: u_t
  allocate(u_t(N_st,N_det))
  do k=1,N_st
    call dset_order(u_0(1,k),psi_bilinear_matrix_order,N_det)
  enddo
  v_0 = 0.d0
  s_0 = 0.d0
  call dtranspose(                                                   &
      u_0,                                                           &
      size(u_0, 1),                                                  &
      u_t,                                                           &
      size(u_t, 1),                                                  &
      N_det, N_st)

  call H_S2_u_0_nstates_openmp_work(v_0,s_0,u_t,N_st,sze_8,1,N_det,0,1)
  deallocate(u_t)

  do k=1,N_st
    call dset_order(v_0(1,k),psi_bilinear_matrix_order_reverse,N_det)
    call dset_order(s_0(1,k),psi_bilinear_matrix_order_reverse,N_det)
    call dset_order(u_0(1,k),psi_bilinear_matrix_order_reverse,N_det)
  enddo

end



subroutine H_S2_u_0_nstates_openmp_work(v_0,s_0,u_t,N_st,sze_8,istart,iend,ishift,istep)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes v_0 = H|u_0> and s_0 = S^2 |u_0>
  !
  ! Default should be 1,N_det,0,1
  END_DOC
  integer, intent(in)            :: N_st,sze_8,istart,iend,ishift,istep
  double precision, intent(in)   :: u_t(N_st,N_det)
  double precision, intent(out)  :: v_0(sze_8,N_st), s_0(sze_8,N_st) 

  
  PROVIDE ref_bitmask_energy

  double precision               :: hij, sij
  integer                        :: i,j,k,l
  integer                        :: k_a, k_b, l_a, l_b, m_a, m_b
  integer                        :: istate
  integer                        :: krow, kcol, krow_b, kcol_b
  integer                        :: lrow, lcol
  integer                        :: mrow, mcol
  integer(bit_kind)              :: spindet(N_int)
  integer(bit_kind)              :: tmp_det(N_int,2)
  integer(bit_kind)              :: tmp_det2(N_int,2)
  integer(bit_kind)              :: tmp_det3(N_int,2)
  integer(bit_kind), allocatable :: buffer(:,:)
  integer                        :: n_singles, n_doubles
  integer, allocatable           :: singles(:), doubles(:)
  integer, allocatable           :: singles_a(:)
  integer, allocatable           :: idx(:), idx0(:)
  logical, allocatable           :: is_single_a(:)
  integer                        :: maxab, n_singles_a, kcol_prev, nmax
  integer*8                      :: k8
  double precision, allocatable  :: v_t(:,:), s_t(:,:)
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: v_t, s_t

  maxab = max(N_det_alpha_unique, N_det_beta_unique)+1
  allocate(idx0(maxab))
  
  do i=1,maxab
    idx0(i) = i
  enddo

  ! Prepare the array of all alpha single excitations
  ! -------------------------------------------------

  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP   SHARED(psi_bilinear_matrix_rows, N_det,                &
      !$OMP          psi_bilinear_matrix_columns,                    &
      !$OMP          psi_det_alpha_unique, psi_det_beta_unique,      &
      !$OMP          n_det_alpha_unique, n_det_beta_unique, N_int,   &
      !$OMP          psi_bilinear_matrix_transp_rows,                &
      !$OMP          psi_bilinear_matrix_transp_columns,             &
      !$OMP          psi_bilinear_matrix_transp_order, N_st,         &
      !$OMP          psi_bilinear_matrix_order_transp_reverse,       &
      !$OMP          singles_alpha_csc, singles_alpha_csc_idx,       &
      !$OMP          psi_bilinear_matrix_columns_loc,                &
      !$OMP          singles_alpha_size, sze_8, istart, iend, istep, &
      !$OMP          ishift, idx0, u_t, maxab, v_0, s_0)             &
      !$OMP   PRIVATE(krow, kcol, tmp_det, spindet, k_a, k_b, i,     &
      !$OMP          lcol, lrow, is_single_a,l_a, l_b, nmax,         &
      !$OMP          buffer, singles, doubles, n_singles, n_doubles, &
      !$OMP          tmp_det2, hij, sij, idx, l, kcol_prev, v_t,     &
      !$OMP          singles_a, n_singles_a, s_t, k8)
  
  ! Alpha/Beta double excitations
  ! =============================
    
  allocate( buffer(N_int,maxab),                                     &
      singles(maxab),                                                &
      singles_a(maxab),                                              &
      doubles(maxab),                                                &
      idx(maxab),                                                    &
      v_t(N_st,N_det), s_t(N_st,N_det),                              &
      is_single_a(N_det_alpha_unique))
  is_single_a = .False.
  kcol_prev=-1

  v_t = 0.d0
  s_t = 0.d0


  !$OMP DO SCHEDULE(static,1)
  do k_a=istart+ishift,iend,istep

    krow = psi_bilinear_matrix_rows(k_a)
    kcol = psi_bilinear_matrix_columns(k_a)
    
    tmp_det(1:N_int,1) = psi_det_alpha_unique(1:N_int, krow)
    tmp_det(1:N_int,2) = psi_det_beta_unique (1:N_int, kcol)
    
    do k8=singles_alpha_csc_idx(krow), singles_alpha_csc_idx(krow+1)-1
      is_single_a( singles_alpha_csc(k8) ) = .True.
    enddo

    if (kcol /= kcol_prev) then
      call get_all_spin_singles(                                   &
          psi_det_beta_unique, idx0, tmp_det(1,2), N_int, N_det_beta_unique,&
          singles_a, n_singles_a)
    endif
    kcol_prev = kcol

    ! Loop over singly excited beta columns
    ! -------------------------------------

    do i=1,n_singles_a
      lcol = singles_a(i)
      if (lcol <= kcol) cycle

      tmp_det2(1:N_int,2) = psi_det_beta_unique(1:N_int, lcol)

      l_a = psi_bilinear_matrix_columns_loc(lcol)

      ! Loop over alpha singles
      ! -----------------------

      do while ( l_a < psi_bilinear_matrix_columns_loc(lcol+1) )
        do l=l_a,psi_bilinear_matrix_columns_loc(lcol+1)-1
          lrow = psi_bilinear_matrix_rows(l)
          if (is_single_a(lrow)) exit
        enddo
        if (l >= psi_bilinear_matrix_columns_loc(lcol+1)) exit
        l_a = l
        tmp_det2(1:N_int,1) = psi_det_alpha_unique(1:N_int, lrow)
        call i_H_j_double_alpha_beta(tmp_det,tmp_det2,N_int,hij)
        call get_s2(tmp_det,tmp_det2,N_int,sij)
        do l=1,N_st
          v_t(l,k_a) = v_t(l,k_a) + hij * u_t(l,l_a)
          v_t(l,l_a) = v_t(l,l_a) + hij * u_t(l,k_a)
          s_t(l,k_a) = s_t(l,k_a) + sij * u_t(l,l_a)
          s_t(l,l_a) = s_t(l,l_a) + sij * u_t(l,k_a)
        enddo
        l_a = l_a+1
      enddo
    enddo
    do k8=singles_alpha_csc_idx(krow), singles_alpha_csc_idx(krow+1)-1
      is_single_a( singles_alpha_csc(k8) ) = .False.
    enddo

  enddo

  !$OMP DO SCHEDULE(static,1)
  do k_a=istart+ishift,iend,istep


    ! Single and double alpha excitations
    ! ===================================
    
    
    ! Initial determinant is at k_a in alpha-major representation
    ! -----------------------------------------------------------------------
    
    krow = psi_bilinear_matrix_rows(k_a)
    kcol = psi_bilinear_matrix_columns(k_a)
    
    tmp_det(1:N_int,1) = psi_det_alpha_unique(1:N_int, krow)
    tmp_det(1:N_int,2) = psi_det_beta_unique (1:N_int, kcol)
    
    ! Initial determinant is at k_b in beta-major representation
    ! ----------------------------------------------------------------------
    
    k_b = psi_bilinear_matrix_order_transp_reverse(k_a)

    spindet(1:N_int) = tmp_det(1:N_int,1)
    
    ! Loop inside the beta column to gather all the connected alphas
    l_a = k_a+1
    nmax = min(N_det_alpha_unique, N_det - l_a)
    do i=1,nmax
      lcol = psi_bilinear_matrix_columns(l_a)
      if (lcol /= kcol) exit
      lrow = psi_bilinear_matrix_rows(l_a)
      buffer(1:N_int,i) = psi_det_alpha_unique(1:N_int, lrow)
      idx(i) = l_a
      l_a = l_a+1
    enddo
    i = i-1
    
    call get_all_spin_singles_and_doubles(                           &
        buffer, idx, spindet, N_int, i,                              &
        singles, doubles, n_singles, n_doubles )

    ! Compute Hij for all alpha singles
    ! ----------------------------------

    tmp_det2(1:N_int,2) = psi_det_beta_unique (1:N_int, kcol)
    do i=1,n_singles
      l_a = singles(i)
      lrow = psi_bilinear_matrix_rows(l_a)
      tmp_det2(1:N_int,1) = psi_det_alpha_unique(1:N_int, lrow)
      call i_H_j_mono_spin( tmp_det, tmp_det2, N_int, 1, hij)
      do l=1,N_st
        v_t(l,l_a) = v_t(l,l_a) + hij * u_t(l,k_a)
        v_t(l,k_a) = v_t(l,k_a) + hij * u_t(l,l_a)
        ! single => sij = 0 
      enddo
    enddo

    
    ! Compute Hij for all alpha doubles
    ! ----------------------------------
    
    do i=1,n_doubles
      l_a = doubles(i)
      lrow = psi_bilinear_matrix_rows(l_a)
      call i_H_j_double_spin( tmp_det(1,1), psi_det_alpha_unique(1, lrow), N_int, hij)
      do l=1,N_st
        v_t(l,l_a) = v_t(l,l_a) + hij * u_t(l,k_a)
        v_t(l,k_a) = v_t(l,k_a) + hij * u_t(l,l_a)
        ! same spin => sij = 0
      enddo
    enddo
    


    ! Single and double beta excitations
    ! ==================================

    
    ! Initial determinant is at k_a in alpha-major representation
    ! -----------------------------------------------------------------------
    
    krow = psi_bilinear_matrix_rows(k_a)
    kcol = psi_bilinear_matrix_columns(k_a)
    
    tmp_det(1:N_int,1) = psi_det_alpha_unique(1:N_int, krow)
    tmp_det(1:N_int,2) = psi_det_beta_unique (1:N_int, kcol)
    
    spindet(1:N_int) = tmp_det(1:N_int,2)
    
    ! Initial determinant is at k_b in beta-major representation
    ! -----------------------------------------------------------------------

    k_b = psi_bilinear_matrix_order_transp_reverse(k_a) 
    
    ! Loop inside the alpha row to gather all the connected betas
    l_b = k_b+1
    nmax = min(N_det_beta_unique, N_det - l_b)
    do i=1,nmax
      lrow = psi_bilinear_matrix_transp_rows(l_b)
      if (lrow /= krow) exit
      lcol = psi_bilinear_matrix_transp_columns(l_b)
      buffer(1:N_int,i) = psi_det_beta_unique(1:N_int, lcol)
      idx(i) = l_b
      l_b = l_b+1
    enddo
    i = i-1
  
    call get_all_spin_singles_and_doubles(                           &
        buffer, idx, spindet, N_int, i,                              &
        singles, doubles, n_singles, n_doubles )
    
    ! Compute Hij for all beta singles
    ! ----------------------------------
    
    tmp_det2(1:N_int,1) = psi_det_alpha_unique(1:N_int, krow)
    do i=1,n_singles
      l_b = singles(i)
      lcol = psi_bilinear_matrix_transp_columns(l_b)
      tmp_det2(1:N_int,2) = psi_det_beta_unique (1:N_int, lcol)
      call i_H_j_mono_spin( tmp_det, tmp_det2, N_int, 2, hij)
      l_a = psi_bilinear_matrix_transp_order(l_b)
      do l=1,N_st
        v_t(l,l_a) = v_t(l,l_a) + hij * u_t(l,k_a)
        v_t(l,k_a) = v_t(l,k_a) + hij * u_t(l,l_a)
        ! single => sij = 0 
      enddo
    enddo
    
    ! Compute Hij for all beta doubles
    ! ----------------------------------
    
    do i=1,n_doubles
      l_b = doubles(i)
      lcol = psi_bilinear_matrix_transp_columns(l_b)
      call i_H_j_double_spin( tmp_det(1,2), psi_det_beta_unique(1, lcol), N_int, hij)
      l_a = psi_bilinear_matrix_transp_order(l_b)
      do l=1,N_st
        v_t(l,l_a) = v_t(l,l_a) + hij * u_t(l,k_a)
        v_t(l,k_a) = v_t(l,k_a) + hij * u_t(l,l_a)
        ! same spin => sij = 0 
      enddo
    enddo


    ! Diagonal contribution
    ! =====================

    
    ! Initial determinant is at k_a in alpha-major representation
    ! -----------------------------------------------------------------------
    
    krow = psi_bilinear_matrix_rows(k_a)
    kcol = psi_bilinear_matrix_columns(k_a)
    
    tmp_det(1:N_int,1) = psi_det_alpha_unique(1:N_int, krow)
    tmp_det(1:N_int,2) = psi_det_beta_unique (1:N_int, kcol)
    
    double precision, external :: diag_H_mat_elem, diag_S_mat_elem
  
    hij = diag_H_mat_elem(tmp_det,N_int) 
    sij = diag_S_mat_elem(tmp_det,N_int)
    do l=1,N_st
      v_t(l,k_a) = v_t(l,k_a) + hij * u_t(l,k_a)
      s_t(l,k_a) = s_t(l,k_a) + sij * u_t(l,k_a)
    enddo

  end do
  !$OMP END DO NOWAIT

  !$OMP CRITICAL
  do l=1,N_st
    do i=1, N_det
      v_0(i,l) = v_0(i,l) + v_t(l,i)
      s_0(i,l) = s_0(i,l) + s_t(l,i)
    enddo
  enddo
  !$OMP END CRITICAL

  !$OMP BARRIER
  !$OMP END PARALLEL

end





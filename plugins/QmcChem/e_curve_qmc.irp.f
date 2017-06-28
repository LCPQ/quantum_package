program e_curve
  use bitmasks
  implicit none
  integer :: i,j,k, kk, nab, m, l
  double precision :: norm, E, hij, num, ci, cj
  integer, allocatable :: iorder(:)
  double precision , allocatable :: norm_sort(:)
  double precision :: e_0(N_states)
  PROVIDE mo_bielec_integrals_in_map

  nab = n_det_alpha_unique+n_det_beta_unique
  allocate ( norm_sort(0:nab), iorder(0:nab) )

  double precision, allocatable  :: u_t(:,:), v_t(:,:), s_t(:,:)
  double precision, allocatable  :: u_0(:,:), v_0(:,:)
  allocate(u_t(N_states,N_det),v_t(N_states,N_det),s_t(N_states,N_det))
  allocate(u_0(N_states,N_det),v_0(N_states,N_det))



  norm_sort(0) = 0.d0
  iorder(0) = 0
  do i=1,n_det_alpha_unique
   norm_sort(i) = det_alpha_norm(i)
   iorder(i) = i
  enddo
  
  do i=1,n_det_beta_unique
   norm_sort(i+n_det_alpha_unique) = det_beta_norm(i)
   iorder(i+n_det_alpha_unique) = -i
  enddo
  
  call dsort(norm_sort(1),iorder(1),nab)

  if (.not.read_wf) then
    stop 'Please set read_wf to true'
  endif

  PROVIDE psi_bilinear_matrix_values nuclear_repulsion 
  print *,  ''
  print *,  '=============================='
  print *,  'Energies at different cut-offs'
  print *,  '=============================='
  print *,  ''
  print *,  '=========================================================='
  print '(A8,2X,A8,2X,A12,2X,A10,2X,A12)',  'Thresh.', 'Ndet', 'Cost', 'Norm', 'E'
  print *,  '=========================================================='
  double precision :: thresh
  integer(bit_kind), allocatable :: det_i(:,:), det_j(:,:)
  thresh = 1.d-10
  do j=0,nab
    i = iorder(j)
    if (i<0) then
      do k=1,n_det
        if (psi_bilinear_matrix_columns(k) == -i) then
          psi_bilinear_matrix_values(k,1) = 0.d0
        endif
      enddo
    else
      do k=1,n_det
        if (psi_bilinear_matrix_rows(k) ==  i) then
          psi_bilinear_matrix_values(k,1) = 0.d0
        endif
      enddo
    endif
    if (thresh > norm_sort(j)) then
      cycle
    endif

    u_0 = psi_bilinear_matrix_values(1:N_det,1:N_states)
    v_t = 0.d0
    s_t = 0.d0
    call dtranspose(                                                   &
        u_0,                                                           &
        size(u_0, 1),                                                  &
        u_t,                                                           &
        size(u_t, 1),                                                  &
        N_det, N_states)
    call H_S2_u_0_nstates_openmp_work(v_t,s_t,u_t,N_states,N_det,1,N_det,0,1)
    call dtranspose(                                                   &
        v_t,                                                           &
        size(v_t, 1),                                                  &
        v_0,                                                           &
        size(v_0, 1),                                                  &
        N_states, N_det)
    
    double precision, external :: u_dot_u, u_dot_v
    do i=1,N_states
      e_0(i) = u_dot_v(v_t(1,i),u_0(1,i),N_det)/u_dot_u(u_0(1,i),N_det)
    enddo

    m = 0
    do k=1,n_det
     if (psi_bilinear_matrix_values(k,1) /= 0.d0) then
      m = m+1
     endif
    enddo

    if (m == 0) then
       exit
    endif
    E = E_0(1) + nuclear_repulsion
    norm = u_dot_u(u_0(1,1),N_det)
    print '(E9.1,2X,I8,2X,F10.2,2X,F10.8,2X,F12.6)',  thresh, m, &
      dble( elec_alpha_num**3 + elec_alpha_num**2 * (nab-1) ) / &
      dble( elec_alpha_num**3 + elec_alpha_num**2 * (j-1)), norm, E
    thresh = thresh * dsqrt(10.d0)
  enddo
  print *,  '=========================================================='

  deallocate (iorder, norm_sort)
end


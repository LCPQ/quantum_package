program e_curve
  use bitmasks
  implicit none
  integer :: i,j,k, kk, nab, m, l
  double precision :: norm, E, hij, num, ci, cj
  integer, allocatable :: iorder(:)
  double precision , allocatable :: norm_sort(:)
  nab = n_det_alpha_unique+n_det_beta_unique
  allocate ( norm_sort(0:nab), iorder(0:nab) )


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
    num = 0.d0
    norm = 0.d0
    m = 0
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(k,kk,l,det_i,det_j,ci,cj,hij) REDUCTION(+:norm,m,num)
    allocate( det_i(N_int,2), det_j(N_int,2))
    !$OMP DO SCHEDULE(guided)
    do k=1,n_det
      if (psi_bilinear_matrix_values(k,1) == 0.d0) then
        cycle
      endif
      ci = psi_bilinear_matrix_values(k,1)
      do kk=1,N_int
        det_i(kk,1) = psi_det_alpha_unique(kk,psi_bilinear_matrix_rows(k))
        det_i(kk,2) = psi_det_beta_unique(kk,psi_bilinear_matrix_columns(k))
      enddo
      do l=1,n_det
        if (psi_bilinear_matrix_values(l,1) == 0.d0) then
          cycle
        endif
        cj = psi_bilinear_matrix_values(l,1)
        do kk=1,N_int
          det_j(kk,1) = psi_det_alpha_unique(kk,psi_bilinear_matrix_rows(l))
          det_j(kk,2) = psi_det_beta_unique(kk,psi_bilinear_matrix_columns(l))
        enddo
        call i_h_j(det_i, det_j, N_int, hij)
        num = num + ci*cj*hij
      enddo
     norm = norm + ci*ci
     m = m+1
    enddo
    !$OMP END DO
    deallocate (det_i,det_j)
    !$OMP END PARALLEL
    if (m == 0) then
       exit
    endif
    E = num / norm + nuclear_repulsion
    print '(E9.1,2X,I8,2X,F10.2,2X,F10.8,2X,F12.6)',  thresh, m, &
      dble( elec_alpha_num**3 + elec_alpha_num**2 * (nab-1) ) / &
      dble( elec_alpha_num**3 + elec_alpha_num**2 * (j-1)), norm, E
    thresh = thresh * 2.d0
  enddo
  print *,  '=========================================================='

  deallocate (iorder, norm_sort)
end


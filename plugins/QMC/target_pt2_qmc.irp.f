program e_curve
  use bitmasks
  implicit none
  integer :: i,j,k, nab, m, l, n_up, n_dn, n
  double precision :: norm, E, hij, num, ci, cj
  integer, allocatable :: iorder(:)
  double precision , allocatable :: norm_sort(:), psi_bilinear_matrix_values_save(:)
  nab = n_det_alpha_unique+n_det_beta_unique

  allocate ( norm_sort(0:nab), iorder(0:nab), psi_bilinear_matrix_values_save(N_det) )


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

  psi_bilinear_matrix_values_save = psi_bilinear_matrix_values(:,1)
  print *,  '=========================================================='
  print '(A8,2X,A8,2X,A12,2X,A10,2X,A12)',  'Thresh.', 'Ndet', 'Cost', 'Norm', 'E'
  print *,  '=========================================================='
  integer(bit_kind), allocatable :: det_i(:,:), det_j(:,:)

  double precision :: thresh, E_min, E_max, E_prev
  thresh = 0.d0
  call compute_energy(psi_bilinear_matrix_values_save,E_max,m,norm)
  call i_h_j(psi_det_sorted(1,1,1), psi_det_sorted(1,1,1), N_int, E_min)
  print *,  E_min, E_max

  n_up = nab
  n_dn = 0
  do while (n_up > n_dn)
    n = n_dn + (n_up-n_dn)/2
    psi_bilinear_matrix_values_save = psi_bilinear_matrix_values(:,1)
    do j=1,n
      i = iorder(j)
      if (i<0) then
        do k=1,n_det
          if (psi_bilinear_matrix_columns(k) == -i) then
            psi_bilinear_matrix_values_save(k) = 0.d0
          endif
        enddo
      else
        do k=1,n_det
          if (psi_bilinear_matrix_rows(k) == i) then
            psi_bilinear_matrix_values_save(k) = 0.d0
          endif
        enddo
      endif
    enddo
    call compute_energy(psi_bilinear_matrix_values_save,E,m,norm)
    print '(E9.1,2X,I8,2X,F10.2,2X,F10.6,2X,F12.6)',  norm_sort(n), m, &
      dble( elec_alpha_num**3 + elec_alpha_num**2 * m ) / &
      dble( elec_alpha_num**3 + elec_alpha_num**2 * n ), norm, E
    if (E < target_energy) then
       n_dn = n+1
    else
       n_up = n
    endif
  enddo
  print *,  '=========================================================='
  print *,  norm_sort(n), target_energy

  deallocate (iorder, norm_sort, psi_bilinear_matrix_values_save)
end

subroutine compute_energy(psi_bilinear_matrix_values_save, E, m, norm)
  implicit none
  BEGIN_DOC
  ! Compute an energy when a threshold is applied
  END_DOC
  double precision, intent(in) :: psi_bilinear_matrix_values_save(n_det)
  integer(bit_kind), allocatable :: det_i(:,:), det_j(:,:)
  integer :: i,j, k, l, m
  double precision :: num, norm, ci, cj, hij, E

  
    num = 0.d0
    norm = 0.d0
    m = 0
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(k,l,det_i,det_j,ci,cj,hij) REDUCTION(+:norm,m,num)
    allocate( det_i(N_int,2), det_j(N_int,2))
    !$OMP DO schedule(guided)
    do k=1,n_det
      if (psi_bilinear_matrix_values_save(k) == 0.d0) then
        cycle
      endif
      ci = psi_bilinear_matrix_values_save(k)
      det_i(:,1) = psi_det_alpha_unique(:,psi_bilinear_matrix_rows(k))
      det_i(:,2) = psi_det_beta_unique(:,psi_bilinear_matrix_columns(k))
      do l=1,n_det
        if (psi_bilinear_matrix_values_save(l) == 0.d0) then
          cycle
        endif
        cj = psi_bilinear_matrix_values_save(l)
        det_j(:,1) = psi_det_alpha_unique(:,psi_bilinear_matrix_rows(l))
        det_j(:,2) = psi_det_beta_unique(:,psi_bilinear_matrix_columns(l))
        call i_h_j(det_i, det_j, N_int, hij)
        num = num + ci*cj*hij
      enddo
     norm = norm + ci*ci
     m = m+1
    enddo
    !$OMP END DO
    deallocate (det_i,det_j)
    !$OMP END PARALLEL
    E = num / norm + nuclear_repulsion
end

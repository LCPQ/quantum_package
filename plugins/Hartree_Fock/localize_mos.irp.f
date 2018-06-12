program localize_mos
  implicit none
  integer :: rank, i,j,k
  double precision, allocatable :: W(:,:)
  double precision :: f, f_incr

  allocate (W(ao_num,ao_num))

  W = 0.d0
  do k=1,elec_beta_num
    do j=1,ao_num
      do i=1,ao_num
        W(i,j) = W(i,j) + mo_coef(i,k) * mo_coef(j,k)
      enddo
    enddo
  enddo

!  call svd_mo(ao_num,elec_beta_num,W, size(W,1), &
!    mo_coef(1,1),size(mo_coef,1))
  call cholesky_mo(ao_num,elec_beta_num,W, size(W,1), &
    mo_coef(1,1),size(mo_coef,1),1.d-6,rank)
  print *,  rank

  if (elec_alpha_num>elec_beta_num) then
    W = 0.d0
    do k=elec_beta_num+1,elec_alpha_num
      do j=1,ao_num
        do i=1,ao_num
          W(i,j) = W(i,j) + mo_coef(i,k) * mo_coef(j,k)
        enddo
      enddo
    enddo

!    call svd_mo(ao_num,elec_alpha_num-elec_beta_num,W, size(W,1), &
!    mo_coef(1,1),size(mo_coef,1))
    call cholesky_mo(ao_num,elec_alpha_num-elec_beta_num,W, size(W,1), &
      mo_coef(1,elec_beta_num+1),size(mo_coef,1),1.d-6,rank)
    print *,  rank
  endif

  W = 0.d0
  do k=elec_alpha_num+1,mo_tot_num
    do j=1,ao_num
      do i=1,ao_num
        W(i,j) = W(i,j) + mo_coef(i,k) * mo_coef(j,k)
      enddo
    enddo
  enddo

!  call svd_mo(ao_num,mo_tot_num-elec_alpha_num,W, size(W,1), &
!    mo_coef(1,1),size(mo_coef,1))
  call cholesky_mo(ao_num,mo_tot_num-elec_alpha_num,W, size(W,1), &
    mo_coef(1,elec_alpha_num+1),size(mo_coef,1),1.d-6,rank)
  print *,  rank
  mo_label = "Localized"

  TOUCH mo_coef

  W(1:ao_num,1:mo_tot_num) = mo_coef(1:ao_num,1:mo_tot_num)
  integer :: iorder(mo_tot_num)
  double precision :: s(mo_tot_num), swap(ao_num)
  do k=1,mo_tot_num
    iorder(k) = k
    s(k) = Fock_matrix_diag_mo(k)
  enddo
  call dsort(s(1),iorder(1),elec_beta_num)
  call dsort(s(elec_beta_num+1),iorder(elec_beta_num+1),elec_alpha_num-elec_beta_num)
  call dsort(s(elec_alpha_num+1),iorder(elec_alpha_num+1),mo_tot_num-elec_alpha_num)
  do k=1,mo_tot_num
    mo_coef(1:ao_num,k) = W(1:ao_num,iorder(k))
    print *,  k, s(k)
  enddo
  call save_mos

end

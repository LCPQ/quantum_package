
BEGIN_PROVIDER [ double precision, mo_overlap,(mo_tot_num,mo_tot_num)]
  implicit none
  integer :: i,j,n,l
  double precision :: f
  integer :: lmax
  lmax = (ao_num/4) * 4
  !$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) &
  !$OMP  PRIVATE(i,j,n,l) &
  !$OMP  SHARED(mo_overlap,mo_coef,ao_overlap, &
  !$OMP    mo_tot_num,ao_num,lmax)
  do j=1,mo_tot_num
   do i= 1,mo_tot_num
    mo_overlap(i,j) = 0.d0
    do n = 1, lmax,4
     do l = 1, ao_num
      mo_overlap(i,j) = mo_overlap(i,j) + mo_coef(l,i) * &
           ( mo_coef(n  ,j) * ao_overlap(l,n  )  &
           + mo_coef(n+1,j) * ao_overlap(l,n+1)  &
           + mo_coef(n+2,j) * ao_overlap(l,n+2)  &
           + mo_coef(n+3,j) * ao_overlap(l,n+3)  )
     enddo
    enddo
    do n = lmax+1, ao_num
     do l = 1, ao_num
      mo_overlap(i,j) = mo_overlap(i,j) + mo_coef(n,j) * mo_coef(l,i) * ao_overlap(l,n)
     enddo
    enddo
   enddo
  enddo
  !$OMP END PARALLEL DO
END_PROVIDER


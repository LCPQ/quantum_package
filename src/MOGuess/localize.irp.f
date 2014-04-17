!TODO Ecrire un cholesky avec bitmask


subroutine localize_mos(mask, nint)
  implicit none
  use bitmasks
  integer, intent(in) :: nint
  integer(bit_kind), intent(in) :: mask(nint)
  integer :: i,j,k,l
  double precision, allocatable :: DM(:,:)
  double precision, allocatable :: mo_coef_new(:,:), R(:,:)
  integer :: n
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: DM, mo_coef_new, R
  integer :: rank
  integer, parameter :: n_core = 2

  allocate(R(mo_tot_num,mo_tot_num))
  allocate(DM(ao_num_align,ao_num))
  allocate(mo_coef_new(ao_num_align,mo_tot_num))
  n = ao_num
  mo_coef_new = mo_coef

BEGIN_TEMPLATE
   DM = 0.d0
   if ($START < $END) then
     do k=$START, $END
      do j=1,n
       !DEC$ VECTOR ALIGNED
       do i=1,n
         DM(i,j) += mo_coef_new(i,k)*mo_coef_new(j,k)
       enddo
      enddo
     enddo
     call cholesky_mo(n,$END-$START+1,DM,mo_coef_new(1,$START),size(mo_coef_new,1),-1.d0,rank)
   endif
SUBST [ START, END ]
    1 ; n_core ;;
END_TEMPLATE

  deallocate(DM)
  call find_rotation(mo_coef,ao_num_align,mo_coef_new,ao_num,R,mo_tot_num)
  mo_coef = mo_coef_new
  deallocate(mo_coef_new)

  double precision,allocatable :: mo_energy_new(:)
  integer, allocatable :: iorder(:)
  allocate(mo_energy_new(mo_tot_num),iorder(mo_tot_num))

  do i=1,mo_tot_num
   iorder(i) = i
   mo_energy_new(i) = 0.d0
   do k=1,mo_tot_num
     mo_energy_new(i) += R(k,i)*R(k,i)*mo_energy(k)
   enddo
  enddo
  mo_energy = mo_energy_new
  call dsort(mo_energy(1),iorder(1),n_core)
  allocate (mo_coef_new(ao_num_align,mo_tot_num))
  mo_coef_new = mo_coef
  do j=1,mo_tot_num
   do i=1,ao_num
    mo_coef(i,j) = mo_coef_new(i,iorder(j))
   enddo
  enddo
  deallocate (mo_coef_new,R)
  deallocate(mo_energy_new,iorder)
  mo_label = 'localized'

  SOFT_TOUCH mo_coef mo_energy mo_label
end





subroutine cholesky_mo(n,m,P,C,LDC,tol_in,rank)
 implicit none
 integer, intent(in) :: n,m, LDC
 double precision, intent(in) :: P(LDC,n)
 double precision, intent(out) :: C(LDC,m)
 double precision, intent(in) :: tol_in
 integer, intent(out) :: rank

 integer :: info
 integer :: i,k
 integer :: ipiv(n)
 double precision:: tol
 double precision, allocatable :: W(:,:), work(:)
 !DEC$ ATTRIBUTES ALIGN: 32 :: W
 !DEC$ ATTRIBUTES ALIGN: 32 :: work
 !DEC$ ATTRIBUTES ALIGN: 32 :: ipiv

 allocate(W(LDC,n),work(2*n))
 tol=tol_in

 info = 0
 do i=1,n
  do k=1,i
   W(i,k) = P(i,k)
  enddo
  do k=i+1,n
   W(i,k) = 0.
  enddo
 enddo
 call DPSTRF('L', n, W, LDC, ipiv, rank, tol, work, info )
 do i=1,n
  do k=1,min(m,rank)
   C(ipiv(i),k) = W(i,k)
  enddo
 enddo

 deallocate(W,work)
end


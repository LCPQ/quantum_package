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


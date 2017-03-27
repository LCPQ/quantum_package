subroutine cholesky_mo(n,m,P,LDP,C,LDC,tol_in,rank)
 implicit none
 BEGIN_DOC
! Cholesky decomposition of AO Density matrix
!
! n : Number of AOs

! m : Number of MOs
!
! P(LDP,n) : Density matrix in AO basis
!
! C(LDC,m) : MOs
!
! tol_in : tolerance
!
! rank : Nomber of local MOs (output)
!
 END_DOC
 integer, intent(in) :: n,m, LDC, LDP
 double precision, intent(in) :: P(LDP,n)
 double precision, intent(out) :: C(LDC,m)
 double precision, intent(in) :: tol_in
 integer, intent(out) :: rank

 integer :: info
 integer :: i,k
 integer :: ipiv(n)
 double precision:: tol
 double precision, allocatable :: W(:,:), work(:)

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

subroutine svd_mo(n,m,P,LDP,C,LDC)
 implicit none
 BEGIN_DOC
! Singular value decomposition of the AO Density matrix
!
! n : Number of AOs

! m : Number of MOs
!
! P(LDP,n) : Density matrix in AO basis
!
! C(LDC,m) : MOs
!
! tol_in : tolerance
!
! rank : Nomber of local MOs (output)
!
 END_DOC
 integer, intent(in) :: n,m, LDC, LDP
 double precision, intent(in) :: P(LDP,n)
 double precision, intent(out) :: C(LDC,m)

 integer :: info
 integer :: i,k
 integer :: ipiv(n)
 double precision:: tol
 double precision, allocatable :: W(:,:), work(:)

 allocate(W(LDC,n),work(2*n))
 call svd(P,LDP,C,LDC,W,size(W,1),m,n)

 deallocate(W,work)
end


subroutine cholesky_mo(n,m,P,LDP,C,LDC,tol_in,rank)
 implicit none
 BEGIN_DOC
! Cholesky decomposition of AO Density matrix to
! generate MOs
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

BEGIN_PROVIDER [ double precision, mo_density_matrix, (mo_tot_num_align, mo_tot_num) ]
 implicit none
 BEGIN_DOC
 ! Density matrix in MO basis
 END_DOC
 integer :: i,j,k
 mo_density_matrix = 0.d0
 do k=1,mo_tot_num
   if (mo_occ(k) == 0.d0) then
     cycle
   endif
   do j=1,ao_num
     do i=1,ao_num
       mo_density_matrix(i,j) = mo_density_matrix(i,j) + &
         mo_occ(k) * mo_coef(i,k) * mo_coef(j,k)
     enddo
   enddo
 enddo
END_PROVIDER

BEGIN_PROVIDER [ double precision, mo_density_matrix_virtual, (mo_tot_num_align, mo_tot_num) ]
 implicit none
 BEGIN_DOC
 ! Density matrix in MO basis (virtual MOs)
 END_DOC
 integer :: i,j,k
 mo_density_matrix_virtual = 0.d0
 do k=1,mo_tot_num
   do j=1,ao_num
     do i=1,ao_num
       mo_density_matrix_virtual(i,j) = mo_density_matrix_virtual(i,j) + &
         (2.d0-mo_occ(k)) * mo_coef(i,k) * mo_coef(j,k)
     enddo
   enddo
 enddo
END_PROVIDER

subroutine svd_mo(n,m,P,LDP,C,LDC)
 implicit none
 BEGIN_DOC
! Singular value decomposition of the AO Density matrix
!
! n : Number of AOs
!
! m : Number of MOs
!
! P(LDP,n) : Density matrix in AO basis
!
! C(LDC,m) : MOs
!
 END_DOC
 integer, intent(in) :: n,m, LDC, LDP
 double precision, intent(in) :: P(LDP,n)
 double precision, intent(out) :: C(LDC,m)

 integer :: info
 integer :: i,k
 integer :: ipiv(n)
 double precision:: tol
 double precision, allocatable :: W(:,:), work(:), D(:)

 allocate(W(LDC,n),work(2*n),D(n))
 print*, ''
 do i = 1, n
  print*, P(i,i)
 enddo
 call svd(P,LDP,C,LDC,D,W,size(W,1),m,n)
 double precision :: accu
 accu = 0.d0
 print*, 'm',m
 do i = 1, m
   print*, D(i)
   accu += D(i)
 enddo
 print*,'Sum of D',accu 

 deallocate(W,work)
end

subroutine svd_mo_new(n,m,m_physical,P,LDP,C,LDC)
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
 integer, intent(in) :: n,m,m_physical, LDC, LDP
 double precision, intent(in) :: P(LDP,n)
 double precision, intent(out) :: C(LDC,m)

 integer :: info
 integer :: i,k
 integer :: ipiv(n)
 double precision:: tol
 double precision, allocatable :: W(:,:), work(:), D(:)

 allocate(W(LDC,n),work(2*n),D(n))
 call svd(P,LDP,C,LDC,D,W,size(W,1),m_physical,n)
 double precision :: accu
 accu = 0.d0
 print*, 'm',m_physical
 do i = 1, m_physical
   print*, D(i)
   accu += D(i)
 enddo
 print*,'Sum of D',accu 

 deallocate(W,work)
end


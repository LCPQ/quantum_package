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


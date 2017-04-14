subroutine invert_matrix(A,LDA,na,A_inv,LDA_inv)
implicit none
double precision, intent(in)    :: A (LDA,na)
integer, intent(in)             :: LDA, LDA_inv
integer, intent(in)             :: na
double precision, intent(out)   :: A_inv (LDA_inv,na)

 double precision :: work(LDA_inv*max(na,64))
!DIR$ ATTRIBUTES ALIGN: $IRP_ALIGN :: work
 integer          :: inf
 integer          :: ipiv(LDA_inv)
!DIR$ ATTRIBUTES ALIGN: $IRP_ALIGN :: ipiv
  integer          :: lwork
  A_inv(1:na,1:na) = A(1:na,1:na) 
  call dgetrf(na, na, A_inv, LDA_inv, ipiv, inf )
  lwork = SIZE(work)
  call dgetri(na, A_inv, LDA_inv, ipiv, work, lwork, inf )
end


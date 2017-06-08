BEGIN_PROVIDER [ double precision, cusp_A, (nucl_num, nucl_num) ]
   implicit none
   BEGIN_DOC
   ! Equations to solve : A.X = B
   END_DOC
   
   integer                        :: mu, A, B

   do B=1,nucl_num
     do A=1,nucl_num
        cusp_A(A,B) = 0.d0
        if (A/=B) then
          cusp_A(A,B) -= slater_value_at_nucl(A,B)
        endif
        do mu=1,ao_num
          cusp_A(A,B) += slater_overlap(mu,B) * ao_value_at_nucl(mu,A)
        enddo
     enddo
   enddo

END_PROVIDER

BEGIN_PROVIDER [ double precision, cusp_C, (nucl_num, mo_tot_num) ]
   implicit none
   BEGIN_DOC
   ! Equations to solve : A.C = B
   END_DOC
   
   integer                        :: i, A, info
   
   do i=1,mo_tot_num
     do A=1,nucl_num
       cusp_C(A,i) = mo_value_at_nucl(i,A)
     enddo
   enddo
   
   integer, allocatable           :: ipiv(:)
   allocate ( ipiv(nucl_num) )
   call dgegv(nucl_num, mo_tot_num, cusp_A, size(cusp_A,1),          &
       ipiv, cusp_C, size(cusp_C,1), info)
   deallocate (ipiv)
   
   if (info /= 0) then
     print *, 'Cusp : linear solve failed'
     stop -1
   endif
     
END_PROVIDER


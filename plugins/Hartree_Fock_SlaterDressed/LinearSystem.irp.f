BEGIN_PROVIDER [ double precision, cusp_A, (nucl_num, nucl_num) ]
   implicit none
   BEGIN_DOC
   ! Equations to solve : A.X = B
   END_DOC
   
   integer                        :: mu, A, B

   cusp_A = 0.d0
   do A=1,nucl_num
    cusp_A(A,A) = slater_expo(A)/nucl_charge(A) * slater_value_at_nucl(A,A) 
    do B=1,nucl_num
      cusp_A(A,B) -= slater_value_at_nucl(B,A)
      ! Projector
      do mu=1,mo_tot_num
        cusp_A(A,B) += MOSlaOverlap_matrix(mu,B) * mo_value_at_nucl(mu,A)
      enddo
    enddo
   enddo
END_PROVIDER

BEGIN_PROVIDER [ double precision, cusp_B, (nucl_num, mo_tot_num) ]
   implicit none
   BEGIN_DOC
   ! Equations to solve : A.C = B
   END_DOC
   
   integer                        :: i, A, info
   
   do i=1,mo_tot_num
     do A=1,nucl_num
       cusp_B(A,i) = mo_value_at_nucl(i,A)
     enddo
   enddo
END_PROVIDER

   
BEGIN_PROVIDER [ double precision, cusp_C, (nucl_num, mo_tot_num) ]
   implicit none
   BEGIN_DOC
   ! Equations to solve : A.C = B
   END_DOC
   
   double precision, allocatable           :: AF(:,:)
   integer :: info
   allocate ( AF(nucl_num,nucl_num) )
   
   call get_pseudo_inverse(cusp_A,nucl_num,nucl_num,AF,size(AF,1))
   call dgemm('N','N',nucl_num,mo_tot_num,nucl_num,1.d0, &
     AF,size(AF,1), cusp_B, size(cusp_B,1), 0.d0, cusp_C, size(cusp_C,1))

END_PROVIDER


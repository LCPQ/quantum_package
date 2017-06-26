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
        cusp_A(A,B) += AO_orthoSlaOverlap_matrix(mu,B) * ao_ortho_value_at_nucl(mu,A)
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
  
  integer                        :: info
  integer                        :: ipiv(nucl_num)
  double precision, allocatable  :: AF(:,:)
  allocate ( AF(nucl_num,nucl_num) )
  
  cusp_C(1:nucl_num,1:mo_tot_num) = cusp_B(1:nucl_num,1:mo_tot_num)
  AF(1:nucl_num,1:nucl_num) = cusp_A(1:nucl_num,1:nucl_num)
  call dgetrf(nucl_num,nucl_num,AF,size(AF,1),ipiv,info)
  if (info /= 0) then
    print *,  info
    stop 'dgetrf failed'
  endif
  call dgetrs('N',nucl_num,mo_tot_num,AF,size(AF,1),ipiv,cusp_C,size(cusp_C,1),info)
  if (info /= 0) then
    print *,  info
    stop 'dgetrs failed'
  endif

END_PROVIDER


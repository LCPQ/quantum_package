 BEGIN_PROVIDER [ double precision, diagonal_Fock_matrix_mo, (mo_tot_num) ]
&BEGIN_PROVIDER [ double precision, eigenvectors_Fock_matrix_mo, (ao_num_align,mo_tot_num) ]
   implicit none
   BEGIN_DOC
   ! Diagonal Fock matrix in the MO basis
   END_DOC
   
   double precision, allocatable  :: R(:,:)
   !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: R
   
   allocate(R(mo_tot_num_align,mo_tot_num))
   
   call lapack_diag(diagonal_Fock_matrix_mo,R,Fock_matrix_mo,size(Fock_matrix_mo,1),&
       mo_tot_num)
   
   call dgemm('N','N',ao_num,mo_tot_num,mo_tot_num,1.d0,mo_coef,size(mo_coef,1),&
       R,size(R,1),0.d0,eigenvectors_Fock_matrix_mo,size(eigenvectors_Fock_matrix_mo,1))
   deallocate(R)
END_PROVIDER
 

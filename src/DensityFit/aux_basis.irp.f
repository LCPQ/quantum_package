 BEGIN_PROVIDER [ integer, aux_basis_num ]
&BEGIN_PROVIDER [ integer, aux_basis_num_8 ]
 implicit none
 BEGIN_DOC
 ! Number of auxiliary basis functions
 END_DOC
 integer :: align_double
 aux_basis_num = ao_num * (ao_num+1)/2
 aux_basis_num_8 = align_double(aux_basis_num)
END_PROVIDER

BEGIN_PROVIDER [ integer, aux_basis_idx, (2,aux_basis_num) ]
 implicit none
 BEGIN_DOC
! aux_basis_idx(k) -> i,j
 END_DOC
 integer :: i,j,k
 k=0
 do j=1,ao_num
   do i=1,j
     k = k+1
     aux_basis_idx(1,k) = i
     aux_basis_idx(2,k) = j
   enddo
 enddo
END_PROVIDER


BEGIN_PROVIDER [ double precision, aux_basis_overlap_matrix, (aux_basis_num_8,aux_basis_num) ]
 implicit none
 BEGIN_DOC  
! Auxiliary basis set
 END_DOC
 integer :: m,n,i,j,k,l
 double precision :: ao_four_overlap

 aux_basis_overlap_matrix(1,1) = ao_four_overlap(1,1,1,1)
 !$OMP PARALLEL DO PRIVATE(i,j,k,l,m,n) SCHEDULE(GUIDED)
 do m=1,aux_basis_num
   i = aux_basis_idx(1,m)
   j = aux_basis_idx(2,m)
   do n=1,m
     k = aux_basis_idx(1,n)
     l = aux_basis_idx(2,n)
     aux_basis_overlap_matrix(m,n) = ao_four_overlap(i,j,k,l)
     aux_basis_overlap_matrix(n,m) = aux_basis_overlap_matrix(m,n) 
   enddo
 enddo
 !$OMP END PARALLEL DO

END_PROVIDER




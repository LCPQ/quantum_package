 BEGIN_PROVIDER [ integer, aux_basis_num_sqrt ]
&BEGIN_PROVIDER [ integer, aux_basis_num ]
&BEGIN_PROVIDER [ integer, aux_basis_num_8 ]
 implicit none
 BEGIN_DOC
 ! Number of auxiliary basis functions
 END_DOC
 integer :: align_double

 if (do_pseudo) then
   aux_basis_num_sqrt = ao_num + ao_pseudo_num
!   aux_basis_num_sqrt = ao_num 
 else
 endif

 aux_basis_num = aux_basis_num_sqrt * (aux_basis_num_sqrt+1)/2 
 aux_basis_num_8 = align_double(aux_basis_num)
END_PROVIDER

BEGIN_PROVIDER [ integer, aux_basis_idx, (2,aux_basis_num) ]
 implicit none
 BEGIN_DOC
! aux_basis_idx(k) -> i,j
 END_DOC
 integer :: i,j,k
 k=0
 do j=1,aux_basis_num_sqrt
   do i=1,j
     k = k+1
     aux_basis_idx(1,k) = i
     aux_basis_idx(2,k) = j
   enddo
 enddo
END_PROVIDER

 BEGIN_PROVIDER [ double precision, aux_basis_expo_transp, (ao_prim_num_max_align,aux_basis_num_sqrt) ]
&BEGIN_PROVIDER [ double precision, aux_basis_coef_transp, (ao_prim_num_max_align,aux_basis_num_sqrt) ]
&BEGIN_PROVIDER [ integer, aux_basis_prim_num, (aux_basis_num_sqrt) ]
&BEGIN_PROVIDER [ integer, aux_basis_power, (aux_basis_num_sqrt,3) ]
&BEGIN_PROVIDER [ integer, aux_basis_nucl, (aux_basis_num_sqrt) ]
 implicit none
 BEGIN_DOC
 ! Exponents of the auxiliary basis
 END_DOC
 integer                        :: i,j
 do j=1,ao_num
   do i=1,ao_prim_num_max
     aux_basis_expo_transp(i,j) = ao_expo_ordered_transp(i,j)
     aux_basis_coef_transp(i,j) = ao_coef_normalized_ordered_transp(i,j)
   enddo
 enddo
 do i=1,ao_num
   aux_basis_prim_num(i) = ao_prim_num(i)
   aux_basis_nucl(i) = ao_nucl(i)
   aux_basis_power(i,1:3) = ao_power(i,1:3)
 enddo

 do j=1,ao_pseudo_num
   aux_basis_expo_transp(1,ao_num+j) = 0.5d0*pseudo_ao_expo(j)
   aux_basis_coef_transp(1,ao_num+j) = 1.d0
   aux_basis_power(ao_num+j,1:3) = 0
   aux_basis_prim_num(ao_num+j) = 1
   aux_basis_nucl(ao_num+j) = pseudo_ao_nucl(j)
 enddo
  
END_PROVIDER


BEGIN_PROVIDER [ double precision, aux_basis_overlap_matrix, (aux_basis_num_8,aux_basis_num) ]
 implicit none
 BEGIN_DOC  
! Auxiliary basis set
 END_DOC
 integer :: m,n,i,j,k,l
 double precision :: aux_basis_four_overlap

 aux_basis_overlap_matrix(1,1) = aux_basis_four_overlap(1,1,1,1)
 !$OMP PARALLEL DO PRIVATE(i,j,k,l,m,n) SCHEDULE(GUIDED)
 do m=1,aux_basis_num
   i = aux_basis_idx(1,m)
   j = aux_basis_idx(2,m)
   do n=1,m
     k = aux_basis_idx(1,n)
     l = aux_basis_idx(2,n)
     aux_basis_overlap_matrix(m,n) = aux_basis_four_overlap(i,j,k,l)
     aux_basis_overlap_matrix(n,m) = aux_basis_overlap_matrix(m,n) 
   enddo
 enddo
 !$OMP END PARALLEL DO

END_PROVIDER

 BEGIN_PROVIDER [ double precision, aux_basis_expo, (aux_basis_num_sqrt,aux_basis_prim_num_max) ]
&BEGIN_PROVIDER [ double precision, aux_basis_coef, (aux_basis_num_sqrt,aux_basis_prim_num_max) ]
 implicit none
 BEGIN_DOC
 ! Exponents and coefficients of the auxiliary basis
 END_DOC
 integer :: i,j
 aux_basis_expo = 0.d0
 aux_basis_coef = 0.d0
 do j=1,aux_basis_num_sqrt
   do i=1,aux_basis_prim_num(j)
     aux_basis_expo(j,i) = aux_basis_expo_transp(i,j)
     aux_basis_coef(j,i) = aux_basis_coef_transp(i,j)
   enddo
 enddo

END_PROVIDER

BEGIN_PROVIDER [ integer, aux_basis_prim_num_max ]
 implicit none
 BEGIN_DOC
 ! = ao_prim_num_max
 END_DOC
 aux_basis_prim_num_max = ao_prim_num_max
END_PROVIDER


subroutine save_aux_basis
 implicit none
 call ezfio_set_aux_basis_aux_basis_num(aux_basis_num)
 call ezfio_set_aux_basis_aux_basis_num_sqrt(aux_basis_num_sqrt)
 call ezfio_set_aux_basis_aux_basis_idx(aux_basis_idx)
 call ezfio_set_aux_basis_aux_basis_prim_num(aux_basis_prim_num)
 call ezfio_set_aux_basis_aux_basis_nucl(aux_basis_nucl)
 call ezfio_set_aux_basis_aux_basis_power(aux_basis_power)
 call ezfio_set_aux_basis_aux_basis_coef(aux_basis_coef)
 call ezfio_set_aux_basis_aux_basis_expo(aux_basis_expo)
end

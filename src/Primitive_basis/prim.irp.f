BEGIN_PROVIDER  [integer, prim_num]
 implicit none
 BEGIN_DOC
! Number of uniq primitive basis function
 END_DOC
 PROVIDE ezfio_filename
 call ezfio_get_primitive_basis_prim_num(prim_num)

END_PROVIDER

 BEGIN_PROVIDER [integer, prim_nucl, (prim_num)]
 implicit none
 BEGIN_DOC
 ! Array of the nuclei on which are attached the primitives
 END_DOC
 PROVIDE ezfio_filename
 integer, allocatable :: tmp(:)
 allocate(tmp(prim_num))
 call ezfio_get_primitive_basis_prim_nucl(tmp)
 prim_nucl = tmp
 END_PROVIDER

 BEGIN_PROVIDER [integer, prim_power, (prim_num,3)]
&BEGIN_PROVIDER [integer, prim_l, (prim_num)]
 implicit none
 BEGIN_DOC
 ! Array of the power of the primitives
 ! Array of the L of the primitive
 END_DOC
 PROVIDE ezfio_filename
 integer :: i,j
 integer, allocatable :: tmp(:,:)
 allocate(tmp(3,prim_num))
 call ezfio_get_primitive_basis_prim_power(tmp)
 do i = 1, prim_num
  do j = 1,3 
   prim_power(i,j) = tmp(j,i)
  enddo
 enddo
 do i = 1, prim_num
  prim_l(i) = prim_power(i,1) + prim_power(i,2) + prim_power(i,3)
 enddo
 END_PROVIDER

 BEGIN_PROVIDER [double precision, prim_expo, (prim_num)]
 implicit none
 BEGIN_DOC
 ! Array of the exponents of the primitives
 END_DOC
 PROVIDE ezfio_filename
 double precision, allocatable :: tmp(:)
 allocate(tmp(prim_num))
 call ezfio_get_primitive_basis_prim_expo(tmp)
 prim_expo = tmp
 END_PROVIDER

 BEGIN_PROVIDER [double precision, prim_overlap, (prim_num,prim_num)]
 implicit none
 BEGIN_DOC
 ! Array of the overlap between the primitives
 END_DOC
 integer :: i,j
 prim_overlap = 0.d0
 double precision :: overlap, overlap_x, overlap_y, overlap_z
 double precision :: alpha, beta, c
 double precision :: A_center(3), B_center(3)
 integer :: power_A(3), power_B(3),dim1
 dim1=100
 do j = 1, prim_num
  A_center(1) = nucl_coord( prim_nucl(j), 1 )
  A_center(2) = nucl_coord( prim_nucl(j), 2 )
  A_center(3) = nucl_coord( prim_nucl(j), 3 )
  power_A(1)  = prim_power( j, 1 )
  power_A(2)  = prim_power( j, 2 )
  power_A(3)  = prim_power( j, 3 )
  alpha = prim_expo(j)
  do i = 1, prim_num
   B_center(1) = nucl_coord( prim_nucl(i), 1 )
   B_center(2) = nucl_coord( prim_nucl(i), 2 )
   B_center(3) = nucl_coord( prim_nucl(i), 3 )
   power_B(1)  = prim_power( i, 1 )
   power_B(2)  = prim_power( i, 2 )
   power_B(3)  = prim_power( i, 3 )
   beta = prim_expo(i)
   call overlap_gaussian_xyz(A_center,B_center,alpha,beta,power_A,power_B,overlap_x,overlap_y,overlap_z,overlap,dim1)
   prim_overlap(i,j) = overlap
  enddo
 enddo
 END_PROVIDER

 BEGIN_PROVIDER [ double precision, prim_ao_coef, (prim_num,ao_num)]
 BEGIN_DOC
 ! Developement of the primitive on the AO basis 
 ! prim_ao_coef(i,j) = <prim_i|AO_j>
 END_DOC
 implicit none
 integer :: i,j,k,l

 prim_ao_coef = 0.d0
 do i = 1, prim_num
  do j = 1, ao_num
   if(ao_nucl(j).ne.prim_nucl(i))cycle
   if(ao_l(j) == prim_l(i))then
    if(ao_power(j,1) == prim_power(i,1) .and.   &
       ao_power(j,2) == prim_power(i,2) .and.   &
       ao_power(j,3) == prim_power(i,3)      )then
     ! primitive of the same L than the AO
     do k = 1, ao_prim_num(j)
      if(ao_expo(j,k) == prim_expo(i))then
       prim_ao_coef(i,j) += ao_coef(j,k) 
      endif
     enddo
    endif
   endif
  enddo
 enddo
 END_PROVIDER

 BEGIN_PROVIDER [ double precision, prim_ao_overlap, (ao_num,ao_num)]
 implicit none
 BEGIN_dOC
 ! Array of the overlap of the AO basis, explicitely calculated on the primitive basis
 END_DOC
 integer :: i,j,k,l
 do i= 1, ao_num
  do j = 1,ao_num
   prim_ao_overlap(i,j) = 0.d0
   do k = 1, prim_num
    do l = 1, prim_num
      prim_ao_overlap(i,j) += prim_ao_coef(k,i) * prim_overlap(k,l) * prim_ao_coef(l,j)
    enddo
   enddo
  enddo
 enddo

 END_PROVIDER

 BEGIN_PROVIDER [ double precision, prim_mo_coef, (prim_num,mo_tot_num)]
 BEGIN_DOC
 ! Developement of the primitive on the MO basis 
 ! prim_ao_coef(i,j) = <prim(i)|ao(j)>
 END_DOC

 integer :: i,j,k,l
 double precision :: coef
 prim_mo_coef = 0.d0
 do i = 1, mo_tot_num
  do k = 1, ao_num
   coef = mo_coef(k,i)
   do l = 1, prim_num
    prim_mo_coef(l,i) += coef * prim_ao_coef(l,k)
   enddo
  enddo
 enddo

 END_PROVIDER

 BEGIN_PROVIDER [ double precision, prim_mo_overlap, (mo_tot_num,mo_tot_num)]
 implicit none
 BEGIN_dOC
 ! Array of the overlap of the AO basis, explicitely calculated on the primitive basis
 END_DOC
 integer :: i,j,k,l
 do i= 1, mo_tot_num
  do j = 1,mo_tot_num
   prim_mo_overlap(i,j) = 0.d0
   do k = 1, prim_num
    do l = 1, prim_num
      prim_mo_overlap(i,j) += prim_mo_coef(k,i) * prim_overlap(k,l) * prim_mo_coef(l,j)
    enddo
   enddo
  enddo
 enddo

 END_PROVIDER

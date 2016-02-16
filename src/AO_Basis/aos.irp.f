BEGIN_PROVIDER [ integer, ao_num_align ]
   implicit none
   
   BEGIN_DOC
   ! Number of atomic orbitals align
   END_DOC
   
   integer                        :: align_double
   ao_num_align = align_double(ao_num)
END_PROVIDER 

 BEGIN_PROVIDER [ integer, ao_prim_num_max ]
&BEGIN_PROVIDER [ integer, ao_prim_num_max_align ]
 implicit none
 ao_prim_num_max = 0
 PROVIDE ezfio_filename
 call ezfio_get_ao_basis_ao_prim_num_max(ao_prim_num_max)
 integer :: align_double
 ao_prim_num_max_align = align_double(ao_prim_num_max)
 END_PROVIDER

 BEGIN_PROVIDER [ double precision, ao_coef_normalized, (ao_num_align,ao_prim_num_max) ]
&BEGIN_PROVIDER [ double precision, ao_coef_normalization_factor, (ao_num) ]
  implicit none
  BEGIN_DOC
  ! Coefficients including the AO normalization
  END_DOC
  double precision               :: norm, norm2,overlap_x,overlap_y,overlap_z,C_A(3), c
  integer                        :: l, powA(3), nz
  integer                        :: i,j,k
  nz=100
  C_A(1) = 0.d0
  C_A(2) = 0.d0
  C_A(3) = 0.d0
  ao_coef_normalized = 0.d0
  do i=1,ao_num
    powA(1) = ao_power(i,1)
    powA(2) = ao_power(i,2)
    powA(3) = ao_power(i,3)
    do j=1,ao_prim_num(i)
      call overlap_gaussian_xyz(C_A,C_A,ao_expo(i,j),ao_expo(i,j),powA,powA,overlap_x,overlap_y,overlap_z,norm,nz)
      ao_coef_normalized(i,j) = ao_coef(i,j)/sqrt(norm)
    enddo
    ! Normalization of the contracted basis functions
    norm = 0.d0
    do j=1,ao_prim_num(i)
     do k=1,ao_prim_num(i)
      call overlap_gaussian_xyz(C_A,C_A,ao_expo(i,j),ao_expo(i,k),powA,powA,overlap_x,overlap_y,overlap_z,c,nz)
      norm = norm+c*ao_coef_normalized(i,j)*ao_coef_normalized(i,k)
     enddo
    enddo
    ao_coef_normalization_factor(i) = 1.d0/sqrt(norm)
    do j=1,ao_prim_num(i)
      ao_coef_normalized(i,j) = ao_coef_normalized(i,j) * ao_coef_normalization_factor(i)
    enddo
  enddo
END_PROVIDER

 BEGIN_PROVIDER [ double precision, ao_coef_normalized_ordered, (ao_num_align,ao_prim_num_max) ]
&BEGIN_PROVIDER [ double precision, ao_expo_ordered, (ao_num_align,ao_prim_num_max) ]
   implicit none
   BEGIN_DOC
   ! Sorted primitives to accelerate 4 index MO transformation
   END_DOC
   
   integer                        :: iorder(ao_prim_num_max)
   double precision               :: d(ao_prim_num_max,2)
   integer                        :: i,j
   do i=1,ao_num
     do j=1,ao_prim_num(i)
       iorder(j) = j
       d(j,1) = ao_expo(i,j)
       d(j,2) = ao_coef_normalized(i,j)
     enddo
     call dsort(d(1,1),iorder,ao_prim_num(i))
     call dset_order(d(1,2),iorder,ao_prim_num(i))
     do j=1,ao_prim_num(i)
       ao_expo_ordered(i,j) = d(j,1)
       ao_coef_normalized_ordered(i,j) = d(j,2)
     enddo
   enddo
END_PROVIDER


BEGIN_PROVIDER [ double precision, ao_coef_normalized_ordered_transp, (ao_prim_num_max_align,ao_num) ]
  implicit none
  BEGIN_DOC
  ! Transposed ao_coef_normalized_ordered
  END_DOC
  integer                        :: i,j
  do j=1, ao_num
    do i=1, ao_prim_num_max
      ao_coef_normalized_ordered_transp(i,j) = ao_coef_normalized_ordered(j,i)
    enddo
  enddo
  
END_PROVIDER

BEGIN_PROVIDER [ double precision, ao_expo_ordered_transp, (ao_prim_num_max_align,ao_num) ]
  implicit none
  BEGIN_DOC
  ! Transposed ao_expo_ordered
  END_DOC
  integer                        :: i,j
  do j=1, ao_num
    do i=1, ao_prim_num_max
      ao_expo_ordered_transp(i,j) = ao_expo_ordered(j,i)
    enddo
  enddo
  
END_PROVIDER

 BEGIN_PROVIDER [ integer, ao_l, (ao_num) ]
&BEGIN_PROVIDER [ integer, ao_l_max  ]
 implicit none
 BEGIN_DOC
! ao_l = l value of the AO: a+b+c in x^a y^b z^c
 END_DOC
 integer :: i
 do i=1,ao_num
   ao_l(i) = ao_power(i,1) + ao_power(i,2) + ao_power(i,3) 
 enddo
 ao_l_max = maxval(ao_l)
END_PROVIDER

BEGIN_PROVIDER [ integer, ao_prim_num_max_align ]
 implicit none
 BEGIN_DOC
! Number of primitives per atomic orbital aligned
 END_DOC

 integer :: align_double
 ao_prim_num_max_align = align_double(ao_prim_num_max)
END_PROVIDER

integer function ao_power_index(nx,ny,nz)
  implicit none
  integer, intent(in)            :: nx, ny, nz
  BEGIN_DOC
  ! Unique index given to a triplet of powers:
  !
  ! 1/2 (l-n_x)*(l-n_x+1) + n_z + 1
  END_DOC
  integer                        :: l
  l = nx + ny + nz
  ao_power_index = ((l-nx)*(l-nx+1))/2 + nz + 1
end

 BEGIN_PROVIDER [ integer, ao_l, (ao_num) ]
&BEGIN_PROVIDER [ integer, ao_l_max ]
&BEGIN_PROVIDER [ character*(128), ao_l_char, (ao_num) ]
 implicit none
 BEGIN_DOC
! ao_l = l value of the AO: a+b+c in x^a y^b z^c
 END_DOC
 integer :: i
 do i=1,ao_num
   ao_l(i) = ao_power(i,1) + ao_power(i,2) + ao_power(i,3)
   ao_l_char(i) = l_to_charater(ao_l(i))
 enddo
 ao_l_max = maxval(ao_l)
END_PROVIDER

BEGIN_PROVIDER [ character*(128), l_to_charater, (0:4)]
 BEGIN_DOC
 ! character corresponding to the "L" value of an AO orbital
 END_DOC
 implicit none
 l_to_charater(0)='S'
 l_to_charater(1)='P'
 l_to_charater(2)='D'
 l_to_charater(3)='F'
 l_to_charater(4)='G'
END_PROVIDER


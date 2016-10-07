 BEGIN_PROVIDER [ double precision, ao_overlap,(ao_num_align,ao_num) ]
&BEGIN_PROVIDER [ double precision, ao_overlap_x,(ao_num_align,ao_num) ]
&BEGIN_PROVIDER [ double precision, ao_overlap_y,(ao_num_align,ao_num) ]
&BEGIN_PROVIDER [ double precision, ao_overlap_z,(ao_num_align,ao_num) ]
  implicit none
  BEGIN_DOC  
! Overlap between atomic basis functions:
! :math:`\int \chi_i(r) \chi_j(r) dr)`
  END_DOC
  integer :: i,j,n,l
  double precision :: f
  integer :: dim1
  double precision :: overlap, overlap_x, overlap_y, overlap_z
  double precision :: alpha, beta, c
  double precision :: A_center(3), B_center(3)
  integer :: power_A(3), power_B(3)
!   if (read_ao_one_integrals) then
!     call ezfio_get_ao_basis_integral_overlap(ao_overlap(1:ao_num, 1:ao_num))
!     print *,  'AO overlap integrals read from disk'
!  else
    dim1=100
    !$OMP PARALLEL DO SCHEDULE(GUIDED) &
    !$OMP DEFAULT(NONE) &
    !$OMP PRIVATE(A_center,B_center,power_A,power_B,&
    !$OMP  overlap_x,overlap_y, overlap_z, overlap, &
    !$OMP  alpha, beta,i,j,c) &
    !$OMP SHARED(nucl_coord,ao_power,ao_prim_num, &
    !$OMP  ao_overlap_x,ao_overlap_y,ao_overlap_z,ao_overlap,ao_num,ao_coef_normalized_ordered_transp,ao_nucl, &
    !$OMP  ao_expo_ordered_transp,dim1)
    do j=1,ao_num
    A_center(1) = nucl_coord( ao_nucl(j), 1 )
    A_center(2) = nucl_coord( ao_nucl(j), 2 )
    A_center(3) = nucl_coord( ao_nucl(j), 3 )
    power_A(1)  = ao_power( j, 1 )
    power_A(2)  = ao_power( j, 2 )
    power_A(3)  = ao_power( j, 3 )
    !DEC$ VECTOR ALIGNED
    !DEC$ VECTOR ALWAYS
    do i= 1,ao_num
      ao_overlap(i,j)= 0.d0
      ao_overlap_x(i,j)= 0.d0
      ao_overlap_y(i,j)= 0.d0
      ao_overlap_z(i,j)= 0.d0
      B_center(1) = nucl_coord( ao_nucl(i), 1 )
      B_center(2) = nucl_coord( ao_nucl(i), 2 )
      B_center(3) = nucl_coord( ao_nucl(i), 3 )
      power_B(1)  = ao_power( i, 1 )
      power_B(2)  = ao_power( i, 2 )
      power_B(3)  = ao_power( i, 3 )
      do n = 1,ao_prim_num(j)
      alpha = ao_expo_ordered_transp(n,j)
      !DEC$ VECTOR ALIGNED
      do l = 1, ao_prim_num(i)
        beta = ao_expo_ordered_transp(l,i)
        call overlap_gaussian_xyz(A_center,B_center,alpha,beta,power_A,power_B,overlap_x,overlap_y,overlap_z,overlap,dim1)
        c = ao_coef_normalized_ordered_transp(n,j) * ao_coef_normalized_ordered_transp(l,i)
        ao_overlap(i,j) += c * overlap
        ao_overlap_x(i,j) += c * overlap_x
        ao_overlap_y(i,j) += c * overlap_y
        ao_overlap_z(i,j) += c * overlap_z
      enddo
      enddo
    enddo
    enddo
    !$OMP END PARALLEL DO
!  endif
!  if (write_ao_one_integrals) then
!     call ezfio_set_ao_basis_integral_overlap(ao_overlap(1:ao_num, 1:ao_num))
!     print *,  'AO overlap integrals written to disk'
!  endif

END_PROVIDER


BEGIN_PROVIDER [ double precision, ao_overlap_abs,(ao_num_align,ao_num) ]
  implicit none
  BEGIN_DOC  
! Overlap between absolute value of atomic basis functions:
! :math:`\int |\chi_i(r)| |\chi_j(r)| dr)`
  END_DOC
  integer :: i,j,n,l
  double precision :: f
  integer :: dim1
  double precision :: overlap, overlap_x, overlap_y, overlap_z
  double precision :: alpha, beta
  double precision :: A_center(3), B_center(3)
  integer :: power_A(3), power_B(3)
  double precision :: lower_exp_val, dx
  dim1=100
  lower_exp_val = 40.d0
  !$OMP PARALLEL DO SCHEDULE(GUIDED) &
  !$OMP DEFAULT(NONE) &
  !$OMP PRIVATE(A_center,B_center,power_A,power_B,&
  !$OMP  overlap_x,overlap_y, overlap_z, overlap, &
  !$OMP  alpha, beta,i,j,dx) &
  !$OMP SHARED(nucl_coord,ao_power,ao_prim_num, &
  !$OMP  ao_overlap_abs,ao_num,ao_coef_normalized_ordered_transp,ao_nucl, &
  !$OMP  ao_expo_ordered_transp,dim1,lower_exp_val)
  do j=1,ao_num
   A_center(1) = nucl_coord( ao_nucl(j), 1 )
   A_center(2) = nucl_coord( ao_nucl(j), 2 )
   A_center(3) = nucl_coord( ao_nucl(j), 3 )
   power_A(1)  = ao_power( j, 1 )
   power_A(2)  = ao_power( j, 2 )
   power_A(3)  = ao_power( j, 3 )
   !DEC$ VECTOR ALIGNED
   !DEC$ VECTOR ALWAYS
   do i= 1,ao_num
    ao_overlap_abs(i,j)= 0.d0
    B_center(1) = nucl_coord( ao_nucl(i), 1 )
    B_center(2) = nucl_coord( ao_nucl(i), 2 )
    B_center(3) = nucl_coord( ao_nucl(i), 3 )
    power_B(1)  = ao_power( i, 1 )
    power_B(2)  = ao_power( i, 2 )
    power_B(3)  = ao_power( i, 3 )
    do n = 1,ao_prim_num(j)
     alpha = ao_expo_ordered_transp(n,j)
     !DEC$ VECTOR ALIGNED
     do l = 1, ao_prim_num(i)
      beta = ao_expo_ordered_transp(l,i)
      call overlap_x_abs(A_center(1),B_center(1),alpha,beta,power_A(1),power_B(1),overlap_x,lower_exp_val,dx,dim1)
      call overlap_x_abs(A_center(2),B_center(2),alpha,beta,power_A(2),power_B(2),overlap_y,lower_exp_val,dx,dim1)
      call overlap_x_abs(A_center(3),B_center(3),alpha,beta,power_A(3),power_B(3),overlap_z,lower_exp_val,dx,dim1)
      ao_overlap_abs(i,j) += abs(ao_coef_normalized_ordered_transp(n,j) * ao_coef_normalized_ordered_transp(l,i)) * overlap_x * overlap_y * overlap_z
     enddo
    enddo
   enddo
  enddo
  !$OMP END PARALLEL DO
END_PROVIDER


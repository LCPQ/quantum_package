BEGIN_PROVIDER [ double precision, sym_box, (3,2) ]
  implicit none
  BEGIN_DOC
  ! Opposite points of the box containing the molecule
  END_DOC
  integer                        :: i,xyz
  sym_box(:,:) = 0.d0
  do xyz=1,3
    do i=1,nucl_num
      sym_box(xyz,1) = min(sym_box(xyz,1), nucl_coord_sym(i,xyz))
      sym_box(xyz,2) = max(sym_box(xyz,2), nucl_coord_sym(i,xyz))
    enddo
  enddo
  sym_box(:,1) = sym_box(:,1) - 2.d0
  sym_box(:,2) = sym_box(:,2) + 2.d0
END_PROVIDER

subroutine generate_sym_coord(n_sym_points,result)
  implicit none
  integer, intent(in)            :: n_sym_points
  double precision, intent(out)  :: result(3,n_sym_points)
  BEGIN_DOC
  ! xyz coordinates of points to check the symmetry, drawn uniformly in the molecular box.
  END_DOC
  integer                        :: i, xyz
  
  do i=1,n_sym_points
    call random_number(result(1,i))
    call random_number(result(2,i))
    call random_number(result(3,i))
  enddo
  do xyz=1,3
    result(xyz,1:n_sym_points) = sym_box(xyz,1) + result(xyz,:) * (sym_box(xyz,2)-sym_box(xyz,1))
  enddo
  
end


subroutine compute_sym_ao_values(sym_points, n_sym_points, result)
  implicit none
  BEGIN_DOC
  ! Values of the AO symmetry functions
  END_DOC
  integer, intent(in)            :: n_sym_points
  double precision, intent(in)   :: sym_points(3,n_sym_points)
  double precision, intent(out)  :: result(n_sym_points, ao_num)
  integer                        :: i, j
  double precision               :: point(3)
  double precision               :: x, y, z
  double precision               :: x2, y2, z2
  integer                        :: k
  
  result (:,:) = 0.d0
  do j=1,ao_num
    do i=1,n_sym_points
      call point_to_input_orientation(sym_points(:,i), point)
      x = point(1) - nucl_coord_transp(1,ao_nucl(j))
      y = point(2) - nucl_coord_transp(2,ao_nucl(j))
      z = point(3) - nucl_coord_transp(3,ao_nucl(j))
      x2 = x*x + y*y + z*z
      result(i,j) = 0.d0
      do k=1,ao_prim_num(j)
        result(i,j) += ao_coef_normalized_ordered_transp(k,j)*exp(-ao_expo_ordered_transp(k,j)*x2)
      enddo
      x = x**ao_power(j,1)
      y = y**ao_power(j,2)
      z = z**ao_power(j,3)
      result(i,j) = x*y*z*result(i,j)
    enddo
  enddo
  
end

subroutine compute_sym_mo_values(sym_points, n_sym_points, result)
  implicit none
  BEGIN_DOC
  ! Values of the MO symmetry functions
  END_DOC
  integer, intent(in)            :: n_sym_points
  double precision, intent(in)   :: sym_points(3,n_sym_points)
  double precision, intent(out)  :: result(n_sym_points, mo_tot_num)

  double precision, allocatable :: tmp(:,:)
  allocate(tmp(n_sym_points,ao_num))
  call compute_sym_ao_values(sym_points,n_sym_points,tmp)
  call dgemm('N','N',n_sym_points,mo_tot_num,ao_num, &
    1.d0, tmp,size(tmp,1), mo_coef, size(mo_coef,1), &
    0.d0, result,size(result,1))
  deallocate(tmp)
end


subroutine compute_sym_det_values(sym_points, n_sym_points, result)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Values of the determinant symmetry functions
  END_DOC
  integer, intent(in)            :: n_sym_points
  double precision, intent(in)   :: sym_points(3,n_sym_points)
  double precision, intent(out)  :: result(n_sym_points, N_det)

  integer                        :: list(N_int*bit_kind_size,2)
  integer                        :: n_elements(2)

  integer :: i, j, imo

  double precision, allocatable :: tmp(:,:)

  allocate(tmp(n_sym_points,mo_tot_num))
  call compute_sym_mo_values(sym_points, n_sym_points, tmp)

  result = 1.d0
  do i=1,N_det
    call bitstring_to_list_ab(psi_det(1,1,i), list, n_elements, N_int)
    do j=1,n_elements(1)
      imo = list(j,1)
      result(:,i) *= tmp(:,imo)
    enddo
    do j=1,n_elements(2)
      imo = list(j,2)
      result(:,i) *= tmp(:,imo)
    enddo
  enddo

  deallocate(tmp)
end

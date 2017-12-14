program Symmetry
  implicit none
  BEGIN_DOC
! TODO
  END_DOC
  integer :: n_sym_points, i, j
  double precision, allocatable :: tmp(:,:), sym_points(:,:)
  character*8 :: sym
  n_sym_points = 3

  print *,  'Molecule is linear:', molecule_is_linear
  print *,  'Has center of inversion:', molecule_has_center_of_inversion
  print *,  'Symmetry rotation axis:', sym_rotation_axis(:)
  call find_symmetry(sym)
  print *,  'Group:'//sym
  return
  allocate(tmp(n_sym_points,n_det), sym_points(3,n_sym_points))

  call generate_sym_coord(n_sym_points,sym_points)
  call compute_sym_det_values(sym_points,n_sym_points,tmp)
  do i=1,mo_tot_num
    print *,  i, real(tmp(1:3,i))
  enddo
  sym_points(1,:) = -sym_points(1,:)
  call compute_sym_det_values(sym_points,n_sym_points,tmp)
  do i=1,n_det
    print *,  i, real(tmp(1:3,i))
  enddo

end

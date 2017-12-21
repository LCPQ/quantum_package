program Symmetry
  implicit none
  BEGIN_DOC
! TODO
  END_DOC
  integer :: i, j
  character*8 :: sym

  print *,  'Molecule is linear:  ', molecule_is_linear
  print *,  'Has center of inversion:  ', molecule_has_center_of_inversion
  print *,  'Has S2n improper rotation:  ', molecule_has_improper_rotation
  print *,  'Symmetry rotation axis:  ', sym_rotation_axis(:)
  print *,  'Group:  '//point_group
  print *,  'Symmetry irreps :  ', sym_irrep(1:n_irrep)
  print *,  'Symmetry operations :  ', sym_operation(1:n_irrep)
  print *,  'Character table'
  do i=1,n_irrep
    print *,  i, real(character_table(i,:))
  enddo
  PROVIDE mo_sym
end

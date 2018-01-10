program Symmetry
  implicit none
  BEGIN_DOC
! TODO
  END_DOC
  integer :: i, j, k
  character*8 :: sym

do k=1,n_irrep
  print *,  sym_operation(k)
  do i=1,mo_tot_num
    print '(1000(F8.4,X))', mo_symm(i,:,k), sum(mo_symm(i,:,k))
  enddo
  print *,  ''
enddo

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
end

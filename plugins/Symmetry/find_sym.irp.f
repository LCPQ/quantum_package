BEGIN_PROVIDER [ logical, molecule_is_linear ]
 implicit none
 BEGIN_DOC
 ! True if the molecule is linear
 END_DOC
 molecule_is_linear = (minval(inertia_tensor_eigenvalues) < 1.d-5)
END_PROVIDER

BEGIN_PROVIDER [ logical, molecule_has_center_of_inversion ]
  implicit none
  BEGIN_DOC
  ! If true, there is a center of inversion in the WF
  END_DOC
  molecule_has_center_of_inversion = .True.
  integer                        :: i,j,k
  double precision               :: point(3)
  logical                        :: found
  double precision, external     :: u_dot_u
  do i=1,nucl_num
    found = .False.
    do j=1,nucl_num
      if (nucl_charge(i) /= nucl_charge(j)) cycle
      point(:) = nucl_coord_sym_transp(:,i) + nucl_coord_sym_transp(:,j)
      if (u_dot_u(point,3) < 1.d-5) then
        found = .True.
        exit
      endif
    enddo
    if (.not.found) then
      molecule_has_center_of_inversion = .False.
      exit
    endif
  enddo

END_PROVIDER


BEGIN_PROVIDER [ integer, sym_rotation_axis, (3) ]
  implicit none
  BEGIN_DOC
  ! Order of the rotation axis
  END_DOC
  include 'constants.include.F'
  
  integer                        :: i,j,k
  double precision               :: point(3), point2(3)
  logical                        :: found
  double precision, external     :: u_dot_u
  integer                        :: iorder, iaxis

  do iaxis=1,3
    do iorder=12,2,-1
      sym_rotation_axis(iaxis) = iorder
      do i=1,nucl_num
        found = .False.
        call sym_apply_rotation(dble(iorder),iaxis,nucl_coord_sym_transp(1,i),point)
        do j=1,nucl_num
          if (nucl_charge(i) /= nucl_charge(j)) cycle
          point2(:) = nucl_coord_sym_transp(:,j) - point(:)
          if (u_dot_u(point2,3) < 1.d-5) then
            found = .True.
            exit
          endif
        enddo
        if (.not.found) then
          sym_rotation_axis(iaxis) = 1
          exit
        endif
      enddo
      if (sym_rotation_axis(iaxis) /= 1) then
        exit
      endif
    enddo
  enddo

END_PROVIDER

 BEGIN_PROVIDER [ integer, molecule_principal_axis ]
&BEGIN_PROVIDER [ integer, molecule_secondary_axis ]
&BEGIN_PROVIDER [ integer, molecule_ternary_axis ]
&BEGIN_PROVIDER [ logical, molecule_has_secondary_c2_rotation ]
  implicit none
  BEGIN_DOC
! Which axis is the Z axis
  END_DOC
  molecule_principal_axis = maxloc(sym_rotation_axis,1)
  if (molecule_principal_axis == 1) then
    if (sym_rotation_axis(2) > sym_rotation_axis(3)) then 
      molecule_secondary_axis = 2
      molecule_ternary_axis = 3
    else
      molecule_secondary_axis = 3
      molecule_ternary_axis = 2
    endif
  else if (molecule_principal_axis == 2) then
    if (sym_rotation_axis(1) > sym_rotation_axis(3)) then 
      molecule_secondary_axis = 1
      molecule_ternary_axis = 3
    else
      molecule_secondary_axis = 3
      molecule_ternary_axis = 1
    endif
  else if (molecule_principal_axis == 3) then
    if (sym_rotation_axis(1) > sym_rotation_axis(2)) then 
      molecule_secondary_axis = 1
      molecule_ternary_axis = 2
    else
      molecule_secondary_axis = 2
      molecule_ternary_axis = 1
    endif
  endif

  if (molecule_principal_axis == 1) then
    molecule_has_secondary_c2_rotation = (sym_rotation_axis(2)==2) .or. (sym_rotation_axis(3)==2)
  else if (molecule_principal_axis == 2) then
    molecule_has_secondary_c2_rotation = (sym_rotation_axis(1)==2) .or. (sym_rotation_axis(3)==2)
  else if (molecule_principal_axis == 3) then
    molecule_has_secondary_c2_rotation = (sym_rotation_axis(1)==2) .or. (sym_rotation_axis(2)==2)
  endif

  if (molecule_has_secondary_c2_rotation) then
    integer :: swap
    if ( (sym_rotation_axis(molecule_secondary_axis) /= 2).and. &
         (sym_rotation_axis(molecule_ternary_axis) == 2) ) then
      swap = molecule_secondary_axis
      molecule_secondary_axis = molecule_ternary_axis
      molecule_ternary_axis = swap
    endif
  endif

END_PROVIDER



BEGIN_PROVIDER [ logical, molecule_has_improper_rotation ]
  implicit none
  BEGIN_DOC
  ! Order of the rotation axis
  END_DOC
  include 'constants.include.F'
  
  integer                        :: i,j,k
  double precision               :: point(3), point2(3)
  logical                        :: found
  double precision, external     :: u_dot_u
  integer                        :: iorder, iaxis
  iaxis=molecule_principal_axis
  iorder = 2*sym_rotation_axis(iaxis)
  molecule_has_improper_rotation = .True.
  do i=1,nucl_num
    found = .False.
    call sym_apply_improper_rotation(dble(iorder),iaxis,nucl_coord_sym_transp(1,i),point)
    do j=1,nucl_num
      if (nucl_charge(i) /= nucl_charge(j)) cycle
      point2(:) = nucl_coord_sym_transp(:,j) - point(:)
      if (u_dot_u(point2,3) < 1.d-5) then
        found = .True.
        exit
      endif
    enddo
    if (.not.found) then
      molecule_has_improper_rotation = .False.
      exit
    endif
  enddo
  
END_PROVIDER

BEGIN_PROVIDER [ logical, molecule_has_center_of_inversion ]
  implicit none
  BEGIN_DOC
  ! If true, there is a center of inversion in the WF
  END_DOC
  molecule_has_center_of_inversion = .True.
  integer                        :: i,j,k
  double precision               :: point(3)
  logical                        :: found
  double precision, external     :: u_dot_u
  do i=1,nucl_num
    found = .False.
    do j=1,nucl_num
      if (nucl_charge(i) /= nucl_charge(j)) cycle
      point(:) = nucl_coord_sym_transp(:,i) + nucl_coord_sym_transp(:,j)
      if (u_dot_u(point,3) < 1.d-5) then
        found = .True.
        exit
      endif
    enddo
    if (.not.found) then
      molecule_has_center_of_inversion = .False.
      exit
    endif
  enddo

END_PROVIDER


BEGIN_PROVIDER [ logical, molecule_has_sigma_plane, (3) ]
  implicit none
  BEGIN_DOC
  ! If true, there is a symmetry plane perpendicular to the main axis
  END_DOC
  integer                        :: i,j,k
  double precision               :: point(3), point2(3)
  logical                        :: found
  double precision, external     :: u_dot_u
  integer                        :: iaxis
  do iaxis=1,3
    molecule_has_sigma_plane(iaxis) = .True.
    do i=1,nucl_num
      found = .False.
      point(:) = nucl_coord_sym_transp(:,i)
      point(iaxis) = -point(iaxis)
      do j=1,nucl_num
        if (nucl_charge(i) /= nucl_charge(j)) cycle
        point2(:) = nucl_coord_sym_transp(:,j) - point(:)
        if (u_dot_u(point2,3) < 1.d-5) then
          found = .True.
          exit
        endif
      enddo
      if (.not.found) then
        molecule_has_sigma_plane(iaxis) = .False.
        exit
      endif
    enddo
  enddo

END_PROVIDER

BEGIN_PROVIDER [ character*16, point_group ]
 implicit none
 BEGIN_DOC
! Point group of the molecule
 END_DOC

  character*2, save  :: i_to_a(24) = (/ '1 ', '2 ', '3 ', '4 ', '5 ', '6 ', '7 ', '8 ', '9 ', &
      '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', &
      '21', '22', '23', '24'  /)
  point_group = 'C1'
  if (molecule_is_linear) then
    if (molecule_has_center_of_inversion) then
      point_group = 'Dinfh'
    else
      point_group = 'Cinfv'
    endif
  else
    if (maxval(sym_rotation_axis) == 1) then
      if (molecule_has_sigma_plane(1).or.molecule_has_sigma_plane(2).or.&
            molecule_has_sigma_plane(3) ) then
        point_group = 'Cs'
      else
        if (molecule_has_center_of_inversion) then
          point_group = 'Ci'
        endif
      endif
    else
      if (molecule_has_secondary_c2_rotation) then
        if (molecule_has_sigma_plane(molecule_principal_axis)) then
          point_group = 'D'//trim(i_to_a(sym_rotation_axis(molecule_principal_axis)))//'h'
        else
          if (molecule_has_sigma_plane(molecule_secondary_axis)) then
            point_group = 'D'//trim(i_to_a(sym_rotation_axis(molecule_principal_axis)))//'d'
          else
            if (molecule_has_sigma_plane(molecule_ternary_axis)) then
              point_group = 'D'//trim(i_to_a(sym_rotation_axis(molecule_principal_axis)))//'d'
            else
              if ( (sym_rotation_axis(1) == 2).and. &
                   (sym_rotation_axis(2) == 2).and. &
                   (sym_rotation_axis(3) == 2).and. &
                   (inertia_tensor_eigenvalues(1) == inertia_tensor_eigenvalues(2)).and. &
                   (inertia_tensor_eigenvalues(1) == inertia_tensor_eigenvalues(3)) ) then
                point_group = 'Td'
              else
                point_group = 'D'//trim(i_to_a(sym_rotation_axis(molecule_principal_axis)))
              endif
            endif
          endif
        endif
      else
        if (molecule_has_sigma_plane(molecule_principal_axis)) then
          point_group = 'C'//trim(i_to_a(sym_rotation_axis(molecule_principal_axis)))//'h'
        else
          if (molecule_has_sigma_plane(molecule_secondary_axis)) then
            point_group = 'C'//trim(i_to_a(sym_rotation_axis(molecule_principal_axis)))//'v'
          else
            if (molecule_has_sigma_plane(molecule_ternary_axis)) then
              point_group = 'C'//trim(i_to_a(sym_rotation_axis(molecule_principal_axis)))//'v'
            else
              if (molecule_has_improper_rotation) then
                point_group = 'S'//trim(i_to_a(2*sym_rotation_axis(molecule_principal_axis)))
              else
                point_group = 'C'//trim(i_to_a(sym_rotation_axis(molecule_principal_axis)))
              endif
            endif
          endif
        endif
      endif
    endif
  endif
END_PROVIDER



BEGIN_PROVIDER [ integer, n_irrep ]
 implicit none
 BEGIN_DOC
 ! Number of Irreducible representations
 END_DOC
 integer                        :: iunit, n, i
 character*(256)                :: qproot, buffer
 integer, external              :: getUnitAndOpen
 call getenv('QP_ROOT',qproot)
 iunit = getUnitAndOpen(trim(qproot)//'/data/Symmetry/'//trim(point_group),'r')
 read(iunit,*) ! 1st line
 read(iunit,*) buffer, n_irrep
 close(iunit)
END_PROVIDER

 BEGIN_PROVIDER [ character*8, sym_irrep, (n_irrep) ]
&BEGIN_PROVIDER [ character*8, sym_operation, (n_irrep) ]
&BEGIN_PROVIDER [ double precision, character_table, (n_irrep,n_irrep) ]
 implicit none
 BEGIN_DOC
 ! Irreducible representation labels, labels of symmetry operations and
 ! Character table : columns are sym operations and lines are Irreps
 END_DOC
 integer                        :: iunit, n, i
 character*(256)                :: qproot, buffer
 integer, external              :: getUnitAndOpen
 call getenv('QP_ROOT',qproot)
 iunit = getUnitAndOpen(trim(qproot)//'/data/Symmetry/'//trim(point_group),'r')
 buffer = ''
 read(iunit,*) ! 1st line
 read(iunit,*) buffer, n
 read(iunit,*) ! empty line
 read(iunit,*) ! Irred Operation
 do i=1,n
   read(iunit,*) buffer, sym_irrep(i), sym_operation(i)
 enddo
 read(iunit,*) ! empty line
 read(iunit,*) ! Table
 read(iunit,*) ! 1 2 3 ...
 do i=1,n
  read(iunit,*) buffer, character_table(i,1:n)
 enddo
 close(iunit)
END_PROVIDER


BEGIN_PROVIDER [ integer, mo_sym, (mo_tot_num) ]
  implicit none
  BEGIN_DOC
  ! Symmetry operations applied on MOs
  END_DOC
  
  double precision, allocatable  :: sym_points(:,:), ref_points(:,:)
  double precision, allocatable  :: val(:,:,:)
  integer                        :: iangle, n_sym_points
  double precision               :: angle
  integer                        :: iop, imo, ipoint, l, i
  double precision               :: sym_operations_on_mos(mo_tot_num)
  logical                        :: possible_irrep(n_irrep,mo_tot_num)

  do imo=1,mo_tot_num
    print *,  'MO ', imo
    do i=1,n_irrep
      if (possible_irrep(i,imo)) then
        print *,  sym_irrep(i)
      endif
    enddo
    print *,  ''
  enddo

END_PROVIDER


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
      point(:) = nucl_coord_transp(:,i) + nucl_coord_transp(:,j)
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
  double precision               :: theta, sin_t, cos_t
  do iaxis=1,3
    do iorder=12,2,-1
      sym_rotation_axis(iaxis) = iorder
      theta = 2.d0*pi/dble(iorder)
      sin_t = dsin(theta)
      cos_t = dcos(theta)
      do i=1,nucl_num
        found = .False.
        if (iaxis==1) then
          point(1) = nucl_coord_transp(1,i)
          point(2) = nucl_coord_transp(2,i)*cos_t - nucl_coord_transp(3,i)*sin_t
          point(3) = nucl_coord_transp(2,i)*sin_t + nucl_coord_transp(3,i)*cos_t
        else if (iaxis==2) then
          point(1) = nucl_coord_transp(1,i)*cos_t - nucl_coord_transp(3,i)*sin_t
          point(2) = nucl_coord_transp(2,i)
          point(3) = nucl_coord_transp(1,i)*sin_t + nucl_coord_transp(3,i)*cos_t
        endif
        if (iaxis==3) then
          point(1) = nucl_coord_transp(1,i)*cos_t - nucl_coord_transp(2,i)*sin_t
          point(2) = nucl_coord_transp(1,i)*sin_t + nucl_coord_transp(2,i)*cos_t
          point(3) = nucl_coord_transp(3,i)
        endif
        do j=1,nucl_num
          if (nucl_charge(i) /= nucl_charge(j)) cycle
          point2(:) = nucl_coord_transp(:,j) - point(:)
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
  implicit none
  BEGIN_DOC
! Which axis is the Z axis
  END_DOC
  molecule_principal_axis = maxloc(sym_rotation_axis,1)
  if (molecule_principal_axis == 1) then
    if (sym_rotation_axis(2) >= sym_rotation_axis(3)) then 
      molecule_secondary_axis = 2
      molecule_ternary_axis = 3
    else
      molecule_secondary_axis = 3
      molecule_ternary_axis = 2
    endif
  else if (molecule_principal_axis == 2) then
    if (sym_rotation_axis(1) >= sym_rotation_axis(3)) then 
      molecule_secondary_axis = 1
      molecule_ternary_axis = 3
    else
      molecule_secondary_axis = 3
      molecule_ternary_axis = 1
    endif
  else if (molecule_principal_axis == 3) then
    if (sym_rotation_axis(1) >= sym_rotation_axis(2)) then 
      molecule_secondary_axis = 1
      molecule_ternary_axis = 2
    else
      molecule_secondary_axis = 2
      molecule_ternary_axis = 1
    endif
  endif
END_PROVIDER

BEGIN_PROVIDER [ logical, molecule_has_secondary_c2_rotation ]
  implicit none
  BEGIN_DOC
! If true, the molecule has a C2 rotation perpendicular to the main axis
  END_DOC
  if (molecule_principal_axis == 1) then
    molecule_has_secondary_c2_rotation = (sym_rotation_axis(2)==2) .or. (sym_rotation_axis(3)==2)
  else if (molecule_principal_axis == 2) then
    molecule_has_secondary_c2_rotation = (sym_rotation_axis(1)==2) .or. (sym_rotation_axis(3)==2)
  else if (molecule_principal_axis == 3) then
    molecule_has_secondary_c2_rotation = (sym_rotation_axis(1)==2) .or. (sym_rotation_axis(2)==2)
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
  double precision               :: theta, sin_t, cos_t
  iaxis=molecule_principal_axis
  iorder = 2*sym_rotation_axis(iaxis)
  molecule_has_improper_rotation = .True.
  theta = 2.d0*pi/dble(iorder)
  sin_t = dsin(theta)
  cos_t = dcos(theta)
  do i=1,nucl_num
    found = .False.
    if (iaxis==1) then
      point(1) = -nucl_coord_transp(1,i)
      point(2) = nucl_coord_transp(2,i)*cos_t - nucl_coord_transp(3,i)*sin_t
      point(3) = nucl_coord_transp(2,i)*sin_t + nucl_coord_transp(3,i)*cos_t
    else if (iaxis==2) then
      point(1) = nucl_coord_transp(1,i)*cos_t - nucl_coord_transp(3,i)*sin_t
      point(2) = -nucl_coord_transp(2,i)
      point(3) = nucl_coord_transp(1,i)*sin_t + nucl_coord_transp(3,i)*cos_t
    endif
    if (iaxis==3) then
      point(1) = nucl_coord_transp(1,i)*cos_t - nucl_coord_transp(2,i)*sin_t
      point(2) = nucl_coord_transp(1,i)*sin_t + nucl_coord_transp(2,i)*cos_t
      point(3) = -nucl_coord_transp(3,i)
    endif
    do j=1,nucl_num
      if (nucl_charge(i) /= nucl_charge(j)) cycle
      point2(:) = nucl_coord_transp(:,j) - point(:)
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
      point(:) = nucl_coord_transp(:,i) + nucl_coord_transp(:,j)
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
      point(:) = nucl_coord_transp(:,i)
      point(iaxis) = -point(iaxis)
      do j=1,nucl_num
        if (nucl_charge(i) /= nucl_charge(j)) cycle
        point2(:) = nucl_coord_transp(:,j) - point(:)
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

subroutine find_symmetry(result)
  implicit none
  BEGIN_DOC
! Finds the point group of the molecule
  END_DOC
  character*16       :: result
  character*2, save  :: i_to_a(24) = (/ '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12',&
      '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24'  /)
  result = 'C1'
  if (molecule_is_linear) then
    if (molecule_has_center_of_inversion) then
      result = 'D*h'
    else
      result = 'C*v'
    endif
  else
    if (maxval(sym_rotation_axis) == 1) then
      if (molecule_has_sigma_plane(1).or.molecule_has_sigma_plane(2).or.&
            molecule_has_sigma_plane(3) ) then
        result = 'Cs'
      else
        if (molecule_has_center_of_inversion) then
          result = 'Ci'
        endif
      endif
    else
      if (molecule_has_secondary_c2_rotation) then
        if (molecule_has_sigma_plane(molecule_principal_axis)) then
          result = 'D'//trim(i_to_a(sym_rotation_axis(molecule_principal_axis)))//'h'
        else
          if (molecule_has_sigma_plane(molecule_secondary_axis)) then
            result = 'D'//trim(i_to_a(sym_rotation_axis(molecule_principal_axis)))//'d'
          else
            if (molecule_has_sigma_plane(molecule_ternary_axis)) then
              result = 'D'//trim(i_to_a(sym_rotation_axis(molecule_principal_axis)))//'d'
            else
              result = 'D'//trim(i_to_a(sym_rotation_axis(molecule_principal_axis)))
            endif
          endif
        endif
      else
        if (molecule_has_sigma_plane(molecule_principal_axis)) then
          result = 'C'//trim(i_to_a(sym_rotation_axis(molecule_principal_axis)))//'h'
        else
          if (molecule_has_sigma_plane(molecule_secondary_axis)) then
            result = 'C'//trim(i_to_a(sym_rotation_axis(molecule_principal_axis)))//'v'
          else
            if (molecule_has_sigma_plane(molecule_ternary_axis)) then
              result = 'C'//trim(i_to_a(sym_rotation_axis(molecule_principal_axis)))//'v'
            else
              if (molecule_has_improper_rotation) then
                result = 'S'//trim(i_to_a(2*sym_rotation_axis(molecule_principal_axis)))
              else
                result = 'C'//trim(i_to_a(sym_rotation_axis(molecule_principal_axis)))
              endif
            endif
          endif
        endif
      endif
    endif
  endif
end

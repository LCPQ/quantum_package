subroutine point_to_standard_orientation(point_in,point_out)
  implicit none
  double precision, intent(in)   :: point_in(3)
  double precision, intent(out)  :: point_out(3)
  BEGIN_DOC
  ! Returns the coordinates of a point in the standard orientation
  END_DOC
  double precision :: point_tmp(3)

  point_tmp(1) = point_in(1) - center_of_mass(1)
  point_tmp(2) = point_in(2) - center_of_mass(2)
  point_tmp(3) = point_in(3) - center_of_mass(3)

  point_out(1) = point_tmp(1)*inertia_tensor_eigenvectors(1,1) +     &
                 point_tmp(2)*inertia_tensor_eigenvectors(2,1) +     &
                 point_tmp(3)*inertia_tensor_eigenvectors(3,1)

  point_out(2) = point_tmp(1)*inertia_tensor_eigenvectors(1,2) +     &
                 point_tmp(2)*inertia_tensor_eigenvectors(2,2) +     &
                 point_tmp(3)*inertia_tensor_eigenvectors(3,2)

  point_out(3) = point_tmp(1)*inertia_tensor_eigenvectors(1,3) +     &
                 point_tmp(2)*inertia_tensor_eigenvectors(2,3) +     &
                 point_tmp(3)*inertia_tensor_eigenvectors(3,3)
  
end

subroutine point_to_input_orientation(point_in,point_out)
  implicit none
  double precision, intent(in)   :: point_in(3)
  double precision, intent(out)  :: point_out(3)
  BEGIN_DOC
  ! Returns the coordinates of a point in the input orientation
  END_DOC
  double precision :: point_tmp(3)

  point_tmp(1) = point_in(1)*inertia_tensor_eigenvectors(1,1) +     &
                 point_in(2)*inertia_tensor_eigenvectors(1,2) +     &
                 point_in(3)*inertia_tensor_eigenvectors(1,3)

  point_tmp(2) = point_in(1)*inertia_tensor_eigenvectors(2,1) +     &
                 point_in(2)*inertia_tensor_eigenvectors(2,2) +     &
                 point_in(3)*inertia_tensor_eigenvectors(2,3)

  point_tmp(3) = point_in(1)*inertia_tensor_eigenvectors(3,1) +     &
                 point_in(2)*inertia_tensor_eigenvectors(3,2) +     &
                 point_in(3)*inertia_tensor_eigenvectors(3,3)
  
  point_out(1) = point_tmp(1) + center_of_mass(1)
  point_out(2) = point_tmp(2) + center_of_mass(2)
  point_out(3) = point_tmp(3) + center_of_mass(3)

end

BEGIN_PROVIDER [ double precision, nucl_coord_sym,  (nucl_num,3) ]
   implicit none
   
   BEGIN_DOC
   ! Nuclear coordinates in standard orientation
   END_DOC
   
   if (mpi_master) then
     integer :: i
     do i=1,nucl_num
       call point_to_standard_orientation(nucl_coord(i,:), nucl_coord_sym(i,:))
     enddo
     
     character*(64), parameter      :: f = '(A16, 4(1X,F12.6))'
     character*(64), parameter      :: ft= '(A16, 4(1X,A12  ))'
     double precision, parameter    :: a0= 0.529177249d0
     
     call write_time(6)
     write(6,'(A)') ''
     write(6,'(A)') 'Nuclear Coordinates in standard orientation (Angstroms)'
     write(6,'(A)') '======================================================='
     write(6,'(A)') ''
     write(6,ft)                                         &
         '================','============','============','============','============'
     write(6,*)                                          &
         '     Atom          Charge          X            Y            Z '
     write(6,ft)                                         &
         '================','============','============','============','============'
     do i=1,nucl_num
       write(6,f) nucl_label(i), nucl_charge(i),         &
           nucl_coord_sym(i,1)*a0,                                       &
           nucl_coord_sym(i,2)*a0,                                       &
           nucl_coord_sym(i,3)*a0
     enddo
     write(6,ft)                                         &
         '================','============','============','============','============'
     write(6,'(A)') ''
     
   endif
   
   IRP_IF MPI
     include 'mpif.h'
     integer                        :: ierr
     call MPI_BCAST( nucl_coord_sym, 3*nucl_num, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
     if (ierr /= MPI_SUCCESS) then
       stop 'Unable to read nucl_coord_sym with MPI'
     endif
   IRP_ENDIF

END_PROVIDER
 
 
BEGIN_PROVIDER [ double precision, nucl_coord_sym_transp, (3,nucl_num) ]
   implicit none
   BEGIN_DOC
   ! Transposed array of nucl_coord
   END_DOC
   integer                        :: i, k
   nucl_coord_sym_transp = 0.d0
   
   do i=1,nucl_num
     nucl_coord_sym_transp(1,i) = nucl_coord_sym(i,1)
     nucl_coord_sym_transp(2,i) = nucl_coord_sym(i,2)
     nucl_coord_sym_transp(3,i) = nucl_coord_sym(i,3)
   enddo
END_PROVIDER




BEGIN_PROVIDER [ double precision, nucl_coord_sym,  (nucl_num,3) ]
   implicit none
   
   BEGIN_DOC
   ! Nuclear coordinates in standard orientation
   END_DOC
   
   if (mpi_master) then
     integer :: i
     do i=1,nucl_num
       nucl_coord_sym(i,1) = (nucl_coord(i,1) - center_of_mass(1))*inertia_tensor_eigenvectors(1,1) + &
                         (nucl_coord(i,2) - center_of_mass(2))*inertia_tensor_eigenvectors(2,1) + &
                         (nucl_coord(i,3) - center_of_mass(3))*inertia_tensor_eigenvectors(3,1) 
       nucl_coord_sym(i,2) = (nucl_coord(i,1) - center_of_mass(1))*inertia_tensor_eigenvectors(1,2) + &
                         (nucl_coord(i,2) - center_of_mass(2))*inertia_tensor_eigenvectors(2,2) + &
                         (nucl_coord(i,3) - center_of_mass(3))*inertia_tensor_eigenvectors(3,2) 
       nucl_coord_sym(i,3) = (nucl_coord(i,1) - center_of_mass(1))*inertia_tensor_eigenvectors(1,3) + &
                         (nucl_coord(i,2) - center_of_mass(2))*inertia_tensor_eigenvectors(2,3) + &
                         (nucl_coord(i,3) - center_of_mass(3))*inertia_tensor_eigenvectors(3,3) 
     enddo
     
     character*(64), parameter      :: f = '(A16, 4(1X,F12.6))'
     character*(64), parameter      :: ft= '(A16, 4(1X,A12  ))'
     double precision, parameter    :: a0= 0.529177249d0
     
     call write_time(output_Nuclei)
     write(output_Nuclei,'(A)') ''
     write(output_Nuclei,'(A)') 'Nuclear Coordinates in standard orientation (Angstroms)'
     write(output_Nuclei,'(A)') '======================================================='
     write(output_Nuclei,'(A)') ''
     write(output_Nuclei,ft)                                         &
         '================','============','============','============','============'
     write(output_Nuclei,*)                                          &
         '     Atom          Charge          X            Y            Z '
     write(output_Nuclei,ft)                                         &
         '================','============','============','============','============'
     do i=1,nucl_num
       write(output_Nuclei,f) nucl_label(i), nucl_charge(i),         &
           nucl_coord_sym(i,1)*a0,                                       &
           nucl_coord_sym(i,2)*a0,                                       &
           nucl_coord_sym(i,3)*a0
     enddo
     write(output_Nuclei,ft)                                         &
         '================','============','============','============','============'
     write(output_Nuclei,'(A)') ''
     
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

BEGIN_PROVIDER [ double precision, sym_molecule_rotation, (3,3) ]
 implicit none
 BEGIN_DOC
 ! Rotation of the molecule to go from input orientation to standard orientation
 END_DOC
 call find_rotation(nucl_coord, size(nucl_coord,1), nucl_coord_sym, 3, sym_molecule_rotation, 3)
END_PROVIDER

BEGIN_PROVIDER [ double precision, sym_molecule_rotation_inv, (3,3) ]
 implicit none
 BEGIN_DOC
 ! Rotation of the molecule to go from standard orientation to input orientation
 END_DOC
 call find_rotation(nucl_coord_sym, size(nucl_coord_sym,1), nucl_coord, 3, sym_molecule_rotation_inv, 3)
END_PROVIDER


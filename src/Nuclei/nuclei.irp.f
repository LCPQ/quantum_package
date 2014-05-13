 BEGIN_PROVIDER [ integer, nucl_num ]
&BEGIN_PROVIDER [ integer, nucl_num_aligned ]
   implicit none
   BEGIN_DOC
   ! Number of nuclei
   END_DOC
   
   PROVIDE ezfio_filename
   nucl_num = 0
   logical                        :: has
   call ezfio_has_nuclei_nucl_num(has)
   if (has) then
     call ezfio_get_nuclei_nucl_num(nucl_num)
   else
     print *, irp_here
     stop 1
   endif
   ASSERT (nucl_num > 0)
   integer                        :: align_double
   nucl_num_aligned = align_double(nucl_num)
END_PROVIDER
 
BEGIN_PROVIDER [ double precision, nucl_charge, (nucl_num) ]
   implicit none
   BEGIN_DOC
   ! Nuclear charges
   END_DOC
   PROVIDE ezfio_filename
   nucl_charge = -1.d0
   logical                        :: has
   call ezfio_has_nuclei_nucl_charge(has)
   if (has) then
     call ezfio_get_nuclei_nucl_charge(nucl_charge)
   else
     print *, irp_here
     stop 1
   endif
   ASSERT (minval(nucl_charge) >= 0.d0)
END_PROVIDER
 
BEGIN_PROVIDER [ character*(32), nucl_label, (nucl_num) ]
   implicit none
   BEGIN_DOC
   ! Nuclear labels
   END_DOC
   PROVIDE ezfio_filename
   nucl_label = ""
   logical                        :: has
   call ezfio_has_nuclei_nucl_label(has)
   if (has) then
     call ezfio_get_nuclei_nucl_label(nucl_label)
   endif
END_PROVIDER
 
BEGIN_PROVIDER [ double precision, nucl_coord,  (nucl_num_aligned,3) ]
   implicit none
   
   BEGIN_DOC
   ! Nuclear coordinates in the format (:, {x,y,z})
   END_DOC
   PROVIDE ezfio_filename
   
   double precision, allocatable  :: buffer(:,:)
   nucl_coord = 0.d0
   allocate (buffer(nucl_num,3))
   buffer = 0.d0
   logical                        :: has
   call ezfio_has_nuclei_nucl_coord(has)
   if (.not.has) then
     print *, irp_here
     stop 1
   endif
   call ezfio_get_nuclei_nucl_coord(buffer)
   integer                        :: i,j
   
   do i=1,3
     do j=1,nucl_num
       nucl_coord(j,i) = buffer(j,i)
     enddo
   enddo
   deallocate(buffer)
   
   character*(64), parameter      :: f = '(A16, 4(X,F12.6))'
   character*(64), parameter      :: ft= '(A16, 4(X,A12  ))'
   double precision, parameter    :: a0= 0.529177249d0
   call write_time(output_Nuclei)
   write(output_Nuclei,'(A)') ''
   write(output_Nuclei,'(A)') 'Nuclear Coordinates (Angstroms)'
   write(output_Nuclei,'(A)') '==============================='
   write(output_Nuclei,'(A)') ''
   write(output_Nuclei,ft)                                           &
       '================','============','============','============','============'
   write(output_Nuclei,*)                                            &
       '     Atom          Charge          X            Y            Z '
   write(output_Nuclei,ft)                                           &
       '================','============','============','============','============'
   do i=1,nucl_num
     write(output_Nuclei,f) nucl_label(i), nucl_charge(i),           &
         nucl_coord(i,1)*a0,                                         &
         nucl_coord(i,2)*a0,                                         &
         nucl_coord(i,3)*a0
   enddo
   write(output_Nuclei,ft)                                           &
       '================','============','============','============','============'
   write(output_Nuclei,'(A)') ''
   
END_PROVIDER
 
 
BEGIN_PROVIDER [ double precision, nucl_coord_transp, (3,nucl_num) ]
   implicit none
   BEGIN_DOC
   ! Transposed array of nucl_coord
   END_DOC
   integer                        :: i, k
   nucl_coord_transp = 0.d0
   
   do i=1,nucl_num
     nucl_coord_transp(1,i) = nucl_coord(i,1)
     nucl_coord_transp(2,i) = nucl_coord(i,2)
     nucl_coord_transp(3,i) = nucl_coord(i,3)
   enddo
END_PROVIDER
 
 BEGIN_PROVIDER [ double precision, nucl_dist_2, (nucl_num_aligned,nucl_num) ]
&BEGIN_PROVIDER [ double precision, nucl_dist_vec_x, (nucl_num_aligned,nucl_num) ]
&BEGIN_PROVIDER [ double precision, nucl_dist_vec_y, (nucl_num_aligned,nucl_num) ]
&BEGIN_PROVIDER [ double precision, nucl_dist_vec_z, (nucl_num_aligned,nucl_num) ]
&BEGIN_PROVIDER [ double precision, nucl_dist, (nucl_num_aligned,nucl_num) ]
   implicit none
   BEGIN_DOC
   ! nucl_dist     : Nucleus-nucleus distances
   
   ! nucl_dist_2   : Nucleus-nucleus distances squared
   
   ! nucl_dist_vec : Nucleus-nucleus distances vectors
   END_DOC
   
   integer                        :: ie1, ie2, l
   integer,save                   :: ifirst = 0
   if (ifirst == 0) then
     ifirst = 1
     nucl_dist = 0.d0
     nucl_dist_2 = 0.d0
     nucl_dist_vec_x = 0.d0
     nucl_dist_vec_y = 0.d0
     nucl_dist_vec_z = 0.d0
   endif
   
   do ie2 = 1,nucl_num
     !DEC$ VECTOR ALWAYS
     !DEC$ VECTOR ALIGNED
     do ie1 = 1,nucl_num_aligned
       nucl_dist_vec_x(ie1,ie2) = nucl_coord(ie1,1) - nucl_coord(ie2,1)
       nucl_dist_vec_y(ie1,ie2) = nucl_coord(ie1,2) - nucl_coord(ie2,2)
       nucl_dist_vec_z(ie1,ie2) = nucl_coord(ie1,3) - nucl_coord(ie2,3)
     enddo
     !DEC$ VECTOR ALWAYS
     !DEC$ VECTOR ALIGNED
     do ie1 = 1,nucl_num_aligned
       nucl_dist_2(ie1,ie2) = nucl_dist_vec_x(ie1,ie2)*nucl_dist_vec_x(ie1,ie2) +&
           nucl_dist_vec_y(ie1,ie2)*nucl_dist_vec_y(ie1,ie2) +       &
           nucl_dist_vec_z(ie1,ie2)*nucl_dist_vec_z(ie1,ie2)
       nucl_dist(ie1,ie2) = sqrt(nucl_dist_2(ie1,ie2))
       ASSERT (nucl_dist(ie1,ie2) > 0.d0)
     enddo
   enddo
   
END_PROVIDER
 
BEGIN_PROVIDER [ double precision, nuclear_repulsion ]
   implicit none
   BEGIN_DOC
   ! Nuclear repulsion energy
   END_DOC
   integer                        :: k,l
   double precision               :: Z12, r2, x(3)
   nuclear_repulsion = 0.d0
   do l = 1, nucl_num
     do  k = 1, nucl_num
       if(k /= l) then
         Z12 = nucl_charge(k)*nucl_charge(l)
         x(1) = nucl_coord(k,1) - nucl_coord(l,1)
         x(2) = nucl_coord(k,2) - nucl_coord(l,2)
         x(3) = nucl_coord(k,3) - nucl_coord(l,3)
         r2 = x(1)*x(1) + x(2)*x(2) + x(3)*x(3)
         nuclear_repulsion += Z12/dsqrt(r2)
       endif
     enddo
   enddo
   nuclear_repulsion *= 0.5d0
   
   call write_time(output_Nuclei)
   call write_double(output_Nuclei,nuclear_repulsion,                &
       'Nuclear repulsion energy')
END_PROVIDER
 
 

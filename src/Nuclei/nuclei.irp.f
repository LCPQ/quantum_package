BEGIN_PROVIDER [ double precision, nucl_coord_input,  (nucl_num,3) ]
   implicit none
   
   BEGIN_DOC
   ! Nuclear coordinates in the format (:, {x,y,z})
   END_DOC
   PROVIDE ezfio_filename nucl_label nucl_charge
   
   if (mpi_master) then
     double precision, allocatable  :: buffer(:,:)
     nucl_coord_input = 0.d0
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
         nucl_coord_input(j,i) = buffer(j,i)
       enddo
     enddo
     deallocate(buffer)
     
     character*(64), parameter      :: f = '(A16, 4(1X,F12.6))'
     character*(64), parameter      :: ft= '(A16, 4(1X,A12  ))'
     double precision, parameter    :: a0= 0.529177249d0
     
     call write_time(output_Nuclei)
     write(output_Nuclei,'(A)') ''
     write(output_Nuclei,'(A)') 'Input Nuclear Coordinates (Angstroms)'
     write(output_Nuclei,'(A)') '====================================='
     write(output_Nuclei,'(A)') ''
     write(output_Nuclei,ft)                                         &
         '================','============','============','============','============'
     write(output_Nuclei,*)                                          &
         '     Atom          Charge          X            Y            Z '
     write(output_Nuclei,ft)                                         &
         '================','============','============','============','============'
     do i=1,nucl_num
       write(output_Nuclei,f) nucl_label(i), nucl_charge(i),         &
           nucl_coord_input(i,1)*a0,                                       &
           nucl_coord_input(i,2)*a0,                                       &
           nucl_coord_input(i,3)*a0
     enddo
     write(output_Nuclei,ft)                                         &
         '================','============','============','============','============'
     write(output_Nuclei,'(A)') ''
     
   endif
   
   IRP_IF MPI
     include 'mpif.h'
     integer                        :: ierr
     call MPI_BCAST( nucl_coord_input, 3*nucl_num, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
     if (ierr /= MPI_SUCCESS) then
       stop 'Unable to read nucl_coord_input with MPI'
     endif
   IRP_ENDIF

END_PROVIDER
 
BEGIN_PROVIDER [ double precision, nucl_coord,  (nucl_num,3) ]
   implicit none
   
   BEGIN_DOC
   ! Nuclear coordinates in standard orientation
   END_DOC
   
   if (mpi_master) then
     integer :: i
     do i=1,nucl_num
       nucl_coord(i,1) = (nucl_coord_input(i,1) - center_of_mass(1))*inertia_tensor_eigenvectors(1,1) + &
                         (nucl_coord_input(i,2) - center_of_mass(2))*inertia_tensor_eigenvectors(2,1) + &
                         (nucl_coord_input(i,3) - center_of_mass(3))*inertia_tensor_eigenvectors(3,1) 
       nucl_coord(i,2) = (nucl_coord_input(i,1) - center_of_mass(1))*inertia_tensor_eigenvectors(1,2) + &
                         (nucl_coord_input(i,2) - center_of_mass(2))*inertia_tensor_eigenvectors(2,2) + &
                         (nucl_coord_input(i,3) - center_of_mass(3))*inertia_tensor_eigenvectors(3,2) 
       nucl_coord(i,3) = (nucl_coord_input(i,1) - center_of_mass(1))*inertia_tensor_eigenvectors(1,3) + &
                         (nucl_coord_input(i,2) - center_of_mass(2))*inertia_tensor_eigenvectors(2,3) + &
                         (nucl_coord_input(i,3) - center_of_mass(3))*inertia_tensor_eigenvectors(3,3) 
     enddo
     
     character*(64), parameter      :: f = '(A16, 4(1X,F12.6))'
     character*(64), parameter      :: ft= '(A16, 4(1X,A12  ))'
     double precision, parameter    :: a0= 0.529177249d0
     
     call write_time(output_Nuclei)
     write(output_Nuclei,'(A)') ''
     write(output_Nuclei,'(A)') 'Nuclear Coordinates (Angstroms)'
     write(output_Nuclei,'(A)') '==============================='
     write(output_Nuclei,'(A)') ''
     write(output_Nuclei,ft)                                         &
         '================','============','============','============','============'
     write(output_Nuclei,*)                                          &
         '     Atom          Charge          X            Y            Z '
     write(output_Nuclei,ft)                                         &
         '================','============','============','============','============'
     do i=1,nucl_num
       write(output_Nuclei,f) nucl_label(i), nucl_charge(i),         &
           nucl_coord(i,1)*a0,                                       &
           nucl_coord(i,2)*a0,                                       &
           nucl_coord(i,3)*a0
     enddo
     write(output_Nuclei,ft)                                         &
         '================','============','============','============','============'
     write(output_Nuclei,'(A)') ''
     
   endif
   
   IRP_IF MPI
     include 'mpif.h'
     integer                        :: ierr
     call MPI_BCAST( nucl_coord, 3*nucl_num, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
     if (ierr /= MPI_SUCCESS) then
       stop 'Unable to read nucl_coord with MPI'
     endif
   IRP_ENDIF

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
 
 BEGIN_PROVIDER [ double precision, nucl_dist_2, (nucl_num,nucl_num) ]
&BEGIN_PROVIDER [ double precision, nucl_dist_vec_x, (nucl_num,nucl_num) ]
&BEGIN_PROVIDER [ double precision, nucl_dist_vec_y, (nucl_num,nucl_num) ]
&BEGIN_PROVIDER [ double precision, nucl_dist_vec_z, (nucl_num,nucl_num) ]
&BEGIN_PROVIDER [ double precision, nucl_dist, (nucl_num,nucl_num) ]
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
     do ie1 = 1,nucl_num
       nucl_dist_vec_x(ie1,ie2) = nucl_coord(ie1,1) - nucl_coord(ie2,1)
       nucl_dist_vec_y(ie1,ie2) = nucl_coord(ie1,2) - nucl_coord(ie2,2)
       nucl_dist_vec_z(ie1,ie2) = nucl_coord(ie1,3) - nucl_coord(ie2,3)
     enddo
     do ie1 = 1,nucl_num
       nucl_dist_2(ie1,ie2) = nucl_dist_vec_x(ie1,ie2)*nucl_dist_vec_x(ie1,ie2) +&
           nucl_dist_vec_y(ie1,ie2)*nucl_dist_vec_y(ie1,ie2) +       &
           nucl_dist_vec_z(ie1,ie2)*nucl_dist_vec_z(ie1,ie2)
       nucl_dist(ie1,ie2) = sqrt(nucl_dist_2(ie1,ie2))
       ASSERT (nucl_dist(ie1,ie2) > 0.d0)
     enddo
   enddo
   
END_PROVIDER
 
BEGIN_PROVIDER [ double precision, positive_charge_barycentre,(3)]
  implicit none
  BEGIN_DOC
  ! Centroid of the positive charges
  END_DOC
  integer                        :: l
  positive_charge_barycentre(1) = 0.d0
  positive_charge_barycentre(2) = 0.d0
  positive_charge_barycentre(3) = 0.d0
  do l = 1, nucl_num
    positive_charge_barycentre(1) += nucl_charge(l) * nucl_coord(l,1)
    positive_charge_barycentre(2) += nucl_charge(l) * nucl_coord(l,2)
    positive_charge_barycentre(3) += nucl_charge(l) * nucl_coord(l,3)
  enddo
END_PROVIDER

BEGIN_PROVIDER [ double precision, nuclear_repulsion ]
   implicit none
   BEGIN_DOC
   ! Nuclear repulsion energy
   END_DOC

   PROVIDE mpi_master nucl_coord nucl_charge nucl_num
   if (disk_access_nuclear_repulsion.EQ.'Read') then
     logical                        :: has

     if (mpi_master) then
       call ezfio_has_nuclei_nuclear_repulsion(has)
       if (has) then
         call ezfio_get_nuclei_nuclear_repulsion(nuclear_repulsion)
       else
         print *, 'nuclei/nuclear_repulsion not found in EZFIO file'
         stop 1
       endif
       print*, 'Read nuclear_repulsion'
     endif
     IRP_IF MPI
      include 'mpif.h'
      integer                        :: ierr
      call MPI_BCAST( nuclear_repulsion, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
      if (ierr /= MPI_SUCCESS) then
        stop 'Unable to read nuclear_repulsion with MPI'
      endif
     IRP_ENDIF
     
     
   else

      integer                        :: k,l
      double precision               :: Z12, r2, x(3)
      nuclear_repulsion = 0.d0
      do l = 1, nucl_num
         do  k = 1, nucl_num
           if(k == l) then
             cycle
           endif
           Z12 = nucl_charge(k)*nucl_charge(l)
           x(1) = nucl_coord(k,1) - nucl_coord(l,1)
           x(2) = nucl_coord(k,2) - nucl_coord(l,2)
           x(3) = nucl_coord(k,3) - nucl_coord(l,3)
           r2 = x(1)*x(1) + x(2)*x(2) + x(3)*x(3)
           nuclear_repulsion += Z12/dsqrt(r2)
         enddo
      enddo
      nuclear_repulsion *= 0.5d0
   end if

   call write_time(output_Nuclei)
   call write_double(output_Nuclei,nuclear_repulsion,                &
       'Nuclear repulsion energy')

   if (disk_access_nuclear_repulsion.EQ.'Write') then
     if (mpi_master) then
        call ezfio_set_nuclei_nuclear_repulsion(nuclear_repulsion)
     endif
   endif
END_PROVIDER

BEGIN_PROVIDER [ character*(128), element_name, (78)] 
 BEGIN_DOC
 ! Array of the name of element, sorted by nuclear charge (integer)
 END_DOC
 element_name(1) = 'H'
 element_name(2) = 'He'
 element_name(3) = 'Li'
 element_name(4) = 'Be'
 element_name(5) = 'B'
 element_name(6) = 'C'
 element_name(7) = 'N'
 element_name(8) = 'O'
 element_name(9) = 'F'
 element_name(10) = 'Ne'
 element_name(11) = 'Na'
 element_name(12) = 'Mg'
 element_name(13) = 'Al'
 element_name(14) = 'Si'
 element_name(15) = 'P'
 element_name(16) = 'S'
 element_name(17) = 'Cl'
 element_name(18) = 'Ar'
 element_name(19) = 'K'
 element_name(20) = 'Ca'
 element_name(21) = 'Sc'
 element_name(22) = 'Ti'
 element_name(23) = 'V'
 element_name(24) = 'Cr'
 element_name(25) = 'Mn'
 element_name(26) = 'Fe'
 element_name(27) = 'Co'
 element_name(28) = 'Ni'
 element_name(29) = 'Cu'
 element_name(30) = 'Zn'
 element_name(31) = 'Ga'
 element_name(32) = 'Ge'
 element_name(33) = 'As'
 element_name(34) = 'Se'
 element_name(35) = 'Br'
 element_name(36) = 'Kr'
 element_name(37) = 'Rb'
 element_name(38) = 'Sr'
 element_name(39) = 'Y'
 element_name(40) = 'Zr'
 element_name(41) = 'Nb'
 element_name(42) = 'Mo'
 element_name(43) = 'Tc'
 element_name(44) = 'Ru'
 element_name(45) = 'Rh'
 element_name(46) = 'Pd'
 element_name(47) = 'Ag'
 element_name(48) = 'Cd'
 element_name(49) = 'In'
 element_name(50) = 'Sn'
 element_name(51) = 'Sb'
 element_name(52) = 'Te'
 element_name(53) = 'I'
 element_name(54) = 'Xe'
 element_name(55) = 'Cs'
 element_name(56) = 'Ba'
 element_name(57) = 'La'
 element_name(58) = 'Ce'
 element_name(59) = 'Pr'
 element_name(60) = 'Nd'
 element_name(61) = 'Pm'
 element_name(62) = 'Sm'
 element_name(63) = 'Eu'
 element_name(64) = 'Gd'
 element_name(65) = 'Tb'
 element_name(66) = 'Dy'
 element_name(67) = 'Ho'
 element_name(68) = 'Er'
 element_name(69) = 'Tm'
 element_name(70) = 'Yb'
 element_name(71) = 'Lu'
 element_name(72) = 'Hf'
 element_name(73) = 'Ta'
 element_name(74) = 'W'
 element_name(75) = 'Re'
 element_name(76) = 'Os'
 element_name(77) = 'Ir'
 element_name(78) = 'Pt'

END_PROVIDER

BEGIN_PROVIDER [ double precision, mass, (0:110) ]
  implicit none
  BEGIN_DOC
  ! Atomic masses
  END_DOC
  
  mass(  0   )  =  0.
  mass(  1   )  =  1.0079
  mass(  2   )  =  4.00260
  mass(  3   )  =  6.941
  mass(  4   )  =  9.01218
  mass(  5   )  =  10.81
  mass(  6   )  =  12.011
  mass(  7   )  =  14.0067
  mass(  8   )  =  15.9994
  mass(  9   )  =  18.998403
  mass(  10  )  =  20.179
  mass(  11  )  =  22.98977
  mass(  12  )  =  24.305
  mass(  13  )  =  26.98154
  mass(  14  )  =  28.0855
  mass(  15  )  =  30.97376
  mass(  16  )  =  32.06
  mass(  17  )  =  35.453
  mass(  18  )  =  39.948
  mass(  19  )  =  39.0983
  mass(  20  )  =  40.08
  mass(  21  )  =  44.9559
  mass(  22  )  =  47.90
  mass(  23  )  =  50.9415
  mass(  24  )  =  51.996
  mass(  25  )  =  54.9380
  mass(  26  )  =  55.9332
  mass(  27  )  =  58.9332
  mass(  28  )  =  58.70
  mass(  29  )  =  63.546
  mass(  30  )  =  65.38
  mass(  31  )  =  69.72
  mass(  32  )  =  72.59
  mass(  33  )  =  74.9216
  mass(  34  )  =  78.96
  mass(  35  )  =  79.904
  mass(  36  )  =  83.80
  mass(  37  )  =  85.4678
  mass(  38  )  =  87.62
  mass(  39  )  =  88.90584
  mass(  40  )  =  91.224
  mass(  41  )  =  92.90637
  mass(  42  )  =  95.95
  mass(  43  )  =  98.
  mass(  44  )  =  101.07
  mass(  45  )  =  102.90550
  mass(  46  )  =  106.42
  mass(  47  )  =  107.8682
  mass(  48  )  =  112.414
  mass(  49  )  =  114.818
  mass(  50  )  =  118.710
  mass(  51  )  =  121.760
  mass(  52  )  =  127.60
  mass(  53  )  =  126.90447
  mass(  54  )  =  131.293
  mass(  78  )  =  195.084
END_PROVIDER

BEGIN_PROVIDER [ double precision, center_of_mass, (3) ]
  implicit none
  BEGIN_DOC
  ! Center of mass of the molecule
  END_DOC
  integer                        :: i,j
  double precision               :: s
  center_of_mass(:) = 0.d0
  s = 0.d0
  do i=1,nucl_num
    do j=1,3
      center_of_mass(j) += nucl_coord_input(i,j)* mass(int(nucl_charge(i)))
    enddo
    s += mass(int(nucl_charge(i)))
  enddo
  s = 1.d0/s
  center_of_mass(:) = center_of_mass(:)*s
END_PROVIDER


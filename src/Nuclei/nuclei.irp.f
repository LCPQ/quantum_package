BEGIN_PROVIDER [ integer, nucl_num_aligned ]
   implicit none
   BEGIN_DOC
   ! Number of nuclei algined
   END_DOC
   
   PROVIDE ezfio_filename
   integer                        :: align_double
   nucl_num_aligned = align_double(nucl_num)
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
   
   character*(64), parameter      :: f = '(A16, 4(1X,F12.6))'
   character*(64), parameter      :: ft= '(A16, 4(1X,A12  ))'
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

   IF (disk_access_nuclear_repulsion.EQ.'Read') THEN
          print*, 'nuclear_repulsion read from disk'
          LOGICAL :: has
          call ezfio_has_nuclei_nuclear_repulsion(has)
          if (has) then
                 call ezfio_get_nuclei_nuclear_repulsion(nuclear_repulsion)
          else
                 print *, 'nuclei/nuclear_repulsion not found in EZFIO file'
                 stop 1
          endif

   ELSE

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
   END IF

   call write_time(output_Nuclei)
   call write_double(output_Nuclei,nuclear_repulsion,                &
       'Nuclear repulsion energy')

   IF (disk_access_nuclear_repulsion.EQ.'Write') THEN
        call ezfio_set_nuclei_nuclear_repulsion(nuclear_repulsion)
   END IF
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

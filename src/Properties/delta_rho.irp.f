 BEGIN_PROVIDER [integer, N_z_pts]
&BEGIN_PROVIDER [double precision, z_min]
&BEGIN_PROVIDER [double precision, z_max]
&BEGIN_PROVIDER [double precision, delta_z]
 implicit none
 z_min = -20.d0
 z_max = 20.d0
 delta_z = 0.1d0
 N_z_pts = (z_max - z_min)/delta_z
 print*,'N_z_pts = ',N_z_pts

END_PROVIDER

BEGIN_PROVIDER [double precision, integrated_delta_rho, (N_z_pts)]
 BEGIN_DOC
! 
! integrated_rho(alpha,z) - integrated_rho(beta,z)
!
 END_DOC
 implicit none
 integer :: i,j,k,l,i_z,h
 double precision :: z,function_integrated_delta_rho,c_k,c_j,n_i_h,accu
 integrated_delta_rho = 0.d0
 do i_z = 1, N_z_pts
  do i = 1, mo_tot_num
    do h = 1, mo_tot_num
     n_i_h = one_body_spin_density_mo(i,h)
 !   print*,'n_i_h = ',n_i_h
     if(dabs(n_i_h).lt.1.d-10)cycle
     do j = 1, ao_num
      c_j = mo_coef(j,i)   ! coefficient of the ith MO on the jth primitive
      do k = 1, ao_num
       c_k = mo_coef(k,h)   ! coefficient of the ith MO on the jth primitive
       integrated_delta_rho(i_z) += c_k * c_j * n_i_h *  SABpartial_tab_ao(j,k,i_z)
      enddo
     enddo
    enddo
  enddo
 enddo

 z = z_min
 accu = 0.d0
 do i = 1, N_z_pts
  accu += integrated_delta_rho(i)
  write(i_unit_integrated_delta_rho,*)z,integrated_delta_rho(i)
  z += delta_z
 enddo
 print*,'sum of integrated_delta_rho = ',accu

END_PROVIDER




BEGIN_PROVIDER [ double precision, SABpartial_tab_ao, (ao_num_align, ao_num, N_z_pts)]
 BEGIN_DOC
!  array of the overlap in x,y between the AO function and integrated between [z,z+dz] in the z axis
!  for all the z points that are given (N_z_pts)
 END_DOC
  implicit none
  integer :: i,j,n,l
  double precision :: f,accu
  integer :: dim1
  double precision :: overlap, overlap_x, overlap_y, overlap_z
  double precision :: alpha, beta, c
  double precision :: A_center(3), B_center(3)
  integer :: power_A(3), power_B(3)
  integer :: i_z
  double precision :: z,SABpartial,accu_x,accu_y
  double precision,allocatable :: SAB_array(:,:,:)
  allocate(SAB_array(ao_num_align,ao_num,N_z_pts))
  dim1=100
 z = z_min
 do i_z = 1, N_z_pts
  do j=1,ao_num
   A_center(1) = nucl_coord( ao_nucl(j), 1 )
   A_center(2) = nucl_coord( ao_nucl(j), 2 )
   A_center(3) = nucl_coord( ao_nucl(j), 3 )
   power_A(1)  = ao_power( j, 1 )
   power_A(2)  = ao_power( j, 2 )
   power_A(3)  = ao_power( j, 3 )
   do i= 1,ao_num
    B_center(1) = nucl_coord( ao_nucl(i), 1 )
    B_center(2) = nucl_coord( ao_nucl(i), 2 )
    B_center(3) = nucl_coord( ao_nucl(i), 3 )
    power_B(1)  = ao_power( i, 1 )
    power_B(2)  = ao_power( i, 2 )
    power_B(3)  = ao_power( i, 3 )

     SAB_array(j,i,i_z) = 0.d0
     accu_x = 0.d0
     accu_y = 0.d0
     do n = 1,ao_prim_num(j)
      alpha = ao_expo_transp(n,j)
      do l = 1, ao_prim_num(i)
       beta = ao_expo_transp(l,i)
       call overlap_gaussian_xyz(A_center,B_center,alpha,beta,power_A,power_B,overlap_x,overlap_y,overlap_z,overlap,dim1)

       c = ao_coef_transp(n,j) * ao_coef_transp(l,i) 
       accu_x += c * overlap_x
       accu_y += c * overlap_y
       SAB_array(j,i,i_z) += c* overlap_x * overlap_y * SABpartial(z,z+delta_z,A_center,B_center,power_A,power_B,alpha,beta)
      enddo
     enddo
   enddo
  enddo
  z += delta_z
 enddo
 do i_z = 1,N_z_pts
  do i = 1, ao_num
   do j = 1, ao_num
    SABpartial_tab_ao(i,j,i_z) = SAB_array(i,j,i_z)
   enddo
  enddo
 enddo
END_PROVIDER

BEGIN_PROVIDER [integer, i_unit_integrated_delta_rho]
 implicit none
 BEGIN_DOC
! fortran unit for the writing of the integrated delta_rho
 END_DOC
 integer :: getUnitAndOpen
 character*(128) :: output_i_unit_integrated_delta_rho
 output_i_unit_integrated_delta_rho=trim(ezfio_filename)//'/delta_rho'
 i_unit_integrated_delta_rho= getUnitAndOpen(output_i_unit_integrated_delta_rho,'w')

END_PROVIDER

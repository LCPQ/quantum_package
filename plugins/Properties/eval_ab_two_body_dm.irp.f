program two_bod_ab_dm
 implicit none
 read_wf = .True.
 touch read_wf
 call routine

end

subroutine routine
 implicit none
 integer :: i,j,k,l
 double precision :: dx1,dx2
 double precision :: interval_1,interval_2
 integer :: nx1,nx2
 double precision :: r1(3),r2(3)
 double precision :: xmin_1,xmax_1,xmin_2,xmax_2
 double precision :: compute_extra_diag_two_body_dm_ab,two_bod_extra_diag
 double precision :: compute_diag_two_body_dm_ab,two_bod_diag

 double precision,allocatable :: mos_array_r1(:),mos_array_r2(:)
 double precision :: test_diag, test_extra_diag
 double precision, allocatable :: x_array(:),y_array(:),z_diag_array(:),z_extra_diag_array(:),z_total_array(:)
 allocate(mos_array_r1(mo_tot_num),mos_array_r2(mo_tot_num))
 


 ! He triplet S
 ! r1 = x
 ! r2 = z
 nx1 = 100
 nx2 = 100
 allocate(x_array(nx1),y_array(nx2),z_diag_array(nx1),z_extra_diag_array(nx1),z_total_array(nx1))
 xmin_1 = -2.d0
 xmax_1 =  2.d0
 xmin_2 = -2.d0
 xmax_2 =  2.d0
 interval_1 = xmax_1 - xmin_1
 interval_2 = xmax_2 - xmin_2
 dx1 = interval_1/dble(nx1)
 dx2 = interval_2/dble(nx2)
 r1 = 0.d0
 r2 = 0.d0
 
 double precision :: x_tmp,y_tmp
 x_tmp = xmin_1
 do i = 1, nx1
  x_array(i) = x_tmp 
  write(i_unit_x_two_body_dm_ab,'(10000(F16.10,X))')x_array(i)
  x_tmp += dx1
 enddo
 x_tmp = xmin_2
 do i = 1, nx1
  y_array(i) = x_tmp 
  write(i_unit_y_two_body_dm_ab,'(10000(F16.10,X))')x_array(i)
  x_tmp += dx2
 enddo


 ! initialization 
 r1(1) = xmin_1
 do i = 1, nx1
  r2(3) = xmin_2
  do j = 1, nx2
   two_bod_extra_diag = compute_extra_diag_two_body_dm_ab(r1,r2)
   two_bod_diag = compute_diag_two_body_dm_ab(r1,r2)
   z_diag_array(j) = two_bod_diag 
   z_extra_diag_array(j) = two_bod_extra_diag 
   z_total_array(j) = two_bod_extra_diag + two_bod_diag
!  write(i_unit_two_body_dm_ab,'(100(F16.10,X))')r1(1),r2(3),two_bod_diag,two_bod_extra_diag,two_bod_diag+two_bod_extra_diag
   r2(3) += dx2
  enddo
  write(i_unit_z_two_body_diag_dm_ab,'(10000(F16.10,X))')z_diag_array(:)
  write(i_unit_z_two_body_extra_diag_dm_ab,'(10000(F16.10,X))')z_extra_diag_array(:)
  write(i_unit_z_two_body_total_dm_ab,'(10000(F16.10,X))')z_total_array(:)
   r1(1) += dx1
 enddo

 deallocate(mos_array_r1,mos_array_r2)

end

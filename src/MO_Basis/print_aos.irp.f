program pouet
 implicit none
 integer :: i,j,k
 double precision :: r(3)
 double precision, allocatable :: aos_array(:),mos_array(:),ao_ortho_array(:)
 allocate(aos_array(ao_num),mos_array(mo_tot_num), ao_ortho_array(ao_num))
 integer :: nx,ny
 double precision :: interval_x
 double precision :: xmin,xmax
 double precision :: dx

 double precision :: interval_y
 double precision :: ymin,ymax
 double precision :: dy
 
 double precision :: val_max
 
!do i = 1, ao_num
!  write(41,'(100(F16.10,X))'),ao_ortho_canonical_overlap(i,:)
!enddo

!stop


 xmin = nucl_coord(1,1)-6.d0
 xmax = nucl_coord(2,1)+6.d0
 interval_x = xmax - xmin
!interval_x = nucl_dist(1,3) 
 nx = 500
 dx = interval_x/dble(nx)
!dx = dabs(interval_x)/dble(nx) * 1.d0/sqrt(2.d0)

 r = 0.d0
 r(3) = xmin
!r(2) =  nucl_coord(1,2)
!r(3) =  nucl_coord(1,3)
!r(1) = nucl_coord(2,1)
!r(2) = 1.D0
!r(3) = nucl_coord(2,3)
 double precision :: dr(3)
!dr = 0.d0
!dr(1) = -dx
!dr(3) =  dx
 do j = 1, nx+1
   call give_all_mos_at_r(r,mos_array)
   write(37,'(100(F16.10,X))') r(3),mos_array(1)*mos_array(1) , mos_array(2)*mos_array(2), mos_array(1)*mos_array(2) 
   write(38,'(100(F16.10,X))') r(3),mos_array(1), mos_array(2), mos_array(1)*mos_array(2) 
!  write(38,'(100(F16.10,X))') r(3),mos_array(10), mos_array(2) - 0.029916d0 * mos_array(10),mos_array(2) + 0.029916d0 * mos_array(10)
   r(3) += dx
!  r += dr
 enddo
 deallocate(aos_array,mos_array, ao_ortho_array)
end

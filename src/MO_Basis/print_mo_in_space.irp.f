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


 xmin = -4.d0
 xmax = 4.d0
 interval_x = xmax - xmin
 nx = 100
 dx = dabs(interval_x)/dble(nx)

 r = 0.d0
!r(3) = xmin
 r(1) = xmin
 val_max = 0.d0
 do j = 1, nx
! call give_all_aos_at_r(r,aos_array)
  call give_all_mos_at_r(r,mos_array)
   write(36,'(100(F16.10,X))') r(1), mos_array(1), mos_array(2), mos_array(3), mos_array(17), mos_array(23)
  !write(36,'(100(F16.10,X))') r(1), mos_array(1), mos_array(2), mos_array(4)
  !write(37,'(100(F16.10,X))') r(1),mos_array(1) * mos_array(2), mos_array(4)*mos_array(2)
!  if(val_max.le.aos_array(1)  * aos_array(2) )then
!   val_max = aos_array(1)  * aos_array(2) 
!  endif
   r(1) += dx
!  r(3) += dx
 enddo
!write(40,'(100(F16.10,X))')nucl_coord(1,2),nucl_coord(1,3),val_max * 1.5d0
!write(41,'(100(F16.10,X))')nucl_coord(2,2),nucl_coord(2,3),val_max * 1.5d0
 deallocate(aos_array,mos_array, ao_ortho_array)
end

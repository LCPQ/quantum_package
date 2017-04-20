double precision function ao_value(i,r)
 implicit none
 BEGIN_DOC
! return the value of the ith ao at point r
 END_DOC
 double precision, intent(in) :: r(3)
 integer, intent(in) :: i

 integer :: m,num_ao
 double precision :: center_ao(3)
 double precision :: beta
 integer :: power_ao(3) 
 num_ao = ao_nucl(i)
 power_ao(1:3)= ao_power(i,1:3)
 center_ao(1:3) = nucl_coord(num_ao,1:3)
 double precision :: accu,dx,dy,dz,r2
 dx = (r(1) - center_ao(1)) 
 dy = (r(2) - center_ao(2)) 
 dz = (r(3) - center_ao(3)) 
 r2 = dx*dx + dy*dy + dz*dz
 dx = dx**power_ao(1)
 dy = dy**power_ao(2)
 dz = dz**power_ao(3)

 accu = 0.d0
 do m=1,ao_prim_num(i)
   beta = ao_expo_ordered_transp(m,i)
   accu += ao_coef_normalized_ordered_transp(m,i) * dexp(-beta*r2) 
 enddo
 ao_value = accu * dx * dy * dz

end

subroutine give_all_aos_at_r(r,aos_array)
 implicit none
 BEGIN_dOC
! gives the values of aos at a given point r
 END_DOC
 double precision, intent(in) :: r(3)
 double precision, intent(out) :: aos_array(ao_num)
 integer :: i
 double precision :: ao_value
 do i = 1, ao_num 
  aos_array(i) = ao_value(i,r)
 enddo


end

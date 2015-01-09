double precision function mo_r(i,r)
 implicit none
 integer :: i
 double precision :: r(3)
 integer :: j
 double precision :: x_center,y_center,z_center,r_center
 mo_r = 0.d0
 do j = 1, prim_num
  if(dabs(prim_mo_coef(j,i)).lt.1.d-10)cycle
  x_center = r(1) - nucl_coord(prim_nucl(j),1)
  y_center = r(2) - nucl_coord(prim_nucl(j),2)
  z_center = r(3) - nucl_coord(prim_nucl(j),3)
  r_center = x_center*x_center + y_center*y_center + z_center*z_center
  mo_r = mo_r + prim_mo_coef(j,i) * (x_center ** (prim_power(j,1)) * y_center ** (prim_power(j,2)) * z_center ** (prim_power(j,3)) & 
                * dexp(-prim_expo(j) * r_center))
 enddo

end

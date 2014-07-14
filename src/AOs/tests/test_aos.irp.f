program test_aos
  implicit none
  integer                        :: i,j,n
  character*(32), parameter      :: f = '(A,i3,x,i2,x,A,E25.16)'
  
  do i=1,ao_num
    print *, 'nucl', i, ':', ao_nucl(i)
    do j=1,ao_prim_num(i)
      print f, 'expo', i, j, ':', ao_expo_transp(j,i)
      print f, 'coef', i, j, ':', ao_coef_transp(j,i)
    enddo
    do j=1,3
      print *, 'power',i, j, ':', ao_power(i,j)
    enddo
  enddo
end

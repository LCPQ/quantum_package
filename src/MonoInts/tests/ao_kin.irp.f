program test ao_kin
  implicit none
  integer :: i, j
  do i=1,ao_num
   do j=1,i
    if (abs(ao_kinetic_integral(i,j)) > 1.d-9) then
      print '(I4,A,I4,2x,A, 2x,e16.8)' ,  i,',',j, ' : ', ao_kinetic_integral(i,j)
    endif
   enddo
  enddo
end


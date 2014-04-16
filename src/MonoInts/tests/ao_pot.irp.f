program test ao_pot
  implicit none
  integer :: i, j
  do i=1,ao_num
   do j=1,i
    if (abs(ao_nucl_elec_integral(i,j)) > 1.d-9) then
      print '(I4,A,I4,2x,A, 2x,e16.8)' ,  i,',',j, ' : ', ao_nucl_elec_integral(i,j)
    endif
   enddo
  enddo
end


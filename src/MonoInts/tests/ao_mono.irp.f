program test ao_mono
  implicit none
  integer :: i, j
  do i=1,ao_num
   do j=1,i
    if (abs(ao_overlap(i,j)) > 1.d-9) then
      print '(I4,A,I4,2x,A, 2x,e16.8)' ,  i,',',j, ' : ', ao_overlap(i,j)
    endif
   enddo
  enddo
end


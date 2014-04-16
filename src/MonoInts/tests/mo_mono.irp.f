program test mo_mono
  implicit none
  integer :: i, j
  integer :: ortho
  ortho = 1
  do i=1,mo_tot_num
   do j=1,i-1
    if (abs(mo_overlap(i,j)) > 1.d-9) then
      print '(I4,A,I4,2x,A, 2x,e16.8)' ,  i,',',j, ' : ', ao_overlap(i,j)
      ortho = 0
    endif
   enddo
   if (abs(1.d0-mo_overlap(i,j)) > 1.d-9) then
      print '(I4,A,I4,2x,A, 2x,e16.8)' ,  i,',',j, ' : ', ao_overlap(i,j)
      ortho = 0
   endif
  enddo
  print *,  'Orthonormal : ', ortho
end


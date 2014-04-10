program test_normalization
  implicit none
  integer                        :: i,j
  character*(64), parameter      :: f = '(i3.3,a,i3.3,a3,F12.8)'
  do j=1,ao_num
    do i=1,ao_num
      if (abs(ao_overlap(i,j)) > 1.d-6) then
        print f, i,'_',j, ' : ', ao_overlap(i,j)
      endif
    enddo
  enddo
end

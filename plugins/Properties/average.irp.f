subroutine get_average(array,density,average)
 implicit none
 double precision, intent(in) :: array(mo_tot_num,mo_tot_num)
 double precision, intent(in) :: density(mo_tot_num,mo_tot_num)
 double precision, intent(out):: average
 integer :: i,j
 BEGIN_DOC
! computes the average value of a pure MONO ELECTRONIC OPERATOR 
! whom integrals on the MO basis are stored in "array" 
! and with the density is stored in  "density"
 END_DOC
 average = 0.d0

 !$OMP PARALLEL DO DEFAULT(none) &
 !$OMP PRIVATE(i,j) &
 !$OMP SHARED(mo_tot_num,array,density) &
 !$OMP REDUCTION(+:average)
 do i = 1, mo_tot_num
  do j = 1, mo_tot_num
   average += density(j,i) * array(j,i)
  enddo
 enddo
 !$OMP END PARALLEL DO

end

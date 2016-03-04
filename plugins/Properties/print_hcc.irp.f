program print_hcc
 implicit none
 read_wf = .True.
 touch read_wf
 call test
end
subroutine test
 implicit none
 double precision :: accu
 integer :: i,j
 print*,'Z               AU           GAUSS              MHZ             cm^-1'
 do i = 1, nucl_num
  write(*,'(I2,X,F3.1,X,4(F16.6,X))')i,nucl_charge(i),spin_density_at_nucleous(i),iso_hcc_gauss(i),iso_hcc_mhz(i),iso_hcc_cm_1(i)
 enddo

end


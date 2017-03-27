program rotate_mos
 implicit none
 integer :: i,j
 write(*,*)'Which couple of MOs would you like to mix ?'
 read(5,*)i,j
 call mix_mo_jk(i,j)
 call save_mos

end

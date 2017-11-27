program permut_mos
 implicit none
 integer :: mo1,mo2
 integer :: i,j,k,l
 double precision :: mo_coef_tmp(ao_num,2)
 print*,'Which MOs would you like to change ?'
 read(5,*)mo1,mo2
 print*,''
 do i= 1,ao_num
  mo_coef_tmp(i,1) = mo_coef(i,mo1)
  mo_coef_tmp(i,2) = mo_coef(i,mo2)
 enddo
 do i = 1,ao_num
  mo_coef(i,mo1) =  mo_coef_tmp(i,2)
  mo_coef(i,mo2) =  mo_coef_tmp(i,1)
 enddo
 touch mo_coef
 call save_mos

end

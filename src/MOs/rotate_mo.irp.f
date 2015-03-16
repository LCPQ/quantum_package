program rotate
 implicit none
 integer :: i,j,k,l
!j=2
!k=3
 print*,'Enter the two orbitals you want to rotate'
 read(5,*)j,k
 call mix_mo_jk(j,k)
 soft_touch mo_coef 
 call save_mos
end

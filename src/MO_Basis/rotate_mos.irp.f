program rotate
 implicit none
 integer :: iorb,jorb 
 print*, 'which mos would you like to rotate'
 read(5,*)iorb,jorb
 call mix_mo_jk(iorb,jorb)
 call save_mos
end

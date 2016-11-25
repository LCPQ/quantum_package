program loc_int
 implicit none
 integer :: i,j,k,l,iorb,jorb
 double precision :: exchange_int(mo_tot_num)
 integer :: iorder(mo_tot_num)
 integer :: indices(mo_tot_num,2)
 logical :: list_core_inact_check(mo_tot_num)
 integer :: n_rot
 indices = 0
 list_core_inact_check = .True. 
 n_rot = 0
 do i = 1, n_core_inact_orb
  iorb = list_core_inact(i)
  exchange_int = 0.d0
  iorder = 0
  print*,''
  if(list_core_inact_check(iorb) .eqv. .False.)cycle
  do j = i+1, n_core_inact_orb
   jorb = list_core_inact(j)
   iorder(jorb) = jorb
   exchange_int(jorb) = -mo_bielec_integral_jj_exchange(iorb,jorb)
  enddo
  n_rot += 1
  call dsort(exchange_int,iorder,mo_tot_num)
  indices(n_rot,1) = iorb
  indices(n_rot,2) = iorder(1)
  list_core_inact_check(iorder(1)) = .False.
  print*,indices(n_rot,1),indices(n_rot,2)
  print*,''
  print*,''
 enddo
  print*,'****************************'
  print*,'-+++++++++++++++++++++++++'
 do i = 1, n_rot
  iorb = indices(i,1) 
  jorb = indices(i,2) 
  print*,iorb,jorb
  call mix_mo_jk(iorb,jorb)
 enddo


 call save_mos
 

end

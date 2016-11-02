subroutine give_all_act_mos_at_r(r,mos_array)
 implicit none
 double precision, intent(in) :: r(3)
 double precision, intent(out) :: mos_array(n_act_orb)
 double precision :: aos_array(ao_num),accu
 integer :: i,j,iorb
!print*,'n_act_orb = ',n_act_orb
 call give_all_aos_at_r(r,aos_array)
 do i = 1, n_act_orb
  iorb = list_act(i)
  accu = 0.d0
  do j = 1, ao_num
   accu += mo_coef(j,iorb) * aos_array(j)
  enddo
  mos_array(i) = accu
 enddo
end

subroutine give_all_core_mos_at_r(r,mos_array)
 implicit none
 double precision, intent(in) :: r(3)
 double precision, intent(out) :: mos_array(n_core_orb)
 double precision :: aos_array(ao_num),accu
 integer :: i,j,iorb
 call give_all_aos_at_r(r,aos_array)
 do i = 1, n_core_orb
  iorb = list_core(i)
  accu = 0.d0
  do j = 1, ao_num
   accu += mo_coef(j,iorb) * aos_array(j)
  enddo
  mos_array(i) = accu
 enddo
end


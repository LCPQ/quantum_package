program FourIdx
  use map_module
  implicit none
  BEGIN_DOC
! Performs a four index transformation of the two-electron integrals
  END_DOC

  type(map_type) :: test_map
  integer(key_kind)              :: key_max
  integer(map_size_kind)         :: sze

  call bielec_integrals_index(ao_num,ao_num,ao_num,ao_num,key_max)
  sze = key_max
  call map_init(test_map,sze)
    
!  call four_index_transform(ao_integrals_map,test_map,               &
!      mo_coef, size(mo_coef,1),                                      &
!      1, 1, 1, 1, ao_num, ao_num, ao_num, ao_num,                    &
!      1, 1, 1, 1, mo_tot_num, mo_tot_num, mo_tot_num, mo_tot_num)

  double precision :: t0,t1
  call wall_time(t0)
  call four_index_transform_sym(ao_integrals_map,test_map,               &
      mo_coef, size(mo_coef,1),                                      &
      1, 1, 1, 1, ao_num, ao_num, ao_num, ao_num,                    &
      1, 1, 1, 1, mo_tot_num, mo_tot_num, mo_tot_num, mo_tot_num)
  call wall_time(t1)
  print *,  'Time: ', t1-t0, 's'

  integer :: i,j,k,l
  real(integral_kind) :: integral1, integral2

  provide mo_bielec_integrals_in_map

  do i=1,mo_tot_num
    do j=1,mo_tot_num
      do k=1,mo_tot_num
        do l=1,mo_tot_num
          call bielec_integrals_index(i,j,k,l,key_max)
          call map_get(test_map,key_max,integral1)
          call map_get(mo_integrals_map,key_max,integral2)
          if (dabs(integral2) >=1.d-10 ) then
          if (dabs(integral1 / integral2 -1.d0) > .001d0) then
            print *,  i,j,k,l
            print *,  integral1, integral2
            print *,  ''
          endif
          endif
        enddo
      enddo
    enddo
  enddo
end

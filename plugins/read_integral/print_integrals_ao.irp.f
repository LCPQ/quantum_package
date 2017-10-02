program print_integrals

  PROVIDE ezfio_filename
  call ezfio_set_integrals_monoelec_disk_access_ao_one_integrals('None')
  call ezfio_set_integrals_bielec_disk_access_ao_integrals('None')
  call run
end

subroutine run
  implicit none
  
  integer :: iunit
  integer :: getunitandopen

  integer ::i,j,k,l
  double precision :: integral

  iunit = getunitandopen('kinetic_ao','w')
  do i=1,ao_num
    do j=1,ao_num
      integral = ao_kinetic_integral(i,j)
      if (dabs(integral) > ao_integrals_threshold) then
        write(iunit,*) i,j, integral
      endif
    enddo
  enddo
  close(iunit)
  
  iunit = getunitandopen('overlap_ao','w')
  do i=1,ao_num
    do j=1,ao_num
      integral = ao_overlap(i,j)
      if (dabs(integral) > ao_integrals_threshold) then
        write(iunit,*) i,j, integral
      endif
    enddo
  enddo
  close(iunit)
  
  iunit = getunitandopen('nuclear_ao','w')
  do i=1,ao_num
    do j=1,ao_num
      integral = ao_nucl_elec_integral(i,j)
      if (dabs(integral) > ao_integrals_threshold) then
        write(iunit,*) i,j, integral
      endif
    enddo
  enddo
  close(iunit)

!  iunit = getunitandopen('pseudo_ao','w')
!  do i=1,ao_num
!    do j=1,ao_num
!      write(iunit,*) i,j, ao_pseudo_integral(i,j)
!    enddo
!  enddo
!  close(iunit)

  PROVIDE ao_bielec_integrals_in_map
  iunit = getunitandopen('bielec_ao','w')

  integer*8                      :: i8
  integer                        :: i_idx, n_elements_max, k1, n_elements
  integer                        :: ii(8), jj(8), kk(8), ll(8)
  double precision, external     :: ao_bielec_integral
  integer(key_kind), allocatable :: keys(:)
  double precision, allocatable  :: values(:)


  call get_cache_map_n_elements_max(ao_integrals_map,n_elements_max)
  allocate(keys(n_elements_max), values(n_elements_max))

!  do i8=0_8,ao_integrals_map%map_size
!     n_elements = n_elements_max
!     call get_cache_map(ao_integrals_map,i8,keys,values,n_elements)
!     do k1=1,n_elements
!      call bielec_integrals_index_reverse(kk,ii,ll,jj,keys(k1))
!      if ( (kk(1)>ao_num).or.                                        &
!            (ii(1)>ao_num).or.                                       &
!            (jj(1)>ao_num).or.                                       &
!            (ll(1)>ao_num) ) then
!            cycle
!      endif
!      k = kk(1)
!      i = ii(1)
!      l = ll(1)
!      j = jj(1)
!      integral = values(k1)
!      write (iunit,'(4(I6,X),F20.15)') k,i,l,j, integral 
!    enddo
!  enddo

  do i=1,ao_num
    do k=1,ao_num
      do j=1,ao_num
        do l=1,ao_num
          double precision, external :: get_ao_bielec_integral
          integral = get_ao_bielec_integral(i,j,k,l,ao_integrals_map)
          if (dabs(integral)>=1.e-15) then
            write (iunit,'(4(I6),F20.15)') i,j,k,l, integral 
          endif
        enddo
      enddo
    enddo
  enddo

  close(iunit)
end

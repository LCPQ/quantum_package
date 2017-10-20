program print_integrals
  implicit none
  
  integer :: iunit
  integer :: getunitandopen

  integer ::i,j,k,l
  double precision :: integral

  iunit = getunitandopen('kinetic_mo','w')
  do i=1,mo_tot_num
    do j=1,mo_tot_num
      write(iunit,*) i,j, mo_kinetic_integral(i,j)
    enddo
  enddo
  close(iunit)

  iunit = getunitandopen('overlap_mo','w')
  do i=1,mo_tot_num
    do j=1,mo_tot_num
      write(iunit,*) i,j, mo_overlap(i,j)
    enddo
  enddo
  close(iunit)

  iunit = getunitandopen('nuclear_mo','w')
  do i=1,mo_tot_num
    do j=1,mo_tot_num
      write(iunit,*) i,j, mo_nucl_elec_integral(i,j)
    enddo
  enddo
  close(iunit)

  !iunit = getunitandopen('pseudo_mo','w')
  !do i=1,mo_tot_num
  !  do j=1,mo_tot_num
  !    write(iunit,*) i,j, mo_pseudo_integral(i,j)
  !  enddo
  !enddo
  !close(iunit)

  PROVIDE mo_bielec_integrals_in_map
  iunit = getunitandopen('bielec_mo','w')
  do l=1,mo_tot_num
   do k=1,mo_tot_num
    do j=l,mo_tot_num
     do i=k,mo_tot_num
       !if (i>=j) then
          double precision :: get_mo_bielec_integral
          integral = get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
          if (dabs(integral) > mo_integrals_threshold) then
            write (iunit,'(4(I6,X),F20.15)') i,j,k,l, integral 
          endif
       !end if
     enddo
    enddo
   enddo
  enddo

  close(iunit)
end

program fcidump
  implicit none

  integer :: i,j,k,l
  integer :: ii(8), jj(8), kk(8),ll(8)
  integer*8 :: m
  character*(2), allocatable :: A(:)

  print *, '&FCI NORB=', mo_tot_num, ', NELEC=', elec_num, &
   ', MS2=', (elec_alpha_num-elec_beta_num), ','
  allocate (A(mo_tot_num))
  A = '1,'
  print *, 'ORBSYM=', (A(i), i=1,mo_tot_num) 
  print *,'ISYM=0,'
  print *,'/'
  deallocate(A)
  
  integer*8 :: i8, k1
  integer(key_kind), allocatable :: keys(:)
  double precision, allocatable  :: values(:)
  integer(cache_map_size_kind)   :: n_elements, n_elements_max
  PROVIDE mo_bielec_integrals_in_map

  double precision :: get_mo_bielec_integral, integral

  do l=1,mo_tot_num
   do k=1,mo_tot_num
    do j=l,mo_tot_num
     do i=k,mo_tot_num
       if (i>=j) then
          integral = get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
          if (dabs(integral) > mo_integrals_threshold) then 
            print *, integral, i,k,j,l
          endif
       end if
     enddo
    enddo
   enddo
  enddo

  do j=1,mo_tot_num
   do i=j,mo_tot_num
      integral = mo_mono_elec_integral(i,j)
      if (dabs(integral) > mo_integrals_threshold) then 
        print *, integral, i,j,0,0
      endif
   enddo
  enddo
  print *, 0.d0, 0, 0, 0, 0
end

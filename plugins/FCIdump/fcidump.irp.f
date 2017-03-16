program fcidump
  implicit none
  character*(128) :: output
  integer :: i_unit_output,getUnitAndOpen
  output=trim(ezfio_filename)//'.FCIDUMP'
  i_unit_output = getUnitAndOpen(output,'w')

  integer :: i,j,k,l
  integer :: i1,j1,k1,l1
  integer :: i2,j2,k2,l2
  integer*8 :: m
  character*(2), allocatable :: A(:)

  write(i_unit_output,*) '&FCI NORB=', n_act_orb, ', NELEC=', elec_num-n_core_orb*2, &
   ', MS2=', (elec_alpha_num-elec_beta_num), ','
  allocate (A(n_act_orb))
  A = '1,'
  write(i_unit_output,*) 'ORBSYM=', (A(i), i=1,n_act_orb) 
  write(i_unit_output,*) 'ISYM=0,'
  write(i_unit_output,*) '/'
  deallocate(A)
  
  integer(key_kind), allocatable :: keys(:)
  double precision, allocatable  :: values(:)
  integer(cache_map_size_kind)   :: n_elements, n_elements_max
  PROVIDE mo_bielec_integrals_in_map

  double precision :: get_mo_bielec_integral, integral

  do l=1,n_act_orb
   l1 = list_act(l)
   do k=1,n_act_orb
    k1 = list_act(k)
    do j=l,n_act_orb
     j1 = list_act(j)
     do i=k,n_act_orb
      i1 = list_act(i)
       if (i1>=j1) then
          integral = get_mo_bielec_integral(i1,j1,k1,l1,mo_integrals_map)
          if (dabs(integral) > mo_integrals_threshold) then 
            write(i_unit_output,*) integral, i,k,j,l
          endif
       end if
     enddo
    enddo
   enddo
  enddo

  do j=1,n_act_orb
   j1 = list_act(j)
   do i=j,n_act_orb
    i1 = list_act(i)
      integral = mo_mono_elec_integral(i1,j1) + core_fock_operator(i1,j1)
      if (dabs(integral) > mo_integrals_threshold) then 
        write(i_unit_output,*) integral, i,j,0,0
      endif
   enddo
  enddo
  write(i_unit_output,*) core_energy, 0, 0, 0, 0
end

subroutine mo_bielec_integrals_index(i,j,k,l,i1)
  use map_module
  implicit none
  BEGIN_DOC
  ! Computes an unique index for i,j,k,l integrals
  END_DOC
  integer, intent(in)            :: i,j,k,l
  integer(key_kind), intent(out) :: i1
  integer(key_kind)              :: p,q,r,s,i2
  p = min(i,k)
  r = max(i,k)
  p = p+ishft(r*r-r,-1)
  q = min(j,l)
  s = max(j,l)
  q = q+ishft(s*s-s,-1)
  i1 = min(p,q)
  i2 = max(p,q)
  i1 = i1+ishft(i2*i2-i2,-1)
end


BEGIN_PROVIDER [ logical, mo_bielec_integrals_in_map ]
  use map_module
  implicit none
  integer(bit_kind)              :: mask_ijkl(N_int,4)
  integer(bit_kind)              :: mask_ijk(N_int,3)
  
  BEGIN_DOC
  ! If True, the map of MO bielectronic integrals is provided
  END_DOC
  
  mo_bielec_integrals_in_map = .True.
  if (read_mo_integrals) then
    print*,'Reading the MO integrals'
    call map_load_from_disk(trim(ezfio_filename)//'/work/mo_ints',mo_integrals_map)
    print*, 'MO integrals provided'
    return
  else
    PROVIDE ao_bielec_integrals_in_map
  endif
  
  if(no_vvvv_integrals)then
    integer                        :: i,j,k,l
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  I I I I !!!!!!!!!!!!!!!!!!!!
    ! (core+inact+act) ^ 4
    ! <ii|ii>
    print*, ''
    print*, '<ii|ii>'
    do i = 1,N_int
      mask_ijkl(i,1) =  core_inact_act_bitmask_4(i,1)
      mask_ijkl(i,2) =  core_inact_act_bitmask_4(i,1)
      mask_ijkl(i,3) =  core_inact_act_bitmask_4(i,1)
      mask_ijkl(i,4) =  core_inact_act_bitmask_4(i,1)
    enddo
    call add_integrals_to_map(mask_ijkl)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  I I V V !!!!!!!!!!!!!!!!!!!!
    ! (core+inact+act) ^ 2  (virt) ^2
    ! <iv|iv>  = J_iv
    print*, ''
    print*, '<iv|iv>'
    do i = 1,N_int
      mask_ijkl(i,1) =  core_inact_act_bitmask_4(i,1)
      mask_ijkl(i,2) =  virt_bitmask(i,1)
      mask_ijkl(i,3) =  core_inact_act_bitmask_4(i,1)
      mask_ijkl(i,4) =  virt_bitmask(i,1)
    enddo
    call add_integrals_to_map(mask_ijkl)
    
    ! (core+inact+act) ^ 2  (virt) ^2
    ! <ii|vv> = (iv|iv)
    print*, ''
    print*, '<ii|vv>'
    do i = 1,N_int
      mask_ijkl(i,1) = core_inact_act_bitmask_4(i,1)
      mask_ijkl(i,2) = core_inact_act_bitmask_4(i,1)
      mask_ijkl(i,3) = virt_bitmask(i,1)
      mask_ijkl(i,4) = virt_bitmask(i,1)
    enddo
    call add_integrals_to_map(mask_ijkl)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! V V V !!!!!!!!!!!!!!!!!!!!!!!
    if(.not.no_vvv_integrals)then
      print*, ''
      print*, '<rv|sv> and <rv|vs>'
      do i = 1,N_int
        mask_ijk(i,1) =  virt_bitmask(i,1)
        mask_ijk(i,2) =  virt_bitmask(i,1)
        mask_ijk(i,3) =  virt_bitmask(i,1)
      enddo
      call add_integrals_to_map_three_indices(mask_ijk)
    endif
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  I I I V !!!!!!!!!!!!!!!!!!!!
    ! (core+inact+act) ^ 3  (virt) ^1
    ! <iv|ii>
    print*, ''
    print*, '<iv|ii>'
    do i = 1,N_int
      mask_ijkl(i,1) =  core_inact_act_bitmask_4(i,1)
      mask_ijkl(i,2) =  core_inact_act_bitmask_4(i,1)
      mask_ijkl(i,3) =  core_inact_act_bitmask_4(i,1)
      mask_ijkl(i,4) =  virt_bitmask(i,1)
    enddo
    call add_integrals_to_map(mask_ijkl)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  I V V V !!!!!!!!!!!!!!!!!!!!
    ! (core+inact+act) ^ 1  (virt) ^3
    ! <iv|vv>
    if(.not.no_ivvv_integrals)then
      print*, ''
      print*, '<iv|vv>'
      do i = 1,N_int
        mask_ijkl(i,1) =  core_inact_act_bitmask_4(i,1)
        mask_ijkl(i,2) =  virt_bitmask(i,1)
        mask_ijkl(i,3) =  virt_bitmask(i,1)
        mask_ijkl(i,4) =  virt_bitmask(i,1)
      enddo
      call add_integrals_to_map_no_exit_34(mask_ijkl)
    endif
    
  else
!    call add_integrals_to_map(full_ijkl_bitmask_4)

!     call four_index_transform_block(ao_integrals_map,mo_integrals_map, &
!         mo_coef, size(mo_coef,1),                                      &
!         1, 1, 1, 1, ao_num, ao_num, ao_num, ao_num,                    &
!         1, 1, 1, 1, mo_tot_num, mo_tot_num, mo_tot_num, mo_tot_num)

     call four_index_transform(ao_integrals_map,mo_integrals_map, &
         mo_coef, size(mo_coef,1),                                      &
         1, 1, 1, 1, ao_num, ao_num, ao_num, ao_num,                    &
         1, 1, 1, 1, mo_tot_num, mo_tot_num, mo_tot_num, mo_tot_num)

    integer*8                      :: get_mo_map_size, mo_map_size
    mo_map_size = get_mo_map_size()
    
    print*,'Molecular integrals provided'
  endif
  if (write_mo_integrals) then
    call ezfio_set_work_empty(.False.)
    call map_save_to_disk(trim(ezfio_filename)//'/work/mo_ints',mo_integrals_map)
    call ezfio_set_integrals_bielec_disk_access_mo_integrals("Read")
  endif
  
END_PROVIDER

subroutine set_integrals_jj_into_map
  use bitmasks
  implicit none
  integer                        :: i,j,n_integrals,i0,j0
  double precision               :: buffer_value(mo_tot_num * mo_tot_num)
  integer(key_kind)              :: buffer_i(mo_tot_num*mo_tot_num)
  n_integrals = 0
  do j0 = 1, n_virt_orb
    j = list_virt(j0)
    do i0 = j0, n_virt_orb
      i = list_virt(i0)
      n_integrals += 1
      !    mo_bielec_integral_jj_exchange(i,j) = mo_bielec_integral_vv_exchange_from_ao(i,j)
      call mo_bielec_integrals_index(i,j,i,j,buffer_i(n_integrals))
      buffer_value(n_integrals) = mo_bielec_integral_vv_from_ao(i,j)
    enddo
  enddo
  call insert_into_mo_integrals_map(n_integrals,buffer_i,buffer_value,&
      real(mo_integrals_threshold,integral_kind))
  call map_merge(mo_integrals_map)
end

subroutine set_integrals_exchange_jj_into_map
  use bitmasks
  implicit none
  integer                        :: i,j,n_integrals,i0,j0
  double precision               :: buffer_value(mo_tot_num * mo_tot_num)
  integer(key_kind)              :: buffer_i(mo_tot_num*mo_tot_num)
  n_integrals = 0
  do j0 = 1, n_virt_orb
    j = list_virt(j0)
    do i0 = j0+1, n_virt_orb
      i = list_virt(i0)
      n_integrals += 1
      call mo_bielec_integrals_index(i,j,j,i,buffer_i(n_integrals))
      buffer_value(n_integrals) = mo_bielec_integral_vv_exchange_from_ao(i,j)
    enddo
  enddo
  call insert_into_mo_integrals_map(n_integrals,buffer_i,buffer_value,&
      real(mo_integrals_threshold,integral_kind))
  call map_merge(mo_integrals_map)
  
end

subroutine add_integrals_to_map(mask_ijkl)
  use bitmasks
  implicit none
  
  BEGIN_DOC
  ! Adds integrals to tha MO map according to some bitmask
  END_DOC
  
  integer(bit_kind), intent(in)  :: mask_ijkl(N_int,4)
  
  integer                        :: i,j,k,l
  integer                        :: i0,j0,k0,l0
  double precision               :: c, cpu_1, cpu_2, wall_1, wall_2, wall_0
  
  integer, allocatable           :: list_ijkl(:,:)
  integer                        :: n_i, n_j, n_k, n_l
  integer, allocatable           :: bielec_tmp_0_idx(:)
  real(integral_kind), allocatable :: bielec_tmp_0(:,:)
  double precision, allocatable  :: bielec_tmp_1(:)
  double precision, allocatable  :: bielec_tmp_2(:,:)
  double precision, allocatable  :: bielec_tmp_3(:,:,:)
  !DEC$ ATTRIBUTES ALIGN : 64    :: bielec_tmp_1, bielec_tmp_2, bielec_tmp_3
  
  integer                        :: n_integrals
  integer                        :: size_buffer
  integer(key_kind),allocatable  :: buffer_i(:)
  real(integral_kind),allocatable :: buffer_value(:)
  double precision               :: map_mb
  
  integer                        :: i1,j1,k1,l1, ii1, kmax, thread_num
  integer                        :: i2,i3,i4
  double precision,parameter     :: thr_coef = 1.d-10
  
  PROVIDE ao_bielec_integrals_in_map  mo_coef
  
  !Get list of MOs for i,j,k and l
  !-------------------------------
  
  allocate(list_ijkl(mo_tot_num,4))
  call bitstring_to_list( mask_ijkl(1,1), list_ijkl(1,1), n_i, N_int )
  call bitstring_to_list( mask_ijkl(1,2), list_ijkl(1,2), n_j, N_int )
  call bitstring_to_list( mask_ijkl(1,3), list_ijkl(1,3), n_k, N_int )
  call bitstring_to_list( mask_ijkl(1,4), list_ijkl(1,4), n_l, N_int )
  character*(2048)               :: output(1)
  print*, 'i'
  call bitstring_to_str( output(1), mask_ijkl(1,1), N_int )
  print *,  trim(output(1))
  j = 0
  do i = 1, N_int
    j += popcnt(mask_ijkl(i,1))
  enddo
  if(j==0)then
    return
  endif
  
  print*, 'j'
  call bitstring_to_str( output(1), mask_ijkl(1,2), N_int )
  print *,  trim(output(1))
  j = 0
  do i = 1, N_int
    j += popcnt(mask_ijkl(i,2))
  enddo
  if(j==0)then
    return
  endif
  
  print*, 'k'
  call bitstring_to_str( output(1), mask_ijkl(1,3), N_int )
  print *,  trim(output(1))
  j = 0
  do i = 1, N_int
    j += popcnt(mask_ijkl(i,3))
  enddo
  if(j==0)then
    return
  endif
  
  print*, 'l'
  call bitstring_to_str( output(1), mask_ijkl(1,4), N_int )
  print *,  trim(output(1))
  j = 0
  do i = 1, N_int
    j += popcnt(mask_ijkl(i,4))
  enddo
  if(j==0)then
    return
  endif
  
  size_buffer = min(ao_num*ao_num*ao_num,16000000)
  print*, 'Providing the molecular integrals '
  print*, 'Buffers : ', 8.*(mo_tot_num_align*(n_j)*(n_k+1) + mo_tot_num_align +&
      ao_num+ao_num*ao_num+ size_buffer*3)/(1024*1024), 'MB / core'
  
  call wall_time(wall_1)
  call cpu_time(cpu_1)
  double precision               :: accu_bis
  accu_bis = 0.d0
  
  !$OMP PARALLEL PRIVATE(l1,k1,j1,i1,i2,i3,i4,i,j,k,l,c, ii1,kmax,   &
      !$OMP  bielec_tmp_0_idx, bielec_tmp_0, bielec_tmp_1,bielec_tmp_2,bielec_tmp_3,&
      !$OMP  buffer_i,buffer_value,n_integrals,wall_2,i0,j0,k0,l0,   &
      !$OMP  wall_0,thread_num,accu_bis)                             &
      !$OMP  DEFAULT(NONE)                                           &
      !$OMP  SHARED(size_buffer,ao_num,mo_tot_num,n_i,n_j,n_k,n_l,mo_tot_num_align,&
      !$OMP  mo_coef_transp,                                         &
      !$OMP  mo_coef_transp_is_built, list_ijkl,                     &
      !$OMP  mo_coef_is_built, wall_1,                               &
      !$OMP  mo_coef,mo_integrals_threshold,mo_integrals_map)
  n_integrals = 0
  wall_0 = wall_1
  allocate(bielec_tmp_3(mo_tot_num_align, n_j, n_k),                 &
      bielec_tmp_1(mo_tot_num_align),                                &
      bielec_tmp_0(ao_num,ao_num),                                   &
      bielec_tmp_0_idx(ao_num),                                      &
      bielec_tmp_2(mo_tot_num_align, n_j),                           &
      buffer_i(size_buffer),                                         &
      buffer_value(size_buffer) )
  
  thread_num = 0
  !$  thread_num = omp_get_thread_num()
  !$OMP DO SCHEDULE(guided)
  do l1 = 1,ao_num
    !DEC$ VECTOR ALIGNED
    bielec_tmp_3 = 0.d0
    do k1 = 1,ao_num
      !DEC$ VECTOR ALIGNED
      bielec_tmp_2 = 0.d0
      do j1 = 1,ao_num
        call get_ao_bielec_integrals(j1,k1,l1,ao_num,bielec_tmp_0(1,j1))
        ! call compute_ao_bielec_integrals(j1,k1,l1,ao_num,bielec_tmp_0(1,j1))
      enddo
      do j1 = 1,ao_num
        kmax = 0
        do i1 = 1,ao_num
          c = bielec_tmp_0(i1,j1)
          if (c == 0.d0) then
            cycle
          endif
          kmax += 1
          bielec_tmp_0(kmax,j1) = c
          bielec_tmp_0_idx(kmax) = i1
        enddo
        
        if (kmax==0) then
          cycle
        endif
        
        !DEC$ VECTOR ALIGNED
        bielec_tmp_1 = 0.d0
        ii1=1
        do ii1 = 1,kmax-4,4
          i1 = bielec_tmp_0_idx(ii1)
          i2 = bielec_tmp_0_idx(ii1+1)
          i3 = bielec_tmp_0_idx(ii1+2)
          i4 = bielec_tmp_0_idx(ii1+3)
          do i = list_ijkl(1,1), list_ijkl(n_i,1)
            bielec_tmp_1(i)  =  bielec_tmp_1(i) +                    &
                mo_coef_transp(i,i1) * bielec_tmp_0(ii1,j1) +        &
                mo_coef_transp(i,i2) * bielec_tmp_0(ii1+1,j1) +      &
                mo_coef_transp(i,i3) * bielec_tmp_0(ii1+2,j1) +      &
                mo_coef_transp(i,i4) * bielec_tmp_0(ii1+3,j1)
          enddo ! i
        enddo  ! ii1
        
        i2 = ii1
        do ii1 = i2,kmax
          i1 = bielec_tmp_0_idx(ii1)
          do i = list_ijkl(1,1), list_ijkl(n_i,1)
            bielec_tmp_1(i) = bielec_tmp_1(i) + mo_coef_transp(i,i1) * bielec_tmp_0(ii1,j1)
          enddo ! i
        enddo  ! ii1
        c = 0.d0
        
        do i = list_ijkl(1,1), list_ijkl(n_i,1)
          c = max(c,abs(bielec_tmp_1(i)))
          if (c>mo_integrals_threshold) exit
        enddo
        if ( c < mo_integrals_threshold ) then
          cycle
        endif
        
        do j0 = 1, n_j
          j = list_ijkl(j0,2)
          c = mo_coef_transp(j,j1)
          if (abs(c) < thr_coef) then
            cycle
          endif
          do i = list_ijkl(1,1), list_ijkl(n_i,1)
            bielec_tmp_2(i,j0)  = bielec_tmp_2(i,j0) + c * bielec_tmp_1(i)
          enddo ! i
        enddo  ! j
      enddo !j1
      if ( maxval(abs(bielec_tmp_2)) < mo_integrals_threshold ) then
        cycle
      endif
      
      
      do k0 = 1, n_k
        k = list_ijkl(k0,3)
        c = mo_coef_transp(k,k1)
        if (abs(c) < thr_coef) then
          cycle
        endif
        
        do j0 = 1, n_j
          j = list_ijkl(j0,2)
          do i = list_ijkl(1,1), k
            bielec_tmp_3(i,j0,k0) = bielec_tmp_3(i,j0,k0) + c* bielec_tmp_2(i,j0)
          enddo!i
        enddo !j
        
      enddo  !k
    enddo   !k1
    
    
    
    do l0 = 1,n_l
      l = list_ijkl(l0,4)
      c = mo_coef_transp(l,l1)
      if (abs(c) < thr_coef) then
        cycle
      endif
      j1 = ishft((l*l-l),-1)
      do j0 = 1, n_j
        j = list_ijkl(j0,2)
        if (j > l)  then
          exit
        endif
        j1 += 1
        do k0 = 1, n_k
          k = list_ijkl(k0,3)
          i1 = ishft((k*k-k),-1)
          if (i1<=j1) then
            continue
          else
            exit
          endif
          bielec_tmp_1 = 0.d0
          do i0 = 1, n_i
            i = list_ijkl(i0,1)
            if (i>k) then
              exit
            endif
            bielec_tmp_1(i) = c*bielec_tmp_3(i,j0,k0)
            !           i1+=1
          enddo
          
          do i0 = 1, n_i
            i = list_ijkl(i0,1)
            if(i> min(k,j1-i1+list_ijkl(1,1)-1))then
              exit
            endif
            if (abs(bielec_tmp_1(i)) < mo_integrals_threshold) then
              cycle
            endif
            n_integrals += 1
            buffer_value(n_integrals) = bielec_tmp_1(i)
            !DEC$ FORCEINLINE
            call mo_bielec_integrals_index(i,j,k,l,buffer_i(n_integrals))
            if (n_integrals == size_buffer) then
              call insert_into_mo_integrals_map(n_integrals,buffer_i,buffer_value,&
                  real(mo_integrals_threshold,integral_kind))
              n_integrals = 0
            endif
          enddo
        enddo
      enddo
    enddo
    
    call wall_time(wall_2)
    if (thread_num == 0) then
      if (wall_2 - wall_0 > 1.d0) then
        wall_0 = wall_2
        print*, 100.*float(l1)/float(ao_num), '% in ',               &
            wall_2-wall_1, 's', map_mb(mo_integrals_map) ,'MB'
      endif
    endif
  enddo
  !$OMP END DO NOWAIT
  deallocate (bielec_tmp_1,bielec_tmp_2,bielec_tmp_3)
  
  integer                        :: index_needed
  
  call insert_into_mo_integrals_map(n_integrals,buffer_i,buffer_value,&
      real(mo_integrals_threshold,integral_kind))
  deallocate(buffer_i, buffer_value)
  !$OMP END PARALLEL
  call map_merge(mo_integrals_map)
  
  call wall_time(wall_2)
  call cpu_time(cpu_2)
  integer*8                      :: get_mo_map_size, mo_map_size
  mo_map_size = get_mo_map_size()
  
  deallocate(list_ijkl)
  
  
  print*,'Molecular integrals provided:'
  print*,' Size of MO map           ', map_mb(mo_integrals_map) ,'MB'
  print*,' Number of MO integrals: ',  mo_map_size
  print*,' cpu  time :',cpu_2 - cpu_1, 's'
  print*,' wall time :',wall_2 - wall_1, 's  ( x ', (cpu_2-cpu_1)/(wall_2-wall_1), ')'
  
end


subroutine add_integrals_to_map_three_indices(mask_ijk)
  use bitmasks
  implicit none
  
  BEGIN_DOC
  ! Adds integrals to tha MO map according to some bitmask
  END_DOC
  
  integer(bit_kind), intent(in)  :: mask_ijk(N_int,3)
  
  integer                        :: i,j,k,l
  integer                        :: i0,j0,k0,l0
  double precision               :: c, cpu_1, cpu_2, wall_1, wall_2, wall_0
  
  integer, allocatable           :: list_ijkl(:,:)
  integer                        :: n_i, n_j, n_k
  integer                        :: m
  integer, allocatable           :: bielec_tmp_0_idx(:)
  real(integral_kind), allocatable :: bielec_tmp_0(:,:)
  double precision, allocatable  :: bielec_tmp_1(:)
  double precision, allocatable  :: bielec_tmp_2(:,:)
  double precision, allocatable  :: bielec_tmp_3(:,:,:)
  !DEC$ ATTRIBUTES ALIGN : 64    :: bielec_tmp_1, bielec_tmp_2, bielec_tmp_3
  
  integer                        :: n_integrals
  integer                        :: size_buffer
  integer(key_kind),allocatable  :: buffer_i(:)
  real(integral_kind),allocatable :: buffer_value(:)
  double precision               :: map_mb
  
  integer                        :: i1,j1,k1,l1, ii1, kmax, thread_num
  integer                        :: i2,i3,i4
  double precision,parameter     :: thr_coef = 1.d-10
  
  PROVIDE ao_bielec_integrals_in_map  mo_coef
  
  !Get list of MOs for i,j,k and l
  !-------------------------------
  
  allocate(list_ijkl(mo_tot_num,4))
  call bitstring_to_list( mask_ijk(1,1), list_ijkl(1,1), n_i, N_int )
  call bitstring_to_list( mask_ijk(1,2), list_ijkl(1,2), n_j, N_int )
  call bitstring_to_list( mask_ijk(1,3), list_ijkl(1,3), n_k, N_int )
  character*(2048)               :: output(1)
  print*, 'i'
  call bitstring_to_str( output(1), mask_ijk(1,1), N_int )
  print *,  trim(output(1))
  j = 0
  do i = 1, N_int
    j += popcnt(mask_ijk(i,1))
  enddo
  if(j==0)then
    return
  endif
  
  print*, 'j'
  call bitstring_to_str( output(1), mask_ijk(1,2), N_int )
  print *,  trim(output(1))
  j = 0
  do i = 1, N_int
    j += popcnt(mask_ijk(i,2))
  enddo
  if(j==0)then
    return
  endif
  
  print*, 'k'
  call bitstring_to_str( output(1), mask_ijk(1,3), N_int )
  print *,  trim(output(1))
  j = 0
  do i = 1, N_int
    j += popcnt(mask_ijk(i,3))
  enddo
  if(j==0)then
    return
  endif
  
  size_buffer = min(ao_num*ao_num*ao_num,16000000)
  print*, 'Providing the molecular integrals '
  print*, 'Buffers : ', 8.*(mo_tot_num_align*(n_j)*(n_k+1) + mo_tot_num_align +&
      ao_num+ao_num*ao_num+ size_buffer*3)/(1024*1024), 'MB / core'
  
  call wall_time(wall_1)
  call cpu_time(cpu_1)
  double precision               :: accu_bis
  accu_bis = 0.d0
  !$OMP PARALLEL PRIVATE(m,l1,k1,j1,i1,i2,i3,i4,i,j,k,l,c, ii1,kmax, &
      !$OMP  bielec_tmp_0_idx, bielec_tmp_0, bielec_tmp_1,bielec_tmp_2,bielec_tmp_3,&
      !$OMP  buffer_i,buffer_value,n_integrals,wall_2,i0,j0,k0,l0,   &
      !$OMP  wall_0,thread_num,accu_bis)                             &
      !$OMP  DEFAULT(NONE)                                           &
      !$OMP  SHARED(size_buffer,ao_num,mo_tot_num,n_i,n_j,n_k,mo_tot_num_align,&
      !$OMP  mo_coef_transp,                                         &
      !$OMP  mo_coef_transp_is_built, list_ijkl,                     &
      !$OMP  mo_coef_is_built, wall_1,                               &
      !$OMP  mo_coef,mo_integrals_threshold,mo_integrals_map)
  n_integrals = 0
  wall_0 = wall_1
  allocate(bielec_tmp_3(mo_tot_num_align, n_j, n_k),                 &
      bielec_tmp_1(mo_tot_num_align),                                &
      bielec_tmp_0(ao_num,ao_num),                                   &
      bielec_tmp_0_idx(ao_num),                                      &
      bielec_tmp_2(mo_tot_num_align, n_j),                           &
      buffer_i(size_buffer),                                         &
      buffer_value(size_buffer) )
  
  thread_num = 0
  !$  thread_num = omp_get_thread_num()
  !$OMP DO SCHEDULE(guided)
  do l1 = 1,ao_num
    !DEC$ VECTOR ALIGNED
    bielec_tmp_3 = 0.d0
    do k1 = 1,ao_num
      !DEC$ VECTOR ALIGNED
      bielec_tmp_2 = 0.d0
      do j1 = 1,ao_num
        call get_ao_bielec_integrals(j1,k1,l1,ao_num,bielec_tmp_0(1,j1))
      enddo
      do j1 = 1,ao_num
        kmax = 0
        do i1 = 1,ao_num
          c = bielec_tmp_0(i1,j1)
          if (c == 0.d0) then
            cycle
          endif
          kmax += 1
          bielec_tmp_0(kmax,j1) = c
          bielec_tmp_0_idx(kmax) = i1
        enddo
        
        if (kmax==0) then
          cycle
        endif
        
        !DEC$ VECTOR ALIGNED
        bielec_tmp_1 = 0.d0
        ii1=1
        do ii1 = 1,kmax-4,4
          i1 = bielec_tmp_0_idx(ii1)
          i2 = bielec_tmp_0_idx(ii1+1)
          i3 = bielec_tmp_0_idx(ii1+2)
          i4 = bielec_tmp_0_idx(ii1+3)
          do i = list_ijkl(1,1), list_ijkl(n_i,1)
            bielec_tmp_1(i)  =  bielec_tmp_1(i) +                    &
                mo_coef_transp(i,i1) * bielec_tmp_0(ii1,j1) +        &
                mo_coef_transp(i,i2) * bielec_tmp_0(ii1+1,j1) +      &
                mo_coef_transp(i,i3) * bielec_tmp_0(ii1+2,j1) +      &
                mo_coef_transp(i,i4) * bielec_tmp_0(ii1+3,j1)
          enddo ! i
        enddo  ! ii1
        
        i2 = ii1
        do ii1 = i2,kmax
          i1 = bielec_tmp_0_idx(ii1)
          do i = list_ijkl(1,1), list_ijkl(n_i,1)
            bielec_tmp_1(i) = bielec_tmp_1(i) + mo_coef_transp(i,i1) * bielec_tmp_0(ii1,j1)
          enddo ! i
        enddo  ! ii1
        c = 0.d0
        
        do i = list_ijkl(1,1), list_ijkl(n_i,1)
          c = max(c,abs(bielec_tmp_1(i)))
          if (c>mo_integrals_threshold) exit
        enddo
        if ( c < mo_integrals_threshold ) then
          cycle
        endif
        
        do j0 = 1, n_j
          j = list_ijkl(j0,2)
          c = mo_coef_transp(j,j1)
          if (abs(c) < thr_coef) then
            cycle
          endif
          do i = list_ijkl(1,1), list_ijkl(n_i,1)
            bielec_tmp_2(i,j0)  = bielec_tmp_2(i,j0) + c * bielec_tmp_1(i)
          enddo ! i
        enddo  ! j
      enddo !j1
      if ( maxval(abs(bielec_tmp_2)) < mo_integrals_threshold ) then
        cycle
      endif
      
      
      do k0 = 1, n_k
        k = list_ijkl(k0,3)
        c = mo_coef_transp(k,k1)
        if (abs(c) < thr_coef) then
          cycle
        endif
        
        do j0 = 1, n_j
          j = list_ijkl(j0,2)
          do i = list_ijkl(1,1), k
            bielec_tmp_3(i,j0,k0) = bielec_tmp_3(i,j0,k0) + c* bielec_tmp_2(i,j0)
          enddo!i
        enddo !j
        
      enddo  !k
    enddo   !k1
    
    
    
    do l0 = 1,n_j
      l = list_ijkl(l0,2)
      c = mo_coef_transp(l,l1)
      if (abs(c) < thr_coef) then
        cycle
      endif
      do k0 = 1, n_k
        k = list_ijkl(k0,3)
        i1 = ishft((k*k-k),-1)
        bielec_tmp_1 = 0.d0
        j0 = l0
        j = list_ijkl(j0,2)
        do i0 = 1, n_i
          i = list_ijkl(i0,1)
          if (i>k) then
            exit
          endif
          bielec_tmp_1(i) = c*bielec_tmp_3(i,j0,k0)
        enddo
        
        do i0 = 1, n_i
          i = list_ijkl(i0,1)
          if (i>k) then !min(k,j1-i1)
            exit
          endif
          if (abs(bielec_tmp_1(i)) < mo_integrals_threshold) then
            cycle
          endif
          n_integrals += 1
          buffer_value(n_integrals) = bielec_tmp_1(i)
          if(i==k .and. j==l .and. i.ne.j)then
            buffer_value(n_integrals) = buffer_value(n_integrals) *0.5d0
          endif
          !DEC$ FORCEINLINE
          call mo_bielec_integrals_index(i,j,k,l,buffer_i(n_integrals))
          if (n_integrals == size_buffer) then
            call insert_into_mo_integrals_map(n_integrals,buffer_i,buffer_value,&
                real(mo_integrals_threshold,integral_kind))
            n_integrals = 0
          endif
        enddo
      enddo
    enddo
    
    do l0 = 1,n_j
      l = list_ijkl(l0,2)
      c = mo_coef_transp(l,l1)
      if (abs(c) < thr_coef) then
        cycle
      endif
      do k0 = 1, n_k
        k = list_ijkl(k0,3)
        i1 = ishft((k*k-k),-1)
        bielec_tmp_1 = 0.d0
        j0 = k0
        j = list_ijkl(k0,2)
        i0 = l0
        i = list_ijkl(i0,2)
        if (k==l) then
          cycle
        endif
        bielec_tmp_1(i) = c*bielec_tmp_3(i,j0,k0)
        
        n_integrals += 1
        buffer_value(n_integrals) = bielec_tmp_1(i)
        !DEC$ FORCEINLINE
        call mo_bielec_integrals_index(i,j,k,l,buffer_i(n_integrals))
        if (n_integrals == size_buffer) then
          call insert_into_mo_integrals_map(n_integrals,buffer_i,buffer_value,&
              real(mo_integrals_threshold,integral_kind))
          n_integrals = 0
        endif
      enddo
    enddo
    
    call wall_time(wall_2)
    if (thread_num == 0) then
      if (wall_2 - wall_0 > 1.d0) then
        wall_0 = wall_2
        print*, 100.*float(l1)/float(ao_num), '% in ',               &
            wall_2-wall_1, 's', map_mb(mo_integrals_map) ,'MB'
      endif
    endif
  enddo
  !$OMP END DO NOWAIT
  deallocate (bielec_tmp_1,bielec_tmp_2,bielec_tmp_3)
  
  integer                        :: index_needed
  
  call insert_into_mo_integrals_map(n_integrals,buffer_i,buffer_value,&
      real(mo_integrals_threshold,integral_kind))
  deallocate(buffer_i, buffer_value)
  !$OMP END PARALLEL
  call map_merge(mo_integrals_map)
  
  call wall_time(wall_2)
  call cpu_time(cpu_2)
  integer*8                      :: get_mo_map_size, mo_map_size
  mo_map_size = get_mo_map_size()
  
  deallocate(list_ijkl)
  
  
  print*,'Molecular integrals provided:'
  print*,' Size of MO map           ', map_mb(mo_integrals_map) ,'MB'
  print*,' Number of MO integrals: ',  mo_map_size
  print*,' cpu  time :',cpu_2 - cpu_1, 's'
  print*,' wall time :',wall_2 - wall_1, 's  ( x ', (cpu_2-cpu_1)/(wall_2-wall_1), ')'
  
end


subroutine add_integrals_to_map_no_exit_34(mask_ijkl)
  use bitmasks
  implicit none
  
  BEGIN_DOC
  ! Adds integrals to tha MO map according to some bitmask
  END_DOC
  
  integer(bit_kind), intent(in)  :: mask_ijkl(N_int,4)
  
  integer                        :: i,j,k,l
  integer                        :: i0,j0,k0,l0
  double precision               :: c, cpu_1, cpu_2, wall_1, wall_2, wall_0
  
  integer, allocatable           :: list_ijkl(:,:)
  integer                        :: n_i, n_j, n_k, n_l
  integer, allocatable           :: bielec_tmp_0_idx(:)
  real(integral_kind), allocatable :: bielec_tmp_0(:,:)
  double precision, allocatable  :: bielec_tmp_1(:)
  double precision, allocatable  :: bielec_tmp_2(:,:)
  double precision, allocatable  :: bielec_tmp_3(:,:,:)
  !DEC$ ATTRIBUTES ALIGN : 64    :: bielec_tmp_1, bielec_tmp_2, bielec_tmp_3
  
  integer                        :: n_integrals
  integer                        :: size_buffer
  integer(key_kind),allocatable  :: buffer_i(:)
  real(integral_kind),allocatable :: buffer_value(:)
  double precision               :: map_mb
  
  integer                        :: i1,j1,k1,l1, ii1, kmax, thread_num
  integer                        :: i2,i3,i4
  double precision,parameter     :: thr_coef = 1.d-10
  
  PROVIDE ao_bielec_integrals_in_map  mo_coef
  
  !Get list of MOs for i,j,k and l
  !-------------------------------
  
  allocate(list_ijkl(mo_tot_num,4))
  call bitstring_to_list( mask_ijkl(1,1), list_ijkl(1,1), n_i, N_int )
  call bitstring_to_list( mask_ijkl(1,2), list_ijkl(1,2), n_j, N_int )
  call bitstring_to_list( mask_ijkl(1,3), list_ijkl(1,3), n_k, N_int )
  call bitstring_to_list( mask_ijkl(1,4), list_ijkl(1,4), n_l, N_int )
  
  size_buffer = min(ao_num*ao_num*ao_num,16000000)
  print*, 'Providing the molecular integrals '
  print*, 'Buffers : ', 8.*(mo_tot_num_align*(n_j)*(n_k+1) + mo_tot_num_align +&
      ao_num+ao_num*ao_num+ size_buffer*3)/(1024*1024), 'MB / core'
  
  call wall_time(wall_1)
  call cpu_time(cpu_1)
  
  !$OMP PARALLEL PRIVATE(l1,k1,j1,i1,i2,i3,i4,i,j,k,l,c, ii1,kmax,   &
      !$OMP  bielec_tmp_0_idx, bielec_tmp_0, bielec_tmp_1,bielec_tmp_2,bielec_tmp_3,&
      !$OMP  buffer_i,buffer_value,n_integrals,wall_2,i0,j0,k0,l0,   &
      !$OMP  wall_0,thread_num)                                      &
      !$OMP  DEFAULT(NONE)                                           &
      !$OMP  SHARED(size_buffer,ao_num,mo_tot_num,n_i,n_j,n_k,n_l,mo_tot_num_align,&
      !$OMP  mo_coef_transp,                                         &
      !$OMP  mo_coef_transp_is_built, list_ijkl,                     &
      !$OMP  mo_coef_is_built, wall_1,                               &
      !$OMP  mo_coef,mo_integrals_threshold,mo_integrals_map)
  n_integrals = 0
  wall_0 = wall_1
  allocate(bielec_tmp_3(mo_tot_num_align, n_j, n_k),                 &
      bielec_tmp_1(mo_tot_num_align),                                &
      bielec_tmp_0(ao_num,ao_num),                                   &
      bielec_tmp_0_idx(ao_num),                                      &
      bielec_tmp_2(mo_tot_num_align, n_j),                           &
      buffer_i(size_buffer),                                         &
      buffer_value(size_buffer) )
  
  thread_num = 0
  !$  thread_num = omp_get_thread_num()
  !$OMP DO SCHEDULE(guided)
  do l1 = 1,ao_num
    !IRP_IF COARRAY
    !    if (mod(l1-this_image(),num_images()) /= 0 ) then
    !      cycle
    !    endif
    !IRP_ENDIF
    !DEC$ VECTOR ALIGNED
    bielec_tmp_3 = 0.d0
    do k1 = 1,ao_num
      !DEC$ VECTOR ALIGNED
      bielec_tmp_2 = 0.d0
      do j1 = 1,ao_num
        call get_ao_bielec_integrals(j1,k1,l1,ao_num,bielec_tmp_0(1,j1))
        ! call compute_ao_bielec_integrals(j1,k1,l1,ao_num,bielec_tmp_0(1,j1))
      enddo
      do j1 = 1,ao_num
        kmax = 0
        do i1 = 1,ao_num
          c = bielec_tmp_0(i1,j1)
          if (c == 0.d0) then
            cycle
          endif
          kmax += 1
          bielec_tmp_0(kmax,j1) = c
          bielec_tmp_0_idx(kmax) = i1
        enddo
        
        if (kmax==0) then
          cycle
        endif
        
        !DEC$ VECTOR ALIGNED
        bielec_tmp_1 = 0.d0
        ii1=1
        do ii1 = 1,kmax-4,4
          i1 = bielec_tmp_0_idx(ii1)
          i2 = bielec_tmp_0_idx(ii1+1)
          i3 = bielec_tmp_0_idx(ii1+2)
          i4 = bielec_tmp_0_idx(ii1+3)
          do i = list_ijkl(1,1), list_ijkl(n_i,1)
            bielec_tmp_1(i)  =  bielec_tmp_1(i) +                    &
                mo_coef_transp(i,i1) * bielec_tmp_0(ii1,j1) +        &
                mo_coef_transp(i,i2) * bielec_tmp_0(ii1+1,j1) +      &
                mo_coef_transp(i,i3) * bielec_tmp_0(ii1+2,j1) +      &
                mo_coef_transp(i,i4) * bielec_tmp_0(ii1+3,j1)
          enddo ! i
        enddo  ! ii1
        
        i2 = ii1
        do ii1 = i2,kmax
          i1 = bielec_tmp_0_idx(ii1)
          do i = list_ijkl(1,1), list_ijkl(n_i,1)
            bielec_tmp_1(i) = bielec_tmp_1(i) + mo_coef_transp(i,i1) * bielec_tmp_0(ii1,j1)
          enddo ! i
        enddo  ! ii1
        c = 0.d0
        
        do i = list_ijkl(1,1), list_ijkl(n_i,1)
          c = max(c,abs(bielec_tmp_1(i)))
          if (c>mo_integrals_threshold) exit
        enddo
        if ( c < mo_integrals_threshold ) then
          cycle
        endif
        
        do j0 = 1, n_j
          j = list_ijkl(j0,2)
          c = mo_coef_transp(j,j1)
          if (abs(c) < thr_coef) then
            cycle
          endif
          do i = list_ijkl(1,1), list_ijkl(n_i,1)
            bielec_tmp_2(i,j0)  = bielec_tmp_2(i,j0) + c * bielec_tmp_1(i)
          enddo ! i
        enddo  ! j
      enddo !j1
      if ( maxval(abs(bielec_tmp_2)) < mo_integrals_threshold ) then
        cycle
      endif
      
      
      do k0 = 1, n_k
        k = list_ijkl(k0,3)
        c = mo_coef_transp(k,k1)
        if (abs(c) < thr_coef) then
          cycle
        endif
        
        do j0 = 1, n_j
          j = list_ijkl(j0,2)
          do i = list_ijkl(1,1), k
            bielec_tmp_3(i,j0,k0) = bielec_tmp_3(i,j0,k0) + c* bielec_tmp_2(i,j0)
          enddo!i
        enddo !j
        
      enddo  !k
    enddo   !k1
    
    
    
    do l0 = 1,n_l
      l = list_ijkl(l0,4)
      c = mo_coef_transp(l,l1)
      if (abs(c) < thr_coef) then
        cycle
      endif
      j1 = ishft((l*l-l),-1)
      do j0 = 1, n_j
        j = list_ijkl(j0,2)
        if (j > l)  then
          exit
        endif
        j1 += 1
        do k0 = 1, n_k
          k = list_ijkl(k0,3)
          i1 = ishft((k*k-k),-1)
          bielec_tmp_1 = 0.d0
          do i0 = 1, n_i
            i = list_ijkl(i0,1)
            if (i>k) then
              exit
            endif
            bielec_tmp_1(i) = c*bielec_tmp_3(i,j0,k0)
          enddo
          
          do i0 = 1, n_i
            i = list_ijkl(i0,1)
            if(i> k)then
              exit
            endif
            
            if (abs(bielec_tmp_1(i)) < mo_integrals_threshold) then
              cycle
            endif
            n_integrals += 1
            buffer_value(n_integrals) = bielec_tmp_1(i)
            !DEC$ FORCEINLINE
            call mo_bielec_integrals_index(i,j,k,l,buffer_i(n_integrals))
            if (n_integrals == size_buffer) then
              call insert_into_mo_integrals_map(n_integrals,buffer_i,buffer_value,&
                  real(mo_integrals_threshold,integral_kind))
              n_integrals = 0
            endif
          enddo
        enddo
      enddo
    enddo
    
    call wall_time(wall_2)
    if (thread_num == 0) then
      if (wall_2 - wall_0 > 1.d0) then
        wall_0 = wall_2
        print*, 100.*float(l1)/float(ao_num), '% in ',               &
            wall_2-wall_1, 's', map_mb(mo_integrals_map) ,'MB'
      endif
    endif
  enddo
  !$OMP END DO NOWAIT
  deallocate (bielec_tmp_1,bielec_tmp_2,bielec_tmp_3)
  
  call insert_into_mo_integrals_map(n_integrals,buffer_i,buffer_value,&
      real(mo_integrals_threshold,integral_kind))
  deallocate(buffer_i, buffer_value)
  !$OMP END PARALLEL
  !IRP_IF COARRAY
  !  print*, 'Communicating the map'
  !  call communicate_mo_integrals()
  !IRP_ENDIF
  call map_merge(mo_integrals_map)
  
  call wall_time(wall_2)
  call cpu_time(cpu_2)
  integer*8                      :: get_mo_map_size, mo_map_size
  mo_map_size = get_mo_map_size()
  
  deallocate(list_ijkl)
  
  
  print*,'Molecular integrals provided:'
  print*,' Size of MO map           ', map_mb(mo_integrals_map) ,'MB'
  print*,' Number of MO integrals: ',  mo_map_size
  print*,' cpu  time :',cpu_2 - cpu_1, 's'
  print*,' wall time :',wall_2 - wall_1, 's  ( x ', (cpu_2-cpu_1)/(wall_2-wall_1), ')'
  
  
end



 BEGIN_PROVIDER [ double precision, mo_bielec_integral_jj_from_ao, (mo_tot_num_align,mo_tot_num) ]
&BEGIN_PROVIDER [ double precision, mo_bielec_integral_jj_exchange_from_ao, (mo_tot_num_align,mo_tot_num) ]
&BEGIN_PROVIDER [ double precision, mo_bielec_integral_jj_anti_from_ao, (mo_tot_num_align,mo_tot_num) ]
  implicit none
  BEGIN_DOC
  ! mo_bielec_integral_jj_from_ao(i,j) = J_ij
  ! mo_bielec_integral_jj_exchange_from_ao(i,j) = J_ij
  ! mo_bielec_integral_jj_anti_from_ao(i,j) = J_ij - K_ij
  END_DOC
  
  integer                        :: i,j,p,q,r,s
  double precision               :: c
  real(integral_kind)            :: integral
  integer                        :: n, pp
  real(integral_kind), allocatable :: int_value(:)
  integer, allocatable           :: int_idx(:)
  
  double precision, allocatable  :: iqrs(:,:), iqsr(:,:), iqis(:), iqri(:)
  
  if (.not.do_direct_integrals) then
    PROVIDE ao_bielec_integrals_in_map mo_coef
  endif
  
  mo_bielec_integral_jj_from_ao = 0.d0
  mo_bielec_integral_jj_exchange_from_ao = 0.d0
  
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: iqrs, iqsr
  
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP PRIVATE (i,j,p,q,r,s,integral,c,n,pp,int_value,int_idx,  &
      !$OMP  iqrs, iqsr,iqri,iqis)                                   &
      !$OMP SHARED(mo_tot_num,mo_coef_transp,mo_tot_num_align,ao_num,&
      !$OMP  ao_integrals_threshold,do_direct_integrals)             &
      !$OMP REDUCTION(+:mo_bielec_integral_jj_from_ao,mo_bielec_integral_jj_exchange_from_ao)
  
  allocate( int_value(ao_num), int_idx(ao_num),                      &
      iqrs(mo_tot_num_align,ao_num), iqis(mo_tot_num), iqri(mo_tot_num),&
      iqsr(mo_tot_num_align,ao_num) )
  
  !$OMP DO SCHEDULE (guided)
  do s=1,ao_num
    do q=1,ao_num
      
      do j=1,ao_num
        !DIR$ VECTOR ALIGNED
        do i=1,mo_tot_num
          iqrs(i,j) = 0.d0
          iqsr(i,j) = 0.d0
        enddo
      enddo
      
      if (do_direct_integrals) then
        double precision               :: ao_bielec_integral
        do r=1,ao_num
          call compute_ao_bielec_integrals(q,r,s,ao_num,int_value)
          do p=1,ao_num
            integral = int_value(p)
            if (abs(integral) > ao_integrals_threshold) then
              !DIR$ VECTOR ALIGNED
              do i=1,mo_tot_num
                iqrs(i,r) += mo_coef_transp(i,p) * integral
              enddo
            endif
          enddo
          call compute_ao_bielec_integrals(q,s,r,ao_num,int_value)
          do p=1,ao_num
            integral = int_value(p)
            if (abs(integral) > ao_integrals_threshold) then
              !DIR$ VECTOR ALIGNED
              do i=1,mo_tot_num
                iqsr(i,r) += mo_coef_transp(i,p) * integral
              enddo
            endif
          enddo
        enddo
        
      else
        
        do r=1,ao_num
          call get_ao_bielec_integrals_non_zero(q,r,s,ao_num,int_value,int_idx,n)
          do pp=1,n
            p = int_idx(pp)
            integral = int_value(pp)
            if (abs(integral) > ao_integrals_threshold) then
              !DIR$ VECTOR ALIGNED
              do i=1,mo_tot_num
                iqrs(i,r) += mo_coef_transp(i,p) * integral
              enddo
            endif
          enddo
          call get_ao_bielec_integrals_non_zero(q,s,r,ao_num,int_value,int_idx,n)
          do pp=1,n
            p = int_idx(pp)
            integral = int_value(pp)
            if (abs(integral) > ao_integrals_threshold) then
              !DIR$ VECTOR ALIGNED
              do i=1,mo_tot_num
                iqsr(i,r) += mo_coef_transp(i,p) * integral
              enddo
            endif
          enddo
        enddo
      endif
      iqis = 0.d0
      iqri = 0.d0
      do r=1,ao_num
        !DIR$ VECTOR ALIGNED
        do i=1,mo_tot_num
          iqis(i) += mo_coef_transp(i,r) * iqrs(i,r)
          iqri(i) += mo_coef_transp(i,r) * iqsr(i,r)
        enddo
      enddo
      do i=1,mo_tot_num
        !DIR$ VECTOR ALIGNED
        do j=1,mo_tot_num
          c = mo_coef_transp(j,q)*mo_coef_transp(j,s)
          mo_bielec_integral_jj_from_ao(j,i) += c * iqis(i)
          mo_bielec_integral_jj_exchange_from_ao(j,i) += c * iqri(i)
        enddo
      enddo
      
    enddo
  enddo
  !$OMP END DO NOWAIT
  deallocate(iqrs,iqsr,int_value,int_idx)
  !$OMP END PARALLEL
  
  mo_bielec_integral_jj_anti_from_ao = mo_bielec_integral_jj_from_ao - mo_bielec_integral_jj_exchange_from_ao
  
  
END_PROVIDER

 BEGIN_PROVIDER [ double precision, mo_bielec_integral_vv_from_ao, (mo_tot_num_align,mo_tot_num) ]
&BEGIN_PROVIDER [ double precision, mo_bielec_integral_vv_exchange_from_ao, (mo_tot_num_align,mo_tot_num) ]
&BEGIN_PROVIDER [ double precision, mo_bielec_integral_vv_anti_from_ao, (mo_tot_num_align,mo_tot_num) ]
  implicit none
  BEGIN_DOC
  ! mo_bielec_integral_vv_from_ao(i,j) = J_ij
  ! mo_bielec_integral_vv_exchange_from_ao(i,j) = J_ij
  ! mo_bielec_integral_vv_anti_from_ao(i,j) = J_ij - K_ij
  ! but only for the virtual orbitals
  END_DOC
  
  integer                        :: i,j,p,q,r,s
  integer                        :: i0,j0
  double precision               :: c
  real(integral_kind)            :: integral
  integer                        :: n, pp
  real(integral_kind), allocatable :: int_value(:)
  integer, allocatable           :: int_idx(:)
  
  double precision, allocatable  :: iqrs(:,:), iqsr(:,:), iqis(:), iqri(:)
  
  if (.not.do_direct_integrals) then
    PROVIDE ao_bielec_integrals_in_map mo_coef
  endif
  
  mo_bielec_integral_vv_from_ao = 0.d0
  mo_bielec_integral_vv_exchange_from_ao = 0.d0
  
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: iqrs, iqsr
  
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP PRIVATE (i0,j0,i,j,p,q,r,s,integral,c,n,pp,int_value,int_idx,&
      !$OMP  iqrs, iqsr,iqri,iqis)                                   &
      !$OMP SHARED(n_virt_orb,mo_tot_num,list_virt,mo_coef_transp,mo_tot_num_align,ao_num,&
      !$OMP  ao_integrals_threshold,do_direct_integrals)             &
      !$OMP REDUCTION(+:mo_bielec_integral_vv_from_ao,mo_bielec_integral_vv_exchange_from_ao)
  
  allocate( int_value(ao_num), int_idx(ao_num),                      &
      iqrs(mo_tot_num_align,ao_num), iqis(mo_tot_num), iqri(mo_tot_num),&
      iqsr(mo_tot_num_align,ao_num) )
  
  !$OMP DO SCHEDULE (guided)
  do s=1,ao_num
    do q=1,ao_num
      
      do j=1,ao_num
        !DIR$ VECTOR ALIGNED
        do i0=1,n_virt_orb
          i = list_virt(i0)
          iqrs(i,j) = 0.d0
          iqsr(i,j) = 0.d0
        enddo
      enddo
      
      if (do_direct_integrals) then
        double precision               :: ao_bielec_integral
        do r=1,ao_num
          call compute_ao_bielec_integrals(q,r,s,ao_num,int_value)
          do p=1,ao_num
            integral = int_value(p)
            if (abs(integral) > ao_integrals_threshold) then
              !DIR$ VECTOR ALIGNED
              do i0=1,n_virt_orb
                i = list_virt(i0)
                iqrs(i,r) += mo_coef_transp(i,p) * integral
              enddo
            endif
          enddo
          call compute_ao_bielec_integrals(q,s,r,ao_num,int_value)
          do p=1,ao_num
            integral = int_value(p)
            if (abs(integral) > ao_integrals_threshold) then
              !DIR$ VECTOR ALIGNED
              do i0=1,n_virt_orb
                i =list_virt(i0)
                iqsr(i,r) += mo_coef_transp(i,p) * integral
              enddo
            endif
          enddo
        enddo
        
      else
        
        do r=1,ao_num
          call get_ao_bielec_integrals_non_zero(q,r,s,ao_num,int_value,int_idx,n)
          do pp=1,n
            p = int_idx(pp)
            integral = int_value(pp)
            if (abs(integral) > ao_integrals_threshold) then
              !DIR$ VECTOR ALIGNED
              do i0=1,n_virt_orb
                i =list_virt(i0)
                iqrs(i,r) += mo_coef_transp(i,p) * integral
              enddo
            endif
          enddo
          call get_ao_bielec_integrals_non_zero(q,s,r,ao_num,int_value,int_idx,n)
          do pp=1,n
            p = int_idx(pp)
            integral = int_value(pp)
            if (abs(integral) > ao_integrals_threshold) then
              !DIR$ VECTOR ALIGNED
              do i0=1,n_virt_orb
                i = list_virt(i0)
                iqsr(i,r) += mo_coef_transp(i,p) * integral
              enddo
            endif
          enddo
        enddo
      endif
      iqis = 0.d0
      iqri = 0.d0
      do r=1,ao_num
        !DIR$ VECTOR ALIGNED
        do i0=1,n_virt_orb
          i = list_virt(i0)
          iqis(i) += mo_coef_transp(i,r) * iqrs(i,r)
          iqri(i) += mo_coef_transp(i,r) * iqsr(i,r)
        enddo
      enddo
      do i0=1,n_virt_orb
        i= list_virt(i0)
        !DIR$ VECTOR ALIGNED
        do j0=1,n_virt_orb
          j = list_virt(j0)
          c = mo_coef_transp(j,q)*mo_coef_transp(j,s)
          mo_bielec_integral_vv_from_ao(j,i) += c * iqis(i)
          mo_bielec_integral_vv_exchange_from_ao(j,i) += c * iqri(i)
        enddo
      enddo
      
    enddo
  enddo
  !$OMP END DO NOWAIT
  deallocate(iqrs,iqsr,int_value,int_idx)
  !$OMP END PARALLEL
  
  mo_bielec_integral_vv_anti_from_ao = mo_bielec_integral_vv_from_ao - mo_bielec_integral_vv_exchange_from_ao
  ! print*, '**********'
  ! do i0 =1, n_virt_orb
  !  i = list_virt(i0)
  !  print*, mo_bielec_integral_vv_from_ao(i,i)
  ! enddo
  ! print*, '**********'
  
  
END_PROVIDER


 BEGIN_PROVIDER [ double precision, mo_bielec_integral_jj, (mo_tot_num_align,mo_tot_num) ]
&BEGIN_PROVIDER [ double precision, mo_bielec_integral_jj_exchange, (mo_tot_num_align,mo_tot_num) ]
&BEGIN_PROVIDER [ double precision, mo_bielec_integral_jj_anti, (mo_tot_num_align,mo_tot_num) ]
  implicit none
  BEGIN_DOC
  ! mo_bielec_integral_jj(i,j) = J_ij
  ! mo_bielec_integral_jj_exchange(i,j) = K_ij
  ! mo_bielec_integral_jj_anti(i,j) = J_ij - K_ij
  END_DOC
  
  integer                        :: i,j
  double precision               :: get_mo_bielec_integral
  
  PROVIDE mo_bielec_integrals_in_map
  mo_bielec_integral_jj = 0.d0
  mo_bielec_integral_jj_exchange = 0.d0
  
  do j=1,mo_tot_num
    do i=1,mo_tot_num
      mo_bielec_integral_jj(i,j) = get_mo_bielec_integral(i,j,i,j,mo_integrals_map)
      mo_bielec_integral_jj_exchange(i,j) = get_mo_bielec_integral(i,j,j,i,mo_integrals_map)
      mo_bielec_integral_jj_anti(i,j) = mo_bielec_integral_jj(i,j) - mo_bielec_integral_jj_exchange(i,j)
    enddo
  enddo
  
END_PROVIDER


subroutine clear_mo_map
  implicit none
  BEGIN_DOC
  ! Frees the memory of the MO map
  END_DOC
  call map_deinit(mo_integrals_map)
  FREE mo_integrals_map mo_bielec_integral_jj mo_bielec_integral_jj_anti
  FREE mo_bielec_integral_jj_exchange mo_bielec_integrals_in_map
  
  
end

subroutine provide_all_mo_integrals
  implicit none
  provide mo_integrals_map mo_bielec_integral_jj mo_bielec_integral_jj_anti
  provide mo_bielec_integral_jj_exchange mo_bielec_integrals_in_map
  
end

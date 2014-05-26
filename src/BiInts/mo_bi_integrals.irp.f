subroutine mo_bielec_integrals_index(i,j,k,l,i1)
  implicit none
  BEGIN_DOC
  ! Computes an unique index for i,j,k,l integrals
  END_DOC
  integer, intent(in)            :: i,j,k,l
  integer*8, intent(out)         :: i1
  integer*8                      :: p,q,r,s,i2
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
  implicit none
  
  BEGIN_DOC
  ! If True, the map of MO bielectronic integrals is provided
  END_DOC
  
  mo_bielec_integrals_in_map = .True.
  if (read_mo_integrals) then
    integer                        :: load_mo_integrals
    if (load_mo_integrals(trim(ezfio_filename)//'/work/mo_integrals.bin') == 0) then
      write(output_BiInts,*) 'MO integrals provided'
      return
    endif
  endif
  
  call add_integrals_to_map(full_ijkl_bitmask)
END_PROVIDER

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
  integer*8,allocatable          :: buffer_i(:)
  real(integral_kind),allocatable :: buffer_value(:)
  real                           :: map_mb
  
  integer                        :: i1,j1,k1,l1, ii1, kmax, thread_num
  integer                        :: i2,i3,i4
  double precision,parameter     :: thr_coef = 0.d0
  
  PROVIDE N_int ao_bielec_integrals_in_map ao_integrals_map mo_coef mo_coef_transp
  
  !Get list of MOs for i,j,k and l
  !-------------------------------
  
  allocate(list_ijkl(mo_tot_num,4))
  call bitstring_to_list( mask_ijkl(1,1), list_ijkl(1,1), n_i, N_int )
  call bitstring_to_list( mask_ijkl(1,2), list_ijkl(1,2), n_j, N_int )
  call bitstring_to_list( mask_ijkl(1,3), list_ijkl(1,3), n_k, N_int )
  call bitstring_to_list( mask_ijkl(1,4), list_ijkl(1,4), n_l, N_int )
  
  size_buffer = min(ao_num*ao_num*ao_num,16000000)
  write(output_BiInts,*) 'Providing the molecular integrals '
  write(output_BiInts,*) 'Buffers : ', 8.*(mo_tot_num_align*(n_j)*(n_k+1) + mo_tot_num_align +&
      ao_num+ao_num*ao_num+ size_buffer*3)/(1024*1024), 'MB / core'
  
  call wall_time(wall_1)
  call cpu_time(cpu_1)
  mo_integrals_threshold = 0.d0
  
  !$OMP PARALLEL PRIVATE(l1,k1,j1,i1,i2,i3,i4,i,j,k,l,c, ii1,kmax,   &
      !$OMP  bielec_tmp_0_idx, bielec_tmp_0, bielec_tmp_1,bielec_tmp_2,bielec_tmp_3,&
      !$OMP  buffer_i,buffer_value,n_integrals,wall_2,i0,j0,k0,l0,   &
      !$OMP  wall_0,thread_num)   &
      !$OMP  DEFAULT(NONE)                                           &
      !$OMP  SHARED(size_buffer,ao_num,mo_tot_num,n_i,n_j,n_k,n_l,mo_tot_num_align,&
      !$OMP  mo_coef_transp,output_BiInts,                           &
      !$OMP  mo_coef_transp_is_built, list_ijkl,                     &
      !$OMP  mo_coef_is_built, wall_1, abort_here,                   &
      !$OMP  mo_coef,mo_integrals_threshold,ao_integrals_map,mo_integrals_map)
  n_integrals = 0
  allocate(bielec_tmp_3(mo_tot_num_align, n_j, n_k),                 &
      bielec_tmp_1(mo_tot_num_align),                                &
      bielec_tmp_0(ao_num,ao_num),                                   &
      bielec_tmp_0_idx(ao_num),                                      &
      bielec_tmp_2(mo_tot_num_align, n_j),                           &
      buffer_i(size_buffer),                                         &
      buffer_value(size_buffer) )
  
!$  thread_num = omp_get_thread_num()
  !$OMP DO SCHEDULE(guided)
  do l1 = 1,ao_num
    if (abort_here) then
      cycle
    endif
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
          enddo
          
          do i = 1, min(k,j1-i1)
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
        write(output_BiInts,*) 100.*float(l1)/float(ao_num), '% in ',  &
            wall_2-wall_1, 's', map_mb(ao_integrals_map) ,'MB'
      endif
    endif
  enddo
  !$OMP END DO NOWAIT
  deallocate (bielec_tmp_1,bielec_tmp_2,bielec_tmp_3)
  
  call insert_into_mo_integrals_map(n_integrals,buffer_i,buffer_value,&
      real(mo_integrals_threshold,integral_kind))
  deallocate(buffer_i, buffer_value)
  !$OMP END PARALLEL
  if (abort_here) then
    stop 'Aborting in MO integrals calculation'
  endif
  call map_unique(mo_integrals_map)
  
  call wall_time(wall_2)
  call cpu_time(cpu_2)
  integer*8                      :: get_mo_map_size, mo_map_size
  mo_map_size = get_mo_map_size()
  
  deallocate(list_ijkl)
  
  
  write(output_BiInts,*)'Molecular integrals provided:'
  write(output_BiInts,*)' Size of MO map           ', map_mb(mo_integrals_map) ,'MB'
  write(output_BiInts,*)' Number of MO integrals: ',  mo_map_size
  write(output_BiInts,*)' cpu  time :',cpu_2 - cpu_1, 's'
  write(output_BiInts,*)' wall time :',wall_2 - wall_1, 's  ( x ', (cpu_2-cpu_1)/(wall_2-wall_1), ')'
  
  if (write_mo_integrals) then
    call dump_mo_integrals(trim(ezfio_filename)//'/work/mo_integrals.bin')
  endif
  
  
end





 BEGIN_PROVIDER [ double precision, mo_bielec_integral_jj, (mo_tot_num_align,mo_tot_num) ]
&BEGIN_PROVIDER [ double precision, mo_bielec_integral_jj_exchange, (mo_tot_num_align,mo_tot_num) ]
&BEGIN_PROVIDER [ double precision, mo_bielec_integral_jj_anti, (mo_tot_num_align,mo_tot_num) ]
  implicit none
  BEGIN_DOC
  ! Transform Bi-electronic integrals <ij|ij> and <ij|ji>
  END_DOC
  
  integer                        :: i,j,p,q,r,s
  double precision               :: c
  real(integral_kind)            :: integral
  integer                        :: n, pp
  real(integral_kind), allocatable :: int_value(:)
  integer, allocatable           :: int_idx(:)
  
  double precision, allocatable  :: iqrs(:,:), iqsr(:,:), iqis(:), iqri(:)
  
  PROVIDE ao_integrals_threshold
  if (.not.do_direct_integrals) then
    PROVIDE ao_bielec_integrals_in_map
  else
    PROVIDE ao_overlap_abs
  endif
  
  mo_bielec_integral_jj = 0.d0
  mo_bielec_integral_jj_exchange = 0.d0
  
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: iqrs, iqsr
  
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP PRIVATE (i,j,p,q,r,s,integral,c,n,pp,int_value,int_idx,  &
      !$OMP  iqrs, iqsr,iqri,iqis)                                   &
      !$OMP SHARED(mo_tot_num,mo_coef_transp,mo_tot_num_align,ao_num,&
      !$OMP  ao_integrals_threshold,do_direct_integrals,abort_here)  &
      !$OMP REDUCTION(+:mo_bielec_integral_jj,mo_bielec_integral_jj_exchange)
  
  allocate( int_value(ao_num), int_idx(ao_num),                      &
      iqrs(mo_tot_num_align,ao_num), iqis(mo_tot_num), iqri(mo_tot_num),&
      iqsr(mo_tot_num_align,ao_num) )
  
  !$OMP DO SCHEDULE (guided)
  do s=1,ao_num
    if (abort_here) then
      cycle
    endif
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
          mo_bielec_integral_jj(j,i) += c * iqis(i)
          mo_bielec_integral_jj_exchange(j,i) += c * iqri(i)
        enddo
      enddo
      
    enddo
  enddo
  !$OMP END DO NOWAIT
  deallocate(iqrs,iqsr,int_value,int_idx)
  !$OMP END PARALLEL
  if (abort_here) then
    stop 'Aborting in MO integrals calculation'
  endif
  
  mo_bielec_integral_jj_anti = mo_bielec_integral_jj - mo_bielec_integral_jj_exchange
  
  
END_PROVIDER


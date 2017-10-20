subroutine four_index_transform(map_a,map_c,matrix_B,LDB,            &
      i_start, j_start, k_start, l_start,                            &
      i_end  , j_end  , k_end  , l_end  ,                            &
      a_start, b_start, c_start, d_start,                            &
      a_end  , b_end  , c_end  , d_end  )
  implicit none
  use map_module
  use mmap_module
  BEGIN_DOC
! Performs a four-index transformation of map_a(N^4) into map_c(M^4) using b(NxM)
! C_{abcd} = \sum_{ijkl} A_{ijkl}.B_{ia}.B_{jb}.B_{kc}.B_{ld}
! Loops run over *_start->*_end
  END_DOC
  type(map_type), intent(in)     :: map_a
  type(map_type), intent(inout)  :: map_c
  integer, intent(in)            :: LDB
  double precision, intent(in)   :: matrix_B(LDB,*)
  integer, intent(in)            :: i_start, j_start, k_start, l_start
  integer, intent(in)            :: i_end  , j_end  , k_end  , l_end
  integer, intent(in)            :: a_start, b_start, c_start, d_start
  integer, intent(in)            :: a_end  , b_end  , c_end  , d_end

  double precision, allocatable  :: T(:,:,:), U(:,:,:), V(:,:,:)
  integer                        :: i_max, j_max, k_max, l_max
  integer                        :: i_min, j_min, k_min, l_min
  integer                        :: i, j, k, l
  integer                        :: a, b, c, d
  double precision, external     :: get_ao_bielec_integral
  integer(key_kind)              :: idx
  real(integral_kind)            :: tmp
  integer(key_kind), allocatable :: key(:)
  real(integral_kind), allocatable :: value(:)

  ASSERT (k_start == i_start)
  ASSERT (l_start == j_start)
  ASSERT (a_start == c_start)
  ASSERT (b_start == d_start)

  i_min = min(i_start,a_start)
  i_max = max(i_end  ,a_end  )
  j_min = min(j_start,b_start)
  j_max = max(j_end  ,b_end  )
  k_min = min(k_start,c_start)
  k_max = max(k_end  ,c_end  )
  l_min = min(l_start,d_start)
  l_max = max(l_end  ,d_end  )

  ASSERT (0 < i_max)
  ASSERT (0 < j_max)
  ASSERT (0 < k_max)
  ASSERT (0 < l_max)
  ASSERT (LDB >= i_max)
  ASSERT (LDB >= j_max)
  ASSERT (LDB >= k_max)
  ASSERT (LDB >= l_max)

  ! Create a temporary memory-mapped file
  integer                        :: fd
  type(c_ptr)                    :: c_pointer
  integer*8, pointer             :: a_array(:,:,:)
  call mmap(trim(ezfio_filename)//'/work/four_idx',                  &
      (/ 4_8,int(i_end-i_start+1,8),int(j_end-j_start+1,8),int(k_end-k_start+1,8), int(l_end-l_start+1,8) /), 8, fd, .False., c_pointer)
  call c_f_pointer(c_pointer, a_array, (/ 4, (i_end-i_start+1)*(j_end-j_start+1)*(k_end-k_start+1), l_end-l_start+1 /))


  !$OMP PARALLEL DEFAULT(NONE) SHARED(a_array,c_pointer,fd,          &
      !$OMP  a_start,a_end,b_start,b_end,c_start,c_end,d_start,d_end,&
      !$OMP  i_start,i_end,j_start,j_end,k_start,k_end,l_start,l_end,&
      !$OMP  i_min,i_max,j_min,j_max,k_min,k_max,l_min,l_max,        &
      !$OMP  map_a,map_c,matrix_B)                                   &
      !$OMP  PRIVATE(key,value,T,U,V,i,j,k,l,idx,   &
      !$OMP  a,b,c,d,tmp)
  allocate( key(i_max*j_max*k_max), value(i_max*j_max*k_max) )
  allocate( U(a_start:a_end, c_start:c_end, b_start:b_end) )


  !$OMP DO SCHEDULE(dynamic,4)
  do l=l_start,l_end
    a = 1
    do j=j_start,j_end
      do k=k_start,k_end
        do i=i_start,i_end
          call bielec_integrals_index(i,j,k,l,idx)
          call map_get(map_a,idx,tmp)
          if (tmp /= 0.d0) then
            a = a+1
            a_array(1,a,l-l_start+1) = i
            a_array(2,a,l-l_start+1) = j
            a_array(3,a,l-l_start+1) = k
            a_array(4,a,l-l_start+1) = transfer(dble(tmp), 1_8)
          endif
        enddo
      enddo
    enddo
    a_array(1,1,l-l_start+1) = a
    print *,  l
  enddo
  !$OMP END DO

  !$OMP DO SCHEDULE(dynamic)
  do d=d_start,d_end
    U = 0.d0
    do l=l_start,l_end
      if (dabs(matrix_B(l,d)) < 1.d-10) then
        cycle
      endif
      print *,  d, l

      allocate( T(i_start:i_end, k_start:k_end, j_start:j_end), &
                V(a_start:a_end, k_start:k_end, j_start:j_end) )

      T = 0.d0
      do a=2,a_array(1,1,l-l_start+1)
        i = a_array(1,a,l-l_start+1)
        j = a_array(2,a,l-l_start+1)
        k = a_array(3,a,l-l_start+1)
        T(i, k,j) = transfer(a_array(4,a,l-l_start+1), 1.d0)
      enddo

      call DGEMM('T','N', (a_end-a_start+1),                         &
          (k_end-k_start+1)*(j_end-j_start+1),                       &
          (i_end-i_start+1), 1.d0,                                   &
          matrix_B(i_start,a_start), size(matrix_B,1),               &
          T(i_start,k_start,j_start), size(T,1),  0.d0,              &
          V(a_start,k_start,j_start), size(V, 1) )

      deallocate(T)
      allocate( T(a_start:a_end, k_start:k_end, b_start:d) )

      call DGEMM('N','N', (a_end-a_start+1)*(k_end-k_start+1),       &
              (b_end-b_start+1),                                     &
              (j_end-j_start+1), 1.d0,                               &
              V(a_start,k_start,j_start), size(V,1)*size(V,2),       &
              matrix_B(j_start,b_start), size(matrix_B,1),0.d0,      &
              T(a_start,k_start,b_start), size(T,1)*size(T,2) )

      deallocate(V)

      do b=b_start,b_end
        call DGEMM('N','N', (a_end-a_start+1), (c_end-c_start+1),    &
            (k_end-k_start+1), matrix_B(l, d),                   &
            T(a_start,k_start,b), size(T,1),                     &
            matrix_B(k_start,c_start), size(matrix_B,1), 1.d0,   &
            U(a_start,c_start,b), size(U,1) )
      enddo

      deallocate(T)

    enddo

    idx = 0_8
    do b=b_start,b_end
      do c=c_start,c_end
        do a=a_start,a_end
          if (dabs(U(a,c,b)) < 1.d-15) then
            cycle
          endif
          idx = idx+1_8
          call bielec_integrals_index(a,b,c,d,key(idx))
          value(idx) = U(a,c,b)
        enddo
      enddo
    enddo

    !$OMP CRITICAL
    call map_append(map_c, key, value, idx) 
    call map_sort(map_c)
    !$OMP END CRITICAL


  enddo
  !$OMP END DO

  deallocate(key,value)
  !$OMP END PARALLEL

  call munmap( &
      (/ 4_8,int(i_end-i_start+1,8),int(j_end-j_start+1,8),int(k_end-k_start+1,8), int(l_end-l_start+1,8) /), 8, fd, c_pointer)

end

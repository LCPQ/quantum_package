subroutine four_index_transform_sym(map_a,map_c,matrix_B,LDB,            &
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

  double precision, allocatable  :: T(:,:), U(:,:,:), V(:,:)
  double precision, allocatable  :: T2d(:,:), V2d(:,:)
  integer                        :: i_max, j_max, k_max, l_max
  integer                        :: i_min, j_min, k_min, l_min
  integer                        :: i, j, k, l, ik, ll
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
  integer*8, pointer             :: a_array(:,:)
  call mmap(trim(ezfio_filename)//'/work/four_idx',                  &
      (/ (int(i_end-i_start+1,8)*int(j_end-j_start+2,8)*int(k_end-k_start+1,8)),int(l_end-l_start+1,8) /), 16, fd, .False., c_pointer)
  call c_f_pointer(c_pointer, a_array, (/ ((i_end-i_start+1)*(j_end-j_start+1)*(k_end-k_start+1)*3_8)/2_8, l_end-l_start+1_8 /))

  !$OMP PARALLEL DEFAULT(NONE) SHARED(a_array,c_pointer,fd,          &
      !$OMP  a_start,a_end,b_start,b_end,c_start,c_end,d_start,d_end,&
      !$OMP  i_start,i_end,j_start,j_end,k_start,k_end,l_start,l_end,&
      !$OMP  i_min,i_max,j_min,j_max,k_min,k_max,l_min,l_max,        &
      !$OMP  map_a,map_c,matrix_B)                                   &
      !$OMP  PRIVATE(key,value,T,U,V,i,j,k,l,idx,ik,ll,   &
      !$OMP  a,b,c,d,tmp,T2d,V2d)
  allocate( key(i_max*j_max*k_max), value(i_max*j_max*k_max) )
  allocate( U(a_start:a_end, c_start:c_end, b_start:b_end) )


  !$OMP DO SCHEDULE(dynamic,4)
  do l=l_start,l_end
    a = 1
    ll = l-l_start+1
    do j=j_start,j_end
      ik=0
      do k=k_start,k_end
        do i=i_start,k
          ik = ik+1
          call bielec_integrals_index(i,j,k,l,idx)
          call map_get(map_a,idx,tmp)
          if (tmp /= 0.d0) then
            a = a+1
            a_array(a,ll) = ik
            a = a+1
            a_array(a,ll) = j
            a = a+1
            a_array(a,ll) = transfer(dble(tmp), 1_8)
          endif
        enddo
      enddo
    enddo
    a_array(a+1,ll) = 0
    a_array(1,ll) = a
  enddo
  !$OMP END DO

  allocate( T2d((i_end-i_start+1)*(k_end-k_start+2)/2, j_start:j_end), &
            V2d((i_end-i_start+1)*(k_end-k_start+2)/2, b_start:b_end), &
            V(i_start:i_end, k_start:k_end), &
            T(k_start:k_end, a_start:a_end))


  !$OMP DO SCHEDULE(dynamic)
  do d=d_start,d_end
    U = 0.d0
    do l=l_start,l_end
      if (dabs(matrix_B(l,d)) < 1.d-10) then
        cycle
      endif
      
      ll = l-l_start+1
!      T2d = 0.d0
!      do a=2,a_array(1,ll),3
!        ik = a_array(a,ll)
!        j = a_array(a+1,ll)
!        T2d(ik,j) = transfer(a_array(a+2,ll), 1.d0)
!      enddo

      a=2
      do j=j_start,j_end
        ik=0
        do k=k_start,k_end
          do i=i_start,k
            ik = ik+1
            if ( (ik /= a_array(a,ll)).or.(j /= a_array(a+1,ll)) ) then
              T2d(ik,j) = 0.d0
            else
              T2d(ik,j) = transfer(a_array(a+2,ll), 1.d0)
              a=a+3
            endif
          enddo
        enddo
      enddo
      call DGEMM('N','N', ishft( (i_end-i_start+1)*(i_end-i_start+2), -1),&
          (d-b_start+1),                                             &
          (j_end-j_start+1), 1.d0,                                   &
          T2d(1,j_start), size(T2d,1),                               &
          matrix_B(j_start,b_start), size(matrix_B,1),0.d0,          &
          V2d(1,b_start), size(V2d,1) )

      do b=b_start,d
        ik = 0
        do k=k_start,k_end
          do i=i_start,k
            ik = ik+1
            V(i,k) = V2d(ik,b)
          enddo
        enddo

!        T = 0.d0
!        do a=a_start,b
!          do k=k_start,k_end
!            do i=i_start,k
!              T(k,a) = T(k,a) + V(i,k)*matrix_B(i,a)
!            enddo
!            do i=k+1,i_end
!              T(k,a) = T(k,a) + V(k,i)*matrix_B(i,a)
!            enddo
!          enddo
!        enddo
        call DSYMM('L','U', (k_end-k_start+1), (b-a_start+1),        &
            1.d0,                                                    &
            V(i_start,k_start), size(V,1),                           &
            matrix_B(i_start,a_start), size(matrix_B,1),0.d0,        &
            T(k_start,a_start), size(T,1) )

!        do c=c_start,b
!          do a=a_start,c
!            do k=k_start,k_end
!              U(a,c,b) = U(a,c,b) + T(k,a)*matrix_B(k,c)*matrix_B(l,d)
!            enddo
!          enddo
!        enddo
        call DGEMM('T','N', (b-a_start+1), (b-c_start+1),            &
            (k_end-k_start+1), matrix_B(l, d),                       &
            T(k_start,a_start), size(T,1),                           &
            matrix_B(k_start,c_start), size(matrix_B,1), 1.d0,       &
            U(a_start,c_start,b), size(U,1) )
!        do c=b+1,c_end
!          do a=a_start,b
!            do k=k_start,k_end
!              U(a,c,b) = U(a,c,b) + T(k,a)*matrix_B(k,c)*matrix_B(l,d)
!            enddo
!          enddo
!        enddo
        if (b < b_end) then
          call DGEMM('T','N', (b-a_start+1), (c_end-b),              &
              (k_end-k_start+1), matrix_B(l, d),                     &
              T(k_start,a_start), size(T,1),                         &
              matrix_B(k_start,b+1), size(matrix_B,1), 1.d0,         &
              U(a_start,b+1,b), size(U,1) )
        endif
      enddo

    enddo

    idx = 0_8
    do b=b_start,d
      do c=c_start,c_end
        do a=a_start,min(b,c)
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
    !$OMP END CRITICAL


  enddo
  !$OMP END DO

  deallocate(key,value,V,T)
  !$OMP END PARALLEL
  call map_sort(map_c)

  call munmap( &
      (/ (int(i_end-i_start+1,8)*int(j_end-j_start+2,8)*int(k_end-k_start+1,8)),int(l_end-l_start+1,8) /), 16, fd, c_pointer)

end

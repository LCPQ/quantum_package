subroutine four_index_transform(map_a,map_c,matrix_B,LDB,            &
      i_start, j_start, k_start, l_start,                            &
      i_end  , j_end  , k_end  , l_end  ,                            &
      a_start, b_start, c_start, d_start,                            &
      a_end  , b_end  , c_end  , d_end  )
  implicit none
  use map_module
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

  !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(                            &
      !$OMP  a_start,a_end,b_start,b_end,c_start,c_end,d_start,d_end,&
      !$OMP  i_start,i_end,j_start,j_end,k_start,k_end,l_start,l_end,&
      !$OMP  i_min,i_max,j_min,j_max,k_min,k_max,l_min,l_max,        &
      !$OMP  map_a,map_c,matrix_B)
  allocate( key(i_max*j_max*k_max), value(i_max*j_max*k_max) )
  allocate( U(a_start:a_end, b_start:b_end, c_start:c_end) )

  !$OMP DO
  do d=d_start,d_end
    U = 0.d0
    print *,  d
    do l=1,l_end
      if (dabs(matrix_B(l,d)) < 1.d-10) then
        cycle
      endif

      allocate( T(i_start:i_end, k_start:k_end, j_start:j_end), &
                V(a_start:a_end, k_start:k_end, j_start:j_end) )

      do k=k_start,k_end
        do j=j_start,j_end
          do i=i_start,k
            call bielec_integrals_index(i,j,k,l,idx)
            call map_get(map_a,idx,tmp)
            T(i, k,j) = tmp
            T(k, i,j) = tmp
          enddo
        enddo
      enddo


      call DGEMM('T','N', (a_end-a_start+1),                         &
          (k_end-k_start+1)*(j_end-j_start+1),                       &
          (i_end-i_start+1), 1.d0,                                   &
          matrix_B(i_start,a_start), size(matrix_B,1),               &
          T(i_start,k_start,j_start), size(T,1),  0.d0,              &
          V(a_start,k_start,j_start), size(V, 1) )

      deallocate(T)
      allocate( T(a_start:a_end, k_start:k_end, b_start:b_end) )

      call DGEMM('N','N', (a_end-a_start+1)*(k_end-k_start+1),       &
              (b_end-b_start+1),                                     &
              (j_end-j_start+1), 1.d0,                               &
              V(a_start,k_start,j_start), size(V,1)*size(V,2),       &
              matrix_B(j_start,b_start), size(matrix_B,1),0.d0,      &
              T(a_start,k_start,b_start), size(T,1)*size(T,2) )

      deallocate(V)

      allocate( V(a_start:a_end, b_start:b_end, c_start:c_end) )
      V = 0.d0
      do b=b_start,b_end
        call DGEMM('N','N', (a_end-a_start+1), (c_end-c_start+1),    &
            (k_end-k_start+1), 1.d0,                                 &
            T(a_start,k_start,b), size(T,1),                         &
            matrix_B(k_start,k_start), size(matrix_B,1), 1.d0,       &
            V(a_start,c_start,b), size(V,1) )
      enddo

      U = U + V*matrix_B(l, d)
      deallocate(T,V)

    enddo

    idx = 0_8
    do c=c_start,c_end
      do b=b_start,b_end
        do a=a_start,a_end
!    do c=c_start,c_end
!      do b=b_start,d
!        do a=a_start,min(b,c)
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
    call map_unique(map_c)
    !$OMP END CRITICAL

  enddo
  !$OMP END DO

  deallocate(key,value)
  !$OMP END PARALLEL

end

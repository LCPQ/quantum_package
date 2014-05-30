use bitmasks
BEGIN_PROVIDER [ integer, N_con_int ]
  implicit none
  BEGIN_DOC
  ! Number of integers to represent the connections between determinants
  END_DOC
  N_con_int = 1 + ishft(N_det-1,-13)
END_PROVIDER

BEGIN_PROVIDER [ integer*8, det_connections, (N_con_int,N_det) ]
  implicit none
  BEGIN_DOC
  !
  END_DOC
  integer                        :: i,j
  integer                        :: degree
  integer                        :: j_int, j_k, j_l
  !$OMP PARALLEL DEFAULT (NONE) &
  !$OMP SHARED(N_det, N_con_int, psi_det,N_int, det_connections) &
  !$OMP PRIVATE(i,j_int,j_k,j_l,j,degree)
  !$OMP DO SCHEDULE(guided)
  do i=1,N_det
    do j_int=1,N_con_int
      det_connections(j_int,i) = 0_8
      j_k = ishft(j_int-1,13)
      do j_l = j_k,min(j_k+8191,N_det), 128
        do j = j_l+1,min(j_l+128,i)
          call get_excitation_degree(psi_det(1,1,i),psi_det(1,1,j),degree,N_int)
          if (degree < 3) then
            det_connections(j_int,i) = ibset( det_connections(j_int,i), iand(63,ishft(j_l,-7)) )
            exit
          endif
        enddo
      enddo
    enddo
  enddo
  !$OMP ENDDO
  !$OMP ENDPARALLEL

END_PROVIDER


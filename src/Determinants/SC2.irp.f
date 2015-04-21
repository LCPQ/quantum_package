subroutine CISD_SC2(dets_in,u_in,energies,dim_in,sze,N_st,Nint,convergence)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! CISD+SC2 method              :: take off all the disconnected terms of a CISD (selected or not)
  !
  ! dets_in : bitmasks corresponding to determinants
  !
  ! u_in : guess coefficients on the various states. Overwritten
  !   on exit
  !
  ! dim_in : leftmost dimension of u_in
  !
  ! sze : Number of determinants
  !
  ! N_st : Number of eigenstates
  !
  ! Initial guess vectors are not necessarily orthonormal
  END_DOC
  integer, intent(in)            :: dim_in, sze, N_st, Nint
  integer(bit_kind), intent(in)  :: dets_in(Nint,2,sze)
  double precision, intent(inout) :: u_in(dim_in,N_st)
  double precision, intent(out)  :: energies(N_st)
  double precision, intent(in)   :: convergence
  ASSERT (N_st > 0)
  ASSERT (sze > 0)
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  integer                        :: iter
  integer                        :: i,j,k,l,m
  logical                        :: converged
  double precision               :: overlap(N_st,N_st)
  double precision               :: u_dot_v, u_dot_u
  
  integer                        :: degree,N_double,index_hf
  double precision               :: hij_elec, e_corr_double,e_corr,diag_h_mat_elem,inv_c0
  double precision               :: e_corr_double_before,accu,cpu_2,cpu_1
  integer,allocatable            :: degree_exc(:), index_double(:)
  integer                        :: i_ok
  double precision,allocatable   :: e_corr_array(:),H_jj_ref(:),H_jj_dressed(:),hij_double(:)
  integer(bit_kind), allocatable :: doubles(:,:,:)

  
  allocate (doubles(Nint,2,sze),e_corr_array(sze),H_jj_ref(sze),H_jj_dressed(sze),&
      index_double(sze), degree_exc(sze), hij_double(sze))
  call write_time(output_determinants)
  write(output_determinants,'(A)') ''
  write(output_determinants,'(A)') 'CISD SC2'
  write(output_determinants,'(A)') '========'
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP  SHARED(sze,N_st,                                        &
      !$OMP  H_jj_ref,Nint,dets_in,u_in)                             &
      !$OMP  PRIVATE(i)
  
  !$OMP DO SCHEDULE(guided)
  do i=1,sze
    H_jj_ref(i) = diag_h_mat_elem(dets_in(1,1,i),Nint)
  enddo
  !$OMP END DO NOWAIT
  !$OMP END PARALLEL
  
  N_double = 0
  e_corr = 0.d0
  e_corr_double = 0.d0
  do i = 1, sze
    call get_excitation_degree(ref_bitmask,dets_in(1,1,i),degree,Nint)
    degree_exc(i) = degree+1
    if(degree==0)then
      index_hf=i
    else if (degree == 2)then
      N_double += 1
      index_double(N_double) = i
      doubles(:,:,N_double) = dets_in(:,:,i)
      call i_H_j(ref_bitmask,dets_in(1,1,i),Nint,hij_elec)
      hij_double(N_double) = hij_elec
      e_corr_array(N_double) = u_in(i,1)* hij_elec
      e_corr_double += e_corr_array(N_double)
      e_corr += e_corr_array(N_double)
    else if (degree == 1)then
      call i_H_j(ref_bitmask,dets_in(1,1,i),Nint,hij_elec)
      e_corr += u_in(i,1)* hij_elec
    endif
  enddo
  inv_c0 = 1.d0/u_in(index_hf,1)
  do i = 1, N_double
    e_corr_array(i) = e_corr_array(i) * inv_c0
  enddo
  e_corr = e_corr * inv_c0
  e_corr_double = e_corr_double * inv_c0
  converged = .False.
  e_corr_double_before = e_corr_double
  iter = 0
  do while (.not.converged)
    if (abort_here) then
      exit
    endif
    iter +=1
    !$OMP PARALLEL DEFAULT(NONE)                                     &
        !$OMP PRIVATE(i,j,degree,accu)                               &
        !$OMP SHARED(H_jj_dressed,sze,H_jj_ref,index_hf,N_int,N_double,&
        !$OMP  dets_in,doubles,degree_exc,e_corr_array,e_corr_double)
    !$OMP DO SCHEDULE(STATIC)
    do i=1,sze
      H_jj_dressed(i) = H_jj_ref(i)
      if (i==index_hf)cycle
      accu = -e_corr_double
      select case (N_int)
        case (1)
          do j=1,N_double
            degree =                                                 &
                popcnt(xor( dets_in(1,1,i),doubles(1,1,j))) +        &
                popcnt(xor( dets_in(1,2,i),doubles(1,2,j)))
            
            if (degree<=ishft(degree_exc(i),1)) then
              accu += e_corr_array(j)
            endif
          enddo
        case (2)
          do j=1,N_double
            degree =                                                 &
                popcnt(xor( dets_in(1,1,i),doubles(1,1,j))) +        &
                popcnt(xor( dets_in(1,2,i),doubles(1,2,j))) +        &
                popcnt(xor( dets_in(2,1,i),doubles(2,1,j))) +        &
                popcnt(xor( dets_in(2,2,i),doubles(2,2,j)))
            
            if (degree<=ishft(degree_exc(i),1)) then
              accu += e_corr_array(j)
            endif
          enddo
        case (3)
          do j=1,N_double
            degree =                                                 &
                popcnt(xor( dets_in(1,1,i),doubles(1,1,j))) +        &
                popcnt(xor( dets_in(1,2,i),doubles(1,2,j))) +        &
                popcnt(xor( dets_in(2,1,i),doubles(2,1,j))) +        &
                popcnt(xor( dets_in(2,2,i),doubles(2,2,j))) +        &
                popcnt(xor( dets_in(3,1,i),doubles(3,1,j))) +        &
                popcnt(xor( dets_in(3,2,i),doubles(3,2,j)))
            
            if (degree<=ishft(degree_exc(i),1)) then
              accu += e_corr_array(j)
            endif
          enddo
        case default
          do j=1,N_double
            call get_excitation_degree(dets_in(1,1,i),doubles(1,1,j),degree,N_int)
            if (degree<=degree_exc(i)) then
              accu += e_corr_array(j)
            endif
          enddo
      end select
      H_jj_dressed(i) -= accu
    enddo
    !$OMP END DO
    !$OMP END PARALLEL
    
    if(sze<=N_det_max_jacobi)then
      double precision, allocatable  :: eigenvectors(:,:), eigenvalues(:),H_matrix_tmp(:,:)
      allocate (H_matrix_tmp(size(H_matrix_all_dets,1),sze),eigenvalues(sze),eigenvectors(size(H_matrix_all_dets,1),sze))
      do j=1,sze
        do i=1,sze
          H_matrix_tmp(i,j) = H_matrix_all_dets(i,j)
        enddo
      enddo
      do i = 1,sze
        H_matrix_tmp(i,i) = H_jj_dressed(i)
      enddo
      call lapack_diag(eigenvalues,eigenvectors,                     &
          H_matrix_tmp,size(H_matrix_all_dets,1),sze)
      do j=1,min(N_states_diag,sze)
        do i=1,sze
          u_in(i,j) = eigenvectors(i,j)
        enddo
        energies(j) = eigenvalues(j)
      enddo
      deallocate (H_matrix_tmp, eigenvalues, eigenvectors)
    else
      call davidson_diag_hjj(dets_in,u_in,H_jj_dressed,energies,dim_in,sze,N_st,Nint,output_determinants)
    endif

    e_corr_double = 0.d0
    inv_c0 = 1.d0/u_in(index_hf,1)
    do i = 1, N_double
      e_corr_array(i) = u_in(index_double(i),1)*inv_c0 * hij_double(i)
      e_corr_double += e_corr_array(i)
    enddo
    write(output_determinants,'(A,I3)') 'SC2 Iteration ', iter
    write(output_determinants,'(A)') '------------------'
    write(output_determinants,'(A)') ''
    write(output_determinants,'(A)') '===== ================'
    write(output_determinants,'(A)') 'State Energy          '
    write(output_determinants,'(A)') '===== ================'
    do i=1,N_st
      write(output_determinants,'(I5,X,F16.10)') i, energies(i)+nuclear_repulsion
    enddo
    write(output_determinants,'(A)') '===== ================'
    write(output_determinants,'(A)') ''
    call write_double(output_determinants,(e_corr_double - e_corr_double_before),&
        'Delta(E_corr)')
    converged =  dabs(e_corr_double - e_corr_double_before) < convergence
    converged = converged .or. abort_here
    if (converged) then
      exit
    endif
    e_corr_double_before = e_corr_double
    
  enddo
  
  call write_time(output_determinants)
  deallocate (doubles,e_corr_array,H_jj_ref,H_jj_dressed,            &
      index_double, degree_exc, hij_double)
  
end



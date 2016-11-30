subroutine davidson_diag_mrcc(dets_in,u_in,energies,dim_in,sze,N_st,N_st_diag,Nint,iunit,istate)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Davidson diagonalization.
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
  ! iunit : Unit number for the I/O
  !
  ! Initial guess vectors are not necessarily orthonormal
  END_DOC
  integer, intent(in)            :: dim_in, sze, N_st, Nint, iunit, istate, N_st_diag
  integer(bit_kind), intent(in)  :: dets_in(Nint,2,sze)
  double precision, intent(inout) :: u_in(dim_in,N_st_diag)
  double precision, intent(out)  :: energies(N_st_diag)
  double precision, allocatable  :: H_jj(:)
  
  double precision               :: diag_h_mat_elem
  integer                        :: i
  ASSERT (N_st > 0)
  ASSERT (N_st_diag >= N_st)
  ASSERT (sze > 0)
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  PROVIDE mo_bielec_integrals_in_map
  allocate(H_jj(sze))
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP  SHARED(sze,H_jj,N_det_ref,dets_in,Nint,istate,delta_ii,idx_ref)           &
      !$OMP  PRIVATE(i)
  !$OMP DO SCHEDULE(guided)
  do i=1,sze
    H_jj(i) = diag_h_mat_elem(dets_in(1,1,i),Nint) 
  enddo
  !$OMP END DO 
  !$OMP DO SCHEDULE(guided)
  do i=1,N_det_ref
    H_jj(idx_ref(i)) +=  delta_ii(istate,i)
  enddo
  !$OMP END DO 
  !$OMP END PARALLEL

  call davidson_diag_hjj_mrcc(dets_in,u_in,H_jj,energies,dim_in,sze,N_st,N_st_diag,Nint,iunit,istate)
  deallocate (H_jj)
end

subroutine davidson_diag_hjj_mrcc(dets_in,u_in,H_jj,energies,dim_in,sze,N_st,N_st_diag,Nint,iunit,istate)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Davidson diagonalization with specific diagonal elements of the H matrix
  !
  ! H_jj : specific diagonal H matrix elements to diagonalize de Davidson
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
  ! N_st_diag : Number of states in which H is diagonalized
  !
  ! iunit : Unit for the I/O
  !
  ! Initial guess vectors are not necessarily orthonormal
  END_DOC
  integer, intent(in)            :: dim_in, sze, N_st, N_st_diag, Nint, istate
  integer(bit_kind), intent(in)  :: dets_in(Nint,2,sze)
  double precision,  intent(in)  :: H_jj(sze)
  integer,  intent(in)  :: iunit
  double precision, intent(inout) :: u_in(dim_in,N_st_diag)
  double precision, intent(out)  :: energies(N_st_diag)
  
  integer                        :: sze_8
  integer                        :: iter
  integer                        :: i,j,k,l,m
  logical                        :: converged
  
  double precision, allocatable  :: overlap(:,:)
  double precision               :: u_dot_v, u_dot_u
  
  integer                        :: k_pairs, kl
  
  integer                        :: iter2
  double precision, allocatable  :: W(:,:,:),  U(:,:,:), R(:,:)
  double precision, allocatable  :: y(:,:,:,:), h(:,:,:,:), lambda(:)
  double precision, allocatable  :: c(:), H_small(:,:)
  double precision               :: diag_h_mat_elem
  double precision, allocatable  :: residual_norm(:)
  character*(16384)              :: write_buffer
  double precision               :: to_print(2,N_st)
  double precision               :: cpu, wall
  include 'constants.include.F'
  
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: U, W, R, y, h, lambda

  PROVIDE nuclear_repulsion

  call write_time(iunit)
  call wall_time(wall)
  call cpu_time(cpu)
  write(iunit,'(A)') ''
  write(iunit,'(A)') 'Davidson Diagonalization'
  write(iunit,'(A)') '------------------------'
  write(iunit,'(A)') ''
  call write_int(iunit,N_st,'Number of states')
  call write_int(iunit,N_st_diag,'Number of states in diagonalization')
  call write_int(iunit,sze,'Number of determinants')
  call write_int(iunit,istate,'Using dressing for state ') 
  write(iunit,'(A)') ''
  write_buffer = '===== '
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ ================'
  enddo
  write(iunit,'(A)') trim(write_buffer)
  write_buffer = ' Iter'
  do i=1,N_st
    write_buffer = trim(write_buffer)//'           Energy         Residual'
  enddo
  write(iunit,'(A)') trim(write_buffer)
  write_buffer = '===== '
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ ================'
  enddo
  write(iunit,'(A)') trim(write_buffer)

  integer, external :: align_double
  sze_8 = align_double(sze)

  allocate(                                                          &
      W(sze_8,N_st_diag,davidson_sze_max),                           &
      U(sze_8,N_st_diag,davidson_sze_max),                           &
      R(sze_8,N_st_diag),                                            &
      h(N_st_diag,davidson_sze_max,N_st_diag,davidson_sze_max),      &
      y(N_st_diag,davidson_sze_max,N_st_diag,davidson_sze_max),      &
      residual_norm(N_st_diag),                                      &
      overlap(N_st_diag,N_st_diag),                                  &
      c(N_st_diag*davidson_sze_max),                                 &
      H_small(N_st_diag,N_st_diag),                                  &
      lambda(N_st_diag*davidson_sze_max))
  
  ASSERT (N_st > 0)
  ASSERT (N_st_diag >= N_st)
  ASSERT (sze > 0)
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  
  ! Initialization
  ! ==============
  
  
  do k=1,N_st_diag

    if (k > N_st) then
      do i=1,sze
        double precision               :: r1, r2
        call random_number(r1)
        call random_number(r2)
        u_in(i,k) = dsqrt(-2.d0*dlog(r1))*dcos(dtwo_pi*r2)
      enddo
    endif
    
    ! Gram-Schmidt
    ! ------------
    call dgemv('T',sze,k-1,1.d0,u_in,size(u_in,1),                   &
        u_in(1,k),1,0.d0,c,1)
    call dgemv('N',sze,k-1,-1.d0,u_in,size(u_in,1),                  &
        c,1,1.d0,u_in(1,k),1)
    call normalize(u_in(1,k),sze)
  enddo


  
  converged = .False.
  do while (.not.converged)
    
    do k=1,N_st_diag
      do i=1,sze
        U(i,k,1) = u_in(i,k)
      enddo
    enddo

    do iter=1,davidson_sze_max-1
      
      ! Compute |W_k> = \sum_i |i><i|H|u_k>
      ! -----------------------------------------
      
      call H_u_0_mrcc_nstates(W(1,1,iter),U(1,1,iter),H_jj,sze,dets_in,Nint,istate,N_st_diag,sze_8)
      
      
      ! Compute h_kl = <u_k | W_l> = <u_k| H |u_l>
      ! -------------------------------------------


      call dgemm('T','N', N_st_diag*iter, N_st_diag, sze,            &
          1.d0, U, size(U,1), W(1,1,iter), size(W,1),                &
          0.d0, h(1,1,1,iter), size(h,1)*size(h,2))

      ! Diagonalize h
      ! -------------
      call lapack_diag(lambda,y,h,N_st_diag*davidson_sze_max,N_st_diag*iter)
      
      ! Express eigenvectors of h in the determinant basis
      ! --------------------------------------------------
      
      do k=1,N_st_diag
        do i=1,sze
          U(i,k,iter+1) = 0.d0
          W(i,k,iter+1) = 0.d0
        enddo
      enddo
!      do k=1,N_st_diag
!         do iter2=1,iter
!          do l=1,N_st_diag
!            do i=1,sze
!              U(i,k,iter+1) = U(i,k,iter+1) + U(i,l,iter2)*y(l,iter2,k,1)
!              W(i,k,iter+1) = W(i,k,iter+1) + W(i,l,iter2)*y(l,iter2,k,1)
!            enddo
!          enddo
!        enddo
!      enddo
!
!
      call dgemm('N','N', sze, N_st_diag, N_st_diag*iter,            &
          1.d0, U, size(U,1), y, size(y,1)*size(y,2), 0.d0, U(1,1,iter+1), size(U,1))
      call dgemm('N','N',sze,N_st_diag,N_st_diag*iter,               &
          1.d0, W, size(W,1), y, size(y,1)*size(y,2), 0.d0, W(1,1,iter+1), size(W,1))


      ! Compute residual vector
      ! -----------------------
      
      do k=1,N_st_diag
        do i=1,sze
          R(i,k) = lambda(k) * U(i,k,iter+1) - W(i,k,iter+1)
        enddo
        if (k <= N_st) then
          residual_norm(k) = u_dot_u(R(1,k),sze)
          to_print(1,k) = lambda(k) + nuclear_repulsion
          to_print(2,k) = residual_norm(k)
        endif
      enddo
      
      write(iunit,'(X,I3,X,100(X,F16.10,X,E16.6))')  iter, to_print(:,1:N_st)
      call davidson_converged(lambda,residual_norm,wall,iter,cpu,N_st,converged)
      if (converged) then
        exit
      endif
      
      ! Davidson step
      ! -------------
      
      do k=1,N_st_diag
        do i=1,sze
          U(i,k,iter+1) = -1.d0/max(H_jj(i) - lambda(k),1.d-2) * R(i,k)
        enddo
      enddo
      
      ! Gram-Schmidt
      ! ------------
      
      do k=1,N_st_diag

!        do iter2=1,iter
!          do l=1,N_st_diag
!            c(1) = u_dot_v(U(1,k,iter+1),U(1,l,iter2),sze)
!            do i=1,sze
!              U(i,k,iter+1) = U(i,k,iter+1) - c(1) * U(i,l,iter2)
!            enddo
!          enddo
!        enddo
!
        call dgemv('T',sze,N_st_diag*iter,1.d0,U,size(U,1),  &
              U(1,k,iter+1),1,0.d0,c,1)
        call dgemv('N',sze,N_st_diag*iter,-1.d0,U,size(U,1), &
              c,1,1.d0,U(1,k,iter+1),1)
!
!        do l=1,k-1
!          c(1) = u_dot_v(U(1,k,iter+1),U(1,l,iter+1),sze)
!          do i=1,sze
!            U(i,k,iter+1) = U(i,k,iter+1) - c(1) * U(i,l,iter+1)
!          enddo
!        enddo
!
        call dgemv('T',sze,k-1,1.d0,U(1,1,iter+1),size(U,1),   &
            U(1,k,iter+1),1,0.d0,c,1)
        call dgemv('N',sze,k-1,-1.d0,U(1,1,iter+1),size(U,1),        &
            c,1,1.d0,U(1,k,iter+1),1)

        call normalize( U(1,k,iter+1), sze )
      enddo

    enddo

    if (.not.converged) then
      iter = davidson_sze_max-1
    endif
    
    ! Re-contract to u_in
    ! -----------
    
    do k=1,N_st_diag
      do i=1,sze
        u_in(i,k) = 0.d0
      enddo
    enddo

    call dgemm('N','N', sze, N_st_diag, N_st_diag*iter, 1.d0,      &
        U, size(U,1), y, N_st_diag*davidson_sze_max, &
        0.d0, u_in, size(u_in,1))

  enddo

  do k=1,N_st_diag
    energies(k) = lambda(k)
  enddo
  write_buffer = '===== '
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ ================'
  enddo
  write(iunit,'(A)') trim(write_buffer)
  write(iunit,'(A)') ''
  call write_time(iunit)

  deallocate (                                                       &
      W, residual_norm,                                              &
      U, overlap,                                                    &
      R, c,                                                          &
      h,                                                             &
      y,                                                             &
      lambda                                                         &
      )
end


subroutine u_0_H_u_0_mrcc_nstates(e_0,u_0,n,keys_tmp,Nint,istate,N_st,sze_8)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes e_0 = <u_0|H|u_0>/<u_0|u_0>
  !
  ! n : number of determinants
  !
  END_DOC
  integer, intent(in)            :: n,Nint,N_st,sze_8
  double precision, intent(out)  :: e_0(N_st)
  double precision, intent(in)   :: u_0(sze_8,N_st)
  integer(bit_kind),intent(in)   :: keys_tmp(Nint,2,n)
  integer,intent(in)             :: istate
  
  double precision, allocatable  :: v_0(:,:), H_jj(:)
  double precision               :: u_dot_u,u_dot_v,diag_H_mat_elem
  integer :: i,j
  allocate(H_jj(n), v_0(sze_8,N_st))
  do i = 1, n
   H_jj(i) = diag_H_mat_elem(keys_tmp(1,1,i),Nint)
  enddo

  do i=1,N_det_ref
    H_jj(idx_ref(i)) +=  delta_ii(istate,i)
  enddo
  
  call H_u_0_mrcc_nstates(v_0,u_0,H_jj,n,keys_tmp,Nint,istate,N_st,sze_8)
  do i=1,N_st
    e_0(i) = u_dot_v(v_0(1,i),u_0(1,i),n)/u_dot_u(u_0(1,i),n)
  enddo
  deallocate(H_jj, v_0)
end


subroutine H_u_0_mrcc_nstates(v_0,u_0,H_jj,n,keys_tmp,Nint,istate_in,N_st,sze_8)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes v_0 = H|u_0>
  !   
  ! n : number of determinants
  !     
  ! H_jj : array of <j|H|j>
  END_DOC
  integer, intent(in)            :: n,Nint,istate_in,N_st,sze_8
  double precision, intent(out)  :: v_0(sze_8,N_st)
  double precision, intent(in)   :: u_0(sze_8,N_st)
  double precision, intent(in)   :: H_jj(n)
  integer(bit_kind),intent(in)   :: keys_tmp(Nint,2,n)
  double precision               :: hij
  double precision, allocatable  :: vt(:,:)
  integer                        :: i,j,k,l, jj,ii
  integer                        :: i0, j0
  integer(bit_kind)              :: sorted_i(Nint)

  
  integer,allocatable            :: shortcut(:,:), sort_idx(:,:)
  integer(bit_kind), allocatable :: sorted(:,:,:), version(:,:,:)
  
  
  integer                        :: sh, sh2, ni, exa, ext, org_i, org_j, endi, pass, istate


  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (n>0)
  PROVIDE ref_bitmask_energy 
  allocate (shortcut(0:n+1,2), sort_idx(n,2), sorted(Nint,n,2), version(Nint,n,2))
  v_0 = 0.d0

  call sort_dets_ab_v(keys_tmp, sorted(1,1,1), sort_idx(1,1), shortcut(0,1), version(1,1,1), n, Nint)
  call sort_dets_ba_v(keys_tmp, sorted(1,1,2), sort_idx(1,2), shortcut(0,2), version(1,1,2), n, Nint)
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP PRIVATE(i,hij,j,k,jj,vt,ii,sh,sh2,ni,exa,ext,org_i,org_j,endi,sorted_i,istate)&
      !$OMP SHARED(n,H_jj,u_0,keys_tmp,Nint,v_0,sorted,shortcut,sort_idx,version,N_st,sze_8,&
      !$OMP   istate_in,delta_ij,N_det_ref,N_det_non_ref,idx_ref,idx_non_ref)
  allocate(vt(sze_8,N_st))
  Vt = 0.d0
  
  !$OMP DO SCHEDULE(dynamic)
  do sh=1,shortcut(0,1)
    do sh2=sh,shortcut(0,1)
      exa = 0
      do ni=1,Nint
        exa = exa + popcnt(xor(version(ni,sh,1), version(ni,sh2,1)))
      end do
      if(exa > 2) then
        cycle
      end if
      
      do i=shortcut(sh,1),shortcut(sh+1,1)-1
        org_i = sort_idx(i,1)
        if(sh==sh2) then
          endi = i-1
        else
          endi = shortcut(sh2+1,1)-1
        end if
        do ni=1,Nint
          sorted_i(ni) = sorted(ni,i,1)
        enddo
        
        do j=shortcut(sh2,1),endi
          org_j = sort_idx(j,1)
          ext = exa
          do ni=1,Nint
            ext = ext + popcnt(xor(sorted_i(ni), sorted(ni,j,1)))
          end do
          if(ext <= 4) then
            call i_H_j(keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),Nint,hij)
            do istate=1,N_st
              vt (org_i,istate) = vt (org_i,istate) + hij*u_0(org_j,istate)
              vt (org_j,istate) = vt (org_j,istate) + hij*u_0(org_i,istate)
            enddo
          endif
        enddo
      enddo
    enddo
  enddo
  !$OMP END DO NOWAIT
  
  !$OMP DO SCHEDULE(dynamic)
  do sh=1,shortcut(0,2)
    do i=shortcut(sh,2),shortcut(sh+1,2)-1
      org_i = sort_idx(i,2)
      do j=shortcut(sh,2),i-1
        org_j = sort_idx(j,2)
        ext = 0
        do ni=1,Nint
          ext = ext + popcnt(xor(sorted(ni,i,2), sorted(ni,j,2)))
        end do
        if(ext == 4) then
          call i_H_j(keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),Nint,hij)
          do istate=1,N_st
            vt (org_i,istate) = vt (org_i,istate) + hij*u_0(org_j,istate)
            vt (org_j,istate) = vt (org_j,istate) + hij*u_0(org_i,istate)
          enddo
        end if
      end do
    end do
  enddo
  !$OMP END DO NOWAIT
  
  !$OMP DO 
  do ii=1,n_det_ref
    i = idx_ref(ii)
    do jj = 1, n_det_non_ref
        j = idx_non_ref(jj)
        do istate=1,N_st
          vt (i,istate) = vt (i,istate) + delta_ij(istate_in,jj,ii)*u_0(j,istate)
          vt (j,istate) = vt (j,istate) + delta_ij(istate_in,jj,ii)*u_0(i,istate)
        enddo
    enddo
  enddo
  !$OMP END DO

  !$OMP CRITICAL
  do istate=1,N_st
    do i=n,1,-1
      v_0(i,istate) = v_0(i,istate) + vt(i,istate)
    enddo
  enddo
  !$OMP END CRITICAL

  deallocate(vt)
  !$OMP END PARALLEL
  
  do istate=1,N_st
    do i=1,n
      v_0(i,istate) += H_jj(i) * u_0(i,istate)
    enddo
  enddo
  deallocate (shortcut, sort_idx, sorted, version)
  
end


subroutine davidson_diag_mrcc_hs2(dets_in,u_in,dim_in,energies,sze,N_st,N_st_diag,Nint,iunit,istate)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Davidson diagonalization.
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
  ! iunit : Unit number for the I/O
  !
  ! Initial guess vectors are not necessarily orthonormal
  END_DOC
  integer, intent(in)            :: dim_in, sze, N_st, N_st_diag, Nint, iunit, istate
  integer(bit_kind), intent(in)  :: dets_in(Nint,2,sze)
  double precision, intent(inout) :: u_in(dim_in,N_st_diag)
  double precision, intent(out)  :: energies(N_st_diag)
  double precision, allocatable  :: H_jj(:), S2_jj(:)
  
  double precision               :: diag_h_mat_elem
  integer                        :: i
  ASSERT (N_st > 0)
  ASSERT (sze > 0)
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  PROVIDE mo_bielec_integrals_in_map
  allocate(H_jj(sze), S2_jj(sze))
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP  SHARED(sze,H_jj,S2_jj, dets_in,Nint,N_det_ref,delta_ii, &
      !$OMP  idx_ref, istate)  &
      !$OMP  PRIVATE(i)
  !$OMP DO SCHEDULE(guided)
  do i=1,sze
    H_jj(i) = diag_h_mat_elem(dets_in(1,1,i),Nint)
    call get_s2(dets_in(1,1,i),dets_in(1,1,i),Nint,S2_jj(i))
  enddo
  !$OMP END DO 
  !$OMP DO SCHEDULE(guided)
  do i=1,N_det_ref
    H_jj(idx_ref(i)) +=  delta_ii(istate,i)
  enddo
  !$OMP END DO 
  !$OMP END PARALLEL

  call davidson_diag_hjj_sjj_mrcc(dets_in,u_in,H_jj,S2_jj,energies,dim_in,sze,N_st,N_st_diag,Nint,iunit,istate)
  deallocate (H_jj,S2_jj)
end


subroutine davidson_diag_hjj_sjj_mrcc(dets_in,u_in,H_jj,S2_jj,energies,dim_in,sze,N_st,N_st_diag,Nint,iunit,istate )
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Davidson diagonalization with specific diagonal elements of the H matrix
  !
  ! H_jj : specific diagonal H matrix elements to diagonalize de Davidson
  !
  ! S2_jj : specific diagonal S^2 matrix elements 
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
  ! N_st_diag : Number of states in which H is diagonalized. Assumed > sze
  !
  ! iunit : Unit for the I/O
  !
  ! Initial guess vectors are not necessarily orthonormal
  END_DOC
  integer, intent(in)            :: dim_in, sze, N_st, N_st_diag, Nint, istate
  integer(bit_kind), intent(in)  :: dets_in(Nint,2,sze)
  double precision,  intent(in)  :: H_jj(sze), S2_jj(sze)
  integer,  intent(in)  :: iunit
  double precision, intent(inout) :: u_in(dim_in,N_st_diag)
  double precision, intent(out)  :: energies(N_st_diag)
  
  integer                        :: sze_8
  integer                        :: iter
  integer                        :: i,j,k,l,m
  logical                        :: converged
  
  double precision               :: u_dot_v, u_dot_u
  
  integer                        :: k_pairs, kl
  
  integer                        :: iter2
  double precision, allocatable  :: W(:,:),  U(:,:), S(:,:), overlap(:,:)
  double precision, allocatable  :: y(:,:), h(:,:), lambda(:), s2(:)
  double precision, allocatable  :: c(:), s_(:,:), s_tmp(:,:)
  double precision               :: diag_h_mat_elem
  double precision, allocatable  :: residual_norm(:)
  character*(16384)              :: write_buffer
  double precision               :: to_print(3,N_st)
  double precision               :: cpu, wall
  integer                        :: shift, shift2, itermax
  include 'constants.include.F'
  
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: U, W, S, y, h, lambda
  if (N_st_diag*3 > sze) then
     print *,  'error in Davidson :'
     print *,  'Increase n_det_max_jacobi to ', N_st_diag*3
     stop -1
  endif

  PROVIDE nuclear_repulsion

  call write_time(iunit)
  call wall_time(wall)
  call cpu_time(cpu)
  write(iunit,'(A)') ''
  write(iunit,'(A)') 'Davidson Diagonalization'
  write(iunit,'(A)') '------------------------'
  write(iunit,'(A)') ''
  call write_int(iunit,N_st,'Number of states')
  call write_int(iunit,N_st_diag,'Number of states in diagonalization')
  call write_int(iunit,sze,'Number of determinants')
  call write_int(iunit,istate,'Using dressing for state ')

  write(iunit,'(A)') ''
  write_buffer = '===== '
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ =========== ==========='
  enddo
  write(iunit,'(A)') trim(write_buffer)
  write_buffer = ' Iter'
  do i=1,N_st
    write_buffer = trim(write_buffer)//'      Energy          S^2       Residual '
  enddo
  write(iunit,'(A)') trim(write_buffer)
  write_buffer = '===== '
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ =========== ==========='
  enddo
  write(iunit,'(A)') trim(write_buffer)

  integer, external :: align_double
  sze_8 = align_double(sze)

  itermax = min(davidson_sze_max, sze/N_st_diag)
  allocate(                                                          &
      W(sze_8,N_st_diag*itermax),                                    &
      U(sze_8,N_st_diag*itermax),                                    &
      S(sze_8,N_st_diag*itermax),                                    &
      h(N_st_diag*itermax,N_st_diag*itermax),                        &
      y(N_st_diag*itermax,N_st_diag*itermax),                        &
      s_(N_st_diag*itermax,N_st_diag*itermax),                       &
      s_tmp(N_st_diag*itermax,N_st_diag*itermax),                    &
      residual_norm(N_st_diag),                                      &
      c(N_st_diag*itermax),                                          &
      s2(N_st_diag*itermax),                                         &
      overlap(N_st_diag*itermax,N_st_diag*itermax),                  &
      lambda(N_st_diag*itermax))

  h  = 0.d0
  s_ = 0.d0
  s_tmp = 0.d0
  U = 0.d0
  W = 0.d0
  S = 0.d0
  y = 0.d0


  ASSERT (N_st > 0)
  ASSERT (N_st_diag >= N_st)
  ASSERT (sze > 0)
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  
  ! Davidson iterations
  ! ===================
  
  converged = .False.
  
  double precision               :: r1, r2
  do k=N_st+1,N_st_diag
      u_in(k,k) = 10.d0
      do i=1,sze
        call random_number(r1)
        call random_number(r2)
        r1 = dsqrt(-2.d0*dlog(r1))
        r2 = dtwo_pi*r2
        u_in(i,k) = r1*dcos(r2)
      enddo
  enddo
  do k=1,N_st_diag
      call normalize(u_in(1,k),sze)
  enddo

  
  do while (.not.converged)
    
    do k=1,N_st_diag
      do i=1,sze
        U(i,k) = u_in(i,k)
      enddo
    enddo

    do iter=1,davidson_sze_max-1
      
      shift  = N_st_diag*(iter-1)
      shift2 = N_st_diag*iter

      call ortho_qr(U,size(U,1),sze,shift2)

      ! Compute |W_k> = \sum_i |i><i|H|u_k>
      ! -----------------------------------------
       
      call H_S2_u_0_mrcc_nstates(W(1,shift+1),S(1,shift+1),U(1,shift+1),H_jj,S2_jj,sze,dets_in,Nint,&
        istate,N_st_diag,sze_8)
      
      
      ! Compute h_kl = <u_k | W_l> = <u_k| H |u_l>
      ! -------------------------------------------


      call dgemm('T','N', shift2, shift2, sze,            &
          1.d0, U, size(U,1), W, size(W,1),                &
          0.d0, h, size(h,1))

      call dgemm('T','N', shift2, shift2, sze,            &
          1.d0, U, size(U,1), S, size(S,1),                &
          0.d0, s_, size(s_,1))

!      ! Diagonalize S^2
!      ! ---------------
!
!      call lapack_diag(s2,y,s_,size(s_,1),shift2)
!
!      ! Rotate H in the basis of eigenfunctions of s2
!      ! ---------------------------------------------
!
!      call dgemm('N','N',shift2,shift2,shift2,                       &
!          1.d0, h, size(h,1), y, size(y,1),                          &
!          0.d0, s_tmp, size(s_tmp,1))
!      
!      call dgemm('T','N',shift2,shift2,shift2,                       &
!          1.d0, y, size(y,1), s_tmp, size(s_tmp,1),                  &
!          0.d0, h, size(h,1))
!
!      ! Damp interaction between different spin states
!      ! ------------------------------------------------
!
!      do k=1,shift2
!        do l=1,shift2
!          if (dabs(s2(k) - s2(l)) > 1.d0) then
!            h(k,l) = h(k,l)*(max(0.d0,1.d0 - dabs(s2(k) - s2(l))))
!          endif
!        enddo
!      enddo
!
!      ! Rotate back H 
!      ! -------------
!
!      call dgemm('N','T',shift2,shift2,shift2,                       &
!          1.d0, h, size(h,1), y, size(y,1),                          &
!          0.d0, s_tmp, size(s_tmp,1))
!      
!      call dgemm('N','N',shift2,shift2,shift2,                       &
!          1.d0, y, size(y,1), s_tmp, size(s_tmp,1),                  &
!          0.d0, h, size(h,1))

      
      ! Diagonalize h
      ! -------------
      call lapack_diag(lambda,y,h,size(h,1),shift2)

      ! Compute S2 for each eigenvector
      ! -------------------------------

      call dgemm('N','N',shift2,shift2,shift2,                            &
          1.d0, s_, size(s_,1), y, size(y,1), &
          0.d0, s_tmp, size(s_tmp,1))

      call dgemm('T','N',shift2,shift2,shift2,                            &
          1.d0, y, size(y,1), s_tmp, size(s_tmp,1), &
          0.d0, s_, size(s_,1))

      do k=1,shift2
        s2(k) = s_(k,k) + S_z2_Sz
      enddo

      if (s2_eig) then
          logical :: state_ok(N_st_diag*davidson_sze_max)
          do k=1,shift2
            state_ok(k) = (dabs(s2(k)-expected_s2) < 0.6d0)
          enddo
      else
        do k=1,size(state_ok)
          state_ok(k) = .True.
        enddo
      endif

      do k=1,shift2
        if (.not. state_ok(k)) then
          do l=k+1,shift2
            if (state_ok(l)) then
              call dswap(shift2, y(1,k), 1, y(1,l), 1)
              call dswap(1, s2(k), 1, s2(l), 1)
              call dswap(1, lambda(k), 1, lambda(l), 1)
              state_ok(k) = .True.
              state_ok(l) = .False.
              exit
            endif
          enddo
        endif
      enddo

      if (state_following) then

        ! Compute overlap with U_in
        ! -------------------------
        
        integer                        :: order(N_st_diag)
        double precision               :: cmax
        overlap = -1.d0
        do k=1,shift2
          do i=1,shift2
            overlap(k,i) = dabs(y(k,i))
          enddo
        enddo
        do k=1,N_st
          cmax = -1.d0
          do i=1,N_st
            if (overlap(i,k) > cmax) then
              cmax = overlap(i,k) 
              order(k) = i
            endif
          enddo
          do i=1,shift2
            overlap(order(k),i) = -1.d0
          enddo
        enddo
        overlap = y
        do k=1,N_st
          l = order(k)
          if (k /= l) then
            y(1:shift2,k) = overlap(1:shift2,l)
          endif
        enddo
        do k=1,N_st
          overlap(k,1) = lambda(k)
          overlap(k,2) = s2(k)
        enddo
        do k=1,N_st
          l = order(k)
          if (k /= l) then
            lambda(k) = overlap(l,1)
            s2(k) = overlap(l,2)
          endif
        enddo
        
      endif


      ! Express eigenvectors of h in the determinant basis
      ! --------------------------------------------------
      
      call dgemm('N','N', sze, N_st_diag, shift2,                    &
          1.d0, U, size(U,1), y, size(y,1), 0.d0, U(1,shift2+1), size(U,1))
      call dgemm('N','N', sze, N_st_diag, shift2,                    &
          1.d0, W, size(W,1), y, size(y,1), 0.d0, W(1,shift2+1), size(W,1))
      call dgemm('N','N', sze, N_st_diag, shift2,                    &
          1.d0, S, size(S,1), y, size(y,1), 0.d0, S(1,shift2+1), size(S,1))

      ! Compute residual vector
      ! -----------------------

      do k=1,N_st_diag
!        if (state_ok(k)) then
          do i=1,sze
            U(i,shift2+k) = (lambda(k) * U(i,shift2+k) - W(i,shift2+k) )      &
                * (1.d0 + s2(k) * U(i,shift2+k) - S(i,shift2+k) - S_z2_Sz &
              )/max(H_jj(i) - lambda (k),1.d-2)
          enddo
!        else
!          ! Randomize components with bad <S2>
!            do i=1,sze-2,2
!              call random_number(r1)
!              call random_number(r2)
!              r1 = dsqrt(-2.d0*dlog(r1))
!              r2 = dtwo_pi*r2
!              U(i,shift2+k) = r1*dcos(r2)
!              U(i+1,shift2+k) = r1*dsin(r2)
!            enddo
!            do i=sze-2+1,sze
!              call random_number(r1)
!              call random_number(r2)
!              r1 = dsqrt(-2.d0*dlog(r1))
!              r2 = dtwo_pi*r2
!              U(i,shift2+k) = r1*dcos(r2)
!            enddo
!        endif

        if (k <= N_st) then
          residual_norm(k) = u_dot_u(U(1,shift2+k),sze)
          to_print(1,k) = lambda(k) + nuclear_repulsion
          to_print(2,k) = s2(k)
          to_print(3,k) = residual_norm(k)
        endif
      enddo
      
      write(iunit,'(X,I3,X,100(X,F16.10,X,F11.6,X,E11.3))')  iter, to_print(1:3,1:N_st)
      call davidson_converged(lambda,residual_norm,wall,iter,cpu,N_st,converged)
      do k=1,N_st
        if (residual_norm(k) > 1.e8) then
        print *,  ''
          stop 'Davidson failed'
        endif
      enddo
      if (converged) then
        exit
      endif

    enddo

    ! Re-contract to u_in
    ! -----------
    
    call dgemm('N','N', sze, N_st_diag, shift2,                    &
        1.d0, U, size(U,1), y, size(y,1), 0.d0, u_in, size(u_in,1))

  enddo

  do k=1,N_st_diag
    energies(k) = lambda(k)
  enddo

  write_buffer = '===== '
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ =========== ==========='
  enddo
  write(iunit,'(A)') trim(write_buffer)
  write(iunit,'(A)') ''
  call write_time(iunit)

  deallocate (                                                       &
      W, residual_norm,                                              &
      U, overlap,                                                    &
      c, S,                                                       &
      h,                                                             &
      y, s_, s_tmp,                                                  &
      lambda                                                         &
      )
end


subroutine H_S2_u_0_mrcc_nstates(v_0,s_0,u_0,H_jj,S2_jj,n,keys_tmp,Nint,istate_in,N_st,sze_8)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes v_0 = H|u_0> and s_0 = S^2 |u_0>
  !
  ! n : number of determinants
  !
  ! H_jj : array of <j|H|j>
  !
  ! S2_jj : array of <j|S^2|j>
  END_DOC
  integer, intent(in)            :: N_st,n,Nint, sze_8, istate_in
  double precision, intent(out)  :: v_0(sze_8,N_st), s_0(sze_8,N_st)
  double precision, intent(in)   :: u_0(sze_8,N_st)
  double precision, intent(in)   :: H_jj(n), S2_jj(n)
  integer(bit_kind),intent(in)   :: keys_tmp(Nint,2,n)
  double precision               :: hij,s2
  double precision, allocatable  :: vt(:,:), ut(:,:), st(:,:)
  integer                        :: i,j,k,l, jj,ii
  integer                        :: i0, j0

  integer, allocatable           :: shortcut(:,:), sort_idx(:,:)
  integer(bit_kind), allocatable :: sorted(:,:,:), version(:,:,:)
  integer(bit_kind)              :: sorted_i(Nint)

  integer                        :: sh, sh2, ni, exa, ext, org_i, org_j, endi, istate
  integer                        :: N_st_8

  integer, external              :: align_double
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: vt, ut

  N_st_8 = align_double(N_st)

  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (n>0)
  PROVIDE ref_bitmask_energy

  allocate (shortcut(0:n+1,2), sort_idx(n,2), sorted(Nint,n,2), version(Nint,n,2))
  allocate(ut(N_st_8,n))

  v_0 = 0.d0
  s_0 = 0.d0

  do i=1,n
    do istate=1,N_st
      ut(istate,i) = u_0(i,istate)
    enddo
  enddo

  call sort_dets_ab_v(keys_tmp, sorted(1,1,1), sort_idx(1,1), shortcut(0,1), version(1,1,1), n, Nint)
  call sort_dets_ba_v(keys_tmp, sorted(1,1,2), sort_idx(1,2), shortcut(0,2), version(1,1,2), n, Nint)

  PROVIDE delta_ij_s2 
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP PRIVATE(i,hij,s2,j,k,jj,vt,st,ii,sh,sh2,ni,exa,ext,org_i,org_j,endi,sorted_i,istate)&
      !$OMP SHARED(n,keys_tmp,ut,Nint,v_0,s_0,sorted,shortcut,sort_idx,version,N_st,N_st_8, &
      !$OMP  N_det_ref, idx_ref, N_det_non_ref, idx_non_ref, delta_ij, delta_ij_s2,istate_in)
  allocate(vt(N_st_8,n),st(N_st_8,n))
  Vt = 0.d0
  St = 0.d0

  !$OMP DO SCHEDULE(guided)
  do sh=1,shortcut(0,1)
    do sh2=sh,shortcut(0,1)
      exa = 0
      do ni=1,Nint
        exa = exa + popcnt(xor(version(ni,sh,1), version(ni,sh2,1)))
      end do
      if(exa > 2) then
        cycle
      end if

      do i=shortcut(sh,1),shortcut(sh+1,1)-1
        org_i = sort_idx(i,1)
        if(sh==sh2) then
          endi = i-1
        else
          endi = shortcut(sh2+1,1)-1
        end if
        do ni=1,Nint
          sorted_i(ni) = sorted(ni,i,1)
        enddo

        do j=shortcut(sh2,1),endi
          org_j = sort_idx(j,1)
          ext = exa
          do ni=1,Nint
            ext = ext + popcnt(xor(sorted_i(ni), sorted(ni,j,1)))
          end do
          if(ext <= 4) then
            call i_h_j (keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,hij)
            call get_s2(keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,s2)
            do istate=1,n_st
              vt (istate,org_i) = vt (istate,org_i) + hij*ut(istate,org_j)
              vt (istate,org_j) = vt (istate,org_j) + hij*ut(istate,org_i)
              st (istate,org_i) = st (istate,org_i) + s2*ut(istate,org_j)
              st (istate,org_j) = st (istate,org_j) + s2*ut(istate,org_i)
            enddo
          endif
        enddo
      enddo
    enddo
  enddo
  !$OMP END DO
  !$OMP DO SCHEDULE(guided)
  do sh=1,shortcut(0,2)
    do i=shortcut(sh,2),shortcut(sh+1,2)-1
      org_i = sort_idx(i,2)
      do j=shortcut(sh,2),i-1
        org_j = sort_idx(j,2)
        ext = 0
        do ni=1,Nint
          ext = ext + popcnt(xor(sorted(ni,i,2), sorted(ni,j,2)))
        end do
        if(ext == 4) then
            call i_h_j (keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,hij)
            call get_s2(keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),nint,s2)
            do istate=1,n_st
              vt (istate,org_i) = vt (istate,org_i) + hij*ut(istate,org_j)
              vt (istate,org_j) = vt (istate,org_j) + hij*ut(istate,org_i)
              st (istate,org_i) = st (istate,org_i) + s2*ut(istate,org_j)
              st (istate,org_j) = st (istate,org_j) + s2*ut(istate,org_i)
            enddo
        end if
      end do
    end do
  enddo
  !$OMP END DO

! --------------------------
! Begin Specific to dressing
! --------------------------

  !$OMP DO 
  do ii=1,n_det_ref
    i = idx_ref(ii)
    do jj = 1, n_det_non_ref
        j = idx_non_ref(jj)
        do istate=1,N_st
          vt (istate,i) = vt (istate,i) + delta_ij(istate_in,jj,ii)*ut(istate,j)
          vt (istate,j) = vt (istate,j) + delta_ij(istate_in,jj,ii)*ut(istate,i)
          st (istate,i) = st (istate,i) + delta_ij_s2(istate_in,jj,ii)*ut(istate,j)
          st (istate,j) = st (istate,j) + delta_ij_s2(istate_in,jj,ii)*ut(istate,i)
        enddo
    enddo
  enddo
  !$OMP END DO

! ------------------------
! End Specific to dressing
! ------------------------

  !$OMP CRITICAL
  do istate=1,N_st
    do i=n,1,-1
      v_0(i,istate) = v_0(i,istate) + vt(istate,i)
      s_0(i,istate) = s_0(i,istate) + st(istate,i)
    enddo
  enddo
  !$OMP END CRITICAL

  deallocate(vt,st)
  !$OMP END PARALLEL

  do istate=1,N_st
    do i=1,n
      v_0(i,istate) = v_0(i,istate) + H_jj(i) * u_0(i,istate)
      s_0(i,istate) = s_0(i,istate) + s2_jj(i)* u_0(i,istate)
    enddo
  enddo
  deallocate (shortcut, sort_idx, sorted, version, ut)
end


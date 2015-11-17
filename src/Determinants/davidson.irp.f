BEGIN_PROVIDER [ integer, davidson_iter_max ]
  implicit none
  BEGIN_DOC
  ! Max number of Davidson iterations
  END_DOC
  davidson_iter_max = 100
END_PROVIDER

BEGIN_PROVIDER [ integer, davidson_sze_max ]
  implicit none
  BEGIN_DOC
  ! Max number of Davidson sizes
  END_DOC
  ASSERT (davidson_sze_max <= davidson_iter_max)
  davidson_sze_max = max(8,2*N_states_diag)
END_PROVIDER

subroutine davidson_diag(dets_in,u_in,energies,dim_in,sze,N_st,Nint,iunit)
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
  integer, intent(in)            :: dim_in, sze, N_st, Nint, iunit
  integer(bit_kind), intent(in)  :: dets_in(Nint,2,sze)
  double precision, intent(inout) :: u_in(dim_in,N_st)
  double precision, intent(out)  :: energies(N_st)
  double precision, allocatable  :: H_jj(:)
  
  double precision               :: diag_h_mat_elem
  integer                        :: i
  ASSERT (N_st > 0)
  ASSERT (sze > 0)
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  PROVIDE mo_bielec_integrals_in_map
  allocate(H_jj(sze))
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP  SHARED(sze,H_jj,dets_in,Nint)                           &
      !$OMP  PRIVATE(i)
  !$OMP DO SCHEDULE(guided)
  do i=1,sze
    H_jj(i) = diag_h_mat_elem(dets_in(1,1,i),Nint)
  enddo
  !$OMP END DO 
  !$OMP END PARALLEL

  call davidson_diag_hjj(dets_in,u_in,H_jj,energies,dim_in,sze,N_st,Nint,iunit)
  deallocate (H_jj)
end


logical function det_inf(key1, key2, Nint)
  use bitmasks
  implicit none
  integer(bit_kind),intent(in)       :: key1(Nint, 2), key2(Nint, 2)
  integer,intent(in)                 :: Nint
  integer                            :: i,j
  
  det_inf = .false.
  
  do i=1,2
    do j=Nint,1,-1
      if(key1(j,i) < key2(j,i)) then
        det_inf = .true.
        return
      else if(key1(j,i) > key2(j,i)) then
        return
      end if
    end do
  end do
end function


subroutine tamiser(key, idx, no, n, Nint, N_key)
  use bitmasks
  implicit none
  
  BEGIN_DOC
! Uncodumented : TODO
  END_DOC
  integer,intent(in)                    :: no, n, Nint, N_key
  integer(bit_kind),intent(inout)       :: key(Nint, 2, N_key)
  integer,intent(inout)                 :: idx(N_key)
  integer                               :: k,j,tmpidx
  integer(bit_kind)                     :: tmp(Nint, 2)
  logical                               :: det_inf
  integer                               :: ni
  
  k = no
  j = 2*k
  do while(j <= n)
    if(j < n) then
      if (det_inf(key(1,1,j), key(1,1,j+1), Nint)) then
        j = j+1
      endif
    endif
    if(det_inf(key(1,1,k), key(1,1,j), Nint)) then
      do ni=1,Nint
        tmp(ni,1)   = key(ni,1,k)
        tmp(ni,2)   = key(ni,2,k)
        key(ni,1,k) = key(ni,1,j)
        key(ni,2,k) = key(ni,2,j)
        key(ni,1,j) = tmp(ni,1)
        key(ni,2,j) = tmp(ni,2)
      enddo
      tmpidx = idx(k)
      idx(k) = idx(j)
      idx(j) = tmpidx
      k = j
      j = k+k
    else
      return
    endif
  enddo
end subroutine


subroutine sort_dets_ba_v(key_in, key_out, idx, shortcut, version, N_key, Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
! Uncodumented : TODO
  END_DOC
  integer, intent(in)            :: Nint, N_key
  integer(bit_kind),intent(in)   :: key_in(Nint,2,N_key)
  integer(bit_kind),intent(out)  :: key_out(Nint,N_key)
  integer,intent(out)            :: idx(N_key)
  integer,intent(out)            :: shortcut(0:N_key+1)
  integer(bit_kind),intent(out)  :: version(Nint,N_key+1)
  integer(bit_kind), allocatable :: key(:,:,:)
  integer                        :: i,ni
  
  allocate ( key(Nint,2,N_key) )
  do i=1,N_key
    do ni=1,Nint
      key(ni,1,i) = key_in(ni,2,i)
      key(ni,2,i) = key_in(ni,1,i)
    enddo
  enddo
  
  call sort_dets_ab_v(key, key_out, idx, shortcut, version, N_key, Nint)
  deallocate ( key )
end subroutine



subroutine sort_dets_ab_v(key_in, key_out, idx, shortcut, version, N_key, Nint)
  use bitmasks
  implicit none
  
  BEGIN_DOC
! Uncodumented : TODO
  END_DOC
  integer, intent(in)                   :: Nint, N_key
  integer(bit_kind),intent(in)          :: key_in(Nint,2,N_key)
  integer(bit_kind),intent(out)         :: key_out(Nint,N_key)
  integer,intent(out)                   :: idx(N_key)
  integer,intent(out)                   :: shortcut(0:N_key+1)
  integer(bit_kind),intent(out)         :: version(Nint,N_key+1)
  integer(bit_kind), allocatable        :: key(:,:,:)
  integer(bit_kind)                     :: tmp(Nint, 2)
  integer                               :: tmpidx,i,ni
  
  allocate (key(Nint,2,N_key))
  do i=1,N_key
    do ni=1,Nint
      key(ni,1,i) = key_in(ni,1,i)
      key(ni,2,i) = key_in(ni,2,i)
    enddo
    idx(i) = i
  end do
  
  do i=N_key/2,1,-1
    call tamiser(key, idx, i, N_key, Nint, N_key)
  end do
  
  do i=N_key,2,-1
    do ni=1,Nint
      tmp(ni,1) = key(ni,1,i)
      tmp(ni,2) = key(ni,2,i)
      key(ni,1,i) = key(ni,1,1)
      key(ni,2,i) = key(ni,2,1)
      key(ni,1,1) = tmp(ni,1)
      key(ni,2,1) = tmp(ni,2)
    enddo
    tmpidx = idx(i)
    idx(i) = idx(1)
    idx(1) = tmpidx
    call tamiser(key, idx, 1, i-1, Nint, N_key)
  end do
  
  shortcut(0) = 1
  shortcut(1) = 1
  do ni=1,Nint
    version(ni,1) = key(ni,1,1)
  enddo
  do i=2,N_key
    do ni=1,nint
      if(key(ni,1,i) /= key(ni,1,i-1)) then
        shortcut(0) = shortcut(0) + 1
        shortcut(shortcut(0)) = i
        version(:,shortcut(0)) = key(:,1,i)
        exit
      end if
    end do
  end do
  shortcut(shortcut(0)+1) = N_key+1
  do i=1,N_key
    do ni=1,Nint
      key_out(ni,i) = key(ni,2,i)
    enddo
  enddo
  deallocate (key)
end subroutine


subroutine sort_dets_ab(key, idx, shortcut, N_key, Nint)
  use bitmasks
  implicit none
  
  
  BEGIN_DOC
! Uncodumented : TODO
  END_DOC
  integer(bit_kind),intent(inout)       :: key(Nint,2,N_key)
  integer,intent(out)                   :: idx(N_key)
  integer,intent(out)                   :: shortcut(0:N_key+1)
  integer, intent(in)                   :: Nint, N_key
  integer(bit_kind)                     :: tmp(Nint, 2)
  integer                               :: tmpidx,i,ni
  
  do i=1,N_key
    idx(i) = i
  end do
  
  do i=N_key/2,1,-1
    call tamiser(key, idx, i, N_key, Nint, N_key)
  end do
  
  do i=N_key,2,-1
    do ni=1,Nint
      tmp(ni,1) = key(ni,1,i)
      tmp(ni,2) = key(ni,2,i)
      key(ni,1,i) = key(ni,1,1)
      key(ni,2,i) = key(ni,2,1)
      key(ni,1,1) = tmp(ni,1)
      key(ni,2,1) = tmp(ni,2)
    enddo

    tmpidx = idx(i)
    idx(i) = idx(1)
    idx(1) = tmpidx
    call tamiser(key, idx, 1, i-1, Nint, N_key)
  end do
  
  shortcut(0) = 1
  shortcut(1) = 1
  do i=2,N_key
    do ni=1,nint
      if(key(ni,1,i) /= key(ni,1,i-1)) then
        shortcut(0) = shortcut(0) + 1
        shortcut(shortcut(0)) = i
        exit
      end if
    end do
  end do
  shortcut(shortcut(0)+1) = N_key+1
end subroutine


subroutine davidson_diag_hjj(dets_in,u_in,H_jj,energies,dim_in,sze,N_st,Nint,iunit)
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
  ! iunit : Unit for the I/O
  !
  ! Initial guess vectors are not necessarily orthonormal
  END_DOC
  integer, intent(in)            :: dim_in, sze, N_st, Nint
  integer(bit_kind), intent(in)  :: dets_in(Nint,2,sze)
  double precision,  intent(in)  :: H_jj(sze)
  integer,  intent(in)  :: iunit
  double precision, intent(inout) :: u_in(dim_in,N_st)
  double precision, intent(out)  :: energies(N_st)
  
  integer                        :: iter
  integer                        :: i,j,k,l,m
  logical                        :: converged
  
  double precision               :: overlap(N_st,N_st)
  double precision               :: u_dot_v, u_dot_u
  
  integer, allocatable           :: kl_pairs(:,:)
  integer                        :: k_pairs, kl
  
  integer                        :: iter2
  double precision, allocatable  :: W(:,:,:),  U(:,:,:), R(:,:), Wt(:)
  double precision, allocatable  :: y(:,:,:,:), h(:,:,:,:), lambda(:)
  double precision               :: diag_h_mat_elem
  double precision               :: residual_norm(N_st)
  character*(16384)              :: write_buffer
  double precision               :: to_print(2,N_st)
  double precision               :: cpu, wall
  


  call write_time(iunit)
  call wall_time(wall)
  call cpu_time(cpu)
  write(iunit,'(A)') ''
  write(iunit,'(A)') 'Davidson Diagonalization'
  write(iunit,'(A)') '------------------------'
  write(iunit,'(A)') ''
  call write_int(iunit,N_st,'Number of states')
  call write_int(iunit,sze,'Number of determinants')
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

  allocate(                                                          &
      kl_pairs(2,N_st*(N_st+1)/2),                                   &
      W(sze,N_st,davidson_sze_max),                                                   &
      Wt(sze),                                                        &
      U(sze,N_st,davidson_sze_max),                                  &
      R(sze,N_st),                                                   &
      h(N_st,davidson_sze_max,N_st,davidson_sze_max),                &
      y(N_st,davidson_sze_max,N_st,davidson_sze_max),                &
      lambda(N_st*davidson_sze_max))
  
  ASSERT (N_st > 0)
  ASSERT (sze > 0)
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  
  ! Initialization
  ! ==============
  

  k_pairs=0
  do l=1,N_st
    do k=1,l
      k_pairs+=1
      kl_pairs(1,k_pairs) = k
      kl_pairs(2,k_pairs) = l
    enddo
  enddo
  
  !$OMP PARALLEL DEFAULT(NONE)                                      &
      !$OMP  SHARED(U,sze,N_st,overlap,kl_pairs,k_pairs,             &
      !$OMP  Nint,dets_in,u_in)                                 &
      !$OMP  PRIVATE(k,l,kl,i)
  
  
  ! Orthonormalize initial guess
  ! ============================
  
  !$OMP DO
  do kl=1,k_pairs
    k = kl_pairs(1,kl)
    l = kl_pairs(2,kl)
    if (k/=l) then
      overlap(k,l) = u_dot_v(U_in(1,k),U_in(1,l),sze)
      overlap(l,k) = overlap(k,l)
    else
      overlap(k,k) = u_dot_u(U_in(1,k),sze)
    endif
  enddo
  !$OMP END DO
  !$OMP END PARALLEL

  call ortho_lowdin(overlap,size(overlap,1),N_st,U_in,size(U_in,1),sze)
  
  ! Davidson iterations
  ! ===================
  
  converged = .False.
  
  do while (.not.converged)
    
    !$OMP PARALLEL DEFAULT(NONE)                                     &
        !$OMP PRIVATE(k,i) SHARED(U,u_in,sze,N_st)
    do k=1,N_st
      !$OMP DO
      do i=1,sze
        U(i,k,1) = u_in(i,k)
      enddo
      !$OMP END DO 
    enddo
    !$OMP END PARALLEL
    
    do iter=1,davidson_sze_max-1
      
      ! Compute W_k = H |u_k>
      ! ----------------------
      
      do k=1,N_st
          call H_u_0(W(1,k,iter),U(1,k,iter),H_jj,sze,dets_in,Nint)
      enddo
      
      
      ! Compute h_kl = <u_k | W_l> = <u_k| H |u_l>
      ! -------------------------------------------

      do l=1,N_st
        do k=1,N_st
          do iter2=1,iter-1
            h(k,iter2,l,iter) = u_dot_v(U(1,k,iter2),W(1,l,iter),sze)
            h(k,iter,l,iter2) = h(k,iter2,l,iter)
          enddo
        enddo
        do k=1,l
          h(k,iter,l,iter) = u_dot_v(U(1,k,iter),W(1,l,iter),sze)
          h(l,iter,k,iter) = h(k,iter,l,iter)
        enddo
      enddo

      !DEBUG H MATRIX
      !do i=1,iter
      !  print '(10(x,F16.10))',  h(1,i,1,1:i)
      !enddo
      !print *,  ''
      !END
      
      ! Diagonalize h
      ! -------------
      call lapack_diag(lambda,y,h,N_st*davidson_sze_max,N_st*iter)
      
      ! Express eigenvectors of h in the determinant basis
      ! --------------------------------------------------
      
      do k=1,N_st
        do i=1,sze
          U(i,k,iter+1) = 0.d0
          W(i,k,iter+1) = 0.d0
          do l=1,N_st
            do iter2=1,iter
              U(i,k,iter+1) = U(i,k,iter+1) + U(i,l,iter2)*y(l,iter2,k,1)
              W(i,k,iter+1) = W(i,k,iter+1) + W(i,l,iter2)*y(l,iter2,k,1)
            enddo
          enddo
        enddo
      enddo
      
      ! Compute residual vector
      ! -----------------------
      
      do k=1,N_st
        do i=1,sze
          R(i,k) = lambda(k) * U(i,k,iter+1) - W(i,k,iter+1)
        enddo
        residual_norm(k) = u_dot_u(R(1,k),sze)
        to_print(1,k) = lambda(k) + nuclear_repulsion
        to_print(2,k) = residual_norm(k)
      enddo
      
      write(iunit,'(X,I3,X,100(X,F16.10,X,E16.6))'), iter, to_print(:,1:N_st)
      call davidson_converged(lambda,residual_norm,wall,iter,cpu,N_st,converged)
      if (converged) then
        exit
      endif
      
      
      ! Davidson step
      ! -------------
      
      do k=1,N_st
        do i=1,sze
          U(i,k,iter+1) = -1.d0/max(H_jj(i) - lambda(k),1.d-2) * R(i,k)
        enddo
      enddo
      
      ! Gram-Schmidt
      ! ------------
      
      double precision               :: c
      do k=1,N_st
        do iter2=1,iter
          do l=1,N_st
            c = u_dot_v(U(1,k,iter+1),U(1,l,iter2),sze)
            do i=1,sze
              U(i,k,iter+1) -= c * U(i,l,iter2)
            enddo
          enddo
        enddo
        do l=1,k-1
          c = u_dot_v(U(1,k,iter+1),U(1,l,iter+1),sze)
          do i=1,sze
            U(i,k,iter+1) -= c * U(i,l,iter+1)
          enddo
        enddo
        call normalize( U(1,k,iter+1), sze )
      enddo
    
      !DEBUG : CHECK OVERLAP
      !print *,  '==='
      !do k=1,iter+1
      !  do l=1,k
      !  c = u_dot_v(U(1,1,k),U(1,1,l),sze)
      !  print *,  k,l, c
      !  enddo
      !enddo
      !print *,  '==='
      !pause
      !END DEBUG


    enddo

    if (.not.converged) then
      iter = davidson_sze_max-1
    endif
    
    ! Re-contract to u_in
    ! -----------
    
    do k=1,N_st
      energies(k) = lambda(k)
      do i=1,sze
        u_in(i,k) = 0.d0
        do iter2=1,iter
          do l=1,N_st
            u_in(i,k) += U(i,l,iter2)*y(l,iter2,k,1)
          enddo
        enddo
      enddo
    enddo

  enddo

  write_buffer = '===== '
  do i=1,N_st
    write_buffer = trim(write_buffer)//' ================ ================'
  enddo
  write(iunit,'(A)') trim(write_buffer)
  write(iunit,'(A)') ''
  call write_time(iunit)

  deallocate (                                                       &
      kl_pairs,                                                      &
      W,                                                             &
      Wt,                                                            &
      U,                                                             &
      R,                                                             &
      h,                                                             &
      y,                                                             &
      lambda                                                         &
      )
  abort_here = abort_all
end

BEGIN_PROVIDER [ character(64), davidson_criterion ]
 implicit none
 BEGIN_DOC
 ! Can be : [  energy  | residual | both | wall_time | cpu_time | iterations ]
 END_DOC
 davidson_criterion = 'residual'
END_PROVIDER

subroutine davidson_converged(energy,residual,wall,iterations,cpu,N_st,converged)
  implicit none
  BEGIN_DOC
! True if the Davidson algorithm is converged
  END_DOC
  integer, intent(in) :: N_st, iterations
  logical, intent(out) :: converged
  double precision, intent(in) :: energy(N_st), residual(N_st)
  double precision, intent(in) :: wall, cpu
  double precision :: E(N_st), time
  double precision, allocatable, save :: energy_old(:)

  if (.not.allocated(energy_old)) then
    allocate(energy_old(N_st))
    energy_old = 0.d0
  endif

  E = energy - energy_old
  energy_old = energy
  if (davidson_criterion == 'energy') then
    converged = dabs(maxval(E(1:N_st))) < threshold_davidson 
  else if (davidson_criterion == 'residual') then
    converged = dabs(maxval(residual(1:N_st))) < threshold_davidson 
  else if (davidson_criterion == 'both') then
    converged = dabs(maxval(residual(1:N_st))) + dabs(maxval(E(1:N_st)) ) &
       < threshold_davidson  
  else if (davidson_criterion == 'wall_time') then
    call wall_time(time)
    converged = time - wall > threshold_davidson
  else if (davidson_criterion == 'cpu_time') then
    call cpu_time(time)
    converged = time - cpu > threshold_davidson
  else if (davidson_criterion == 'iterations') then
    converged = iterations >= int(threshold_davidson)
  endif
  converged = converged.or.abort_here
end

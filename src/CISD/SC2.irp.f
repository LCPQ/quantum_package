subroutine CISD_SC2(dets_in,u_in,energies,dim_in,sze,N_st,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! CISD+SC2 method :: take off all the disconnected terms of a CISD (selected or not)
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
  PROVIDE ref_bitmask_energy
  ASSERT (N_st > 0)
  ASSERT (sze > 0)
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  integer                        :: iter
  integer                        :: i,j,k,l,m
  logical                        :: converged
  double precision               :: overlap(N_st,N_st)
  double precision               :: u_dot_v, u_dot_u
  
  integer :: degree,N_double,index_hf,index_double(sze)
  double precision :: hij_elec, e_corr_double,e_corr,diag_h_mat_elem,inv_c0
  double precision :: e_corr_array(sze),H_jj_ref(sze),H_jj_dressed(sze),hij_double(sze)
  double precision :: e_corr_double_before,accu,cpu_2,cpu_1
  integer :: i_ok
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP  SHARED(sze,N_st,                                        &
      !$OMP  H_jj_ref,Nint,dets_in,u_in)                                 &
      !$OMP  PRIVATE(i)
  
  !$OMP DO
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
   if(degree==0)then
    index_hf=i
   else if (degree == 2)then
    N_double += 1
    index_double(N_double) = i
    call i_H_j(ref_bitmask,dets_in(1,1,i),Nint,hij_elec)
    hij_double(N_double) = hij_elec
    e_corr_array(N_double) = u_in(i,1)* hij_elec
    e_corr_double += e_corr_array(N_double)
    e_corr += e_corr_array(N_double)
    index_double(N_double) = i
   else if (degree == 1)then
    call i_H_j(ref_bitmask,dets_in(1,1,i),Nint,hij_elec)
    print*,hij_elec
    e_corr += u_in(i,1)* hij_elec
   endif
  enddo
  inv_c0 = 1.d0/u_in(index_hf,1)
  do i = 1, N_double
   e_corr_array(i) = e_corr_array(i) * inv_c0
  enddo
  e_corr = e_corr * inv_c0
  e_corr_double = e_corr_double * inv_c0
  print*, 'E_corr        = ',e_corr
  print*, 'E_corr_double = ', e_corr_double

  converged = .False.
  e_corr_double_before = e_corr_double
  iter = 0
  do while (.not.converged)

  iter +=1
  print*,'SC2 iteration  : ',iter
   call cpu_time(cpu_1)
   do i=1,sze
     H_jj_dressed(i) = H_jj_ref(i)
     if (i==index_hf)cycle
     accu = 0.d0
     do j=1,N_double
      call repeat_excitation(dets_in(1,1,i),ref_bitmask,dets_in(1,1,index_double(j)),i_ok,Nint)
      if (i_ok==1)cycle! you check if the excitation is possible
      accu += e_corr_array(j)
     enddo
     H_jj_dressed(i) += accu
   enddo

   call cpu_time(cpu_2)
   print*,'time for the excitations = ',cpu_2 - cpu_1
   print*,H_jj_ref(1),H_jj_ref(2)
   print*,H_jj_dressed(1),H_jj_dressed(2)
   print*,u_in(index_hf,1),u_in(index_double(1),1)
   call davidson_diag_hjj(dets_in,u_in,H_jj_dressed,energies,dim_in,sze,N_st,Nint)
   print*,u_in(index_hf,1),u_in(index_double(1),1)
   e_corr_double = 0.d0
   inv_c0 = 1.d0/u_in(index_hf,1)
   do i = 1, N_double
    e_corr_array(i) = u_in(index_double(i),1)*inv_c0 * hij_double(i)
    e_corr_double += e_corr_array(i)
   enddo
   print*,'E_corr   =    ',e_corr_double
   print*,'delta E_corr =',e_corr_double - e_corr_double_before
   converged =  dabs(e_corr_double - e_corr_double_before) < 1.d-10
   if (converged) then
     exit
   endif
   e_corr_double_before = e_corr_double

 enddo



end

subroutine davidson_diag_hjj(dets_in,u_in,H_jj,energies,dim_in,sze,N_st,Nint)
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
  ! Initial guess vectors are not necessarily orthonormal
  END_DOC
  integer, intent(in)            :: dim_in, sze, N_st, Nint
  integer(bit_kind), intent(in)  :: dets_in(Nint,2,sze)
  double precision,  intent(in)  :: H_jj(dim_in)
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
  double precision, allocatable  :: W(:,:,:),  U(:,:,:), R(:,:)
  double precision, allocatable  :: y(:,:,:,:), h(:,:,:,:), lambda(:)
  double precision               :: diag_h_mat_elem
  double precision               :: residual_norm(N_st)
  
  PROVIDE ref_bitmask_energy

  allocate(                                                          &
      kl_pairs(2,N_st*(N_st+1)/2),                                   &
      W(sze,N_st,davidson_sze_max),                                                   &
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
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
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
      
      ! Diagonalize h
      ! -------------
      call lapack_diag(lambda,y,h,N_st*davidson_sze_max,N_st*iter)
      
      ! Express eigenvectors of h in the determinant basis
      ! --------------------------------------------------
      
  !   call dgemm ( 'N','N', sze, N_st*iter, N_st, &
  !     1.d0, U(1,1,1), size(U,1), y(1,1,1,1), size(y,1)*size(y,2), &
  !     0.d0, U(1,1,iter+1), size(U,1) )
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
      enddo

      print '(I3,15(F16.8,x))', iter, lambda(1:N_st) + nuclear_repulsion
      print '(3x,15(E16.5,x))',       residual_norm(1:N_st)
      
      converged = maxval(residual_norm) < 1.d-10
      if (converged) then
        exit
      endif
      
      ! Davidson step
      ! -------------
      
      do k=1,N_st
        do i=1,sze
          U(i,k,iter+1) = 1.d0/(lambda(k) - H_jj(i)) * R(i,k)
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

  deallocate (                                                       &
      kl_pairs,                                                      &
      W,                                                             &
      U,                                                             &
      R,                                                             &
      h,                                                             &
      y,                                                             &
      lambda                                                         &
      )
end

subroutine repeat_excitation(key_in,key_1,key_2,i_ok,Nint)
  use bitmasks
 implicit none
 integer(bit_kind), intent(in) :: key_in(Nint,2),key_1(Nint,2),key_2(Nint,2),Nint
 integer,intent(out):: i_ok
 integer :: ispin,i_hole,k_hole,j_hole,i_particl,k_particl,j_particl,i_trou,degree,exc(0:2,2,2)
 double precision :: phase
 i_ok = 1
 call get_excitation(key_1,key_2,exc,degree,phase,Nint)
 integer :: h1,p1,h2,p2,s1,s2
 if(degree==2)then
  call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
   !  first hole 
   k_hole = ishft(h1-1,-5)+1
   j_hole = h1-ishft(k_hole-1,5)-1
   if(iand(key_in(k_hole,s1),ibset(0,j_hole)).eq.0)then
    i_ok = 0
    return
   endif

   !  second hole
   k_hole = ishft(h2-1,-5)+1
   j_hole = h2-ishft(k_hole-1,5)-1
   if(iand(key_in(k_hole,s2),ibset(0,j_hole)).eq.0)then
    i_ok = 0
    return
   endif

   ! first particle
   k_particl  = ishft(p1-1,-5)+1
   j_particl = p1-ishft(k_particl-1,5)-1
   if(iand(key_in(k_particl,s1),ibset(0,j_particl)).ne.0)then
    i_ok = 0
    return
   endif

   ! second particle
   k_particl  = ishft(p2-1,-5)+1
   j_particl = p2-ishft(k_particl-1,5)-1
   if(iand(key_in(k_particl,s2),ibset(0,j_particl)).ne.0)then
    i_ok = 0
    return
   endif
   return
 endif
end


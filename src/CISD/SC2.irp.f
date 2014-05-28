subroutine CISD_SC2(dets_in,u_in,energies,dim_in,sze,N_st,Nint)
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
  
  integer                        :: degree,N_double,index_hf,index_double(sze)
  double precision               :: hij_elec, e_corr_double,e_corr,diag_h_mat_elem,inv_c0
  double precision               :: e_corr_array(sze),H_jj_ref(sze),H_jj_dressed(sze),hij_double(sze)
  double precision               :: e_corr_double_before,accu,cpu_2,cpu_1
  integer                        :: degree_exc(sze)
  integer                        :: i_ok
  
  call write_time(output_CISD)
  write(output_CISD,'(A)') ''
  write(output_CISD,'(A)') 'CISD SC2'
  write(output_CISD,'(A)') '========'
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
    degree_exc(i) = degree
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
    iter +=1
    do i=1,sze
      H_jj_dressed(i) = H_jj_ref(i)
      if (i==index_hf)cycle
      accu = 0.d0
      if(degree_exc(i)==1)then
        do j=1,N_double
          call get_excitation_degree(dets_in(1,1,i),dets_in(1,1,index_double(j)),degree,N_int)
          if (degree<=2)cycle
          accu += e_corr_array(j)
        enddo
      else
        do j=1,N_double
          call get_excitation_degree(dets_in(1,1,i),dets_in(1,1,index_double(j)),degree,N_int)
          if (degree<=3)cycle
          accu += e_corr_array(j)
        enddo
      endif
      H_jj_dressed(i) += accu
    enddo
    
    call davidson_diag_hjj(dets_in,u_in,H_jj_dressed,energies,dim_in,sze,N_st,Nint,output_CISD)
    e_corr_double = 0.d0
    inv_c0 = 1.d0/u_in(index_hf,1)
    do i = 1, N_double
      e_corr_array(i) = u_in(index_double(i),1)*inv_c0 * hij_double(i)
      e_corr_double += e_corr_array(i)
    enddo
    write(output_CISD,'(A,I3)') 'SC2 Iteration ', iter
    write(output_CISD,'(A)') '------------------'
    write(output_CISD,'(A)') ''
    write(output_CISD,'(A)') '===== ================'
    write(output_CISD,'(A)') 'State Energy          '
    write(output_CISD,'(A)') '===== ================'
    do i=1,N_st
      write(output_CISD,'(I5,X,F16.10)') i, energies(i)+nuclear_repulsion
    enddo
    write(output_CISD,'(A)') '===== ================'
    write(output_CISD,'(A)') ''
    call write_double(output_CISD,(e_corr_double - e_corr_double_before),&
        'Delta(E_corr)')
    converged =  dabs(e_corr_double - e_corr_double_before) < 1.d-10
    if (converged) then
      exit
    endif
    e_corr_double_before = e_corr_double
    
  enddo
  
  call write_time(output_CISD)
  
end

subroutine repeat_excitation(key_in,key_1,key_2,i_ok,Nint)
  use bitmasks
  implicit none
  integer(bit_kind), intent(in)  :: key_in(Nint,2),key_1(Nint,2),key_2(Nint,2)
  integer                        :: Nint
  integer,intent(out)            :: i_ok
  integer                        :: ispin,i_hole,k_hole,j_hole,i_particl,k_particl,j_particl,i_trou,degree,exc(0:2,2,2)
  double precision               :: phase
  i_ok = 1
  call get_excitation(key_1,key_2,exc,degree,phase,Nint)
  integer                        :: h1,p1,h2,p2,s1,s2
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

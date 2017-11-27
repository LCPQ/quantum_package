subroutine dressing_1h1p(dets_in,u_in,diag_H_elements,dim_in,sze,N_st,Nint,convergence)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! CISD+SC2 method              :: take off all the disconnected terms of a ROHF+1h1p (selected or not)
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
  double precision, intent(out)  :: diag_H_elements(dim_in)
  double precision, intent(in)   :: convergence
  
  integer :: i,j,k,l
  integer :: n_singles
  integer :: index_singles(sze),hole_particles_singles(sze,3)
  integer :: n_doubles
  integer :: index_doubles(sze),hole_particles_doubles(sze,2)
  integer :: index_hf
  double precision :: e_corr_singles(mo_tot_num,2)
  double precision :: e_corr_doubles(mo_tot_num)
  double precision :: e_corr_singles_total(2)
  double precision :: e_corr_doubles_1h1p

  integer :: exc(0:2,2,2),degree
  integer :: h1,h2,p1,p2,s1,s2
  integer :: other_spin(2)
  double precision :: phase
  integer(bit_kind) :: key_tmp(N_int,2)
  integer :: i_ok
  double precision :: phase_single_double,phase_double_hf,get_mo_bielec_integral
  double precision :: hij,c_ref,contrib
  integer :: iorb

  other_spin(1) = 2
  other_spin(2) = 1

  n_singles = 0
  n_doubles = 0
  do i = 1,sze
   call get_excitation(ref_bitmask,dets_in(1,1,i),exc,degree,phase,N_int)
   call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
   call i_H_j(dets_in(1,1,i),dets_in(1,1,i),N_int,hij)
   diag_H_elements(i) = hij
   if(degree == 0)then
    index_hf = i
   else if (degree == 1)then
    n_singles +=1
    index_singles(n_singles) = i
    ! h1 = inactive orbital of the hole
    hole_particles_singles(n_singles,1) = h1
    ! p1 = virtual orbital of the particle
    hole_particles_singles(n_singles,2) = p1
    ! s1 = spin of the electron excited
    hole_particles_singles(n_singles,3) = s1
   else if (degree == 2)then
    n_doubles +=1
    index_doubles(n_doubles) = i
    ! h1 = inactive orbital of the hole (beta of course)
    hole_particles_doubles(n_doubles,1) = h1
    ! p1 = virtual orbital of the particle (alpha of course)
    hole_particles_doubles(n_doubles,2) = p2
   else 
    print*,'PB !! found out other thing than a single or double'
    print*,'stopping ..'
    stop
   endif
  enddo

  e_corr_singles = 0.d0
  e_corr_doubles = 0.d0
  e_corr_singles_total = 0.d0
  e_corr_doubles_1h1p = 0.d0
  c_ref = 1.d0/u_in(index_hf,1)
  print*,'c_ref = ',c_ref
  do i = 1,sze
   call get_excitation(ref_bitmask,dets_in(1,1,i),exc,degree,phase,N_int)
   call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
   call i_H_j(ref_bitmask,dets_in(1,1,i),N_int,hij)
   contrib = hij * u_in(i,1) * c_ref
   if (degree == 1)then
    e_corr_singles(h1,s1) += contrib
    e_corr_singles(p1,s1) += contrib
    e_corr_singles_total(s1)+= contrib
   else if (degree == 2)then
    e_corr_doubles_1h1p += contrib
    e_corr_doubles(h1) += contrib
    e_corr_doubles(p2) += contrib
   endif
  enddo
  print*,'e_corr_singles alpha = ',e_corr_singles_total(1)
  print*,'e_corr_singles beta  = ',e_corr_singles_total(2)
  print*,'e_corr_doubles_1h1p  = ',e_corr_doubles_1h1p
  
  ! repeat all the correlation energy on the singles
  do i = 1,n_singles
   ! you can repeat all the correlation energy of the single excitation of the other spin
   diag_H_elements(index_singles(i)) += e_corr_singles_total(other_spin(hole_particles_singles(i,3)))

   ! you can repeat all the correlation energy of the single excitation of the same spin
   do j = 1, n_inact_orb
    iorb = list_inact(j)
    ! except the one of the hole
    if(iorb == hole_particles_singles(i,1))cycle
    ! ispin = hole_particles_singles(i,3)
    diag_H_elements(index_singles(i)) += e_corr_singles(iorb,hole_particles_singles(i,3))
   enddo
   ! also exclude all the energy coming from the virtual orbital
   diag_H_elements(index_singles(i)) -= e_corr_singles(hole_particles_singles(i,2),hole_particles_singles(i,3))
 
   ! If it is a single excitation alpha, you can repeat : 
   !  +) all the double excitation 1h1p, appart the part involving the virtual orbital "r"
   ! If it is a single excitation alpha, you can repeat : 
   !  +) all the double excitation 1h1p, appart the part involving the inactive orbital "i"
   diag_H_elements(index_singles(i)) += e_corr_doubles_1h1p
   if(hole_particles_singles(i,3) == 1)then  ! alpha single excitation
    diag_H_elements(index_singles(i)) -= e_corr_doubles(hole_particles_singles(i,2))
   else  ! beta single exctitation 
    diag_H_elements(index_singles(i)) -= e_corr_doubles(hole_particles_singles(i,1))
   endif
  enddo

  ! repeat all the correlation energy on the doubles
  ! as all the doubles involve the active space, you cannot repeat any of them one on another
  do i = 1, n_doubles
   ! on a given double, you can repeat all the correlation energy of the singles alpha 
   do j = 1, n_inact_orb
    iorb = list_inact(j)
    ! ispin = hole_particles_singles(i,3)
    diag_H_elements(index_doubles(i)) += e_corr_singles(iorb,1)
   enddo
   ! except the part involving the virtual orbital "hole_particles_doubles(i,2)"
   diag_H_elements(index_doubles(i)) -= e_corr_singles(hole_particles_doubles(i,2),1)
   ! on a given double, you can repeat all the correlation energy of the singles beta
   do j = 1, n_inact_orb
    iorb = list_inact(j)
    ! except the one of the hole
    if(iorb == hole_particles_doubles(i,1))cycle
    ! ispin = hole_particles_singles(i,3)
    diag_H_elements(index_doubles(i)) += e_corr_singles(iorb,2)
   enddo
  enddo


  ! Taking into account the connected part of the 2h2p on the HF determinant 
  ! 1/2 \sum_{ir,js} c_{ir}^{sigma} c_{js}^{sigma}
  
! diag_H_elements(index_hf) += total_corr_e_2h2p
  return
  c_ref = c_ref * c_ref
  print*,'diag_H_elements(index_hf) = ',diag_H_elements(index_hf)
  do i = 1, n_singles
   ! start on the single excitation "|i>"
   h1 = hole_particles_singles(i,1)
   p1 = hole_particles_singles(i,2)
   do j = 1, n_singles
    do k = 1, N_int
     key_tmp(k,1) = dets_in(k,1,index_singles(i))
     key_tmp(k,2) = dets_in(k,2,index_singles(i))
    enddo
    h2 = hole_particles_singles(j,1)
    p2 = hole_particles_singles(j,2)
    call do_mono_excitation(key_tmp,h2,p2,hole_particles_singles(j,3),i_ok)    
    ! apply the excitation operator from the single excitation "|j>"
    if(i_ok .ne. 1)cycle
    double precision :: phase_ref_other_single,diag_H_mat_elem,hijj,contrib_e2,coef_1
    call get_excitation(key_tmp,dets_in(1,1,index_singles(i)),exc,degree,phase_single_double,N_int)
    call get_excitation(ref_bitmask,dets_in(1,1,index_singles(j)),exc,degree,phase_ref_other_single,N_int)
    call i_H_j(ref_bitmask,key_tmp,N_int,hij)
    diag_H_elements(index_hf) += u_in(index_singles(i),1) * u_in(index_singles(j),1) * c_ref * hij & 
                                * phase_single_double * phase_ref_other_single
   enddo
  enddo
  print*,'diag_H_elements(index_hf) = ',diag_H_elements(index_hf)

end


subroutine dressing_1h1p_by_2h2p(dets_in,u_in,diag_H_elements,dim_in,sze,N_st,Nint,convergence)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! CISD+SC2 method              :: take off all the disconnected terms of a ROHF+1h1p (selected or not)
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
  double precision, intent(out)  :: diag_H_elements(0:dim_in)
  double precision, intent(in)   :: convergence
  
  integer :: i,j,k,l
  integer :: r,s,i0,j0,r0,s0
  integer :: n_singles
  integer :: index_singles(sze),hole_particles_singles(sze,3)
  integer :: n_doubles
  integer :: index_doubles(sze),hole_particles_doubles(sze,2)
  integer :: index_hf
  double precision :: e_corr_singles(mo_tot_num,2)
  double precision :: e_corr_doubles(mo_tot_num)
  double precision :: e_corr_singles_total(2)
  double precision :: e_corr_doubles_1h1p

  integer :: exc(0:2,2,2),degree
  integer :: h1,h2,p1,p2,s1,s2
  integer :: other_spin(2)
  double precision :: phase
  integer(bit_kind) :: key_tmp(N_int,2)
  integer :: i_ok
  double precision :: phase_single_double,phase_double_hf,get_mo_bielec_integral
  double precision :: hij,c_ref,contrib
  integer :: iorb

  other_spin(1) = 2
  other_spin(2) = 1

  n_singles = 0
  n_doubles = 0
  do i = 1,sze
   call get_excitation(ref_bitmask,dets_in(1,1,i),exc,degree,phase,N_int)
   call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
   call i_H_j(dets_in(1,1,i),dets_in(1,1,i),N_int,hij)
   diag_H_elements(i) = hij
   if(degree == 0)then
    index_hf = i
   else if (degree == 1)then
    n_singles +=1
    index_singles(n_singles) = i
    ! h1 = inactive orbital of the hole
    hole_particles_singles(n_singles,1) = h1
    ! p1 = virtual orbital of the particle
    hole_particles_singles(n_singles,2) = p1
    ! s1 = spin of the electron excited
    hole_particles_singles(n_singles,3) = s1
   else if (degree == 2)then
    n_doubles +=1
    index_doubles(n_doubles) = i
    ! h1 = inactive orbital of the hole (beta of course)
    hole_particles_doubles(n_doubles,1) = h1
    ! p1 = virtual orbital of the particle (alpha of course)
    hole_particles_doubles(n_doubles,2) = p2
   else 
    print*,'PB !! found out other thing than a single or double'
    print*,'stopping ..'
    stop
   endif
  enddo
  double precision :: delta_e
  double precision :: coef_ijrs
  diag_H_elements = 0.d0
  do i0 = 1, n_core_inact_orb
   i= list_core_inact(i0)
   do j0 = i0+1, n_core_inact_orb
    j = list_core_inact(j0)
    print*, i,j
    do r0 = 1, n_virt_orb
     r = list_virt(r0)
     do s0 = r0+1, n_virt_orb
      s = list_virt(s0)
     !!! alpha (i-->r) / beta (j-->s)
      s1 = 1
      s2 = 2
      key_tmp = ref_bitmask
      call do_mono_excitation(key_tmp,i,r,s1,i_ok)
      if(i_ok .ne.1)then
       print*, 'pb !!'
       stop
      endif
      call do_mono_excitation(key_tmp,j,s,s2,i_ok)
      if(i_ok .ne.1)then
       print*, 'pb !!'
       stop
      endif
      call i_H_j(ref_bitmask, key_tmp, N_int,hij)
      delta_e = Fock_matrix_diag_mo(i) + Fock_matrix_diag_mo(j) - Fock_matrix_diag_mo(r) - Fock_matrix_diag_mo(s)
      coef_ijrs = hij/delta_e
      do k = 1, n_singles
       l = index_singles(k) 
       call i_H_j(dets_in(1,1,l), key_tmp, N_int,hij)
       diag_H_elements(l) += coef_ijrs * hij
      enddo
     !if(i>j.and.r>s)then
       !! alpha (i-->r) / alpha (j-->s)
       s1 = 1
       s2 = 1
       key_tmp = ref_bitmask
       call do_mono_excitation(key_tmp,i,r,s1,i_ok)
       if(i_ok .ne.1)then
        print*, 'pb !!'
        stop
       endif
       call do_mono_excitation(key_tmp,j,s,s2,i_ok)
       if(i_ok .ne.1)then
        print*, 'pb !!'
        stop
       endif
       call i_H_j(ref_bitmask, key_tmp, N_int,hij)
       delta_e = Fock_matrix_diag_mo(i) + Fock_matrix_diag_mo(j) - Fock_matrix_diag_mo(r) - Fock_matrix_diag_mo(s)
       coef_ijrs = hij/delta_e
       do k = 1, n_singles
        l = index_singles(k) 
        call i_H_j(dets_in(1,1,l), key_tmp, N_int,hij)
        diag_H_elements(l) += coef_ijrs * hij
       enddo
       !! beta (i-->r) / beta (j-->s)
       s1 = 2
       s2 = 2
       key_tmp = ref_bitmask
       call do_mono_excitation(key_tmp,i,r,s1,i_ok)
       if(i_ok .ne.1)then
        print*, 'pb !!'
        stop
       endif
       call do_mono_excitation(key_tmp,j,s,s2,i_ok)
       if(i_ok .ne.1)then
        print*, 'pb !!'
        stop
       endif
       call i_H_j(ref_bitmask, key_tmp, N_int,hij)
       delta_e = Fock_matrix_diag_mo(i) + Fock_matrix_diag_mo(j) - Fock_matrix_diag_mo(r) - Fock_matrix_diag_mo(s)
       coef_ijrs = hij/delta_e
       do k = 1, n_singles
        l = index_singles(k) 
        call i_H_j(dets_in(1,1,l), key_tmp, N_int,hij)
        diag_H_elements(l) += coef_ijrs * hij
       enddo
     !endif
     enddo
    enddo
   enddo
  enddo
  c_ref = 1.d0/u_in(index_hf,1)
  do k = 1, n_singles
   l = index_singles(k)
   diag_H_elements(0) -= diag_H_elements(l)
  enddo
! do k = 1, n_doubles
!  l = index_doubles(k)
!  diag_H_elements(0) += diag_H_elements(l)
! enddo


end
  

subroutine dressing_1h1p_full(dets_in,u_in,H_matrix,dim_in,sze,N_st,Nint,convergence)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! CISD+SC2 method              :: take off all the disconnected terms of a ROHF+1h1p (selected or not)
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
  double precision, intent(in) :: u_in(dim_in,N_st)
  double precision, intent(inout)  :: H_matrix(sze,sze)
  double precision, intent(in)   :: convergence
  
  integer :: i,j,k,l
  integer :: n_singles
  integer :: index_singles(sze),hole_particles_singles(sze,3)
  integer :: n_doubles
  integer :: index_doubles(sze),hole_particles_doubles(sze,2)
  integer :: index_hf
  double precision :: e_corr_singles(mo_tot_num,2)
  double precision :: e_corr_doubles(mo_tot_num)
  double precision :: e_corr_singles_total(2)
  double precision :: e_corr_doubles_1h1p

  integer :: exc(0:2,2,2),degree
  integer :: h1,h2,p1,p2,s1,s2
  integer :: other_spin(2)
  double precision :: phase
  integer(bit_kind) :: key_tmp(N_int,2)
  integer :: i_ok
  double precision :: phase_single_double,phase_double_hf,get_mo_bielec_integral
  double precision :: hij,c_ref,contrib
  integer :: iorb

  other_spin(1) = 2
  other_spin(2) = 1

  n_singles = 0
  n_doubles = 0
  do i = 1,sze
   call get_excitation(ref_bitmask,dets_in(1,1,i),exc,degree,phase,N_int)
   call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
   if(degree == 0)then
    index_hf = i
   else if (degree == 1)then
    n_singles +=1
    index_singles(n_singles) = i
    ! h1 = inactive orbital of the hole
    hole_particles_singles(n_singles,1) = h1
    ! p1 = virtual orbital of the particle
    hole_particles_singles(n_singles,2) = p1
    ! s1 = spin of the electron excited
    hole_particles_singles(n_singles,3) = s1
   else if (degree == 2)then
    n_doubles +=1
    index_doubles(n_doubles) = i
    ! h1 = inactive orbital of the hole (beta of course)
    hole_particles_doubles(n_doubles,1) = h1
    ! p1 = virtual orbital of the particle (alpha of course)
    hole_particles_doubles(n_doubles,2) = p2
   else 
    print*,'PB !! found out other thing than a single or double'
    print*,'stopping ..'
    stop
   endif
  enddo
  double precision, allocatable :: dressing_H_mat_elem(:)
  allocate(dressing_H_mat_elem(N_det))
  logical :: lmct
  dressing_H_mat_elem = 0.d0
  call dress_diag_elem_2h2p(dressing_H_mat_elem,N_det)
  lmct = .False.
  call dress_diag_elem_2h1p(dressing_H_mat_elem,N_det,lmct,1000)
  lmct = .true.
  call dress_diag_elem_1h2p(dressing_H_mat_elem,N_det,lmct,1000)
  do i = 1, N_det
   H_matrix(i,i) += dressing_H_mat_elem(i)
  enddo

  e_corr_singles = 0.d0
  e_corr_doubles = 0.d0
  e_corr_singles_total = 0.d0
  e_corr_doubles_1h1p = 0.d0
  c_ref = 1.d0/u_in(index_hf,1)
  print*,'c_ref = ',c_ref
  do i = 1,sze
   call get_excitation(ref_bitmask,dets_in(1,1,i),exc,degree,phase,N_int)
   call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
   call i_H_j(ref_bitmask,dets_in(1,1,i),N_int,hij)
   contrib = hij * u_in(i,1) * c_ref
   if (degree == 1)then
    e_corr_singles(h1,s1) += contrib
    e_corr_singles(p1,s1) += contrib
    e_corr_singles_total(s1)+= contrib
   else if (degree == 2)then
    e_corr_doubles_1h1p += contrib
    e_corr_doubles(h1) += contrib
    e_corr_doubles(p2) += contrib
   endif
  enddo
  print*,'e_corr_singles alpha = ',e_corr_singles_total(1)
  print*,'e_corr_singles beta  = ',e_corr_singles_total(2)
  print*,'e_corr_doubles_1h1p  = ',e_corr_doubles_1h1p
 
  
  ! repeat all the correlation energy on the singles
! do i = 1,n_singles
!  ! you can repeat all the correlation energy of the single excitation of the other spin
!  H_matrix(index_singles(i),index_singles(i)) += e_corr_singles_total(other_spin(hole_particles_singles(i,3)))

!  ! you can repeat all the correlation energy of the single excitation of the same spin
!  do j = 1, n_inact_orb
!   iorb = list_inact(j)
!   ! except the one of the hole
!   if(iorb == hole_particles_singles(i,1))cycle
!   ! ispin = hole_particles_singles(i,3)
!   H_matrix(index_singles(i),index_singles(i)) += e_corr_singles(iorb,hole_particles_singles(i,3))
!  enddo
!  ! also exclude all the energy coming from the virtual orbital
!  H_matrix(index_singles(i),index_singles(i)) -= e_corr_singles(hole_particles_singles(i,2),hole_particles_singles(i,3))
!
!  ! If it is a single excitation alpha, you can repeat : 
!  !  +) all the double excitation 1h1p, appart the part involving the virtual orbital "r"
!  ! If it is a single excitation alpha, you can repeat : 
!  !  +) all the double excitation 1h1p, appart the part involving the inactive orbital "i"
!  H_matrix(index_singles(i),index_singles(i)) += e_corr_doubles_1h1p
!  if(hole_particles_singles(i,3) == 1)then  ! alpha single excitation
!   H_matrix(index_singles(i),index_singles(i)) -= e_corr_doubles(hole_particles_singles(i,2))
!  else  ! beta single exctitation 
!   H_matrix(index_singles(i),index_singles(i)) -= e_corr_doubles(hole_particles_singles(i,1))
!  endif
! enddo

! ! repeat all the correlation energy on the doubles
! ! as all the doubles involve the active space, you cannot repeat any of them one on another
! do i = 1, n_doubles
!  ! on a given double, you can repeat all the correlation energy of the singles alpha 
!  do j = 1, n_inact_orb
!   iorb = list_inact(j)
!   ! ispin = hole_particles_singles(i,3)
!   H_matrix(index_doubles(i),index_doubles(i)) += e_corr_singles(iorb,1)
!  enddo
!  ! except the part involving the virtual orbital "hole_particles_doubles(i,2)"
!  H_matrix(index_doubles(i),index_doubles(i)) -= e_corr_singles(hole_particles_doubles(i,2),1)
!  ! on a given double, you can repeat all the correlation energy of the singles beta
!  do j = 1, n_inact_orb
!   iorb = list_inact(j)
!   ! except the one of the hole
!   if(iorb == hole_particles_doubles(i,1))cycle
!   ! ispin = hole_particles_singles(i,3)
!   H_matrix(index_doubles(i),index_doubles(i)) += e_corr_singles(iorb,2)
!  enddo
! enddo


  ! Taking into account the connected part of the 2h2p on the HF determinant 
  ! 1/2 \sum_{ir,js} c_{ir}^{sigma} c_{js}^{sigma}
  
! H_matrix(index_hf) += total_corr_e_2h2p
  print*,'H_matrix(index_hf,index_hf) = ',H_matrix(index_hf,index_hf)
  do i = 1, n_singles
   ! start on the single excitation "|i>"
   h1 = hole_particles_singles(i,1)
   p1 = hole_particles_singles(i,2)
   print*,'i = ',i
   do j = i+1, n_singles
    do k = 1, N_int
     key_tmp(k,1) = dets_in(k,1,index_singles(i))
     key_tmp(k,2) = dets_in(k,2,index_singles(i))
    enddo
    h2 = hole_particles_singles(j,1)
    p2 = hole_particles_singles(j,2)
    call do_mono_excitation(key_tmp,h2,p2,hole_particles_singles(j,3),i_ok)    
    ! apply the excitation operator from the single excitation "|j>"
    if(i_ok .ne. 1)cycle
    double precision :: H_array(sze),diag_H_mat_elem,hjj
    do k = 1, sze
     call get_excitation_degree(dets_in(1,1,k),key_tmp,degree,N_int)
     H_array(k) = 0.d0
     if(degree > 2)cycle
     call i_H_j(dets_in(1,1,k),key_tmp,N_int,hij)
     H_array(k) = hij
    enddo
    hjj = 1.d0/(ref_bitmask_energy - diag_H_mat_elem(key_tmp,N_int))
!   contrib_e2 = 0.5d0 * (delta_e + dsqrt(delta_e * delta_e + 4.d0 * hij * hij))
    do l = 2, sze
!    pause
     H_matrix(l,l) += H_array(l) * H_array(l) * hjj
!    H_matrix(1,l) += H_array(1) * H_array(l) * hjj
!    H_matrix(l,1) += H_array(1) * H_array(l) * hjj
    enddo
   enddo
  enddo
  print*,'H_matrix(index_hf,index_hf) = ',H_matrix(index_hf,index_hf)

end

subroutine SC2_1h1p_full(dets_in,u_in,energies,H_matrix,dim_in,sze,N_st,Nint,convergence)
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
  double precision, intent(out)  :: H_matrix(sze,sze)
  double precision, intent(in)   :: convergence
  integer :: i,j,iter
  print*,'sze = ',sze
  H_matrix = 0.d0
  do iter = 1, 1
!   if(sze<=N_det_max_jacobi)then
      double precision, allocatable  :: eigenvectors(:,:), eigenvalues(:),H_matrix_tmp(:,:)
      allocate (H_matrix_tmp(size(H_matrix_all_dets,1),sze),eigenvalues(sze),eigenvectors(size(H_matrix_all_dets,1),sze))
      H_matrix_tmp = 0.d0
      call dressing_1h1p_full(dets_in,u_in,H_matrix_tmp,dim_in,sze,N_st,Nint,convergence)
      do j=1,sze
        do i=1,sze
          H_matrix_tmp(i,j) += H_matrix_all_dets(i,j)
        enddo
      enddo
      print*,'passed the dressing'
      call lapack_diag(eigenvalues,eigenvectors,                     &
          H_matrix_tmp,size(H_matrix_all_dets,1),sze)
      do j=1,min(N_states_diag,sze)
        do i=1,sze
          u_in(i,j) = eigenvectors(i,j)
        enddo
        energies(j) = eigenvalues(j)
      enddo
      deallocate (H_matrix_tmp, eigenvalues, eigenvectors)
!   else
!     call davidson_diag_hjj(dets_in,u_in,diag_H_elements,energies,dim_in,sze,N_st,Nint,output_determinants)
!   endif
    print*,'E = ',energies(1) + nuclear_repulsion

  enddo

  
end


subroutine SC2_1h1p(dets_in,u_in,energies,diag_H_elements,dim_in,sze,N_st,Nint,convergence)
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
  double precision, intent(out)  :: diag_H_elements(dim_in)
  double precision               :: extra_diag_H_elements(dim_in)
  double precision, intent(in)   :: convergence
  integer :: i,j,iter
  DIAG_H_ELEMENTS = 0.d0
  do iter = 1, 1
!  call dressing_1h1p(dets_in,u_in,diag_H_elements,dim_in,sze,N_st,Nint,convergence)
   call dressing_1h1p_by_2h2p(dets_in,u_in,extra_diag_H_elements,dim_in,sze,N_st,Nint,convergence)
!   if(sze<=N_det_max_jacobi)then
      double precision, allocatable  :: eigenvectors(:,:), eigenvalues(:),H_matrix_tmp(:,:)
      allocate (H_matrix_tmp(size(H_matrix_all_dets,1),sze),eigenvalues(sze),eigenvectors(size(H_matrix_all_dets,1),sze))
      do j=1,sze
        do i=1,sze
          H_matrix_tmp(i,j) = H_matrix_all_dets(i,j)
        enddo
      enddo
      H_matrix_tmp(1,1) += extra_diag_H_elements(1)
      do i = 2,sze
        H_matrix_tmp(1,i) += extra_diag_H_elements(i)
        H_matrix_tmp(i,1) += extra_diag_H_elements(i)
      enddo
     !do i = 1,sze
     !  H_matrix_tmp(i,i) = diag_H_elements(i)
     !enddo
      call lapack_diag(eigenvalues,eigenvectors,                     &
          H_matrix_tmp,size(H_matrix_all_dets,1),sze)
      do j=1,min(N_states_diag,sze)
        do i=1,sze
          u_in(i,j) = eigenvectors(i,j)
        enddo
        energies(j) = eigenvalues(j)
      enddo
      deallocate (H_matrix_tmp, eigenvalues, eigenvectors)
!   else
!     call davidson_diag_hjj(dets_in,u_in,diag_H_elements,energies,dim_in,sze,N_st,Nint,output_determinants)
!   endif
    print*,'E = ',energies(1) + nuclear_repulsion

  enddo

  
end


subroutine density_matrix_1h1p(dets_in,u_in,density_matrix_alpha,density_matrix_beta,norm,dim_in,sze,N_st,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! CISD+SC2 method              :: take off all the disconnected terms of a ROHF+1h1p (selected or not)
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
  double precision, intent(inout)  :: density_matrix_alpha(mo_tot_num,mo_tot_num)
  double precision, intent(inout)  :: density_matrix_beta(mo_tot_num,mo_tot_num)
  double precision, intent(inout)  :: norm
  
  integer :: i,j,k,l
  integer :: n_singles
  integer :: index_singles(sze),hole_particles_singles(sze,3)
  integer :: n_doubles
  integer :: index_doubles(sze),hole_particles_doubles(sze,2)
  integer :: index_hf

  integer :: exc(0:2,2,2),degree
  integer :: h1,h2,p1,p2,s1,s2
  integer :: other_spin(2)
  double precision :: phase
  integer(bit_kind) :: key_tmp(N_int,2)
  integer :: i_ok
  double precision :: phase_single_double,phase_double_hf,get_mo_bielec_integral
  double precision :: hij,c_ref,contrib
  integer :: iorb

  other_spin(1) = 2
  other_spin(2) = 1

  n_singles = 0
  n_doubles = 0
  norm = 0.d0
  do i = 1,sze
   call get_excitation(ref_bitmask,dets_in(1,1,i),exc,degree,phase,N_int)
   call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
   norm += u_in(i,1)* u_in(i,1)
   if(degree == 0)then
    index_hf = i
    c_ref = 1.d0/psi_coef(i,1)
   else if (degree == 1)then
    n_singles +=1
    index_singles(n_singles) = i
    ! h1 = inactive orbital of the hole
    hole_particles_singles(n_singles,1) = h1
    ! p1 = virtual orbital of the particle
    hole_particles_singles(n_singles,2) = p1
    ! s1 = spin of the electron excited
    hole_particles_singles(n_singles,3) = s1
   else if (degree == 2)then
    n_doubles +=1
    index_doubles(n_doubles) = i
    ! h1 = inactive orbital of the hole (beta of course)
    hole_particles_doubles(n_doubles,1) = h1
    ! p1 = virtual orbital of the particle (alpha of course)
    hole_particles_doubles(n_doubles,2) = p2
   else 
    print*,'PB !! found out other thing than a single or double'
    print*,'stopping ..'
    stop
   endif
  enddo
  print*,'norm = ',norm

  ! Taking into account the connected part of the 2h2p on the HF determinant 
  ! 1/2 \sum_{ir,js} c_{ir}^{sigma} c_{js}^{sigma}
  
  do i = 1, n_singles
   ! start on the single excitation "|i>"
   h1 = hole_particles_singles(i,1)
   p1 = hole_particles_singles(i,2)
   do j = 1, n_singles
    do k = 1, N_int
     key_tmp(k,1) = dets_in(k,1,index_singles(i))
     key_tmp(k,2) = dets_in(k,2,index_singles(i))
    enddo
    h2 = hole_particles_singles(j,1)
    p2 = hole_particles_singles(j,2)
    call do_mono_excitation(key_tmp,h2,p2,hole_particles_singles(j,3),i_ok)    
    ! apply the excitation operator from the single excitation "|j>"
    if(i_ok .ne. 1)cycle
    double precision :: coef_ijrs,phase_other_single_ref
    integer                        :: occ(N_int*bit_kind_size,2),n_occ(2)
    call get_excitation(key_tmp,dets_in(1,1,index_singles(i)),exc,degree,phase_single_double,N_int)
    call get_excitation(ref_bitmask,dets_in(1,1,index_singles(j)),exc,degree,phase_other_single_ref,N_int)
    call get_excitation(key_tmp,dets_in(1,1,index_singles(j)),exc,degree,phase_other_single_ref,N_int)
    coef_ijrs = u_in(index_singles(i),1) * u_in(index_singles(j),1) * c_ref * c_ref & 
                                * phase_single_double * phase_other_single_ref
    call bitstring_to_list_ab(key_tmp, occ, n_occ, N_int)
    do k=1,elec_alpha_num
      l = occ(k,1)
      density_matrix_alpha(l,l) += coef_ijrs*coef_ijrs
    enddo
    do k=1,elec_beta_num
      l = occ(k,1)
      density_matrix_beta(l,l) += coef_ijrs*coef_ijrs
    enddo
    norm += coef_ijrs* coef_ijrs
    if(hole_particles_singles(j,3) == 1)then ! single alpha
     density_matrix_alpha(h2,p2) += coef_ijrs * phase_single_double * u_in(index_singles(i),1) * c_ref
     density_matrix_alpha(p2,h2) += coef_ijrs * phase_single_double * u_in(index_singles(i),1) * c_ref
    else
     density_matrix_beta(h2,p2) += coef_ijrs * phase_single_double * u_in(index_singles(i),1) * c_ref
     density_matrix_beta(p2,h2) += coef_ijrs * phase_single_double * u_in(index_singles(i),1) * c_ref
    endif
   enddo
  enddo


  do i = 1, n_doubles
   ! start on the double excitation "|i>"
   h1 = hole_particles_doubles(i,1)
   p1 = hole_particles_doubles(i,2)
   do j = 1, n_singles
    do k = 1, N_int
     key_tmp(k,1) = dets_in(k,1,index_doubles(i))
     key_tmp(k,2) = dets_in(k,2,index_doubles(i))
    enddo
    h2 = hole_particles_singles(j,1)
    p2 = hole_particles_singles(j,2)
    call do_mono_excitation(key_tmp,h2,p2,hole_particles_singles(j,3),i_ok)    
    ! apply the excitation operator from the single excitation "|j>"
    if(i_ok .ne. 1)cycle
    double precision :: coef_ijrs_kv,phase_double_triple
    call get_excitation(key_tmp,dets_in(1,1,index_singles(i)),exc,degree,phase_double_triple,N_int)
    call get_excitation(ref_bitmask,dets_in(1,1,index_singles(j)),exc,degree,phase_other_single_ref,N_int)
    call get_excitation(key_tmp,dets_in(1,1,index_singles(j)),exc,degree,phase_other_single_ref,N_int)
    coef_ijrs_kv = u_in(index_doubles(i),1) * u_in(index_singles(j),1) * c_ref * c_ref & 
                                * phase_double_triple * phase_other_single_ref
    call bitstring_to_list_ab(key_tmp, occ, n_occ, N_int)
    do k=1,elec_alpha_num
      l = occ(k,1)
      density_matrix_alpha(l,l) += coef_ijrs_kv*coef_ijrs_kv
    enddo
    do k=1,elec_beta_num
      l = occ(k,1)
      density_matrix_beta(l,l) += coef_ijrs_kv*coef_ijrs_kv
    enddo
    norm += coef_ijrs_kv* coef_ijrs_kv
    if(hole_particles_singles(j,3) == 1)then ! single alpha
     density_matrix_alpha(h2,p2) += coef_ijrs_kv * phase_double_triple * u_in(index_doubles(i),1) * c_ref
     density_matrix_alpha(p2,h2) += coef_ijrs_kv * phase_double_triple * u_in(index_doubles(i),1) * c_ref
    else
     density_matrix_beta(h2,p2) += coef_ijrs_kv * phase_double_triple * u_in(index_doubles(i),1) * c_ref
     density_matrix_beta(p2,h2) += coef_ijrs_kv * phase_double_triple * u_in(index_doubles(i),1) * c_ref
    endif
   enddo
  enddo




  print*,'norm = ',norm
  norm = 1.d0/norm
  do i = 1, mo_tot_num
   do j = 1, mo_tot_num
    density_matrix_alpha(i,j) *= norm
    density_matrix_beta(i,j) *= norm
   enddo
  enddo
  coef_ijrs = 0.d0
  do i = 1, mo_tot_num
   coef_ijrs += density_matrix_beta(i,i) + density_matrix_beta(i,i)
  enddo
  print*,'accu = ',coef_ijrs

end


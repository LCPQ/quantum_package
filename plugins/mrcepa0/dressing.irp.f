use bitmasks


 BEGIN_PROVIDER [ integer, cepa0_shortcut, (0:N_det_non_ref+1) ]
&BEGIN_PROVIDER [ integer, det_cepa0_idx, (N_det_non_ref) ]
&BEGIN_PROVIDER [ integer(bit_kind), det_cepa0_active, (N_det_non_ref) ]
&BEGIN_PROVIDER [ integer(bit_kind), det_ref_active, (N_det_ref) ]
  use bitmasks
  implicit none
  
  integer(bit_kind) :: det_noactive(N_int, 2, N_det_non_ref), active_sorb(2)
  integer i, II, j, k
  logical, external :: detEq
  
  print *, "provide cepa0"
  active_sorb = (/b'001100000000', b'001100000000'/)
  do i=1, N_det_non_ref
    det_noactive(1,1,i) = iand(psi_non_ref(1,1,i), not(active_sorb(1)))
    det_noactive(1,2,i) = iand(psi_non_ref(1,2,i), not(active_sorb(2))) 	 	
  end do
  
  call sort_dets_ab(det_noactive, det_cepa0_idx, cepa0_shortcut, N_det_non_ref, N_int)
  do i=1,N_det_ref
    det_ref_active(i) = iand(psi_ref(1,1,i), active_sorb(1))
    det_ref_active(i) = det_ref_active(i) + iand(psi_ref(1,2,i), active_sorb(2)) * 2_8**32_8
  end do
  
  cepa0_shortcut(0) = 1
  cepa0_shortcut(1) = 1
  det_cepa0_active(1) = iand(psi_non_ref(1,1,det_cepa0_idx(1)), active_sorb(1))
  det_cepa0_active(1) = det_cepa0_active(1) + iand(psi_non_ref(1,2,det_cepa0_idx(1)), active_sorb(2)) * 2_8**32_8
  
  do i=2,N_det_non_ref
    det_cepa0_active(i) = iand(psi_non_ref(1,1,det_cepa0_idx(i)), active_sorb(1))
    det_cepa0_active(i) = det_cepa0_active(i) + iand(psi_non_ref(1,2,det_cepa0_idx(i)), active_sorb(2)) * 2_8**32_8
    
    if(.not. detEq(det_noactive(1,1,i), det_noactive(1,1,i-1), N_int)) then
      cepa0_shortcut(0) += 1
      cepa0_shortcut(cepa0_shortcut(0)) = i
    end if
  end do
  cepa0_shortcut(0) += 1
  cepa0_shortcut(cepa0_shortcut(0)) = N_det_non_ref+1
  
END_PROVIDER



 BEGIN_PROVIDER [ double precision, delta_cas, (N_det_ref, N_det_ref, N_states) ]
  use bitmasks
  implicit none
  integer :: i,j,k
  double precision :: Hjk, Hki
  integer i_state
  
  i_state = 1
  
  do i=1,N_det_ref
    do j=1,i
      delta_cas(i,j,i_state) = 0d0
      do k=1,N_det_non_ref
        call i_h_j(psi_ref(1,1,j), psi_non_ref(1,1,k),N_int,Hjk)
        call i_h_j(psi_ref(1,1,i), psi_non_ref(1,1,k),N_int,Hki)
        delta_cas(i,j,i_state) += Hjk * Hki * lambda_mrcc(i_state, k)
      end do
      delta_cas(j,i,i_state) = delta_cas(i,j,i_state)
    end do
  end do
  
  print *, "mrcepa0_cas_dressing",  delta_cas(:,:,1)
 END_PROVIDER
 
logical function detEq(a,b,Nint)
   use bitmasks
   implicit none
   integer, intent(in) :: Nint
   integer(bit_kind), intent(in) :: a(Nint,2), b(Nint,2)
   integer :: ni, i
 
   detEq = .false.
   do i=1,2
   do ni=1,Nint
     if(a(ni,i) /= b(ni,i)) return
   end do
   end do
   detEq = .true.
end function



 BEGIN_PROVIDER [ double precision, delta_ij, (N_det_ref,N_det_non_ref,N_states) ]
&BEGIN_PROVIDER [ double precision, delta_ii, (N_det_ref,N_states) ]
  use bitmasks
  implicit none
  
  integer                         :: i_state, i, i_I, J, k, degree, degree2, m, l, deg, ni
  integer                         :: p1,p2,h1,h2,s1,s2, p1_,p2_,h1_,h2_,s1_,s2_
  logical                         :: ok
  double precision                :: phase_iI, phase_Ik, phase_Jl, phase_IJ, phase_al, diI, hIi, hJi, delta_JI, dkI(1), HkI, ci_inv(1), dia_hla(1)
  double precision                :: contrib
  integer, dimension(0:2,2,2)     :: exc_iI, exc_Ik, exc_IJ
  integer(bit_kind)               :: det_tmp(N_int, 2), made_hole, made_particle
  integer, allocatable            :: idx_sorted_bit(:)
  integer, external               :: get_index_in_psi_det_sorted_bit
  logical, external               :: is_in_wavefunction
  
  integer :: II, blok
  
  provide det_cepa0_active
  
  if(N_int /= 1) then
    print *, "mrcepa0 experimental N_int==1"
    stop
  end if
  
  i_state = 1
  delta_ii(:,:) = 0
  delta_ij(:,:,:) = 0
  
!   do i=1,N_det_ref
!     delta_ii(i,i_state) = delta_cas(i,i)
!   end do
  
  provide mo_bielec_integrals_in_map
  allocate(idx_sorted_bit(N_det))
  
  idx_sorted_bit(:) = -1
  do i=1,N_det_non_ref
    idx_sorted_bit(get_index_in_psi_det_sorted_bit(psi_non_ref(1,1,i), N_int)) = i
  enddo
  
  !sd $OMP PARALLEL DO schedule(dynamic,10) default(firstprivate) shared(delta_ij, delta_ii)
  do blok=1,cepa0_shortcut(0)
  do i=cepa0_shortcut(blok), cepa0_shortcut(blok+1)-1
    do II=1,N_det_ref
      
      made_hole = iand(det_cepa0_active(i), xor(det_cepa0_active(i), det_ref_active(II)))
      made_particle = iand(det_ref_active(II), xor(det_cepa0_active(i), det_ref_active(II)))
      
      if(popcnt(made_hole) + popcnt(made_particle) > 2) cycle
      
      do k=cepa0_shortcut(blok), cepa0_shortcut(blok+1)-1
        do J=1,N_det_ref
          if(iand(made_hole, det_ref_active(J)) /= made_hole) cycle
          if(iand(made_particle, det_ref_active(J)) /= 0) cycle
          call i_h_j(psi_ref(1,1,II), psi_non_ref(1,1,det_cepa0_idx(k)),N_int,Hki)
          contrib = Hki * lambda_mrcc(i_state, det_cepa0_idx(k)) * delta_cas(II,J,i_state)
          delta_ij(II, det_cepa0_idx(i), i_state) += contrib
          
!                     if(dabs(psi_ref_coef(II,i_state)).ge.5.d-5) then
!                       !qs$OMP CRITICAL 
!                       delta_ii(II,i_state) -= contrib / psi_ref_coef(II, i_state) * psi_non_ref_coef(det_cepa0_idx(k),i_state)
!                       !dsd$OMP END CRITICAL
!                     endif

        end do
      end do
    end do
  end do
  end do
  !qsd $OMP END PARALLEL DO
  deallocate(idx_sorted_bit)
END_PROVIDER


subroutine set_det_bit(det, p, s)
  use bitmasks
  implicit none
  integer(bit_kind),intent(inout) :: det(N_int, 2)
  integer, intent(in) :: p, s
  integer :: ni, pos
  
  ni = (p-1)/bit_kind_size + 1
  pos = mod(p-1, bit_kind_size)
  det(ni,s) = ibset(det(ni,s), pos)
end subroutine
  
  
  
subroutine apply_excitation(det, exc, res, ok, Nint)
  use bitmasks
  implicit none
  
  integer, intent(in) :: Nint
  integer, intent(in) :: exc(0:2,2,2)
  integer(bit_kind),intent(in) :: det(Nint, 2)
  integer(bit_kind),intent(out) :: res(Nint, 2)
  logical, intent(out) :: ok
  integer :: h1,p1,h2,p2,s1,s2,degree
  integer :: ii, pos
  
  
  ok = .false.
  degree = exc(0,1,1) + exc(0,1,2)
  if(.not. (degree > 0 .and. degree <= 2)) then
    print *, "apply ex"
    STOP
  endif
  
  call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
  res = det
  
  ii = (h1-1)/bit_kind_size + 1
  pos = mod(h1-1, 64)!iand(h1-1,bit_kind_size-1) ! mod 64
  if(iand(det(ii, s1), ishft(1_bit_kind, pos)) == 0_8) return
  res(ii, s1) = ibclr(res(ii, s1), pos)
  
  ii = (p1-1)/bit_kind_size + 1
  pos = mod(p1-1, 64)!iand(p1-1,bit_kind_size-1)
  if(iand(det(ii, s1), ishft(1_bit_kind, pos)) /= 0_8) return
  res(ii, s1) = ibset(res(ii, s1), pos)
  
  if(degree == 2) then
    ii = (h2-1)/bit_kind_size + 1
    pos = mod(h2-1, 64)!iand(h2-1,bit_kind_size-1)
    if(iand(det(ii, s2), ishft(1_bit_kind, pos)) == 0_8) return
    res(ii, s2) = ibclr(res(ii, s2), pos)
    
    ii = (p2-1)/bit_kind_size + 1
    pos = mod(p2-1, 64)!iand(p2-1,bit_kind_size-1)
    if(iand(det(ii, s2), ishft(1_bit_kind, pos)) /= 0_8) return
    res(ii, s2) = ibset(res(ii, s2), pos)
  endif

  ok = .true.
end subroutine









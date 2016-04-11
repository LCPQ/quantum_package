use bitmasks




 BEGIN_PROVIDER [ double precision, delta_ij, (N_states,N_det_non_ref,N_det_ref) ]
&BEGIN_PROVIDER [ double precision, delta_ii, (N_states, N_det_ref) ]
  use bitmasks
  implicit none
  integer :: i, j, i_state
  
  !mrmode : 1=mrcepa0, 2=mrsc2 add, 3=mrsc2 sub
  
  do i_state = 1, N_states
    if(mrmode == 3) then
      do i = 1, N_det_ref
        delta_ii(i_state,i)= delta_mrcepa0_ii(i,i_state) - delta_sub_ii(i,i_state)
        do j = 1, N_det_non_ref
          delta_ij(i_state,j,i) = delta_mrcepa0_ij(i,j,i_state) - delta_sub_ij(i,j,i_state)
        end do
      end do
    else if(mrmode == 2) then
      do i = 1, N_det_ref
        delta_ii(i_state,i)= delta_ii_old(i,i_state)
        do j = 1, N_det_non_ref
          delta_ij(i_state,j,i) = delta_ij_old(i,j,i_state)
        end do
      end do
    else if(mrmode == 1) then
      do i = 1, N_det_ref
        delta_ii(i_state,i)= delta_mrcepa0_ii(i,i_state)
        do j = 1, N_det_non_ref
          delta_ij(i_state,j,i) = delta_mrcepa0_ij(i,j,i_state)
        end do
      end do
    else
      stop "invalid mrmode"
    end if
  end do
END_PROVIDER




 BEGIN_PROVIDER [ integer, cepa0_shortcut, (0:N_det_non_ref+1) ]
&BEGIN_PROVIDER [ integer, det_cepa0_idx, (N_det_non_ref) ]
&BEGIN_PROVIDER [ integer(bit_kind), det_cepa0_active, (N_int,2,N_det_non_ref) ]
&BEGIN_PROVIDER [ integer(bit_kind), det_ref_active, (N_int,2,N_det_ref) ]
&BEGIN_PROVIDER [ integer(bit_kind), active_sorb, (N_int,2) ]
  use bitmasks
  implicit none
  
  integer(bit_kind) :: det_noactive(N_int, 2, N_det_non_ref), nonactive_sorb(N_int,2)
  integer i, II, j, k
  logical, external :: detEq
  
  active_sorb(:,:) = 0_8
  nonactive_sorb(:,:) = not(0_8)
  
  if(N_det_ref > 1) then
    do i=1, N_det_ref
    do k=1, N_int
      active_sorb(k,1) = ior(psi_ref(k,1,i), active_sorb(k,1))
      active_sorb(k,2) = ior(psi_ref(k,2,i), active_sorb(k,2))
      nonactive_sorb(k,1) = iand(psi_ref(k,1,i), nonactive_sorb(k,1))
      nonactive_sorb(k,2) = iand(psi_ref(k,2,i), nonactive_sorb(k,2))
    end do
    end do
    do k=1, N_int
      active_sorb(k,1) = iand(active_sorb(k,1), not(nonactive_sorb(k,1)))
      active_sorb(k,2) = iand(active_sorb(k,2), not(nonactive_sorb(k,2)))
    end do
  end if
    
  do i=1, N_det_non_ref
  do k=1, N_int
    det_noactive(k,1,i) = iand(psi_non_ref(k,1,i), not(active_sorb(k,1)))
    det_noactive(k,2,i) = iand(psi_non_ref(k,2,i), not(active_sorb(k,2))) 
  end do
  end do
  
  call sort_dets_ab(det_noactive, det_cepa0_idx, cepa0_shortcut, N_det_non_ref, N_int)
  
  do i=1,N_det_ref
  do k=1, N_int
    det_ref_active(k,1,i) = iand(psi_ref(k,1,i), active_sorb(k,1))
    det_ref_active(k,2,i) = iand(psi_ref(k,2,i), active_sorb(k,2))
    !det_ref_active(i) = det_ref_active(i) + iand(psi_ref(1,2,i), active_sorb(2)) * 2_8**32_8
  end do
  end do

  cepa0_shortcut(0) = 1
  cepa0_shortcut(1) = 1
  do k=1, N_int
    det_cepa0_active(k,1,1) = iand(psi_non_ref(k,1,det_cepa0_idx(1)), active_sorb(k,1))
    det_cepa0_active(k,2,1) = iand(psi_non_ref(k,2,det_cepa0_idx(1)), active_sorb(k,2))
    !det_cepa0_active(1) = det_cepa0_active(1) + iand(psi_non_ref(1,2,det_cepa0_idx(1)), active_sorb(2)) * 2_8**32_8
  end do
  
  do i=2,N_det_non_ref
    do k=1, N_int
      det_cepa0_active(k,1,i) = iand(psi_non_ref(k,1,det_cepa0_idx(i)), active_sorb(k,1))
      det_cepa0_active(k,2,i) = iand(psi_non_ref(k,2,det_cepa0_idx(i)), active_sorb(k,2))
    end do
!     det_cepa0_active(i) = iand(psi_non_ref(1,1,det_cepa0_idx(i)), active_sorb(1))
!     det_cepa0_active(i) = det_cepa0_active(i) + iand(psi_non_ref(1,2,det_cepa0_idx(i)), active_sorb(2)) * 2_8**32_8
    
    if(.not. detEq(det_noactive(1,1,i), det_noactive(1,1,i-1), N_int)) then
      cepa0_shortcut(0) += 1
      cepa0_shortcut(cepa0_shortcut(0)) = i
    end if
  end do
  cepa0_shortcut(cepa0_shortcut(0)+1) = N_det_non_ref+1
END_PROVIDER



 BEGIN_PROVIDER [ double precision, delta_cas, (N_det_ref, N_det_ref, N_states) ]
  use bitmasks
  implicit none
  integer :: i,j,k
  double precision :: Hjk, Hki, Hij
  integer i_state, degree
  
  provide lambda_mrcc
  do i_state = 1, N_states
    !$OMP PARALLEL DO default(none) schedule(dynamic) private(j,k,Hjk,Hki,degree) shared(no_mono_dressing,lambda_mrcc,i_state, N_det_non_ref,psi_ref, psi_non_ref,N_int,delta_cas,N_det_ref)
    do i=1,N_det_ref
      do j=1,i
        call get_excitation_degree(psi_ref(1,1,i), psi_ref(1,1,j), degree, N_int)
        delta_cas(i,j,i_state) = 0d0
        !if(no_mono_dressing .and. degree == 1) cycle
        do k=1,N_det_non_ref

          call i_h_j(psi_ref(1,1,j), psi_non_ref(1,1,k),N_int,Hjk)
          call i_h_j(psi_non_ref(1,1,k),psi_ref(1,1,i), N_int,Hki)
          
          delta_cas(i,j,i_state) += Hjk * Hki * lambda_mrcc(i_state, k)
        end do
      end do
    end do
    !$OMP END PARALLEL DO
    do i=1,N_det_ref
    do j=1,i
        delta_cas(j,i,i_state) = delta_cas(i,j,i_state)
    end do
    end do 
  end do
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




 BEGIN_PROVIDER [ double precision, delta_mrcepa0_ij, (N_det_ref,N_det_non_ref,N_states) ]
&BEGIN_PROVIDER [ double precision, delta_mrcepa0_ii, (N_det_ref,N_states) ]
  use bitmasks
  implicit none
  
  integer                         :: i_state, i, i_I, J, k, degree, degree2, m, l, deg, ni
  integer                         :: p1,p2,h1,h2,s1,s2, p1_,p2_,h1_,h2_,s1_,s2_
  logical                         :: ok
  double precision                :: phase_iI, phase_Ik, phase_Jl, phase_IJ, phase_al, diI, hIi, hJi, delta_JI, dkI(1), HkI, ci_inv(1), dia_hla(1)
  double precision                :: contrib, HIIi, HJk
  integer, dimension(0:2,2,2)     :: exc_iI, exc_Ik, exc_IJ
  integer(bit_kind)               :: det_tmp(N_int, 2), made_hole(N_int,2), made_particle(N_int,2), myActive(N_int,2)
  integer, allocatable            :: idx_sorted_bit(:)
  integer, external               :: get_index_in_psi_det_sorted_bit
  logical, external               :: is_in_wavefunction
  
  integer :: II, blok
  
  provide det_cepa0_active delta_cas lambda_mrcc
  provide mo_bielec_integrals_in_map
  allocate(idx_sorted_bit(N_det))
  
  idx_sorted_bit(:) = -1
  do i=1,N_det_non_ref
    idx_sorted_bit(get_index_in_psi_det_sorted_bit(psi_non_ref(1,1,i), N_int)) = i
  enddo
    
  
  do i_state = 1, N_states
    delta_mrcepa0_ii(:,:) = 0d0
    delta_mrcepa0_ij(:,:,:) = 0d0

    !$OMP PARALLEL DO default(none) schedule(dynamic) shared(delta_mrcepa0_ij, delta_mrcepa0_ii)       &
    !$OMP private(i,II,J,k,degree,myActive,made_hole,made_particle,hjk,contrib)       &
    !$OMP shared(active_sorb, psi_non_ref, psi_non_ref_coef, psi_ref, psi_ref_coef, cepa0_shortcut, det_cepa0_active)     &
    !$OMP shared(N_det_ref, N_det_non_ref,N_int,det_cepa0_idx,lambda_mrcc,det_ref_active, delta_cas) &
    !$OMP shared(i_state)
    do blok=1,cepa0_shortcut(0)
    do i=cepa0_shortcut(blok), cepa0_shortcut(blok+1)-1
      do II=1,N_det_ref
        call get_excitation_degree(psi_ref(1,1,II),psi_non_ref(1,1,det_cepa0_idx(i)),degree,N_int)
        if (degree > 2 ) cycle
        
        do ni=1,N_int
          made_hole(ni,1) = iand(det_ref_active(ni,1,II), xor(det_cepa0_active(ni,1,i), det_ref_active(ni,1,II)))
          made_hole(ni,2) = iand(det_ref_active(ni,2,II), xor(det_cepa0_active(ni,2,i), det_ref_active(ni,2,II)))
          !made_particle = iand(det_cepa0_active(i),  xor(det_cepa0_active(i), det_ref_active(II)))
          made_particle(ni,1) = iand(det_cepa0_active(ni,1,i), xor(det_cepa0_active(ni,1,i), det_ref_active(ni,1,II)))
          made_particle(ni,2) = iand(det_cepa0_active(ni,2,i), xor(det_cepa0_active(ni,2,i), det_ref_active(ni,2,II)))
        end do
      
        
  kloop: do k=cepa0_shortcut(blok), i ! cepa0_shortcut(blok+1)-1
          if(lambda_mrcc(i_state, det_cepa0_idx(k)) == 0d0) cycle
          
          do ni=1,N_int
            if(iand(made_hole(ni,1), det_cepa0_active(ni,1,k)) /= 0) cycle kloop
            if(iand(made_particle(ni,1), det_cepa0_active(ni,1,k)) /= made_particle(ni,1)) cycle kloop
            if(iand(made_hole(ni,2), det_cepa0_active(ni,2,k)) /= 0) cycle kloop
            if(iand(made_particle(ni,2), det_cepa0_active(ni,2,k)) /= made_particle(ni,2)) cycle kloop
          end do
          do ni=1,N_int
            myActive(ni,1) = xor(det_cepa0_active(ni,1,k), made_hole(ni,1))
            myActive(ni,1) = xor(myActive(ni,1), made_particle(ni,1))
            myActive(ni,2) = xor(det_cepa0_active(ni,2,k), made_hole(ni,2))
            myActive(ni,2) = xor(myActive(ni,2), made_particle(ni,2))
          end do
          
  jloop:  do J=1,N_det_ref
            do ni=1,N_int !!! replace with sort+search
              if(det_ref_active(ni,1,J) /= myActive(ni,1)) cycle jloop
              if(det_ref_active(ni,2,J) /= myActive(ni,2)) cycle jloop
            end do
            call i_h_j(psi_non_ref(1,1,det_cepa0_idx(k)),psi_ref(1,1,J),N_int,HJk)
            contrib = delta_cas(II, J, i_state) * HJk * lambda_mrcc(i_state, det_cepa0_idx(k))
            !$OMP ATOMIC
            delta_mrcepa0_ij(J, det_cepa0_idx(i), i_state) += contrib
            
            if(dabs(psi_ref_coef(J,i_state)).ge.5.d-5) then
              !$OMP ATOMIC
              delta_mrcepa0_ii(J,i_state) -= contrib / psi_ref_coef(J, i_state) * psi_non_ref_coef(det_cepa0_idx(k),i_state)
            end if
            
            exit
          end do jloop
        end do kloop
      end do
    end do
    end do
    !$OMP END PARALLEL DO
  end do
  deallocate(idx_sorted_bit)
END_PROVIDER


 BEGIN_PROVIDER [ double precision, delta_sub_ij, (N_det_ref,N_det_non_ref,N_states) ]
&BEGIN_PROVIDER [ double precision, delta_sub_ii, (N_det_ref, N_states) ]
  use bitmasks
  implicit none
  
  integer                         :: i_state, i, i_I, J, k, degree, degree2, l, deg, ni
  integer                         :: p1,p2,h1,h2,s1,s2, p1_,p2_,h1_,h2_,s1_,s2_
  logical                         :: ok
  double precision                :: phase_Ji, phase_Ik, phase_Ii
  double precision                :: contrib, delta_IJk, HJk, HIk, HIl
  integer, dimension(0:2,2,2)     :: exc_Ik, exc_Ji, exc_Ii
  integer(bit_kind)               :: det_tmp(N_int, 2), det_tmp2(N_int, 2)
  integer, allocatable            :: idx_sorted_bit(:)
  integer, external               :: get_index_in_psi_det_sorted_bit
  
  integer :: II, blok
  
  provide delta_cas lambda_mrcc
  allocate(idx_sorted_bit(N_det))
  idx_sorted_bit(:) = -1
  do i=1,N_det_non_ref
    idx_sorted_bit(get_index_in_psi_det_sorted_bit(psi_non_ref(1,1,i), N_int)) = i
  enddo
    
  do i_state = 1, N_states
    delta_sub_ij(:,:,:) = 0d0
    delta_sub_ii(:,:) = 0d0
    
    provide mo_bielec_integrals_in_map
    
    
    !$OMP PARALLEL DO default(none) schedule(dynamic,10) shared(delta_sub_ij, delta_sub_ii)       &
    !$OMP private(i, J, k, degree, degree2, l, deg, ni)       &
    !$OMP private(p1,p2,h1,h2,s1,s2, p1_,p2_,h1_,h2_,s1_,s2_)     &
    !$OMP private(ok, phase_Ji, phase_Ik, phase_Ii, contrib, delta_IJk, HJk, HIk, HIl, exc_Ik, exc_Ji, exc_Ii) &
    !$OMP private(det_tmp, det_tmp2, II, blok)    &
    !$OMP shared(idx_sorted_bit, N_det_non_ref, N_det_ref, N_int, psi_non_ref, psi_non_ref_coef, psi_ref, psi_ref_coef)   &
    !$OMP shared(i_state,lambda_mrcc, hf_bitmask, active_sorb)
    do i=1,N_det_non_ref
      if(mod(i,1000) == 0) print *, i, "/", N_det_non_ref
      do J=1,N_det_ref
        call get_excitation(psi_ref(1,1,J),psi_non_ref(1,1,i),exc_Ji,degree,phase_Ji,N_int)
        if(degree == -1) cycle
        
        
        do II=1,N_det_ref
          call apply_excitation(psi_ref(1,1,II),exc_Ji,det_tmp,ok,N_int)
          !call get_excitation(psi_ref(1,1,II),psi_non_ref(1,1,i),exc_Ii,degree,phase_Ii,N_int)
          
          if(.not. ok) cycle
          l = get_index_in_psi_det_sorted_bit(det_tmp, N_int)
          if(l == 0) cycle
          l = idx_sorted_bit(l)
          
          call i_h_j(psi_ref(1,1,II), det_tmp, N_int, HIl)
          
          do k=1,N_det_non_ref
            if(lambda_mrcc(i_state, k) == 0d0) cycle
            call get_excitation(psi_ref(1,1,II),psi_non_ref(1,1,k),exc_Ik,degree2,phase_Ik,N_int)
            
            det_tmp(:,:) = 0_bit_kind
            det_tmp2(:,:) = 0_bit_kind
            
            ok = .true.
            do ni=1,N_int
              det_tmp(ni,1) = iand(xor(HF_bitmask(ni,1), psi_non_ref(ni,1,k)), not(active_sorb(ni,1)))
              det_tmp(ni,2) = iand(xor(HF_bitmask(ni,1), psi_non_ref(ni,1,i)), not(active_sorb(ni,1)))
              ok = ok .and. (popcnt(det_tmp(ni,1)) + popcnt(det_tmp(ni,2)) == popcnt(xor(det_tmp(ni,1), det_tmp(ni,2))))

              det_tmp(ni,1) = iand(xor(HF_bitmask(ni,2), psi_non_ref(ni,2,k)), not(active_sorb(ni,2)))
              det_tmp(ni,2) = iand(xor(HF_bitmask(ni,2), psi_non_ref(ni,2,i)), not(active_sorb(ni,2)))
              ok = ok .and. (popcnt(det_tmp(ni,1)) + popcnt(det_tmp(ni,2)) == popcnt(xor(det_tmp(ni,1), det_tmp(ni,2))))
            end do
            
            if(ok) cycle
            
            
            call i_h_j(psi_ref(1,1,J), psi_non_ref(1,1,k), N_int, HJk)
            call i_h_j(psi_ref(1,1,II), psi_non_ref(1,1,k), N_int, HIk)
            if(HJk == 0) cycle
            !assert HIk == 0
            delta_IJk = HJk * HIk * lambda_mrcc(i_state, k)
            call apply_excitation(psi_non_ref(1,1,i),exc_Ik,det_tmp,ok,N_int)
            if(ok) cycle
              contrib = delta_IJk * HIl * lambda_mrcc(i_state,l)   
            !$OMP ATOMIC
            delta_sub_ij(II, i, i_state) += contrib
            if(dabs(psi_ref_coef(II,i_state)).ge.5.d-5) then
              !$OMP ATOMIC
              delta_sub_ii(II,i_state) -= contrib / psi_ref_coef(II, i_state) * psi_non_ref_coef(l,i_state)
            endif
          end do
        end do
      end do
    end do
    !$OMP END PARALLEL DO
  end do
  deallocate(idx_sorted_bit)
END_PROVIDER


subroutine set_det_bit(det, p, s)
   implicit none
   integer(bit_kind),intent(inout) :: det(N_int, 2)
   integer, intent(in) :: p, s
   integer :: ni, pos
 
   ni = (p-1)/bit_kind_size + 1
   pos = mod(p-1, bit_kind_size)
   det(ni,s) = ibset(det(ni,s), pos)
end subroutine


 BEGIN_PROVIDER [ double precision, delta_ij_old, (N_det_ref,N_det_non_ref,N_states) ]
&BEGIN_PROVIDER [ double precision, delta_ii_old, (N_det_ref,N_states) ]
implicit none
  
  integer                         :: i_state, i, i_I, J, k, degree, degree2, m, l, deg, ni
  integer                         :: p1,p2,h1,h2,s1,s2, p1_,p2_,h1_,h2_,s1_,s2_
  logical                         :: ok
  double precision                :: phase_iI, phase_Ik, phase_Jl, phase_IJ, phase_al, diI, hIi, hJi, delta_JI, dkI(N_states), HkI, ci_inv(N_states), dia_hla(N_states)
  double precision                :: contrib
  integer, dimension(0:2,2,2)     :: exc_iI, exc_Ik, exc_IJ
  integer(bit_kind)               :: det_tmp(N_int, 2), det_tmp2(N_int, 2)
  integer, allocatable :: idx_sorted_bit(:)
  integer, external               :: get_index_in_psi_det_sorted_bit
  logical, external               :: is_in_wavefunction
  
    provide mo_bielec_integrals_in_map
  allocate(idx_sorted_bit(N_det))
  
  idx_sorted_bit(:) = -1
  do i=1,N_det_non_ref
    idx_sorted_bit(get_index_in_psi_det_sorted_bit(psi_non_ref(1,1,i), N_int)) = i 
  enddo
  
  
  
  
  do i_state = 1, N_states 

    delta_ii_old(:,:) = 0 
    delta_ij_old(:,:,:) = 0 
    
    !$OMP PARALLEL DO default(none) schedule(dynamic,10) shared(delta_ij_old, delta_ii_old)       &
    !$OMP private(i, J, k, degree, degree2, l, deg, ni)       &
    !$OMP private(ok,p1,p2,h1,h2,s1,s2, p1_,p2_,h1_,h2_,s1_,s2_)     &
    !$OMP private(phase_iI, phase_Ik, phase_Jl, phase_IJ, phase_al, diI, hIi, hJi, delta_JI, dkI, HkI, ci_inv, dia_hla) &
    !$OMP private(contrib, exc_iI, exc_Ik, exc_IJ, det_tmp, det_tmp2)    &
    !$OMP shared(idx_sorted_bit, N_det_non_ref, N_det_ref, N_int, psi_non_ref, psi_non_ref_coef, psi_ref, psi_ref_coef)   &
    !$OMP shared(i_state, lambda_mrcc, hf_bitmask, active_sorb)
    do i = 1 , N_det_non_ref
      if(mod(i,1000) == 0) print *, i, "/", N_det_non_ref
      if(lambda_mrcc(i_state, i) == 0d0) cycle
      do i_I = 1 , N_det_ref
        call get_excitation(psi_ref(1,1,i_I),psi_non_ref(1,1,i),exc_iI,degree2,phase_iI,N_int)
        if(degree2 == -1) cycle
        ci_inv(i_state) = 1.d0 / psi_ref_coef(i_I,i_state)

        call decode_exc(exc_iI,degree2,h1,p1,h2,p2,s1,s2)
      
        call i_h_j(psi_non_ref(1,1,i), psi_ref(1,1,i_I),N_int,hIi)
        diI = hIi * lambda_mrcc(i_state, i)
        do J = 1 , i_I ! N_det_ref !!! 
          call get_excitation(psi_ref(1,1,i_I),psi_ref(1,1,J),exc_IJ,degree,phase_IJ,N_int)
          call i_h_j(psi_non_ref(1,1,i), psi_ref(1,1,J),N_int,hJi)
          delta_JI = hJi * diI 
          do k = 1 , N_det_non_ref
            if(lambda_mrcc(i_state, k) == 0d0) cycle
            
            call get_excitation(psi_ref(1,1,i_I),psi_non_ref(1,1,k),exc_Ik,degree,phase_Ik,N_int)
            if(degree == -1) cycle

            call decode_exc(exc_Ik,degree,h1_,p1_,h2_,p2_,s1_,s2_)

            
            det_tmp(:,:) = 0_bit_kind
            det_tmp2(:,:) = 0_bit_kind
            
            ok = .true.
            do ni=1,N_int
              det_tmp(ni,1) = iand(xor(HF_bitmask(ni,1), psi_non_ref(ni,1,k)), not(active_sorb(ni,1)))
              det_tmp(ni,2) = iand(xor(HF_bitmask(ni,1), psi_non_ref(ni,1,i)), not(active_sorb(ni,1)))
              ok = ok .and. (popcnt(det_tmp(ni,1)) + popcnt(det_tmp(ni,2)) == popcnt(xor(det_tmp(ni,1), det_tmp(ni,2))))
              
              det_tmp(ni,1) = iand(xor(HF_bitmask(ni,2), psi_non_ref(ni,2,k)), not(active_sorb(ni,2)))
              det_tmp(ni,2) = iand(xor(HF_bitmask(ni,2), psi_non_ref(ni,2,i)), not(active_sorb(ni,2)))
              ok = ok .and. (popcnt(det_tmp(ni,1)) + popcnt(det_tmp(ni,2)) == popcnt(xor(det_tmp(ni,1), det_tmp(ni,2))))
            end do
            
            if(.not. ok) cycle
            
            
      
            call apply_excitation(psi_non_ref(1,1,i),exc_Ik,det_tmp,ok,N_int)
      
            call get_excitation(psi_non_ref(1,1,i), det_tmp, exc_Ik, degree, phase_al, N_int)

      
            if(.not. ok) cycle
            if(is_in_wavefunction(det_tmp, N_int)) cycle
      

            call apply_excitation(psi_ref(1,1,J),exc_Ik,det_tmp,ok,N_int)
            if(.not. ok) cycle
      
            call get_excitation(psi_ref(1,1,J), det_tmp, exc_Ik, degree, phase_Jl, N_int)
      
            l = get_index_in_psi_det_sorted_bit(det_tmp, N_int)
            if(l == 0) cycle
            l = idx_sorted_bit(get_index_in_psi_det_sorted_bit(det_tmp, N_int))
            if(l ==-1) cycle
            
            

            call i_h_j(psi_non_ref(1,1,k), psi_ref(1,1,i_I),N_int,HkI)
            dkI(i_state) = HkI * lambda_mrcc(i_state, k) * phase_Jl * phase_Ik
                    
            contrib = dkI(i_state) * delta_JI
            !$OMP ATOMIC  
            delta_ij_old(i_I,l,i_state) += contrib
            if(dabs(psi_ref_coef(i_I,i_state)).ge.5.d-5) then
              !$OMP ATOMIC  
              delta_ii_old(i_I,i_state) -= contrib * ci_inv(i_state) * psi_non_ref_coef(k,i_state)
            endif

          enddo
        enddo
      enddo
    enddo
    !$OMP END PARALLEL DO 
  end do
  deallocate(idx_sorted_bit)
END_PROVIDER







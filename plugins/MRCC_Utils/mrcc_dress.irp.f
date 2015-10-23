use omp_lib
use bitmasks

BEGIN_PROVIDER [ integer(omp_lock_kind), psi_ref_lock, (psi_det_size) ]
 implicit none
 BEGIN_DOC
 ! Locks on ref determinants to fill delta_ij
 END_DOC
 integer :: i
 do i=1,psi_det_size
   call omp_init_lock( psi_ref_lock(i) )
 enddo

END_PROVIDER


subroutine create_minilist(key_mask, fullList, miniList, idx_miniList, N_fullList, N_miniList, Nint)
  use bitmasks
  implicit none
  
  integer(bit_kind), intent(in)            :: fullList(Nint, 2, N_fullList)
  integer, intent(in)                      :: N_fullList
  integer(bit_kind),intent(out)            :: miniList(Nint, 2, N_fullList)
  integer,intent(out)                      :: idx_miniList(N_fullList), N_miniList
  integer, intent(in)                      :: Nint
  integer(bit_kind)                        :: key_mask(Nint, 2)
  integer                                  :: ni, i, n_a, n_b, e_a, e_b
  
  
  n_a = 0
  n_b = 0
  do ni=1,nint
    n_a = n_a + popcnt(key_mask(ni,1))
    n_b = n_b + popcnt(key_mask(ni,2))
  end do
  
  if(n_a == 0) then
    N_miniList = N_fullList
    miniList(:,:,:) = fullList(:,:,:)
    do i=1,N_fullList
      idx_miniList(i) = i
    end do
    return
  end if
  
  N_miniList = 0
  
  do i=1,N_fullList
    e_a = n_a
    e_b = n_b
    do ni=1,nint
      e_a -= popcnt(iand(fullList(ni, 1, i), key_mask(ni, 1)))
      e_b -= popcnt(iand(fullList(ni, 2, i), key_mask(ni, 2)))
    end do
    
    if(e_a + e_b <= 2) then
      N_miniList = N_miniList + 1
      miniList(:,:,N_miniList) = fullList(:,:,i)
      idx_miniList(N_miniList) = i
    end if
  end do
end subroutine


subroutine mrcc_dress(delta_ij_, delta_ii_, Ndet_ref, Ndet_non_ref,i_generator,n_selected,det_buffer,Nint,iproc,key_mask)
 use bitmasks
 implicit none

  integer, intent(in)            :: i_generator,n_selected, Nint, iproc
  integer, intent(in)            :: Ndet_ref, Ndet_non_ref
  double precision, intent(inout) :: delta_ij_(Ndet_ref,Ndet_non_ref,*)
  double precision, intent(inout) :: delta_ii_(Ndet_ref,*)

  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,n_selected)
  integer                        :: i,j,k,l
  integer                        :: degree_alpha(psi_det_size)
  integer                        :: idx_alpha(0:psi_det_size)
  logical                        :: good

  integer(bit_kind)              :: tq(Nint,2,n_selected)
  integer                        :: N_tq, c_ref ,degree
  integer                        :: connected_to_ref

  double precision               :: hIk, hla, hIl, dIk(N_states), dka(N_states), dIa(N_states)
  double precision, allocatable  :: dIa_hla(:,:)
  double precision               :: haj, phase, phase2
  double precision               :: f(N_states), ci_inv(N_states)
  integer                        :: exc(0:2,2,2)
  integer                        :: h1,h2,p1,p2,s1,s2
  integer(bit_kind)              :: tmp_det(Nint,2)
  integer                        :: iint, ipos
  integer                        :: i_state, k_sd, l_sd, i_I, i_alpha
  
  integer(bit_kind)              :: miniList(Nint, 2, N_det_non_ref), key_mask(Nint, 2)
  integer                        :: idx_miniList(N_det_non_ref), N_miniList
  
  
  
!   N_miniList = 0
!   do i=i_generator-1,1,-1
!     k = popcnt(key_mask(1,1)) + popcnt(key_mask(1,2)) - popcnt(iand(key_mask(1,1), psi_det_generators(1,1,i))) - popcnt(iand(key_mask(1,2), psi_det_generators(1,2,i)))
!     if(k == 0) then
!       return
!     end if
!     if(k <= 2) then
!       N_minilist += 1
!       miniList(:,:,N_minilist) = psi_det_generators(:,:,i)
!     end if
!   end do
  
  call find_triples_and_quadruples(i_generator,n_selected,det_buffer,Nint,tq,N_tq,miniList,N_minilist)

  allocate (dIa_hla(N_states,Ndet_non_ref))

  ! |I>

  ! |alpha>
  
   if(N_tq > 0) then
     call create_minilist(key_mask, psi_non_ref, miniList, idx_miniList, N_det_non_ref, N_minilist, Nint)
   end if
  
  
  do i_alpha=1,N_tq
!    call get_excitation_degree_vector(psi_non_ref,tq(1,1,i_alpha),degree_alpha,Nint,N_det_non_ref,idx_alpha)
    call get_excitation_degree_vector(miniList,tq(1,1,i_alpha),degree_alpha,Nint,N_minilist,idx_alpha)
    
    do j=1,idx_alpha(0)
      idx_alpha(j) = idx_miniList(idx_alpha(j))
    end do
     
    ! |I>
    do i_I=1,N_det_ref
       ! Find triples and quadruple grand parents
       call get_excitation_degree(tq(1,1,i_alpha),psi_ref(1,1,i_I),degree,Nint)
       if (degree > 4) then
         cycle
       endif

       do i_state=1,N_states
         dIa(i_state) = 0.d0
       enddo

       ! <I|  <>  |alpha>
       do k_sd=1,idx_alpha(0)
         call get_excitation_degree(psi_ref(1,1,i_I),psi_non_ref(1,1,idx_alpha(k_sd)),degree,Nint)
         if (degree > 2) then
           cycle
         endif
         ! <I| /k\ |alpha>
         ! <I|H|k>
         call i_h_j(psi_ref(1,1,i_I),psi_non_ref(1,1,idx_alpha(k_sd)),Nint,hIk)
         do i_state=1,N_states
           dIk(i_state) = hIk * lambda_mrcc(i_state,idx_alpha(k_sd))
         enddo
         ! |l> = Exc(k -> alpha) |I>
         call get_excitation(psi_non_ref(1,1,idx_alpha(k_sd)),tq(1,1,i_alpha),exc,degree,phase,Nint)
         call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
         do k=1,N_int
           tmp_det(k,1) = psi_ref(k,1,i_I)
           tmp_det(k,2) = psi_ref(k,2,i_I)
         enddo
         ! Hole (see list_to_bitstring)
         iint = ishft(h1-1,-bit_kind_shift) + 1
         ipos = h1-ishft((iint-1),bit_kind_shift)-1
         tmp_det(iint,s1) = ibclr(tmp_det(iint,s1),ipos)

         ! Particle
         iint = ishft(p1-1,-bit_kind_shift) + 1
         ipos = p1-ishft((iint-1),bit_kind_shift)-1
         tmp_det(iint,s1) = ibset(tmp_det(iint,s1),ipos)
         if (degree_alpha(k_sd) == 2) then
           ! Hole (see list_to_bitstring)
           iint = ishft(h2-1,-bit_kind_shift) + 1
           ipos = h2-ishft((iint-1),bit_kind_shift)-1
           tmp_det(iint,s2) = ibclr(tmp_det(iint,s2),ipos)

           ! Particle
           iint = ishft(p2-1,-bit_kind_shift) + 1
           ipos = p2-ishft((iint-1),bit_kind_shift)-1
           tmp_det(iint,s2) = ibset(tmp_det(iint,s2),ipos)
         endif
         
         ! <I| \l/ |alpha>
         do i_state=1,N_states
           dka(i_state) = 0.d0
         enddo
         do l_sd=k_sd+1,idx_alpha(0)
           call get_excitation_degree(tmp_det,psi_non_ref(1,1,idx_alpha(l_sd)),degree,Nint)
           if (degree == 0) then
             call get_excitation(psi_ref(1,1,i_I),psi_non_ref(1,1,idx_alpha(l_sd)),exc,degree,phase2,Nint)
             call i_h_j(psi_ref(1,1,i_I),psi_non_ref(1,1,idx_alpha(l_sd)),Nint,hIl)
             do i_state=1,N_states
               dka(i_state) = hIl * lambda_mrcc(i_state,idx_alpha(l_sd)) * phase * phase2
             enddo
             exit
           endif
         enddo
         do i_state=1,N_states
           dIa(i_state) = dIa(i_state) + dIk(i_state) * dka(i_state) 
         enddo
       enddo

       do i_state=1,N_states
         ci_inv(i_state) = 1.d0/psi_ref_coef(i_I,i_state)
       enddo
       do l_sd=1,idx_alpha(0)
         k_sd = idx_alpha(l_sd)
         call i_h_j(tq(1,1,i_alpha),psi_non_ref(1,1,idx_alpha(l_sd)),Nint,hla)
         do i_state=1,N_states
           dIa_hla(i_state,k_sd) = dIa(i_state) * hla
         enddo
       enddo
       call omp_set_lock( psi_ref_lock(i_I) )
       do l_sd=1,idx_alpha(0)
         k_sd = idx_alpha(l_sd)
         do i_state=1,N_states
           delta_ij_(i_I,k_sd,i_state) += dIa_hla(i_state,k_sd)
           if(dabs(psi_ref_coef(i_I,i_state)).ge.5.d-5)then
            delta_ii_(i_I,i_state) -= dIa_hla(i_state,k_sd) * ci_inv(i_state) * psi_non_ref_coef(k_sd,i_state)
           else
            delta_ii_(i_I,i_state)  = 0.d0
           endif
         enddo
       enddo
       call omp_unset_lock( psi_ref_lock(i_I) )
    enddo
  enddo
  deallocate (dIa_hla)
end







! subroutine mrcc_dress_simple(delta_ij_non_ref_,Ndet_non_ref,i_generator,n_selected,det_buffer,Nint,iproc)
!  use bitmasks
!  implicit none
! 
!   integer, intent(in)            :: i_generator,n_selected, Nint, iproc
!   integer, intent(in) :: Ndet_non_ref
!   double precision, intent(inout) :: delta_ij_non_ref_(Ndet_non_ref,Ndet_non_ref,*)
! 
!   integer(bit_kind), intent(in)  :: det_buffer(Nint,2,n_selected)
!   integer                        :: i,j,k,m
!   integer                        :: new_size
!   integer                        :: degree(psi_det_size)
!   integer                        :: idx(0:psi_det_size)
!   logical                        :: good
! 
!   integer(bit_kind)              :: tq(Nint,2,n_selected)
!   integer                        :: N_tq, c_ref
!   integer                        :: connected_to_ref
!   
! 
!   call find_triples_and_quadruples(i_generator,n_selected,det_buffer,Nint,tq,N_tq)
! 
!   ! Compute <k|H|a><a|H|j> / (E0 - Haa)
!   double precision :: hka, haa
!   double precision :: haj
!   double precision :: f(N_states)
! 
!   do i=1,N_tq
!     call get_excitation_degree_vector(psi_non_ref,tq(1,1,i),degree,Nint,Ndet_non_ref,idx)
!     call i_h_j(tq(1,1,i),tq(1,1,i),Nint,haa)
!     do m=1,N_states
!       f(m) = 1.d0/(ci_electronic_energy(m)-haa)
!     enddo
!     do k=1,idx(0)
!       call i_h_j(tq(1,1,i),psi_non_ref(1,1,idx(k)),Nint,hka)
!       do j=k,idx(0)
!         call i_h_j(tq(1,1,i),psi_non_ref(1,1,idx(j)),Nint,haj)
!         do m=1,N_states
!           delta_ij_non_ref_(idx(k), idx(j),m) += haj*hka* f(m)
!           delta_ij_non_ref_(idx(j), idx(k),m) += haj*hka* f(m)
!         enddo
!       enddo 
!     enddo
!   enddo
! end


 BEGIN_PROVIDER [ integer(bit_kind), gen_det_sorted,  (N_int,2,N_det_generators,2) ]
&BEGIN_PROVIDER [ integer, gen_det_shortcut, (0:N_det_generators,2) ]
&BEGIN_PROVIDER [ integer, gen_det_version, (N_int, N_det_generators,2) ]
&BEGIN_PROVIDER [ integer, gen_det_idx, (N_det_generators,2) ]
  
  gen_det_sorted(:,:,:,1) = psi_det_generators(:,:,:N_det_generators)
  gen_det_sorted(:,:,:,2) = psi_det_generators(:,:,:N_det_generators)
  call sort_dets_ab_v(gen_det_sorted(:,:,:,1), gen_det_idx(:,1), gen_det_shortcut(0:,1), gen_det_version(:,:,1), N_det_generators, N_int)
  call sort_dets_ba_v(gen_det_sorted(:,:,:,2), gen_det_idx(:,2), gen_det_shortcut(0:,2), gen_det_version(:,:,2), N_det_generators, N_int)
  print *, " *********************** ", gen_det_shortcut(0,:)
END_PROVIDER


subroutine find_triples_and_quadruples(i_generator,n_selected,det_buffer,Nint,tq,N_tq,miniList,N_miniList)
 use bitmasks
 implicit none

  integer, intent(in)            :: i_generator,n_selected, Nint

  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,n_selected)
  integer                        :: i,j,k,m
  logical                        :: is_in_wavefunction
  integer                        :: degree(psi_det_size)
  integer                        :: idx(0:psi_det_size)
  logical                        :: good

  integer(bit_kind), intent(out) :: tq(Nint,2,n_selected)
  integer, intent(out)           :: N_tq
  integer                        :: c_ref
  integer                        :: connected_to_ref
  
  
  integer :: na, nb, nt,mex, lex, sh,ni
  
  
  integer(bit_kind),intent(in)              :: miniList(Nint,2,N_det_generators)
  integer,intent(in)            :: N_miniList
  
  
  
  
  N_tq = 0
  do i=1,N_selected
!     c_ref = 0
!     do j=1,N_miniList
!       na = popcnt(xor(miniList(1,1,j), det_buffer(1,1,i)))
!       nb = popcnt(xor(miniList(1,2,j), det_buffer(1,2,i)))
!       if(na+nb <= 4) then
!         c_ref = 1
!         exit
!       end if
!     end do
 
  
!   
!     if(Nint /= 1) then
!       c_ref = 0
!       na = 0
!       nb = 0
!       do ni=1,Nint
!         na += popcnt(xor(det_buffer(ni,1,i), psi_ref(ni,1,1)))
!         nb += popcnt(xor(det_buffer(ni,2,i), psi_ref(ni,2,1)))
!       end do
!       if(na > nb) then
!         mex = 1
!         lex = 2
!       else
!         mex = 2
!         lex = 1
!       end if
!       
!       
!       sh_loop : do sh=1,gen_det_shortcut(0,lex)
!         do ni=1,Nint
!           if(det_buffer(ni,lex,i) /= gen_det_sorted(ni,lex,gen_det_shortcut(sh, lex),lex)) then
!             cycle sh_loop
!           end if
!         end do
!         do j=gen_det_shortcut(sh,lex),gen_det_shortcut(sh+1,lex)-1
!           if(gen_det_idx(j,lex) >= i_generator) then
!             cycle
!           end if
!           nt = 0
!           do ni=1,nint
!             nt += popcnt(xor(det_buffer(ni,mex,i), gen_det_sorted(ni,mex,j,lex)))
!           end do
!           if(nt <= 4) then
!   !           if(gen_det_idx(j,lex) < i_generator) then
!               c_ref = 1!gen_det_idx(j,lex)
!               exit sh_loop
!   !           end if
!           end if
!         end do
!         exit sh_loop
!       end do sh_loop
!       
!       if(c_ref == 0) then
!         sh_loop2 : do sh = 1,gen_det_shortcut(0,mex)
!           na = 0
!           do ni=1,Nint
!             na += popcnt(xor(det_buffer(ni,mex,i), gen_det_sorted(ni,mex,gen_det_shortcut(sh, mex),mex)))
!           end do
!           if(na > 2) then
!             cycle
!           end if

!           do j=gen_det_shortcut(sh, mex), gen_det_shortcut(sh+1,mex)-1
!             if(gen_det_idx(j,mex) >= i_generator) then
!               cycle
!             end if
!             nt = na
!             do ni=1,Nint
!               nt += popcnt(xor(det_buffer(ni,lex,i), gen_det_sorted(ni,lex,j,mex)))
!             end do
!             if(nt <= 4) then
!   !             if(gen_det_idx(j,mex) < i_generator) then
!                 c_ref = 2!gen_det_idx(j,mex)
!                 exit sh_loop2
!   !             end if
!             end if
!           end do
!         end do sh_loop2
!       end if
!     else
!       c_ref = 0
!       na = popcnt(xor(det_buffer(1,1,i), psi_ref(1,1,1)))
!       nb = popcnt(xor(det_buffer(1,2,i), psi_ref(1,2,1)))
!       if(na > nb) then
!         mex = 1
!         lex = 2
!       else
!         mex = 2
!         lex = 1
!       end if
!       
!       
!       sh_loop3 : do sh=1,gen_det_shortcut(0,lex)
!         if(det_buffer(1,lex,i) /= gen_det_version(1,sh,lex)) then!gen_det_sorted(1,lex,gen_det_shortcut(sh, lex),lex)) then
!           cycle sh_loop3
!         end if
!         do j=gen_det_shortcut(sh,lex),gen_det_shortcut(sh+1,lex)-1
!           if(gen_det_idx(j,lex) >= i_generator) then
!             cycle
!           end if
!           nt =  popcnt(xor(det_buffer(1,mex,i), gen_det_sorted(1,mex,j,lex)))
!           if(nt <= 4) then
!   !           if(gen_det_idx(j,lex) < i_generator) then
!               c_ref = 1!gen_det_idx(j,lex)
!               exit sh_loop3
!   !           end if
!           end if
!         end do
!         exit sh_loop3
!       end do sh_loop3
!       
!       if(c_ref == 0) then
!         sh_loop4 : do sh = 1,gen_det_shortcut(0,mex)
!           na = popcnt(xor(det_buffer(1,mex,i),gen_det_version(1,sh,mex)))! gen_det_sorted(1,mex,gen_det_shortcut(sh, mex),mex)))
!           if(na > 2) then
!             cycle sh_loop4
!           end if
!           do j=gen_det_shortcut(sh, mex), gen_det_shortcut(sh+1,mex)-1
!             if(gen_det_idx(j,mex) >= i_generator) then
!               cycle
!             end if
!             nt = na
!             nt += popcnt(xor(det_buffer(1,lex,i), gen_det_sorted(1,lex,j,mex)))
!             if(nt <= 4) then
!   !             if(gen_det_idx(j,mex) < i_generator) then
!                 c_ref = 2!gen_det_idx(j,mex)
!                 exit sh_loop4
!   !             end if
!             end if
!           end do
!         end do sh_loop4
!       end if
!     end if


    c_ref = connected_to_ref(det_buffer(1,1,i),psi_det_generators,Nint, &
       i_generator,N_det_generators)


    if (c_ref /= 0) then
      cycle
    endif

    ! Select determinants that are triple or quadruple excitations
    ! from the ref
    good = .True.
    call get_excitation_degree_vector(psi_ref,det_buffer(1,1,i),degree,Nint,N_det_ref,idx) 
    !good=(idx(0) == 0) tant que degree > 2 pas retourné par get_excitation_degree_vector
    do k=1,idx(0)
      if (degree(k) < 3) then
        good = .False.
        exit
      endif
    enddo
    if (good) then
      if (.not. is_in_wavefunction(det_buffer(1,1,i),Nint,N_det)) then
        N_tq += 1
        do k=1,N_int
          tq(k,1,N_tq) = det_buffer(k,1,i)
          tq(k,2,N_tq) = det_buffer(k,2,i)
        enddo
      endif
    endif
  enddo

end








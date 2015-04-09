subroutine mrcc_dress(delta_ij_,Ndet,i_generator,n_selected,det_buffer,Nint,iproc)
 use bitmasks
 implicit none

  integer, intent(in)            :: i_generator,n_selected, Nint, iproc
  integer, intent(in) :: Ndet
  double precision, intent(inout) :: delta_ij_(Ndet,Ndet,*)

  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,n_selected)
  integer                        :: i,j,k,l,m
  logical                        :: is_in_wavefunction
  integer                        :: degree_alpha(psi_det_size)
  integer                        :: degree_I(psi_det_size)
  integer                        :: idx_I(0:psi_det_size)
  integer                        :: idx_alpha(0:psi_det_size)
  logical                        :: good

  integer(bit_kind)              :: tq(Nint,2,n_selected)
  integer                        :: N_tq, c_ref ,degree
  integer                        :: connected_to_ref

  call find_triples_and_quadruples(i_generator,n_selected,det_buffer,Nint,tq,N_tq)

  double precision :: hIk, hIl, hla, dIk(N_states), dka(N_states), dIa(N_states)
  double precision :: haj, phase, phase2
  double precision :: f(N_states), ci_inv(N_states)
  integer          :: exc(0:2,2,2)
  integer          :: h1,h2,p1,p2,s1,s2
  integer(bit_kind):: tmp_det(Nint,2)
  integer          :: iint, ipos
  integer          :: i_state, k_sd, l_sd, i_I, i_alpha



  ! |I>

  ! |alpha>
  do i_alpha=1,N_tq
    call get_excitation_degree_vector(psi_non_cas,tq(1,1,i_alpha),degree_alpha,Nint,N_det_non_cas,idx_alpha)

    ! |I>
    do i_I=1,N_det_cas
       ! Find triples and quadruple grand parents
       call get_excitation_degree(tq(1,1,i_alpha),psi_cas(1,1,i_I),degree,Nint)
       if (degree > 4) then
         cycle
       endif

       do i_state=1,N_states
         dIa(i_state) = 0.d0
       enddo

       ! <I|  <>  |alpha>
       do k_sd=1,idx_alpha(0)
         call get_excitation_degree(psi_cas(1,1,i_I),psi_non_cas(1,1,idx_alpha(k_sd)),degree,Nint)
         if (degree > 2) then
           cycle
         endif
         ! <I| /k\ |alpha>
         ! <I|H|k>
         call i_h_j(psi_cas(1,1,i_I),psi_non_cas(1,1,idx_alpha(k_sd)),Nint,hIk)
         do i_state=1,N_states
           dIk(i_state) = hIk * lambda_mrcc(idx_alpha(k_sd),i_state)
         enddo
         ! |l> = Exc(k -> alpha) |I>
         call get_excitation(psi_non_cas(1,1,idx_alpha(k_sd)),tq(1,1,i_alpha),exc,degree,phase,Nint)
         call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
         tmp_det(1:Nint,1:2) = psi_cas(1,1,i_I)
         ! Hole (see list_to_bitstring)
         iint = ishft(h1-1,-bit_kind_shift) + 1
         ipos = h1-ishft((iint-1),bit_kind_shift)-1
         tmp_det(iint,s1) = ibclr(tmp_det(iint,s1),ipos)

         ! Particle
         iint = ishft(p1-1,-bit_kind_shift) + 1
         ipos = p1-ishft((iint-1),bit_kind_shift)-1
         tmp_det(iint,s1) = ibset(tmp_det(iint,s1),ipos)
         if (degree == 2) then
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
           call get_excitation_degree(tmp_det,psi_non_cas(1,1,idx_alpha(l_sd)),degree,Nint)
           if (degree == 0) then
             call get_excitation(psi_cas(1,1,i_I),psi_non_cas(1,1,idx_alpha(l_sd)),exc,degree,phase2,Nint)
             call i_h_j(psi_cas(1,1,i_I),psi_non_cas(1,1,idx_alpha(l_sd)),Nint,hIl)
             do i_state=1,N_states
               dka(i_state) = hIl * lambda_mrcc(idx_alpha(l_sd),i_state) * phase * phase2
             enddo
             exit
           endif
         enddo
         do i_state=1,N_states
           dIa(i_state) = dIa(i_state) + dIk(i_state) * dka(i_state)
         enddo
       enddo

       do i_state=1,N_states
         ci_inv(i_state) = 1.d0/psi_cas_coefs(i_I,i_state)
       enddo

       do l_sd=1,idx_alpha(0)
         k_sd = idx_alpha(l_sd)
         call i_h_j(tq(1,1,i_alpha),psi_non_cas(1,1,idx_alpha(l_sd)),Nint,hla)
         do m=1,N_states
           delta_ij_(idx_non_cas(k_sd),idx_cas(i_I),m) += dIa(m) * hla 
           delta_ij_(idx_cas(i_I),idx_non_cas(k_sd),m) += dIa(m) * hla
           delta_ij_(idx_cas(i_I),idx_cas(i_I),m) -= dIa(m) * hla * ci_inv(m) * psi_non_cas_coefs(k_sd,m)
         enddo
       enddo
    enddo
  enddo
end







subroutine mrcc_dress_simple(delta_ij_non_cas_,Ndet_non_cas,i_generator,n_selected,det_buffer,Nint,iproc)
 use bitmasks
 implicit none

  integer, intent(in)            :: i_generator,n_selected, Nint, iproc
  integer, intent(in) :: Ndet_non_cas
  double precision, intent(inout) :: delta_ij_non_cas_(Ndet_non_cas,Ndet_non_cas,*)

  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,n_selected)
  integer                        :: i,j,k,m
  integer                        :: new_size
  logical                        :: is_in_wavefunction
  integer                        :: degree(psi_det_size)
  integer                        :: idx(0:psi_det_size)
  logical                        :: good

  integer(bit_kind)              :: tq(Nint,2,n_selected)
  integer                        :: N_tq, c_ref
  integer                        :: connected_to_ref

  call find_triples_and_quadruples(i_generator,n_selected,det_buffer,Nint,tq,N_tq)

  ! Compute <k|H|a><a|H|j> / (E0 - Haa)
  double precision :: hka, haa
  double precision :: haj
  double precision :: f(N_states)

  do i=1,N_tq
    call get_excitation_degree_vector(psi_non_cas,tq(1,1,i),degree,Nint,Ndet_non_cas,idx)
    call i_h_j(tq(1,1,i),tq(1,1,i),Nint,haa)
    do m=1,N_states
      f(m) = 1.d0/(ci_electronic_energy(m)-haa)
    enddo
    do k=1,idx(0)
      call i_h_j(tq(1,1,i),psi_non_cas(1,1,idx(k)),Nint,hka)
      do j=k,idx(0)
        call i_h_j(tq(1,1,i),psi_non_cas(1,1,idx(j)),Nint,haj)
        do m=1,N_states
          delta_ij_non_cas_(idx(k), idx(j),m) += haj*hka* f(m)
          delta_ij_non_cas_(idx(j), idx(k),m) += haj*hka* f(m)
        enddo
      enddo 
    enddo
  enddo
end


subroutine find_triples_and_quadruples(i_generator,n_selected,det_buffer,Nint,tq,N_tq)
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

  N_tq = 0
  do i=1,N_selected
    c_ref = connected_to_ref(det_buffer(1,1,i),psi_det_generators,Nint, &
       i_generator,N_det_generators)

    if (c_ref /= 0) then
      cycle
    endif

    ! Select determinants that are triple or quadruple excitations
    ! from the CAS
    good = .True.
    call get_excitation_degree_vector(psi_cas,det_buffer(1,1,i),degree,Nint,N_det_cas,idx)
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








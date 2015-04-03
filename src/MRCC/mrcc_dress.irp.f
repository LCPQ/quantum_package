subroutine mrcc_dress_simple(delta_ij_sd_,Ndet_sd,i_generator,n_selected,det_buffer,Nint,iproc)
 use bitmasks
 implicit none

  integer, intent(in)            :: i_generator,n_selected, Nint, iproc
  integer, intent(in) :: Ndet_sd
  double precision, intent(inout) :: delta_ij_sd_(Ndet_sd,Ndet_sd,*)

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

  ! Compute <k|H|a><a|H|j> / (E0 - Haa)
  double precision :: hka, haa
  double precision :: haj
  double precision :: f(N_states)

  do i=1,N_tq
    call get_excitation_degree_vector(psi_sd,tq(1,1,i),degree,Nint,Ndet_sd,idx)
    call i_h_j(tq(1,1,i),tq(1,1,i),Nint,haa)
    do m=1,N_states
      f(m) = 1.d0/(ci_electronic_energy(m)-haa)
    enddo
    do k=1,idx(0)
      call i_h_j(tq(1,1,i),psi_sd(1,1,idx(k)),Nint,hka)
      do j=k,idx(0)
        call i_h_j(tq(1,1,i),psi_sd(1,1,idx(j)),Nint,haj)
        do m=1,N_states
          delta_ij_sd_(idx(k), idx(j),m) += haj*hka* f(m)
          delta_ij_sd_(idx(j), idx(k),m) += haj*hka* f(m)
        enddo
      enddo 
    enddo
  enddo
end








subroutine mrcc_dress(delta_ij_sd_,Ndet_sd,i_generator,n_selected,det_buffer,Nint,iproc)
 use bitmasks
 implicit none

  integer, intent(in)            :: i_generator,n_selected, Nint, iproc
  integer, intent(in) :: Ndet_sd
  double precision, intent(inout) :: delta_ij_sd_(Ndet_sd,Ndet_sd,*)

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

  ! Compute <k|H|a><a|H|j> / (E0 - Haa)
  double precision :: hka, haa
  double precision :: haj
  double precision :: f(N_states)

  do i=1,N_tq
    call get_excitation_degree_vector(psi_sd,tq(1,1,i),degree,Nint,Ndet_sd,idx)
    call i_h_j(tq(1,1,i),tq(1,1,i),Nint,haa)
    do m=1,N_states
      f(m) = 1.d0/(ci_electronic_energy(m)-haa)
    enddo
    do k=1,idx(0)
      call i_h_j(tq(1,1,i),psi_sd(1,1,idx(k)),Nint,hka)
      do j=k,idx(0)
        call i_h_j(tq(1,1,i),psi_sd(1,1,idx(j)),Nint,haj)
        do m=1,N_states
          delta_ij_sd_(idx(k), idx(j),m) += haj*hka* f(m)
          delta_ij_sd_(idx(j), idx(k),m) += haj*hka* f(m)
        enddo
      enddo 
    enddo
  enddo
end


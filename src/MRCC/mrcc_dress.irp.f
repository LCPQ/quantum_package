subroutine mrcc_dress(i_generator,n_selected,det_buffer,Nint,iproc)
 use bitmasks
 implicit none

  integer, intent(in)            :: i_generator,n_selected, Nint, iproc
  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,n_selected)
  integer                        :: i,j,k
  integer                        :: new_size
  logical                        :: is_in_wavefunction
  double precision               :: degree(N_det_cas)
  integer                        :: idx(0:N_det_cas)
  logical                        :: good

  integer(bit_kind)              :: tq(Nint,2,n_selected)
  integer                        :: N_tq, c_ref
  integer                        :: connected_to_ref

  N_tq = 0
  do i=1,N_selected

    c_ref = connected_to_ref(det_buffer(1,1,i),psi_generators,Nint, &
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

  print *, N_tq
  do i=1,N_tq
    call debug_det(det_buffer(1,1,i),Nint)
  enddo
end


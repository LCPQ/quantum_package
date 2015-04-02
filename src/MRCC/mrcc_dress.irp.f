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

BEGIN_PROVIDER [ double precision, delta_ij_sd, (N_det_sd, N_det_sd,N_states) ]
 implicit none
 BEGIN_DOC
 ! Dressing matrix in SD basis
 END_DOC
 delta_ij_sd = 0.d0
 call H_apply_mrcc(delta_ij_sd,N_det_sd)

END_PROVIDER

BEGIN_PROVIDER [ double precision, delta_ij, (N_det,N_det,N_states) ]
 implicit none
 BEGIN_DOC
 ! Dressing matrix in N_det basis
 END_DOC
 integer :: i,j,m
 delta_ij = 0.d0
 do m=1,N_states
  do j=1,N_det_sd
   do i=1,N_det_sd
     delta_ij(idx_sd(i),idx_sd(j),m) = delta_ij_sd(i,j,m) 
   enddo
  enddo
 enddo
END_PROVIDER

BEGIN_PROVIDER [ double precision, h_matrix_dressed, (N_det,N_det) ]
 implicit none
 BEGIN_DOC
 ! Dressed H with Delta_ij
 END_DOC
 integer                        :: i, j
 do j=1,N_det
   do i=1,N_det
     h_matrix_dressed(i,j) = h_matrix_all_dets(i,j) + delta_ij(i,j,1)
     if (i==j) then
       print *,  i, delta_ij(i,j,1), h_matrix_all_dets(i,j) 
     endif
   enddo
 enddo

END_PROVIDER


 BEGIN_PROVIDER [ double precision, CI_electronic_energy_dressed, (N_states_diag) ]
&BEGIN_PROVIDER [ double precision, CI_eigenvectors_dressed, (N_det,N_states_diag) ]
&BEGIN_PROVIDER [ double precision, CI_eigenvectors_s2_dressed, (N_states_diag) ]
  implicit none
  BEGIN_DOC
  ! Eigenvectors/values of the CI matrix
  END_DOC
  integer                        :: i,j
  
  do j=1,N_states_diag
    do i=1,N_det
      CI_eigenvectors_dressed(i,j) = psi_coef(i,j)
    enddo
  enddo
  
  if (diag_algorithm == "Davidson") then
    
    stop 'use Lapack'
!    call davidson_diag(psi_det,CI_eigenvectors_dressed,CI_electronic_energy_dressed, &
!        size(CI_eigenvectors_dressed,1),N_det,N_states_diag,N_int,output_Dets)
    
  else if (diag_algorithm == "Lapack") then
    
    double precision, allocatable  :: eigenvectors(:,:), eigenvalues(:)
    allocate (eigenvectors(size(H_matrix_dressed,1),N_det))
    allocate (eigenvalues(N_det))
    call lapack_diag(eigenvalues,eigenvectors,                       &
        H_matrix_dressed,size(H_matrix_dressed,1),N_det)
    CI_electronic_energy_dressed(:) = 0.d0
    do i=1,N_det
       CI_eigenvectors_dressed(i,1) = eigenvectors(i,1)
    enddo
    integer :: i_state
    double precision :: s2
    i_state = 0
    do j=1,N_det
      call get_s2_u0(psi_det,eigenvectors(1,j),N_det,N_det,s2)
      if(dabs(s2-expected_s2).le.0.3d0)then
       i_state += 1
       do i=1,N_det
         CI_eigenvectors_dressed(i,i_state) = eigenvectors(i,j)
       enddo
       CI_electronic_energy_dressed(i_state) = eigenvalues(j)
       CI_eigenvectors_s2_dressed(i_state) = s2
      endif
      if (i_state.ge.N_states_diag) then
        exit
      endif
    enddo
    deallocate(eigenvectors,eigenvalues)
  endif
  
END_PROVIDER

BEGIN_PROVIDER [ double precision, CI_energy_dressed, (N_states_diag) ]
  implicit none
  BEGIN_DOC
  ! N_states lowest eigenvalues of the dressed CI matrix
  END_DOC
  
  integer                        :: j
  character*(8)                  :: st
  call write_time(output_Dets)
  do j=1,N_states_diag
    CI_energy_dressed(j) = CI_electronic_energy_dressed(j) + nuclear_repulsion
    write(st,'(I4)') j
    call write_double(output_Dets,CI_energy(j),'Energy of state '//trim(st))
    call write_double(output_Dets,CI_eigenvectors_s2(j),'S^2 of state '//trim(st))
  enddo

END_PROVIDER


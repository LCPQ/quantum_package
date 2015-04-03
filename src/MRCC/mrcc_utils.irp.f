 use bitmasks

BEGIN_PROVIDER [ integer(bit_kind), cas_bitmask, (N_int,2,N_cas_bitmask) ]
 implicit none
 BEGIN_DOC
 ! Bitmasks for CAS reference determinants. (N_int, alpha/beta, CAS reference)
 END_DOC
 logical                        :: exists
 integer                        :: i
 PROVIDE ezfio_filename
  
 call ezfio_has_bitmasks_cas(exists)
 if (exists) then                
   call ezfio_get_bitmasks_cas(cas_bitmask)
 else
   do i=1,N_cas_bitmask
     cas_bitmask(:,:,i) = iand(not(HF_bitmask(:,:)),full_ijkl_bitmask(:,:))
   enddo 
 endif  
            
END_PROVIDER

BEGIN_PROVIDER [ integer, N_det_cas ]
  implicit none
  BEGIN_DOC
  ! Number of generator detetrminants
  END_DOC
  integer                        :: i,k,l
  logical                        :: good
  call write_time(output_dets)
  N_det_cas = 0
  do i=1,N_det
    do l=1,n_cas_bitmask
      good = .True.
      do k=1,N_int
        good = good .and. (                                          &
            iand(not(cas_bitmask(k,1,l)), psi_det(k,1,i)) ==         &
            iand(not(cas_bitmask(k,1,l)), psi_det(k,1,1)) ) .and. (  &
            iand(not(cas_bitmask(k,2,l)), psi_det(k,2,i)) ==         &
            iand(not(cas_bitmask(k,2,l)), psi_det(k,2,1)) )
      enddo
      if (good) then
        exit
      endif
    enddo
    if (good) then
      N_det_cas += 1
    endif
  enddo
  N_det_cas = max(N_det_cas, 1)
  call write_int(output_dets,N_det_cas, 'Number of determinants in the CAS')
END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), psi_cas, (N_int,2,N_det_cas) ]
&BEGIN_PROVIDER [ double precision, psi_cas_coefs,  (N_det_cas,n_states) ]
&BEGIN_PROVIDER [ integer, idx_cas, (N_det_cas) ]
  implicit none
  BEGIN_DOC
  ! For Single reference wave functions, the generator is the
  ! Hartree-Fock determinant
  END_DOC
  integer                        :: i, k, l, m
  logical                        :: good
  m=0
  do i=1,N_det
    do l=1,n_cas_bitmask
      good = .True.
      do k=1,N_int
        good = good .and. (                                          &
            iand(not(cas_bitmask(k,1,l)), psi_det(k,1,i)) ==         &
            iand(not(cas_bitmask(k,1,l)), psi_det(k,1,1)) ) .and. (  &
            iand(not(cas_bitmask(k,2,l)), psi_det(k,2,i)) ==         &
            iand(not(cas_bitmask(k,2,l)), psi_det(k,2,1)) )
      enddo
      if (good) then
        exit
      endif
    enddo
    if (good) then
      m = m+1
      do k=1,N_int
        psi_cas(k,1,m) = psi_det(k,1,i)
        psi_cas(k,2,m) = psi_det(k,2,i)
      enddo
      idx_cas(m) = i
      do k=1,N_states
        psi_cas_coefs(m,k) = psi_coef(i,k)
      enddo
    endif
  enddo

END_PROVIDER




 BEGIN_PROVIDER [ integer(bit_kind), psi_sd,  (N_int,2,N_det) ]
&BEGIN_PROVIDER [ double precision, psi_sd_coefs, (N_det,n_states) ]
&BEGIN_PROVIDER [ integer, idx_sd,  (N_det) ]
&BEGIN_PROVIDER [ integer, N_det_sd]
 implicit none
 BEGIN_DOC
 ! SD
 END_DOC
 integer                        :: i_sd,j,k
 integer                        :: degree
 logical                        :: in_cas
 i_sd =0
 do k=1,N_det
   in_cas = .False.
   do j=1,N_det_cas
     call get_excitation_degree(psi_cas(1,1,j), psi_det(1,1,k), degree, N_int)
     if (degree == 0) then
       in_cas = .True.
       exit
     endif
   enddo
   if (.not.in_cas) then
     double precision :: hij
     i_sd += 1
     psi_sd(1:N_int,1:2,i_sd) = psi_det(1:N_int,1:2,k)
     psi_sd_coefs(i_sd,1:N_states) = psi_coef(k,1:N_states)
     idx_sd(i_sd) = k
   endif
 enddo
 N_det_sd = i_sd
END_PROVIDER

BEGIN_PROVIDER [ double precision, lambda_mrcc, (psi_det_size,n_states) ]
 implicit none
 BEGIN_DOC
 ! cm/<Psi_0|H|D_m>
 END_DOC
 integer :: i,k
 double precision :: ihpsi(N_states)
 do i=1,N_det_sd
   call i_h_psi(psi_sd(1,1,i), psi_cas, psi_cas_coefs, N_int, N_det_cas, &
     size(psi_cas_coefs,1), n_states, ihpsi)
   double precision :: hij
   do k=1,N_states
     if (dabs(ihpsi(k)) < 1.d-6) then
       lambda_mrcc(i,k) = 0.d0
     else
       lambda_mrcc(i,k) = psi_sd_coefs(i,k)/ihpsi(k)
       lambda_mrcc(i,k) = min( lambda_mrcc (i,k),0.d0 )
     endif
   enddo
 enddo
END_PROVIDER



BEGIN_PROVIDER [ character*(32), dressing_type ]
 implicit none
 BEGIN_DOC
 ! [ Simple | MRCC ]
 END_DOC
 dressing_type = "MRCC"
END_PROVIDER

BEGIN_PROVIDER [ double precision, delta_ij_sd, (N_det_sd, N_det_sd,N_states) ]
 implicit none
 BEGIN_DOC
 ! Dressing matrix in SD basis
 END_DOC
 delta_ij_sd = 0.d0
 if (dressing_type == "MRCC") then
   call H_apply_mrcc(delta_ij_sd,N_det_sd)
 else if (dressing_type == "Simple") then
   call H_apply_mrcc_simple(delta_ij_sd,N_det_sd)
 else
   print *,  irp_here
   stop 'dressing'
 endif
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


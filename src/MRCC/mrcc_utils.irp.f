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


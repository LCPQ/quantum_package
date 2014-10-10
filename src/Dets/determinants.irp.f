use bitmasks

BEGIN_PROVIDER [ integer, N_det ]
 implicit none
 BEGIN_DOC
 ! Number of determinants in the wave function
 END_DOC
 logical                        :: exists
 character*64                   :: label
 PROVIDE ezfio_filename
 if (read_wf) then
   call ezfio_has_determinants_n_det(exists)
   if (exists) then
     if (exists) then
       call ezfio_has_determinants_mo_label(exists)
       if (exists) then
         call ezfio_get_determinants_mo_label(label)
         exists = (label == mo_label)
       endif
     endif
   endif
   if (exists) then
     call ezfio_get_determinants_n_det(N_det)
   else
     N_det = 1
   endif
 else
   N_det = 1
 endif
 call write_int(output_dets,N_det,'Number of determinants')
 ASSERT (N_det > 0)
END_PROVIDER


BEGIN_PROVIDER [ integer, psi_det_size ]
 implicit none
 BEGIN_DOC
 ! Size of the psi_det/psi_coef arrays
 END_DOC
 PROVIDE ezfio_filename
 logical                        :: exists
 call ezfio_has_determinants_n_det(exists)
 if (exists) then
   call ezfio_get_determinants_n_det(psi_det_size)
 else
   psi_det_size = 1
 endif
 psi_det_size = max(psi_det_size,10000)
 call write_int(output_dets,psi_det_size,'Dimension of the psi arrays')

END_PROVIDER

BEGIN_PROVIDER [ integer(bit_kind), psi_det, (N_int,2,psi_det_size) ]
 implicit none
 BEGIN_DOC
 ! The wave function determinants. Initialized with Hartree-Fock if the EZFIO file
 ! is empty
 END_DOC
  integer                        :: i
  logical                        :: exists
  character*64                   :: label
  
  if (read_wf) then
    call ezfio_has_determinants_N_int(exists)
    if (exists) then
     call ezfio_has_determinants_bit_kind(exists)
     if (exists) then
      call ezfio_has_determinants_N_det(exists)
      if (exists) then
       call ezfio_has_determinants_N_states(exists)
       if (exists) then
        call ezfio_has_determinants_psi_det(exists)
        if (exists) then
          call ezfio_has_determinants_mo_label(exists)
          if (exists) then
            call ezfio_get_determinants_mo_label(label)
            exists = (label == mo_label)
          endif
        endif
       endif
      endif
     endif
    endif
   
    if (exists) then
      call read_dets(psi_det,N_int,N_det)
    else
       psi_det = 0_bit_kind
       do i=1,N_int
         psi_det(i,1,1) = HF_bitmask(i,1)
         psi_det(i,2,1) = HF_bitmask(i,2)
       enddo
    endif
  else
     psi_det = 0_bit_kind
     do i=1,N_int
       psi_det(i,1,1) = HF_bitmask(i,1)
       psi_det(i,2,1) = HF_bitmask(i,2)
     enddo
  endif

END_PROVIDER

subroutine read_dets(det,Nint,Ndet)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Reads the determinants from the EZFIO file
  END_DOC
  
  integer, intent(in)            :: Nint,Ndet
  integer(bit_kind), intent(out) :: det(Nint,2,Ndet)
  integer*8, allocatable         :: psi_det_read(:,:,:)
  double precision, allocatable  :: psi_coef_read(:,:)
  integer*8                      :: det_8(100)
  integer(bit_kind)              :: det_bk((100*8)/bit_kind)
  integer                        :: N_int2
  integer                        :: i,k
  equivalence (det_8, det_bk)
  
!  print*,'coucou'
  call ezfio_get_determinants_N_int(N_int2)
  ASSERT (N_int2 == Nint)
  call ezfio_get_determinants_bit_kind(k)
  ASSERT (k == bit_kind)
  
  N_int2 = (Nint*bit_kind)/8
  allocate (psi_det_read(N_int2,2,Ndet))
  call ezfio_get_determinants_psi_det (psi_det_read)
! print*,'N_int2 = ',N_int2,N_int
! print*,'k',k,bit_kind
! print*,'psi_det_read = ',Ndet
  do i=1,Ndet
    do k=1,N_int2
      det_8(k) = psi_det_read(k,1,i)
    enddo
    do k=1,Nint
      det(k,1,i) = det_bk(k)
    enddo
    do k=1,N_int2
      det_8(k) = psi_det_read(k,2,i)
    enddo
    do k=1,Nint
      det(k,2,i) = det_bk(k)
    enddo
  enddo
  deallocate(psi_det_read)
!  print*,'ciao'
  
end


BEGIN_PROVIDER [ double precision, psi_coef, (psi_det_size,N_states_diag) ]
  implicit none
  BEGIN_DOC
  ! The wave function coefficients. Initialized with Hartree-Fock if the EZFIO file
  ! is empty
  END_DOC
  
  integer                        :: i,k, N_int2
  logical                        :: exists
  double precision, allocatable  :: psi_coef_read(:,:)
  character*(64)                 :: label

  psi_coef = 0.d0
  do i=1,N_states_diag
    psi_coef(i,i) = 1.d0
  enddo
  
  if (read_wf) then
    call ezfio_has_determinants_psi_coef(exists)
    if (exists) then
      call ezfio_has_determinants_mo_label(exists)
      if (exists) then
        call ezfio_get_determinants_mo_label(label)
        exists = (label == mo_label)
      endif
    endif
    
    if (exists) then
      
      allocate (psi_coef_read(N_det,N_states))
      call ezfio_get_determinants_psi_coef(psi_coef_read)
      do k=1,N_states
        do i=1,N_det
          psi_coef(i,k) = psi_coef_read(i,k)
        enddo
      enddo
      deallocate(psi_coef_read)
      
    endif
    
  endif
    
  
END_PROVIDER


BEGIN_PROVIDER [ double precision, psi_average_norm_contrib, (N_det) ]
 implicit none
 BEGIN_DOC
 ! Contribution of determinants to the state-averaged density
 END_DOC
 integer :: i,j,k
 double precision :: f
 f = 1.d0/dble(N_states)
 do i=1,N_det
   psi_average_norm_contrib(i) = psi_coef(i,1)*psi_coef(i,1)*f
 enddo
 do k=2,N_states
   do i=1,N_det
     psi_average_norm_contrib(i) = psi_average_norm_contrib(i) + &
       psi_coef(i,k)*psi_coef(i,k)*f
   enddo
 enddo
END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), psi_det_sorted, (N_int,2,N_det) ]
&BEGIN_PROVIDER [ double precision, psi_coef_sorted, (N_det,N_states) ]
&BEGIN_PROVIDER [ double precision, psi_average_norm_contrib_sorted, (N_det) ]
 implicit none
 BEGIN_DOC
 ! Wave function sorted by determinants contribution to the norm (state-averaged)
 END_DOC
 integer :: i,j,k
 integer, allocatable ::  iorder(:)
 allocate ( iorder(N_det) )
 do i=1,N_det
   psi_average_norm_contrib_sorted(i) = -psi_average_norm_contrib(i)
   iorder(i) = i
 enddo
 call dsort(psi_average_norm_contrib_sorted,iorder,N_det)
 !DIR$ IVDEP
 do i=1,N_det
  do j=1,N_int
    psi_det_sorted(j,1,i) = psi_det(j,1,iorder(i))
    psi_det_sorted(j,2,i) = psi_det(j,2,iorder(i))
  enddo
  do k=1,N_states
    psi_coef_sorted(i,k) = psi_coef(iorder(i),k)
  enddo
  psi_average_norm_contrib_sorted(i) = -psi_average_norm_contrib_sorted(i)
 enddo

 deallocate(iorder)

END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), psi_det_sorted_bit, (N_int,2,N_det) ]
&BEGIN_PROVIDER [ double precision, psi_coef_sorted_bit, (N_det,N_states) ]
 implicit none
 BEGIN_DOC
 ! Determinants on which we apply <i|H|psi> for perturbation.
 !o They are sorted by determinants interpreted as integers. Useful
 ! to accelerate the search of a determinant
 END_DOC
 integer :: i,j,k
 integer, allocatable ::  iorder(:)
 integer*8, allocatable :: bit_tmp(:)
 integer*8, external :: det_search_key

 allocate ( iorder(N_det), bit_tmp(N_det) )

 do i=1,N_det
   iorder(i) = i
   !$DIR FORCEINLINE
   bit_tmp(i) = det_search_key(psi_det(1,1,i),N_int)
 enddo
 call i8sort(bit_tmp,iorder,N_det)
 !DIR$ IVDEP
 do i=1,N_det
  do j=1,N_int
    psi_det_sorted_bit(j,1,i) = psi_det(j,1,iorder(i))
    psi_det_sorted_bit(j,2,i) = psi_det(j,2,iorder(i))
  enddo
  do k=1,N_states
    psi_coef_sorted_bit(i,k) = psi_coef(iorder(i),k)
  enddo
 enddo

 deallocate(iorder)

END_PROVIDER


subroutine save_wavefunction
  implicit none
  use bitmasks
  BEGIN_DOC
!  Save the wave function into the EZFIO file
  END_DOC
  call save_wavefunction_general(N_det,N_states,psi_det_sorted,psi_coef_sorted)
end

subroutine save_wavefunction_general(ndet,nstates,psidet,psicoef)
  implicit none
  BEGIN_DOC
!  Save the wave function into the EZFIO file
  END_DOC
  use bitmasks
  integer, intent(in) :: ndet,nstates
  integer(bit_kind), intent(in) :: psidet(N_int,2,ndet)
  double precision, intent(in)  :: psicoef(ndet,nstates)
  integer*8, allocatable         :: psi_det_save(:,:,:)
  double precision, allocatable  :: psi_coef_save(:,:)
  integer*8                      :: det_8(100)
  integer(bit_kind)              :: det_bk((100*8)/bit_kind)
  integer                        :: N_int2
  equivalence (det_8, det_bk)

  integer :: i,k

  PROVIDE progress_bar
  call start_progress(7,'Saving wfunction',0.d0)

  progress_bar(1) = 1
  progress_value = dble(progress_bar(1))
  call ezfio_set_determinants_N_int(N_int)
  progress_bar(1) = 2
  progress_value = dble(progress_bar(1))
  call ezfio_set_determinants_bit_kind(bit_kind)
  progress_bar(1) = 3
  progress_value = dble(progress_bar(1))
  call ezfio_set_determinants_N_det(ndet)
  progress_bar(1) = 4
  progress_value = dble(progress_bar(1))
  call ezfio_set_determinants_n_states(nstates)
  progress_bar(1) = 5
  progress_value = dble(progress_bar(1))
  call ezfio_set_determinants_mo_label(mo_label)

  progress_bar(1) = 6
  progress_value = dble(progress_bar(1))

  N_int2 = (N_int*bit_kind)/8
  allocate (psi_det_save(N_int2,2,ndet))
  do i=1,ndet
    do k=1,N_int
      det_bk(k) = psidet(k,1,i)
    enddo
    do k=1,N_int2
      psi_det_save(k,1,i) = det_8(k)
    enddo
    do k=1,N_int
      det_bk(k) = psidet(k,2,i)
    enddo
    do k=1,N_int2
      psi_det_save(k,2,i) = det_8(k)
    enddo
!   print*,psi_det_save
  enddo
  call ezfio_set_determinants_psi_det(psi_det_save)
  deallocate (psi_det_save)

  progress_bar(1) = 7
  progress_value = dble(progress_bar(1))
  allocate (psi_coef_save(ndet,nstates))
  do k=1,nstates
    do i=1,ndet
      psi_coef_save(i,k) = psicoef(i,k)
    enddo
  enddo
  call ezfio_set_determinants_psi_coef(psi_coef_save)
  call write_int(output_dets,ndet,'Saved determinants')
  call stop_progress
  deallocate (psi_coef_save)
end

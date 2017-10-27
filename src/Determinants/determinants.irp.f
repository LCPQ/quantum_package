use bitmasks

BEGIN_PROVIDER [ character*(64), diag_algorithm ]
  implicit none
  BEGIN_DOC
  ! Diagonalization algorithm (Davidson or Lapack)
  END_DOC
  if (N_det > N_det_max_jacobi) then
    diag_algorithm = "Davidson"
  else
    diag_algorithm = "Lapack"
  endif

  if (N_det < N_states) then
    diag_algorithm = "Lapack"
  endif
END_PROVIDER


BEGIN_PROVIDER [ integer, N_det ]
 implicit none
 BEGIN_DOC
 ! Number of determinants in the wave function
 END_DOC
 logical                        :: exists
 character*(64)                   :: label
 PROVIDE ezfio_filename
 PROVIDE nproc
 if (read_wf) then
   call ezfio_has_determinants_n_det(exists)
   if (exists) then
     call ezfio_has_determinants_mo_label(exists)
     if (exists) then
       call ezfio_get_determinants_mo_label(label)
       exists = (label == mo_label)
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
 call write_int(output_determinants,N_det,'Number of determinants')
 ASSERT (N_det > 0)
END_PROVIDER

BEGIN_PROVIDER [integer, max_degree_exc]
 implicit none
 integer :: i,degree
 max_degree_exc = 0
 BEGIN_DOC
 ! Maximum degree of excitation in the wf
 END_DOC
 do i = 1, N_det
  call get_excitation_degree(HF_bitmask,psi_det(1,1,i),degree,N_int)
  if(degree.gt.max_degree_exc)then
   max_degree_exc= degree
  endif
 enddo
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
 psi_det_size = max(psi_det_size,100000)
 call write_int(output_determinants,psi_det_size,'Dimension of the psi arrays')

END_PROVIDER

BEGIN_PROVIDER [ integer(bit_kind), psi_det, (N_int,2,psi_det_size) ]
 implicit none
 BEGIN_DOC
 ! The wave function determinants. Initialized with Hartree-Fock if the EZFIO file
 ! is empty
 END_DOC
  integer                        :: i
  logical                        :: exists
  character*(64)                   :: label
  
  psi_det = 0_bit_kind
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



BEGIN_PROVIDER [ double precision, psi_coef, (psi_det_size,N_states) ]
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
  do i=1,min(N_states,psi_det_size)
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


BEGIN_PROVIDER [ double precision, psi_average_norm_contrib, (psi_det_size) ]
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
 f = 1.d0/sum(psi_average_norm_contrib(1:N_det))
 do i=1,N_det
   psi_average_norm_contrib(i) = psi_average_norm_contrib(i)*f
 enddo
END_PROVIDER



!==============================================================================!
!                                                                              !
!                               Sorting providers                              !
!                                                                              !
!==============================================================================!


 BEGIN_PROVIDER [ integer(bit_kind), psi_det_sorted, (N_int,2,psi_det_size) ]
&BEGIN_PROVIDER [ double precision, psi_coef_sorted, (psi_det_size,N_states) ]
&BEGIN_PROVIDER [ double precision, psi_average_norm_contrib_sorted, (psi_det_size) ]
&BEGIN_PROVIDER [ integer, psi_det_sorted_order, (psi_det_size) ]
 implicit none
 BEGIN_DOC
 ! Wave function sorted by determinants contribution to the norm (state-averaged)
 !
 ! psi_det_sorted_order(i) -> k : index in psi_det
 END_DOC
 integer :: i,j,k
 integer, allocatable ::  iorder(:)
 allocate ( iorder(N_det) )
 do i=1,N_det
   psi_average_norm_contrib_sorted(i) = -psi_average_norm_contrib(i)
   iorder(i) = i
 enddo
 call dsort(psi_average_norm_contrib_sorted,iorder,N_det)
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
 do i=1,N_det
   psi_det_sorted_order(iorder(i)) = i
 enddo


 deallocate(iorder)

END_PROVIDER

subroutine flip_generators()
  integer :: i,j,k
  integer(bit_kind) :: detmp(N_int,2)
  double precision :: tmp(N_states)
  
  do i=1,N_det_generators/2
    detmp(:,:) = psi_det_sorted(:,:,i)
    tmp = psi_coef_sorted(i, :)
    psi_det_sorted(:,:,i) = psi_det_sorted(:,:,N_det_generators+1-i)
    psi_coef_sorted(i, :) = psi_coef_sorted(N_det_generators+1-i, :)
    
    psi_det_sorted(:,:,N_det_generators+1-i) = detmp(:,:)
    psi_coef_sorted(N_det_generators+1-i, :) = tmp
  end do
  
  TOUCH psi_det_sorted psi_coef_sorted psi_average_norm_contrib_sorted
end subroutine

 BEGIN_PROVIDER [ integer(bit_kind), psi_det_sorted_bit, (N_int,2,psi_det_size) ]
&BEGIN_PROVIDER [ double precision, psi_coef_sorted_bit, (psi_det_size,N_states) ]
 implicit none
 BEGIN_DOC
 ! Determinants on which we apply <i|H|psi> for perturbation.
 ! They are sorted by determinants interpreted as integers. Useful
 ! to accelerate the search of a random determinant in the wave
 ! function.
 END_DOC

 call sort_dets_by_det_search_key(N_det, psi_det, psi_coef, &
   psi_det_sorted_bit, psi_coef_sorted_bit)
END_PROVIDER

subroutine sort_dets_by_det_search_key(Ndet, det_in, coef_in, det_out, coef_out)
  use bitmasks
  implicit none
  integer, intent(in) :: Ndet
  integer(bit_kind), intent(in)  :: det_in  (N_int,2,psi_det_size)
  double precision , intent(in)  :: coef_in(psi_det_size,N_states)
  integer(bit_kind), intent(out) :: det_out (N_int,2,psi_det_size)
  double precision , intent(out) :: coef_out(psi_det_size,N_states)
 BEGIN_DOC
 ! Determinants are sorted are sorted according to their det_search_key.
 ! Useful to accelerate the search of a random determinant in the wave
 ! function.
 END_DOC
 integer :: i,j,k
 integer, allocatable ::  iorder(:)
 integer*8, allocatable :: bit_tmp(:)
 integer*8, external :: det_search_key

 allocate ( iorder(Ndet), bit_tmp(Ndet) )

 do i=1,Ndet
   iorder(i) = i
   !$DIR FORCEINLINE
   bit_tmp(i) = det_search_key(det_in(1,1,i),N_int)
 enddo
 call i8sort(bit_tmp,iorder,Ndet)
 !DIR$ IVDEP
 do i=1,Ndet
  do j=1,N_int
    det_out(j,1,i) = det_in(j,1,iorder(i))
    det_out(j,2,i) = det_in(j,2,iorder(i))
  enddo
  do k=1,N_states
    coef_out(i,k) = coef_in(iorder(i),k)
  enddo
 enddo

 deallocate(iorder, bit_tmp)

end


      
 BEGIN_PROVIDER [ double precision, psi_coef_max, (N_states) ] 
&BEGIN_PROVIDER [ double precision, psi_coef_min, (N_states) ] 
&BEGIN_PROVIDER [ double precision, abs_psi_coef_max, (N_states) ] 
&BEGIN_PROVIDER [ double precision, abs_psi_coef_min, (N_states) ] 
 implicit none
 BEGIN_DOC
 ! Max and min values of the coefficients
 END_DOC
 integer:: i
 do i=1,N_states
   psi_coef_min(i) = minval(psi_coef(:,i))
   psi_coef_max(i) = maxval(psi_coef(:,i))
   abs_psi_coef_min(i) = minval( dabs(psi_coef(:,i)) )
   abs_psi_coef_max(i) = maxval( dabs(psi_coef(:,i)) )
   call write_double(6,psi_coef_max(i), 'Max coef')
   call write_double(6,psi_coef_min(i), 'Min coef')
   call write_double(6,abs_psi_coef_max(i), 'Max abs coef')
   call write_double(6,abs_psi_coef_min(i), 'Min abs coef')
 enddo

END_PROVIDER


!==============================================================================!
!                                                                              !
!                             Read/write routines                              !
!                                                                              !
!==============================================================================!

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
  
end

subroutine save_ref_determinant
 implicit none
  use bitmasks
  double precision :: buffer(1,1)
  buffer(1,1) = 1.d0
  call save_wavefunction_general(1,1,ref_bitmask,1,buffer)
end




subroutine save_wavefunction
  implicit none
  use bitmasks
  BEGIN_DOC
!  Save the wave function into the EZFIO file
  END_DOC
  call save_wavefunction_general(N_det,N_states,psi_det_sorted,size(psi_coef_sorted,1),psi_coef_sorted)
end


subroutine save_wavefunction_unsorted
  implicit none
  use bitmasks
  BEGIN_DOC
!  Save the wave function into the EZFIO file
  END_DOC
  call save_wavefunction_general(N_det,N_states,psi_det,size(psi_coef,1),psi_coef)
end

subroutine save_wavefunction_general(ndet,nstates,psidet,dim_psicoef,psicoef)
  implicit none
  BEGIN_DOC
!  Save the wave function into the EZFIO file
  END_DOC
  use bitmasks
  include 'constants.include.F'
  integer, intent(in) :: ndet,nstates,dim_psicoef
  integer(bit_kind), intent(in) :: psidet(N_int,2,ndet)
  double precision, intent(in)  :: psicoef(dim_psicoef,nstates)
  integer*8, allocatable         :: psi_det_save(:,:,:)
  double precision, allocatable  :: psi_coef_save(:,:)

  integer :: i,j,k

  call ezfio_set_determinants_N_int(N_int)
  call ezfio_set_determinants_bit_kind(bit_kind)
  call ezfio_set_determinants_N_det(ndet)
  call ezfio_set_determinants_n_states(nstates)
  call ezfio_set_determinants_mo_label(mo_label)

  allocate (psi_det_save(N_int,2,ndet))
  do i=1,ndet
   do j=1,2
    do k=1,N_int
      psi_det_save(k,j,i) = transfer(psidet(k,j,i),1_8)
    enddo
   enddo
  enddo
  call ezfio_set_determinants_psi_det(psi_det_save)
  deallocate (psi_det_save)

  allocate (psi_coef_save(ndet,nstates))
  double precision :: accu_norm(nstates)
  accu_norm = 0.d0
  do k=1,nstates
    do i=1,ndet
      accu_norm(k) = accu_norm(k) + psicoef(i,k) * psicoef(i,k)
      psi_coef_save(i,k) = psicoef(i,k)
    enddo
  enddo
  do k = 1, nstates
   accu_norm(k) = 1.d0/dsqrt(accu_norm(k))
  enddo
  do k=1,nstates
    do i=1,ndet
      psi_coef_save(i,k) = psi_coef_save(i,k) * accu_norm(k)
    enddo
  enddo

  call ezfio_set_determinants_psi_coef(psi_coef_save)
  call write_int(output_determinants,ndet,'Saved determinants')
  deallocate (psi_coef_save)
end



subroutine save_wavefunction_specified(ndet,nstates,psidet,psicoef,ndetsave,index_det_save)
  implicit none
  BEGIN_DOC
!  Save the wave function into the EZFIO file
  END_DOC
  use bitmasks
  integer, intent(in) :: ndet,nstates
  integer(bit_kind), intent(in) :: psidet(N_int,2,ndet)
  double precision, intent(in)  :: psicoef(ndet,nstates)
  integer, intent(in)           :: index_det_save(ndet)
  integer, intent(in)           :: ndetsave
  integer*8, allocatable         :: psi_det_save(:,:,:)
  double precision, allocatable  :: psi_coef_save(:,:)
  integer*8                      :: det_8(100)
  integer(bit_kind)              :: det_bk((100*8)/bit_kind)
  integer                        :: N_int2
  equivalence (det_8, det_bk)

  integer :: i,k

  call ezfio_set_determinants_N_int(N_int)
  call ezfio_set_determinants_bit_kind(bit_kind)
  call ezfio_set_determinants_N_det(ndetsave)
  call ezfio_set_determinants_n_states(nstates)
  call ezfio_set_determinants_mo_label(mo_label)

  N_int2 = (N_int*bit_kind)/8
  allocate (psi_det_save(N_int2,2,ndetsave))
  do i=1,ndetsave
    do k=1,N_int
      det_bk(k) = psidet(k,1,index_det_save(i))
    enddo
    do k=1,N_int2
      psi_det_save(k,1,i) = det_8(k)
    enddo
    do k=1,N_int
      det_bk(k) = psidet(k,2,index_det_save(i))
    enddo
    do k=1,N_int2
      psi_det_save(k,2,i) = det_8(k)
    enddo
  enddo
  call ezfio_set_determinants_psi_det(psi_det_save)
  deallocate (psi_det_save)

  progress_bar(1) = 7
  progress_value = dble(progress_bar(1))
  allocate (psi_coef_save(ndetsave,nstates))
  double precision :: accu_norm(nstates)
  accu_norm = 0.d0
  do k=1,nstates
    do i=1,ndetsave
      accu_norm(k) = accu_norm(k) + psicoef(index_det_save(i),k) * psicoef(index_det_save(i),k)
      psi_coef_save(i,k) = psicoef(index_det_save(i),k)
    enddo
  enddo
  do k = 1, nstates
   accu_norm(k) = 1.d0/dsqrt(accu_norm(k))
  enddo
  do k=1,nstates
    do i=1,ndetsave
      psi_coef_save(i,k) = psi_coef_save(i,k) * accu_norm(k)
    enddo
  enddo

  call ezfio_set_determinants_psi_coef(psi_coef_save)
  call write_int(output_determinants,ndet,'Saved determinants')
  deallocate (psi_coef_save)
end


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


integer function detCmp(a,b,Nint)
   use bitmasks
   implicit none
   integer, intent(in) :: Nint
   integer(bit_kind), intent(in) :: a(Nint,2), b(Nint,2)
   integer :: ni, i
 
   detCmp = 0
   do i=1,2
   do ni=Nint,1,-1
   
     if(a(ni,i) < b(ni,i)) then
       detCmp = -1
       return
     else if(a(ni,i) > b(ni,i)) then
       detCmp = 1
       return
     end if
     
   end do
   end do
end function


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
  
!  call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
! INLINE
  select case(degree)
    case(2)
      if (exc(0,1,1) == 2) then
        h1 = exc(1,1,1)
        h2 = exc(2,1,1)
        p1 = exc(1,2,1)
        p2 = exc(2,2,1)
        s1 = 1
        s2 = 1
      else if (exc(0,1,2) == 2) then
        h1 = exc(1,1,2)
        h2 = exc(2,1,2)
        p1 = exc(1,2,2)
        p2 = exc(2,2,2)
        s1 = 2
        s2 = 2
      else
        h1 = exc(1,1,1)
        h2 = exc(1,1,2)
        p1 = exc(1,2,1)
        p2 = exc(1,2,2)
        s1 = 1
        s2 = 2
      endif
    case(1)
      if (exc(0,1,1) == 1) then
        h1 = exc(1,1,1)
        h2 = 0
        p1 = exc(1,2,1)
        p2 = 0
        s1 = 1
        s2 = 0
      else
        h1 = exc(1,1,2)
        h2 = 0
        p1 = exc(1,2,2)
        p2 = 0
        s1 = 2
        s2 = 0
      endif
    case(0)
      h1 = 0
      p1 = 0
      h2 = 0
      p2 = 0
      s1 = 0
      s2 = 0
    case default
      print *, degree
      print *, "apply ex"
      STOP
  end select
! END INLINE

  res = det 
  
  ii = ishft(h1-1,-bit_kind_shift) + 1 
  pos = h1-1-ishft(ii-1,bit_kind_shift)
  if(iand(det(ii, s1), ibset(0_bit_kind, pos)) == 0_8) return
  res(ii, s1) = ibclr(res(ii, s1), pos)
  
  ii = ishft(p1-1,-bit_kind_shift) + 1 
  pos = p1-1-ishft(ii-1,bit_kind_shift)
  if(iand(det(ii, s1), ishft(1_bit_kind, pos)) /= 0_8) return
  res(ii, s1) = ibset(res(ii, s1), pos)
  
  if(degree == 2) then
    ii = ishft(h2-1,-bit_kind_shift) + 1 
    pos = h2-1-ishft(ii-1,bit_kind_shift)
    if(iand(det(ii, s2), ishft(1_bit_kind, pos)) == 0_8) return
    res(ii, s2) = ibclr(res(ii, s2), pos)
    
    ii = ishft(p2-1,-bit_kind_shift) + 1 
    pos = p2-1-ishft(ii-1,bit_kind_shift)
    if(iand(det(ii, s2), ishft(1_bit_kind, pos)) /= 0_8) return
    res(ii, s2) = ibset(res(ii, s2), pos)
  endif
  ok = .true.
end subroutine


subroutine apply_particles(det, s1, p1, s2, p2, res, ok, Nint)
  use bitmasks
  implicit none
  integer, intent(in) :: Nint
  integer, intent(in) :: s1, p1, s2, p2
  integer(bit_kind),intent(in) :: det(Nint, 2)
  integer(bit_kind),intent(out) :: res(Nint, 2)
  logical, intent(out) :: ok
  integer :: ii, pos 
  
  ok = .false.
  res = det 
  
  if(p1 /= 0) then
  ii = ishft(p1-1,-bit_kind_shift) + 1 
  pos = p1-1-ishft(ii-1,bit_kind_shift)
  if(iand(det(ii, s1), ishft(1_bit_kind, pos)) /= 0_8) return
  res(ii, s1) = ibset(res(ii, s1), pos)
  end if

  ii = ishft(p2-1,-bit_kind_shift) + 1 
  pos = p2-1-ishft(ii-1,bit_kind_shift)
  if(iand(det(ii, s2), ishft(1_bit_kind, pos)) /= 0_8) return
  res(ii, s2) = ibset(res(ii, s2), pos)

  ok = .true.
end subroutine


subroutine apply_holes(det, s1, h1, s2, h2, res, ok, Nint)
  use bitmasks
  implicit none
  integer, intent(in) :: Nint
  integer, intent(in) :: s1, h1, s2, h2
  integer(bit_kind),intent(in) :: det(Nint, 2)
  integer(bit_kind),intent(out) :: res(Nint, 2)
  logical, intent(out) :: ok
  integer :: ii, pos 
  
  ok = .false.
  res = det 
  
  if(h1 /= 0) then
  ii = ishft(h1-1,-bit_kind_shift) + 1 
  pos = h1-1-ishft(ii-1,bit_kind_shift)
  if(iand(det(ii, s1), ishft(1_bit_kind, pos)) == 0_8) return
  res(ii, s1) = ibclr(res(ii, s1), pos)
  end if

  ii = ishft(h2-1,-bit_kind_shift) + 1 
  pos = h2-1-ishft(ii-1,bit_kind_shift)
  if(iand(det(ii, s2), ishft(1_bit_kind, pos)) == 0_8) return
  res(ii, s2) = ibclr(res(ii, s2), pos)

  ok = .true.
end subroutine

subroutine apply_particle(det, s1, p1, res, ok, Nint)
  use bitmasks
  implicit none
  integer, intent(in) :: Nint
  integer, intent(in) :: s1, p1
  integer(bit_kind),intent(in) :: det(Nint, 2)
  integer(bit_kind),intent(out) :: res(Nint, 2)
  logical, intent(out) :: ok
  integer :: ii, pos 
  
  ok = .false.
  res = det 
  
  ii = ishft(p1-1,-bit_kind_shift) + 1 
  pos = p1-1-ishft(ii-1,bit_kind_shift)
  if(iand(det(ii, s1), ishft(1_bit_kind, pos)) /= 0_8) return
  res(ii, s1) = ibset(res(ii, s1), pos)

  ok = .true.
end subroutine


subroutine apply_hole(det, s1, h1, res, ok, Nint)
  use bitmasks
  implicit none
  integer, intent(in) :: Nint
  integer, intent(in) :: s1, h1
  integer(bit_kind),intent(in) :: det(Nint, 2)
  integer(bit_kind),intent(out) :: res(Nint, 2)
  logical, intent(out) :: ok
  integer :: ii, pos 
  
  ok = .false.
  res = det 
  
  ii = ishft(h1-1,-bit_kind_shift) + 1 
  pos = h1-1-ishft(ii-1,bit_kind_shift)
  if(iand(det(ii, s1), ishft(1_bit_kind, pos)) == 0_8) return
  res(ii, s1) = ibclr(res(ii, s1), pos)

  ok = .true.
end subroutine

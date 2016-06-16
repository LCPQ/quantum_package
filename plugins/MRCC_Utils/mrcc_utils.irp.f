
 BEGIN_PROVIDER [ integer, mrmode ]
&BEGIN_PROVIDER [ logical, old_lambda ]
&BEGIN_PROVIDER [ logical, no_mono_dressing ]
  implicit none
  CHARACTER(len=255) :: test
  CALL get_environment_variable("OLD_LAMBDA", test)
  old_lambda = trim(test) /= "" .and. trim(test) /= "0"
  CALL get_environment_variable("NO_MONO_DRESSING", test)
  no_mono_dressing = trim(test) /= "" .and. trim(test) /= "0"
  print *, "old", old_lambda, "mono", no_mono_dressing
  mrmode = 0
END_PROVIDER
 
 

BEGIN_PROVIDER [ double precision, lambda_mrcc_old, (N_states,psi_det_size) ]
&BEGIN_PROVIDER [ integer, lambda_mrcc_pt2_old, (0:psi_det_size) ]
&BEGIN_PROVIDER [ integer, lambda_mrcc_pt3_old, (0:psi_det_size) ]
 implicit none
 BEGIN_DOC
 cm/<Psi_0|H|D_m> or perturbative 1/Delta_E(m)
 END_DOC
 integer :: i,k
  double precision               :: ihpsi_current(N_states)
  integer :: i_pert_count
  double precision :: hii, lambda_pert
  integer                        :: N_lambda_mrcc_pt2, N_lambda_mrcc_pt3
  double precision, parameter :: x = 2.d0
  double precision :: nurm 
  i_pert_count = 0
  lambda_mrcc_old = 0.d0
  N_lambda_mrcc_pt2 = 0
  N_lambda_mrcc_pt3 = 0
  lambda_mrcc_pt2_old(0) = 0
  lambda_mrcc_pt3_old(0) = 0
  if(N_states > 1) stop "old lambda N_states == 1"
  nurm = 0d0
  do i=1,N_det_ref
    nurm += psi_ref_coef(i,1)**2
  end do

   do i=1,N_det_non_ref
     call i_h_psi(psi_non_ref(1,1,i), psi_ref, psi_ref_coef, N_int, N_det_ref, &
                  size(psi_ref_coef,1), N_states,ihpsi_current)
     call i_H_j(psi_non_ref(1,1,i),psi_non_ref(1,1,i),N_int,hii)
     do k=1,N_states
       if (ihpsi_current(k) == 0.d0) then
         ihpsi_current(k) = 1.d-32
       endif
       lambda_mrcc_old(k,i) = psi_non_ref_coef(i,k)/ihpsi_current(k) 
      !if ( dabs(psi_non_ref_coef(i,k)*ihpsi_current(k)) < 1.d-5 .or. lambda_mrcc_old(k,i) > 0d0) then
      if ( dabs(ihpsi_current(k))*sqrt(psi_non_ref_coef(i,k)**2 / nurm) < 1.d-5 .or. lambda_mrcc_old(k,i) > 0d0) then
        i_pert_count += 1
        lambda_mrcc_old(k,i) = 0.d0
        if (lambda_mrcc_pt2_old(N_lambda_mrcc_pt2) /= i) then
          N_lambda_mrcc_pt2 += 1
          lambda_mrcc_pt2_old(N_lambda_mrcc_pt2) = i
        endif
      else
        if (lambda_mrcc_pt3_old(N_lambda_mrcc_pt3) /= i) then
          N_lambda_mrcc_pt3 += 1
          lambda_mrcc_pt3_old(N_lambda_mrcc_pt3) = i
        endif
      endif
!         lambda_pert = 1.d0 / (psi_ref_energy_diagonalized(k)-hii)
!         if((ihpsi_current(k) * lambda_pert) < 0.5d0 * psi_non_ref_coef_restart(i,k) ) then
!            lambda_mrcc_old(k,i) = 0.d0
!         endif
      
      if (lambda_mrcc_old(k,i) > x) then
        lambda_mrcc_old(k,i) = x
      else if (lambda_mrcc_old(k,i) < -x) then
        lambda_mrcc_old(k,i) = -x
      endif
     enddo
  enddo
  lambda_mrcc_pt2_old(0) = N_lambda_mrcc_pt2
  lambda_mrcc_pt3_old(0) = N_lambda_mrcc_pt3
  
  print*,'N_det_non_ref = ',N_det_non_ref
  print*,'Number of ignored determinants = ',i_pert_count
  print*,'psi_coef_ref_ratio = ',psi_ref_coef(2,1)/psi_ref_coef(1,1)
  print*,'lambda min/max = ',maxval(dabs(lambda_mrcc_old)), minval(dabs(lambda_mrcc_old))

END_PROVIDER


 BEGIN_PROVIDER [ double precision, lambda_mrcc, (N_states,psi_det_size) ]
&BEGIN_PROVIDER [ integer, lambda_mrcc_pt2, (0:psi_det_size) ]
&BEGIN_PROVIDER [ integer, lambda_mrcc_pt3, (0:psi_det_size) ]
  implicit none
  BEGIN_DOC
  ! cm/<Psi_0|H|D_m> or perturbative 1/Delta_E(m)
  END_DOC
  integer :: i,k
  double precision               :: ihpsi_current(N_states)
  integer                        :: i_pert_count
  double precision               :: hii, lambda_pert
  integer                        :: N_lambda_mrcc_pt2, N_lambda_mrcc_pt3
  integer                        :: histo(200), j
  histo = 0
  
  if(old_lambda) then
    lambda_mrcc = lambda_mrcc_old
    lambda_mrcc_pt2 = lambda_mrcc_pt2_old
    lambda_mrcc_pt3 = lambda_mrcc_pt3_old
  else
    i_pert_count = 0
    lambda_mrcc = 0.d0
    N_lambda_mrcc_pt2 = 0
    N_lambda_mrcc_pt3 = 0
    lambda_mrcc_pt2(0) = 0
    lambda_mrcc_pt3(0) = 0

    do i=1,N_det_non_ref
      call i_h_psi(psi_non_ref(1,1,i), psi_ref, psi_ref_coef, N_int, N_det_ref,&
          size(psi_ref_coef,1), N_states,ihpsi_current)
      call i_H_j(psi_non_ref(1,1,i),psi_non_ref(1,1,i),N_int,hii)
      do k=1,N_states
        if (ihpsi_current(k) == 0.d0) then
          ihpsi_current(k) = 1.d-32
        endif
        lambda_mrcc(k,i) = min(-1.d-32,psi_non_ref_coef(i,k)/ihpsi_current(k) )
        lambda_pert = 1.d0 / (psi_ref_energy_diagonalized(k)-hii)
        if (lambda_pert / lambda_mrcc(k,i)  < 0.5d0) then
          i_pert_count += 1
          lambda_mrcc(k,i) = 0.d0
          if (lambda_mrcc_pt2(N_lambda_mrcc_pt2) /= i) then
            N_lambda_mrcc_pt2 += 1
            lambda_mrcc_pt2(N_lambda_mrcc_pt2) = i
          endif
        else
          if (lambda_mrcc_pt3(N_lambda_mrcc_pt3) /= i) then
            N_lambda_mrcc_pt3 += 1
            lambda_mrcc_pt3(N_lambda_mrcc_pt3) = i
          endif
        endif
      enddo
    enddo
    lambda_mrcc_pt2(0) = N_lambda_mrcc_pt2
    lambda_mrcc_pt3(0) = N_lambda_mrcc_pt3
  end if
  print*,'N_det_non_ref = ',N_det_non_ref
  print*,'psi_coef_ref_ratio = ',psi_ref_coef(2,1)/psi_ref_coef(1,1)
  print*,'lambda max = ',maxval(dabs(lambda_mrcc))
  print*,'Number of ignored determinants = ',i_pert_count  

END_PROVIDER

!  BEGIN_PROVIDER [ double precision, lambda_mrcc, (N_states,psi_det_size) ]
! &BEGIN_PROVIDER [ integer, lambda_mrcc_pt2, (0:psi_det_size) ]
! &BEGIN_PROVIDER [ integer, lambda_mrcc_pt3, (0:psi_det_size) ]
!   implicit none
!   BEGIN_DOC
!   ! cm/<Psi_0|H|D_m> or perturbative 1/Delta_E(m)
!   END_DOC
!   integer :: i,ii,k
!   double precision               :: ihpsi_current(N_states)
!   integer                        :: i_pert_count
!   double precision               :: hii, lambda_pert, phase
!   integer                        :: N_lambda_mrcc_pt2, N_lambda_mrcc_pt3, degree
!   integer               :: exc(N_int, 2)
!   histo = 0
!   
!   i_pert_count = 0
!   lambda_mrcc = 0.d0
!   N_lambda_mrcc_pt2 = 0
!   N_lambda_mrcc_pt3 = 0
!   lambda_mrcc_pt2(0) = 0
!   lambda_mrcc_pt3(0) = 0
!   
!   do ii=1, N_det_ref
!   do i=1,N_det_non_ref
!     call get_excitation(psi_ref(1,1,II), psi_non_ref(1,1,i), exc, degree, phase, N_int)
!     if(degree == -1) cycle
!     call i_H_j(psi_non_ref(1,1,ii),psi_non_ref(1,1,i),N_int,hii)
!     
!     
!   lambda_mrcc_pt2(0) = N_lambda_mrcc_pt2
!   lambda_mrcc_pt3(0) = N_lambda_mrcc_pt3
!   
!   print*,'N_det_non_ref = ',N_det_non_ref
!   print*,'psi_coef_ref_ratio = ',psi_ref_coef(2,1)/psi_ref_coef(1,1)
!   print*,'lambda max = ',maxval(dabs(lambda_mrcc))
!   print*,'Number of ignored determinants = ',i_pert_count  
! 
! END_PROVIDER


BEGIN_PROVIDER [ double precision, hij_mrcc, (N_det_non_ref,N_det_ref) ]
 implicit none
 BEGIN_DOC
 ! < ref | H | Non-ref > matrix
 END_DOC
 integer :: i_I, k_sd
  do i_I=1,N_det_ref
    do k_sd=1,N_det_non_ref
      call i_h_j(psi_ref(1,1,i_I),psi_non_ref(1,1,k_sd),N_int,hij_mrcc(k_sd,i_I))
    enddo
  enddo

END_PROVIDER

 BEGIN_PROVIDER [ double precision, delta_ij, (N_states,N_det_non_ref,N_det_ref) ]
&BEGIN_PROVIDER [ double precision, delta_ii, (N_states,N_det_ref) ]
 implicit none
 BEGIN_DOC
 ! Dressing matrix in N_det basis
 END_DOC
 integer :: i,j,m
 delta_ij = 0.d0
 delta_ii = 0.d0
 call H_apply_mrcc(delta_ij,delta_ii,N_states,N_det_non_ref,N_det_ref)

END_PROVIDER
             

BEGIN_PROVIDER [ double precision, h_matrix_dressed, (N_det,N_det,N_states) ]
 implicit none
 BEGIN_DOC
 ! Dressed H with Delta_ij
 END_DOC
 integer                        :: i, j,istate,ii,jj
 do istate = 1,N_states
   do j=1,N_det
     do i=1,N_det
       h_matrix_dressed(i,j,istate) = h_matrix_all_dets(i,j) 
     enddo
   enddo
   do ii = 1, N_det_ref
     i =idx_ref(ii)
     h_matrix_dressed(i,i,istate) += delta_ii(istate,ii)
    do jj = 1, N_det_non_ref
     j =idx_non_ref(jj)
     h_matrix_dressed(i,j,istate) += delta_ij(istate,jj,ii)
     h_matrix_dressed(j,i,istate) += delta_ij(istate,jj,ii)
    enddo
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
     
     integer                        :: istate
     istate = 1
     call davidson_diag_mrcc(psi_det,CI_eigenvectors_dressed,CI_electronic_energy_dressed,&
         size(CI_eigenvectors_dressed,1),N_det,N_states_diag,N_int,output_determinants,istate)
     
   else if (diag_algorithm == "Lapack") then
     
     double precision, allocatable  :: eigenvectors(:,:), eigenvalues(:)
     allocate (eigenvectors(size(H_matrix_dressed,1),N_det))
     allocate (eigenvalues(N_det))
     call lapack_diag(eigenvalues,eigenvectors,                      &
         H_matrix_dressed,size(H_matrix_dressed,1),N_det)
     CI_electronic_energy_dressed(:) = 0.d0
     do i=1,N_det
       CI_eigenvectors_dressed(i,1) = eigenvectors(i,1)
     enddo
     integer                        :: i_state
     double precision               :: s2
     i_state = 0
     if (s2_eig) then
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
     else
       do j=1,N_states_diag
         call get_s2_u0(psi_det,eigenvectors(1,j),N_det,N_det,s2)
         i_state += 1
         do i=1,N_det
           CI_eigenvectors_dressed(i,i_state) = eigenvectors(i,j)
         enddo
         CI_electronic_energy_dressed(i_state) = eigenvalues(j)
         CI_eigenvectors_s2_dressed(i_state) = s2
       enddo
     endif
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
  call write_time(output_determinants)
  do j=1,N_states_diag
    CI_energy_dressed(j) = CI_electronic_energy_dressed(j) + nuclear_repulsion
  enddo

END_PROVIDER

subroutine diagonalize_CI_dressed(lambda)
  implicit none
  BEGIN_DOC
!  Replace the coefficients of the CI states by the coefficients of the 
!  eigenstates of the CI matrix
  END_DOC
  double precision, intent(in) :: lambda
  integer :: i,j
  do j=1,N_states_diag
    do i=1,N_det
      psi_coef(i,j) = lambda * CI_eigenvectors_dressed(i,j) + (1.d0 - lambda) * psi_coef(i,j) 
    enddo
    call normalize(psi_coef(1,j), N_det)
  enddo
  SOFT_TOUCH psi_coef 

end


logical function is_generable(det1, det2, Nint)
  use bitmasks
  implicit none
  integer, intent(in) :: Nint
  integer(bit_kind) :: det1(Nint, 2), det2(Nint, 2)
  integer :: degree, f, exc(0:2, 2, 2), t
  integer*2 :: h1, h2, p1, p2, s1, s2
  integer, external :: searchExc
  logical, external :: excEq
  double precision :: phase
  
  is_generable = .false.
  call get_excitation(det1, det2, exc, degree, phase, Nint)
  if(degree == -1) return
  if(degree == 0) then
    is_generable = .true.
    return
  end if
  if(degree > 2) stop "?22??"
  !!!!!
!   call dec_exc(exc, h1, h2, p1, p2)
!   f = searchExc(toutmoun(1,1), (/h1, h2, p1, p2/), hh_shortcut(hh_shortcut(0)+1)-1)
!   !print *, toutmoun(:,1), hh_shortcut(hh_shortcut(0)+1)-1, (/h1, h2, p1, p2/)
!   if(f /= -1) then
!     is_generable = .true.
!     if(.not. excEq(toutmoun(1,f), (/h1, h2, p1, p2/))) stop "????"
!   end if
! !   print *, f
!   return
  
  call decode_exc_int2(exc,degree,h1,p1,h2,p2,s1,s2)
  
  if(degree == 1) then
    h2 = h1
    p2 = p1
    s2 = s1
    h1 = 0
    p1 = 0
    s1 = 0
  end if
  
  if(h1 + (s1-1)*mo_tot_num < h2 + (s2-1)*mo_tot_num) then
    f = searchExc(hh_exists(1,1), (/s1, h1, s2, h2/),  hh_shortcut(0))
  else
    f = searchExc(hh_exists(1,1), (/s2, h2, s1, h1/), hh_shortcut(0))
  end if
  if(f == -1) return
  
  if(p1 + (s1-1)*mo_tot_num < p2 + (s2-1)*mo_tot_num) then
    f = searchExc(pp_exists(1,hh_shortcut(f)), (/s1, p1, s2, p2/), hh_shortcut(f+1)-hh_shortcut(f))
  else
    f = searchExc(pp_exists(1,hh_shortcut(f)), (/s2, p2, s1, p1/), hh_shortcut(f+1)-hh_shortcut(f))
  end if
  
  if(f /= -1) is_generable = .true.
end function



integer function searchDet(dets, det, n, Nint)
  implicit none
  use bitmasks
  
  integer(bit_kind),intent(in) :: dets(Nint,2,n), det(Nint,2)
  integer, intent(in) :: nint, n
  integer :: l, h, c
  integer, external :: detCmp
  logical, external :: detEq

  l = 1
  h = n
  do while(.true.)
    searchDet = (l+h)/2
    c = detCmp(dets(1,1,searchDet), det(1,1), Nint)
    if(c == 0) return
    if(c == 1) then
      h = searchDet-1
    else
      l = searchDet+1
    end if
    if(l > h) then
      searchDet = -1
      return
    end if
    
  end do
end function


integer function unsortedSearchDet(dets, det, n, Nint)
  implicit none
  use bitmasks
  
  integer(bit_kind),intent(in) :: dets(Nint,2,n), det(Nint,2)
  integer, intent(in) :: nint, n
  integer :: l, h, c
  integer, external :: detCmp
  logical, external :: detEq

  do l=1, n
    if(detEq(det, dets(1,1,l), N_int)) then
      unsortedSearchDet = l
      return
    end if
  end do
  unsortedSearchDet = -1
end function


integer function searchExc(excs, exc, n)
  implicit none
  use bitmasks
  
  integer, intent(in) :: n
  integer*2,intent(in) :: excs(4,n), exc(4)
  integer :: l, h, c
  integer, external :: excCmp
  logical, external :: excEq

  l = 1
  h = n
  do
    searchExc = (l+h)/2
    c = excCmp(excs(1,searchExc), exc(1))
    if(c == 0) return
    if(c == 1) then
      h = searchExc-1
    else
      l = searchExc+1
    end if
    if(l > h) then
      searchExc = -1
      return
    end if
  end do
end function


subroutine sort_det(key, idx, N_key, Nint)
  implicit none
  

  integer, intent(in)                   :: Nint, N_key
  integer(8),intent(inout)       :: key(Nint,2,N_key)
  integer,intent(inout)                   :: idx(N_key)
  integer(8)                     :: tmp(Nint, 2)
  integer                               :: tmpidx,i,ni
  
  do i=1,N_key
    idx(i) = i
  end do
  
  do i=N_key/2,1,-1
    call tamiser(key, idx, i, N_key, Nint, N_key)
  end do
  
  do i=N_key,2,-1
    do ni=1,Nint
      tmp(ni,1) = key(ni,1,i)
      tmp(ni,2) = key(ni,2,i)
      key(ni,1,i) = key(ni,1,1)
      key(ni,2,i) = key(ni,2,1)
      key(ni,1,1) = tmp(ni,1)
      key(ni,2,1) = tmp(ni,2)
    enddo

    tmpidx = idx(i)
    idx(i) = idx(1)
    idx(1) = tmpidx
    call tamiser(key, idx, 1, i-1, Nint, N_key)
  end do
end subroutine 


subroutine sort_exc(key, N_key)
  implicit none
  

  integer, intent(in)                   :: N_key
  integer*2,intent(inout)       :: key(4,N_key)
  integer*2                     :: tmp(4)
  integer                               :: i,ni
  
  
  do i=N_key/2,1,-1
    call tamise_exc(key, i, N_key, N_key)
  end do
  
  do i=N_key,2,-1
    do ni=1,4
      tmp(ni) = key(ni,i)
      key(ni,i) = key(ni,1)
      key(ni,1) = tmp(ni)
    enddo

    call tamise_exc(key, 1, i-1, N_key)
  end do
end subroutine 


logical function exc_inf(exc1, exc2)
  implicit none
  integer*2,intent(in) :: exc1(4), exc2(4)
  integer :: i
  exc_inf = .false.
  do i=1,4
    if(exc1(i) < exc2(i)) then
      exc_inf = .true.
      return
    else if(exc1(i) > exc2(i)) then
      return
    end if
  end do
end function


subroutine tamise_exc(key, no, n, N_key)
  use bitmasks
  implicit none
  
  BEGIN_DOC
! Uncodumented : TODO
  END_DOC
  integer,intent(in)                    :: no, n, N_key
  integer*2,intent(inout)       :: key(4, N_key)
  integer                               :: k,j
  integer*2                    :: tmp(4)
  logical                               :: exc_inf
  integer                               :: ni
  
  k = no
  j = 2*k
  do while(j <= n)
    if(j < n) then
      if (exc_inf(key(1,j), key(1,j+1))) then
        j = j+1
      endif
    endif
    if(exc_inf(key(1,k), key(1,j))) then
      do ni=1,4
        tmp(ni)   = key(ni,k)
        key(ni,k) = key(ni,j)
        key(ni,j) = tmp(ni)
      enddo
      k = j
      j = k+k
    else
      return
    endif
  enddo
end subroutine


subroutine dec_exc(exc, h1, h2, p1, p2)
  implicit none
  integer :: exc(0:2,2,2), s1, s2, degree
  integer*2, intent(out) :: h1, h2, p1, p2
  
  degree = exc(0,1,1) + exc(0,1,2)
  
  h1 = 0
  h2 = 0
  p1 = 0
  p2 = 0
    
  if(degree == 0) return
  
  call decode_exc_int2(exc, degree, h1, p1, h2, p2, s1, s2)
  
  h1 += mo_tot_num * (s1-1)
  p1 += mo_tot_num * (s1-1)
  
  if(degree == 2) then
    h2 += mo_tot_num * (s2-1)
    p2 += mo_tot_num * (s2-1)
    if(h1 > h2) then
      s1 = h1
      h1 = h2
      h2 = s1
    end if
    if(p1 > p2) then
      s1 = p1
      p1 = p2
      p2 = s1
    end if
  else
    h2 = h1
    p2 = p1
    p1 = 0
    h1 = 0
  end if
end subroutine


 BEGIN_PROVIDER [ integer, N_hh_exists ]
&BEGIN_PROVIDER [ integer, N_pp_exists ]
&BEGIN_PROVIDER [ integer, N_ex_exists ]
  implicit none
  integer :: exc(0:2, 2, 2), degree, n, on, s, l, i
  integer*2 :: h1, h2, p1, p2
  double precision :: phase
  logical,allocatable :: hh(:,:) , pp(:,:)
  
  allocate(hh(0:mo_tot_num*2, 0:mo_tot_num*2))
  allocate(pp(0:mo_tot_num*2, 0:mo_tot_num*2))
  hh = .false.
  pp = .false.
  N_hh_exists = 0
  N_pp_exists = 0
  N_ex_exists = 0

  n = 0
  do i=1, N_det_ref
    do l=1, N_det_non_ref
      call get_excitation(psi_ref(1,1,i), psi_non_ref(1,1,l), exc, degree, phase, N_int)
      if(degree == -1) cycle
      call dec_exc(exc, h1, h2, p1, p2)
      N_ex_exists += 1
      if(.not. hh(h1,h2)) N_hh_exists = N_hh_exists + 1
      if(.not. pp(p1,p2)) N_pp_exists = N_pp_exists + 1
      hh(h1,h2) = .true.
      pp(p1,p2) = .true.
    end do
  end do
  N_pp_exists = min(N_ex_exists, N_pp_exists * N_hh_exists)
END_PROVIDER



BEGIN_PROVIDER [ double precision, dIj, (hh_shortcut(hh_shortcut(0)+1)-1) ]
  implicit none
  logical :: ok
  integer :: i, j, k, II, pp, hh, ind, wk, nex
  integer, external :: unsortedSearchDet
  integer(bit_kind) :: myDet(N_int, 2), myMask(N_int, 2)
  double precision, allocatable :: A(:,:)
  integer :: N, IPIV(N_det_non_ref), INFO
  double precision, allocatable :: WORK(:)
  integer, allocatable :: IWORK(:)
  
  nex = hh_shortcut(hh_shortcut(0)+1)-1
  print *, "TI", nex, N_det_non_ref
  allocate(A(N_det_non_ref, nex))
  A = 0d0
  do II = 1, N_det_ref
    do hh = 1, hh_shortcut(0)
      call apply_hole(psi_ref(1,1,II), hh_exists(1, hh), myMask, ok, N_int)
      if(.not. ok) cycle
      do pp = hh_shortcut(hh), hh_shortcut(hh+1)-1
        call apply_particle(myMask, pp_exists(1, pp), myDet, ok, N_int)
        if(.not. ok) cycle
        ind = unsortedSearchDet(psi_non_ref(1,1,1), myDet, N_det_non_ref, N_int)
        if(ind /= -1) then
          A(ind, pp) += psi_ref_coef(II, 1)
        end if
      end do
    end do
  end do
  
  double precision, allocatable :: IAtA(:,:), AtB(:), X(:), X_new(:)
  double precision :: norm
  allocate(IAtA(nex, nex), AtB(nex), X(nex), X_new(nex))
  print *, "allocated", size(IAtA, 1), size(A, 2)
  !IAtA = -matmul(transpose(A), A)
  
  IAtA = 0.d0
  do i=1, size(A,2)
    IAtA(i,i) = 1d0
  end do
  call dgemm('T','N',nex,nex,N_det_non_ref,1.d0,A,size(A,1),A,size(A,1),-1.d0,IAtA,size(IAtA,1))
  IaTa = -IATa

  call dgemv('T',N_det_non_ref,nex,1.d0,A,size(A,1),psi_non_ref_coef(1,1),1,0.d0,AtB,1)

  !AtB = matmul(transpose(A), psi_non_ref_coef(:,1))

  X = AtB
  do k=1, 1000
    !X_new = matmul(IAtA, X) + AtB
  x_new = AtB
  call dgemv('N',nex,nex,1.d0,IAtA,size(IAtA,1),X,1,1.d0,x_new,1)
    norm = 0d0
    do j=1, size(X)
      norm += (X_new(j) - X(j))**2
      X(j) = X_new(j)
    end do
    print *, "resudu ", norm
  end do
  dIj = X
  print *, "done"
END_PROVIDER


double precision function get_dij(det1, det2, Nint)
  use bitmasks
  implicit none
  integer, intent(in) :: Nint
  integer(bit_kind) :: det1(Nint, 2), det2(Nint, 2)
  integer :: degree, f, exc(0:2, 2, 2), t
  integer*2 :: h1, h2, p1, p2, s1, s2
  integer, external :: searchExc
  logical, external :: excEq
  double precision :: phase
  
  get_dij = 0d0
  call get_excitation(det1, det2, exc, degree, phase, Nint)
  if(degree == -1) return
  if(degree == 0) then
    stop "get_dij"
  end if
  
  call decode_exc_int2(exc,degree,h1,p1,h2,p2,s1,s2)
  
  if(degree == 1) then
    h2 = h1
    p2 = p1
    s2 = s1
    h1 = 0
    p1 = 0
    s1 = 0
  end if
  
  if(h1 + (s1-1)*mo_tot_num < h2 + (s2-1)*mo_tot_num) then
    f = searchExc(hh_exists(1,1), (/s1, h1, s2, h2/),  hh_shortcut(0))
  else
    f = searchExc(hh_exists(1,1), (/s2, h2, s1, h1/), hh_shortcut(0))
  end if
  if(f == -1) return
  
  if(p1 + (s1-1)*mo_tot_num < p2 + (s2-1)*mo_tot_num) then
    t = searchExc(pp_exists(1,hh_shortcut(f)), (/s1, p1, s2, p2/), hh_shortcut(f+1)-hh_shortcut(f))
  else
    t = searchExc(pp_exists(1,hh_shortcut(f)), (/s2, p2, s1, p1/), hh_shortcut(f+1)-hh_shortcut(f))
  end if
  
  if(t /= -1) then
    get_dij = dIj(t - 1 + hh_shortcut(f))
  end if
end function


 BEGIN_PROVIDER [ integer*2, hh_exists, (4, N_hh_exists) ]
&BEGIN_PROVIDER [ integer, hh_shortcut, (0:N_hh_exists + 1) ]
&BEGIN_PROVIDER [ integer*2, pp_exists, (4, N_pp_exists) ]
  implicit none
  integer*2,allocatable :: num(:,:)
  integer :: exc(0:2, 2, 2), degree, n, on, s, l, i
  integer*2 :: h1, h2, p1, p2
  double precision :: phase
  logical, external :: excEq
  
  allocate(num(4, N_ex_exists+1))
  
  hh_shortcut = 0
  hh_exists = 0
  pp_exists = 0
  num = 0
  
  n = 0
  do i=1, N_det_ref
    do l=1, N_det_non_ref
      call get_excitation(psi_ref(1,1,i), psi_non_ref(1,1,l), exc, degree, phase, N_int)
      if(degree == -1) cycle
      call dec_exc(exc, h1, h2, p1, p2)
      n += 1
      num(:, n) = (/h1, h2, p1, p2/)
    end do
  end do
  
  call sort_exc(num, n)
  
  hh_shortcut(0) = 1
  hh_shortcut(1) = 1
  hh_exists(:,1) = (/1_2, num(1,1), 1_2, num(2,1)/)
  pp_exists(:,1) = (/1_2, num(3,1), 1_2, num(4,1)/)
  s = 1
  do i=2,n
    if(.not. excEq(num(1,i), num(1,s))) then
      s += 1
      num(:, s) = num(:, i)
      pp_exists(:,s) = (/1_2, num(3,s), 1_2, num(4,s)/)
      if(hh_exists(2, hh_shortcut(0)) /= num(1,s) .or. &
            hh_exists(4, hh_shortcut(0)) /= num(2,s)) then
        hh_shortcut(0) += 1
        hh_shortcut(hh_shortcut(0)) = s
        hh_exists(:,hh_shortcut(0)) = (/1_2, num(1,s), 1_2, num(2,s)/)
      end if
    end if
  end do
  hh_shortcut(hh_shortcut(0)+1) = s+1
  
  do s=2,4,2
    do i=1,hh_shortcut(0)
      if(hh_exists(s, i) == 0) then
        hh_exists(s-1, i) = 0
      else if(hh_exists(s, i) > mo_tot_num) then
        hh_exists(s, i) -= mo_tot_num
        hh_exists(s-1, i) = 2
      end if
    end do
    
    do i=1,hh_shortcut(hh_shortcut(0)+1)-1
      if(pp_exists(s, i) == 0) then
        pp_exists(s-1, i) = 0
      else if(pp_exists(s, i) > mo_tot_num) then
        pp_exists(s, i) -= mo_tot_num
        pp_exists(s-1, i) = 2
      end if
    end do
  end do
END_PROVIDER


logical function excEq(exc1, exc2)
  implicit none
  integer*2, intent(in) :: exc1(4), exc2(4)
  integer :: i
  excEq = .false.
  do i=1, 4
    if(exc1(i) /= exc2(i)) return
  end do
  excEq = .true.
end function


integer function excCmp(exc1, exc2)
  implicit none
  integer*2, intent(in) :: exc1(4), exc2(4)
  integer :: i
  excCmp = 0
  do i=1, 4
    if(exc1(i) > exc2(i)) then
      excCmp = 1
      return
    else if(exc1(i) < exc2(i)) then
      excCmp = -1
      return
    end if
  end do
end function


subroutine apply_hole(det, exc, res, ok, Nint)
  use bitmasks
  implicit none
  integer, intent(in) :: Nint
  integer*2, intent(in) :: exc(4)
  integer*2 :: s1, s2, h1, h2
  integer(bit_kind),intent(in) :: det(Nint, 2)
  integer(bit_kind),intent(out) :: res(Nint, 2)
  logical, intent(out) :: ok
  integer :: ii, pos
  
  ok = .false.
  s1 = exc(1)
  h1 = exc(2)
  s2 = exc(3)
  h2 = exc(4)
  res = det
  
  if(h1 /= 0) then
  ii = (h1-1)/bit_kind_size + 1 
  pos = mod(h1-1, 64)!iand(h1-1,bit_kind_size-1) ! mod 64
  if(iand(det(ii, s1), ishft(1_bit_kind, pos)) == 0_8) return
  res(ii, s1) = ibclr(res(ii, s1), pos)
  end if
  
    ii = (h2-1)/bit_kind_size + 1 
    pos = mod(h2-1, 64)!iand(h2-1,bit_kind_size-1)
    if(iand(det(ii, s2), ishft(1_bit_kind, pos)) == 0_8) return
    res(ii, s2) = ibclr(res(ii, s2), pos)

  
  ok = .true.
end subroutine


subroutine apply_particle(det, exc, res, ok, Nint)
  use bitmasks
  implicit none
  integer, intent(in) :: Nint
  integer*2, intent(in) :: exc(4)
  integer*2 :: s1, s2, p1, p2
  integer(bit_kind),intent(in) :: det(Nint, 2)
  integer(bit_kind),intent(out) :: res(Nint, 2)
  logical, intent(out) :: ok
  integer :: ii, pos 
  
  ok = .false.
  s1 = exc(1)
  p1 = exc(2)
  s2 = exc(3)
  p2 = exc(4)
  res = det 
  
  if(p1 /= 0) then
  ii = (p1-1)/bit_kind_size + 1 
  pos = mod(p1-1, 64)!iand(p1-1,bit_kind_size-1)
  if(iand(det(ii, s1), ishft(1_bit_kind, pos)) /= 0_8) return
  res(ii, s1) = ibset(res(ii, s1), pos)
  end if

    ii = (p2-1)/bit_kind_size + 1 
    pos = mod(p2-1, 64)!iand(p2-1,bit_kind_size-1)
    if(iand(det(ii, s2), ishft(1_bit_kind, pos)) /= 0_8) return
    res(ii, s2) = ibset(res(ii, s2), pos)

  
  ok = .true.
end subroutine


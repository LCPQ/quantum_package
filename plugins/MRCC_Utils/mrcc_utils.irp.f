use bitmasks

 BEGIN_PROVIDER [ integer, mrmode ]
  mrmode = 0
END_PROVIDER
 
 
 BEGIN_PROVIDER [ double precision, lambda_mrcc, (N_states, N_det_non_ref) ]
&BEGIN_PROVIDER [ integer, lambda_mrcc_pt2, (0:psi_det_size) ]
&BEGIN_PROVIDER [ integer, lambda_mrcc_kept, (0:psi_det_size) ]
  implicit none
  BEGIN_DOC
  ! cm/<Psi_0|H|D_m> or perturbative 1/Delta_E(m)
  END_DOC
  integer :: i,k
  double precision               :: ihpsi_current(N_states)
  integer                        :: i_pert_count
  double precision               :: hii, lambda_pert
  integer                        :: N_lambda_mrcc_pt2, N_lambda_mrcc_pt3
  
  i_pert_count = 0
  lambda_mrcc = 0.d0
  N_lambda_mrcc_pt2 = 0
  N_lambda_mrcc_pt3 = 0
  lambda_mrcc_pt2(0) = 0
  lambda_mrcc_kept(0) = 0

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
        ! Ignore lamdba
        i_pert_count += 1
        lambda_mrcc(k,i) = 0.d0
        if (lambda_mrcc_pt2(N_lambda_mrcc_pt2) /= i) then
          N_lambda_mrcc_pt2 += 1
          lambda_mrcc_pt2(N_lambda_mrcc_pt2) = i
        endif
      else
        ! Keep lamdba
        if (lambda_mrcc_kept(N_lambda_mrcc_pt3) /= i) then
          N_lambda_mrcc_pt3 += 1
          lambda_mrcc_kept(N_lambda_mrcc_pt3) = i
        endif
      endif
    enddo
  enddo
  lambda_mrcc_pt2(0) = N_lambda_mrcc_pt2
  lambda_mrcc_kept(0) = N_lambda_mrcc_pt3
  print*,'N_det_non_ref = ',N_det_non_ref
  print*,'psi_coef_ref_ratio = ',psi_ref_coef(2,1)/psi_ref_coef(1,1)
  print*,'lambda max = ',maxval(dabs(lambda_mrcc))
  print*,'Number of ignored determinants = ',i_pert_count  

END_PROVIDER



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

! BEGIN_PROVIDER [ double precision, delta_ij, (N_states,N_det_non_ref,N_det_ref) ]
!&BEGIN_PROVIDER [ double precision, delta_ii, (N_states,N_det_ref) ]
! implicit none
! BEGIN_DOC
! ! Dressing matrix in N_det basis
! END_DOC
! integer :: i,j,m
! delta_ij = 0.d0
! delta_ii = 0.d0
! call H_apply_mrcc(delta_ij,delta_ii,N_states,N_det_non_ref,N_det_ref)
!
!END_PROVIDER
             

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
   ! Eigenvectors/values of the dressed CI matrix
   END_DOC
   double precision               :: ovrlp,u_dot_v
   integer                        :: i_good_state
   integer, allocatable           :: index_good_state_array(:)
   logical, allocatable           :: good_state_array(:)
   double precision, allocatable  :: s2_values_tmp(:)
   integer                        :: i_other_state
   double precision, allocatable  :: eigenvectors(:,:), eigenvalues(:)
   integer                        :: i_state
   double precision               :: e_0
   integer                        :: i,j,k
   double precision, allocatable  :: s2_eigvalues(:)
   double precision, allocatable  :: e_array(:)
   integer, allocatable           :: iorder(:)

   integer :: mrcc_state 
   
   do j=1,min(N_states,N_det)
     do i=1,N_det
       CI_eigenvectors_dressed(i,j) = psi_coef(i,j)
     enddo
   enddo
   
   if (diag_algorithm == "Davidson") then
     
     allocate (eigenvectors(size(CI_eigenvectors_dressed,1),size(CI_eigenvectors_dressed,2)), &
     eigenvalues(size(CI_electronic_energy_dressed,1)))
     do j=1,min(N_states,N_det)
       do i=1,N_det
         eigenvectors(i,j) = psi_coef(i,j)
       enddo
     enddo
     do mrcc_state=1,N_states
      do j=mrcc_state,min(N_states,N_det)
        do i=1,N_det
          eigenvectors(i,j) = psi_coef(i,j)
        enddo
      enddo
      call davidson_diag_mrcc_HS2(psi_det,eigenvectors,&
            size(eigenvectors,1), &
            eigenvalues,N_det,N_states,N_states_diag,N_int, &
            output_determinants,mrcc_state)
      CI_eigenvectors_dressed(1:N_det,mrcc_state) = eigenvectors(1:N_det,mrcc_state)
      CI_electronic_energy_dressed(mrcc_state) = eigenvalues(mrcc_state)
   enddo
   do k=N_states+1,N_states_diag
     CI_eigenvectors_dressed(1:N_det,k) = eigenvectors(1:N_det,k)
     CI_electronic_energy_dressed(k) = eigenvalues(k)
   enddo
   call u_0_S2_u_0(CI_eigenvectors_s2_dressed,CI_eigenvectors_dressed,N_det,psi_det,N_int,&
          N_states_diag,size(CI_eigenvectors_dressed,1))
   deallocate (eigenvectors,eigenvalues)

     
   else if (diag_algorithm == "Lapack") then
     
     allocate (eigenvectors(size(H_matrix_dressed,1),N_det))
     allocate (eigenvalues(N_det))
     call lapack_diag(eigenvalues,eigenvectors,                      &
         H_matrix_dressed,size(H_matrix_dressed,1),N_det)
     CI_electronic_energy_dressed(:) = 0.d0
     if (s2_eig) then
       i_state = 0
       allocate (s2_eigvalues(N_det))
       allocate(index_good_state_array(N_det),good_state_array(N_det))
       good_state_array = .False.
       call u_0_S2_u_0(s2_eigvalues,eigenvectors,N_det,psi_det,N_int,&
         N_det,size(eigenvectors,1))
       do j=1,N_det
         ! Select at least n_states states with S^2 values closed to "expected_s2"
         if(dabs(s2_eigvalues(j)-expected_s2).le.0.5d0)then
           i_state += 1
           index_good_state_array(i_state) = j
           good_state_array(j) = .True.
         endif
         if (i_state==N_states) then
           exit
         endif
       enddo
       if (i_state /= 0) then
         ! Fill the first "i_state" states that have a correct S^2 value
         do j = 1, i_state
           do i=1,N_det
             CI_eigenvectors_dressed(i,j) = eigenvectors(i,index_good_state_array(j))
           enddo
           CI_electronic_energy_dressed(j) = eigenvalues(index_good_state_array(j))
           CI_eigenvectors_s2_dressed(j) = s2_eigvalues(index_good_state_array(j))
         enddo
         i_other_state = 0
         do j = 1, N_det
           if(good_state_array(j))cycle
           i_other_state +=1
           if(i_state+i_other_state.gt.n_states_diag)then
             exit
           endif
           do i=1,N_det
             CI_eigenvectors_dressed(i,i_state+i_other_state) = eigenvectors(i,j)
           enddo
           CI_electronic_energy_dressed(i_state+i_other_state) = eigenvalues(j)
           CI_eigenvectors_s2_dressed(i_state+i_other_state) = s2_eigvalues(i_state+i_other_state)
         enddo
         
       else
         print*,''
         print*,'!!!!!!!!   WARNING  !!!!!!!!!'
         print*,'  Within the ',N_det,'determinants selected'
         print*,'  and the ',N_states_diag,'states requested'
         print*,'  We did not find any state with S^2 values close to ',expected_s2
         print*,'  We will then set the first N_states eigenvectors of the H matrix'
         print*,'  as the CI_eigenvectors_dressed'
         print*,'  You should consider more states and maybe ask for s2_eig to be .True. or just enlarge the CI space'
         print*,''
         do j=1,min(N_states_diag,N_det)
           do i=1,N_det
             CI_eigenvectors_dressed(i,j) = eigenvectors(i,j)
           enddo
           CI_electronic_energy_dressed(j) = eigenvalues(j)
           CI_eigenvectors_s2_dressed(j) = s2_eigvalues(j)
         enddo
       endif
       deallocate(index_good_state_array,good_state_array)
       deallocate(s2_eigvalues)
     else
       call u_0_S2_u_0(CI_eigenvectors_s2_dressed,eigenvectors,N_det,psi_det,N_int,&
          min(N_det,N_states_diag),size(eigenvectors,1))
       ! Select the "N_states_diag" states of lowest energy
       do j=1,min(N_det,N_states_diag)
         do i=1,N_det
           CI_eigenvectors_dressed(i,j) = eigenvectors(i,j)
         enddo
         CI_electronic_energy_dressed(j) = eigenvalues(j)
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
  do j=1,min(N_det,N_states)
    write(st,'(I4)') j
    CI_energy_dressed(j) = CI_electronic_energy_dressed(j) + nuclear_repulsion
    call write_double(output_determinants,CI_energy_dressed(j),'Energy of state '//trim(st))
    call write_double(output_determinants,CI_eigenvectors_s2_dressed(j),'S^2 of state '//trim(st))
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
  do j=1,N_states
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
  integer*2 :: tmp_array(4)
  
  is_generable = .false.
  call get_excitation(det1, det2, exc, degree, phase, Nint)
  if(degree == -1) return
  if(degree == 0) then
    is_generable = .true.
    return
  end if
  if(degree > 2) stop "?22??"
  
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
    tmp_array = (/s1, h1, s2, h2/)
  else
    tmp_array = (/s2, h2, s1, h1/)
  end if
  f = searchExc(hh_exists(1,1), tmp_array,  hh_shortcut(0))

  if(p1 + (s1-1)*mo_tot_num < p2 + (s2-1)*mo_tot_num) then
    tmp_array = (/s1, p1, s2, p2/)
  else
    tmp_array = (/s2, p2, s1, p1/)
  end if
  if (f /= -1) then
    f = searchExc(pp_exists(1,hh_shortcut(f)), tmp_array, hh_shortcut(f+1)-hh_shortcut(f))
  endif

  is_generable = (f /= -1) 
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
    if(c == 0) then
      return
    else if(c == 1) then
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
  integer,intent(in)            :: no, n, N_key
  integer*2,intent(inout)       :: key(4, N_key)
  integer                       :: k,j
  integer*2                     :: tmp(4)
  logical                       :: exc_inf
  integer                       :: ni
  
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
  !TODO Openmp
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



 BEGIN_PROVIDER [ integer(bit_kind), psi_non_ref_sorted, (N_int, 2, N_det_non_ref) ]
&BEGIN_PROVIDER [ integer, psi_non_ref_sorted_idx, (N_det_non_ref) ]
  implicit none
  psi_non_ref_sorted = psi_non_ref
  call sort_det(psi_non_ref_sorted, psi_non_ref_sorted_idx, N_det_non_ref, N_int)
END_PROVIDER


 BEGIN_PROVIDER [ double precision, dIj_unique, (hh_nex, N_states) ]
&BEGIN_PROVIDER [ double precision, rho_mrcc, (N_det_non_ref, N_states) ]
  implicit none
  logical                        :: ok
  integer                        :: i, j, k, s, II, pp, ppp, hh, ind, wk, a_col, at_row
  integer, external              :: searchDet, unsortedSearchDet
  integer(bit_kind)              :: myDet(N_int, 2), myMask(N_int, 2)
  integer                        :: N, INFO, r1, r2
  double precision , allocatable :: AtB(:), x(:), x_new(:), A_val_mwen(:,:), t(:)
  double precision               :: norm, cx, res
  integer, allocatable           :: lref(:), A_ind_mwen(:)
  double precision               :: phase
  
  
  double precision, allocatable :: rho_mrcc_init(:)
  integer                        :: a_coll, at_roww
  
  print *, "TI", hh_nex, N_det_non_ref

  allocate(rho_mrcc_init(N_det_non_ref))
  allocate(x_new(hh_nex))
  allocate(x(hh_nex), AtB(hh_nex))

  do s=1,N_states

    AtB(:) = 0.d0
    !$OMP PARALLEL default(none) shared(k, psi_non_ref_coef, active_excitation_to_determinants_idx,&
        !$OMP   active_excitation_to_determinants_val, N_det_ref, hh_nex, N_det_non_ref)          &
        !$OMP private(at_row, a_col, i, j, r1, r2, wk, A_ind_mwen, A_val_mwen, a_coll, at_roww)&
        !$OMP shared(N_states,mrcc_col_shortcut, mrcc_N_col, AtB, mrcc_AtA_val, mrcc_AtA_ind, s, n_exc_active, active_pp_idx)
    
    !$OMP DO schedule(dynamic, 100)
    do at_roww = 1, n_exc_active ! hh_nex
      at_row = active_pp_idx(at_roww)
      do i=1,active_excitation_to_determinants_idx(0,at_roww)
          AtB(at_row) = AtB(at_row) + psi_non_ref_coef(active_excitation_to_determinants_idx(i, at_roww), s) * active_excitation_to_determinants_val(s,i, at_roww)
      end do
    end do
    !$OMP END DO
   
    !$OMP END PARALLEL

    X(:) = 0d0
    
    
    do a_coll = 1, n_exc_active
      a_col = active_pp_idx(a_coll)
      X(a_col) = AtB(a_col)
    end do
    
    rho_mrcc_init = 0d0
    
    allocate(lref(N_det_ref))
    do hh = 1, hh_shortcut(0)
      do pp = hh_shortcut(hh), hh_shortcut(hh+1)-1
        if(is_active_exc(pp)) cycle
        lref = 0
        AtB(pp) = 0.d0
        do II=1,N_det_ref
          call apply_hole_local(psi_ref(1,1,II), hh_exists(1, hh), myMask, ok, N_int)
          if(.not. ok) cycle
          call apply_particle_local(myMask, pp_exists(1, pp), myDet, ok, N_int)
          if(.not. ok) cycle
          ind = searchDet(psi_non_ref_sorted(1,1,1), myDet(1,1), N_det_non_ref, N_int)
          if(ind == -1) cycle
          ind = psi_non_ref_sorted_idx(ind)
          call get_phase(myDet(1,1), psi_ref(1,1,II), phase, N_int)
          AtB(pp) += psi_non_ref_coef(ind, s) * psi_ref_coef(II, s) * phase
          lref(II) = ind
          if(phase < 0.d0) lref(II) = -ind
        end do
        X(pp) =  AtB(pp) 
        do II=1,N_det_ref
          if(lref(II) > 0) then
            rho_mrcc_init(lref(II)) = psi_ref_coef(II,s) * X(pp)
          else if(lref(II) < 0) then
            rho_mrcc_init(-lref(II)) = -psi_ref_coef(II,s) * X(pp)
          end if
        end do
      end do
    end do
    deallocate(lref)

    x_new = x
    
    double precision               :: factor, resold
    factor = 1.d0
    resold = huge(1.d0)

    do k=0,10*hh_nex
      !$OMP PARALLEL default(shared) private(cx, i, a_col, a_coll)
      
      !$OMP DO
      do i=1,N_det_non_ref
        rho_mrcc(i,s) = rho_mrcc_init(i) 
      enddo
      !$OMP END DO
      
      !$OMP DO
      do a_coll = 1, n_exc_active
        a_col = active_pp_idx(a_coll)
        cx = 0.d0
        do i=mrcc_col_shortcut(a_coll), mrcc_col_shortcut(a_coll) + mrcc_N_col(a_coll) - 1
          cx = cx + x(mrcc_AtA_ind(i)) * mrcc_AtA_val(s,i)
        end do
        x_new(a_col) = AtB(a_col) + cx * factor
      end do
      !$OMP END DO

      !$OMP END PARALLEL
      
      
      res = 0.d0
      do a_coll=1,n_exc_active
        a_col = active_pp_idx(a_coll)
        do j=1,N_det_non_ref
          i = active_excitation_to_determinants_idx(j,a_coll)
          if (i==0) exit
          rho_mrcc(i,s) = rho_mrcc(i,s) + active_excitation_to_determinants_val(s,j,a_coll) * X_new(a_col)
        enddo
        res = res + (X_new(a_col) - X(a_col))*(X_new(a_col) - X(a_col))
        X(a_col) = X_new(a_col)
      end do
      if (res > resold) then
        factor = factor * 0.5d0
      endif
      resold = res
      
      if(iand(k, 4095) == 0) then
        print *, "res ", k, res
      end if
      
      if(res < 1d-10) exit
    end do
    
    norm = 0.d0
    do i=1,N_det_non_ref
      norm = norm + rho_mrcc(i,s)*rho_mrcc(i,s)
    enddo
    ! Norm now contains the norm of A.X
    
    do i=1,N_det_ref
      norm = norm + psi_ref_coef(i,s)*psi_ref_coef(i,s)
    enddo
    ! Norm now contains the norm of Psi + A.X
    
    print *, k, "res : ", res, "norm : ", sqrt(norm)
        
!---------------
! double precision               :: e_0, overlap
! double precision, allocatable  :: u_0(:)
! integer(bit_kind), allocatable :: keys_tmp(:,:,:)
! allocate (u_0(N_det), keys_tmp(N_int,2,N_det) )
! k=0
! overlap = 0.d0
! do i=1,N_det_ref
!   k = k+1
!   u_0(k) = psi_ref_coef(i,1)
!   keys_tmp(:,:,k) = psi_ref(:,:,i)
!   overlap += u_0(k)*psi_ref_coef(i,1)
! enddo
! norm = 0.d0
! do i=1,N_det_non_ref
!   k = k+1
!   u_0(k) = psi_non_ref_coef(i,1)
!   keys_tmp(:,:,k) = psi_non_ref(:,:,i)
!   overlap += u_0(k)*psi_non_ref_coef(i,1)
! enddo
! 
! call u_0_H_u_0(e_0,u_0,N_det,keys_tmp,N_int,1,N_det)
! print *,  'Energy of |Psi_CASSD> : ', e_0 + nuclear_repulsion, overlap
!
! k=0
! overlap = 0.d0
! do i=1,N_det_ref
!   k = k+1
!   u_0(k) = psi_ref_coef(i,1)
!   keys_tmp(:,:,k) = psi_ref(:,:,i)
!   overlap += u_0(k)*psi_ref_coef(i,1)
! enddo
! norm = 0.d0
! do i=1,N_det_non_ref
!   k = k+1
!   ! f is such that f.\tilde{c_i} = c_i
!   f = psi_non_ref_coef(i,1) / rho_mrcc(i,1)
!   
!   ! Avoid numerical instabilities
!   f = min(f,2.d0)
!   f = max(f,-2.d0)
!
!   f = 1.d0
!
!   u_0(k) = rho_mrcc(i,1)*f
!   keys_tmp(:,:,k) = psi_non_ref(:,:,i)
!   norm += u_0(k)**2
!   overlap += u_0(k)*psi_non_ref_coef(i,1)
! enddo
! 
! call u_0_H_u_0(e_0,u_0,N_det,keys_tmp,N_int,1,N_det)
! print *,  'Energy of |(1+T)Psi_0> : ', e_0 + nuclear_repulsion, overlap
!
! f = 1.d0/norm
! norm = 1.d0
! do i=1,N_det_ref
!  norm = norm - psi_ref_coef(i,s)*psi_ref_coef(i,s)
! enddo
! f = dsqrt(f*norm)
! overlap = norm
! do i=1,N_det_non_ref
!   u_0(k) = rho_mrcc(i,1)*f
!   overlap += u_0(k)*psi_non_ref_coef(i,1)
! enddo
!
! call u_0_H_u_0(e_0,u_0,N_det,keys_tmp,N_int,1,N_det)
! print *,  'Energy of |(1+T)Psi_0> (normalized) : ', e_0 + nuclear_repulsion,  overlap
!
! k=0
! overlap = 0.d0
! do i=1,N_det_ref
!   k = k+1
!   u_0(k) = psi_ref_coef(i,1)
!   keys_tmp(:,:,k) = psi_ref(:,:,i)
!   overlap += u_0(k)*psi_ref_coef(i,1)
! enddo
! norm = 0.d0
! do i=1,N_det_non_ref
!   k = k+1
!   ! f is such that f.\tilde{c_i} = c_i
!   f = psi_non_ref_coef(i,1) / rho_mrcc(i,1)
!   
!   ! Avoid numerical instabilities
!   f = min(f,2.d0)
!   f = max(f,-2.d0)
!
!   u_0(k) = rho_mrcc(i,1)*f
!   keys_tmp(:,:,k) = psi_non_ref(:,:,i)
!   norm += u_0(k)**2
!   overlap += u_0(k)*psi_non_ref_coef(i,1)
! enddo
! 
! call u_0_H_u_0(e_0,u_0,N_det,keys_tmp,N_int,1,N_det)
! print *,  'Energy of |(1+T)Psi_0> (mu_i): ', e_0 + nuclear_repulsion, overlap
!
! f = 1.d0/norm
! norm = 1.d0
! do i=1,N_det_ref
!  norm = norm - psi_ref_coef(i,s)*psi_ref_coef(i,s)
! enddo
! overlap = norm
! f = dsqrt(f*norm)
! do i=1,N_det_non_ref
!   u_0(k) = rho_mrcc(i,1)*f
!   overlap += u_0(k)*psi_non_ref_coef(i,1)
! enddo
!
! call u_0_H_u_0(e_0,u_0,N_det,keys_tmp,N_int,1,N_det)
! print *,  'Energy of |(1+T)Psi_0> (normalized mu_i) : ', e_0 + nuclear_repulsion, overlap
!
! deallocate(u_0, keys_tmp)
!
!---------------
     
     norm = 0.d0
     double precision               :: f
     do i=1,N_det_non_ref
       if (rho_mrcc(i,s) == 0.d0) then
         rho_mrcc(i,s) = 1.d-32
       endif

       ! f is such that f.\tilde{c_i} = c_i
       f = psi_non_ref_coef(i,s) / rho_mrcc(i,s)

       ! Avoid numerical instabilities
       f = min(f,2.d0)
       f = max(f,-2.d0)

       norm = norm + f*f *rho_mrcc(i,s)*rho_mrcc(i,s)
       rho_mrcc(i,s) = f
     enddo
     ! norm now contains the norm of |T.Psi_0>
     ! rho_mrcc now contains the f factors

     f = 1.d0/norm
     ! f now contains 1/ <T.Psi_0|T.Psi_0>

     norm = 1.d0
     do i=1,N_det_ref
       norm = norm - psi_ref_coef(i,s)*psi_ref_coef(i,s)
     enddo
     ! norm now contains <Psi_SD|Psi_SD>
     f = dsqrt(f*norm)
     ! f normalises T.Psi_0 such that (1+T)|Psi> is normalized

     norm = norm*f
     print *,  'norm of |T Psi_0> = ', dsqrt(norm)
     if (dsqrt(norm) > 1.d0) then
       stop 'Error : Norm of the SD larger than the norm of the reference.'
     endif

     do i=1,N_det_ref
       norm = norm + psi_ref_coef(i,s)*psi_ref_coef(i,s)
     enddo

     do i=1,N_det_non_ref
       rho_mrcc(i,s) = rho_mrcc(i,s) * f
     enddo
     ! rho_mrcc now contains the product of the scaling factors and the
     ! normalization constant
    
    dIj_unique(1:size(X), s) = X(1:size(X))
  end do

END_PROVIDER




BEGIN_PROVIDER [ double precision, dij, (N_det_ref, N_det_non_ref, N_states) ]
  integer :: s,i,j
  double precision, external :: get_dij_index
  print *, "computing amplitudes..."
  do s=1, N_states
    do i=1, N_det_non_ref
      do j=1, N_det_ref
        !DIR$ FORCEINLINE
        dij(j, i, s) = get_dij_index(j, i, s, N_int)
      end do
    end do
  end do
  print *, "done computing amplitudes"
END_PROVIDER




double precision function get_dij_index(II, i, s, Nint)
  integer, intent(in) :: II, i, s, Nint
  double precision, external :: get_dij
  double precision :: HIi, phase

  if(lambda_type == 0) then
    call get_phase(psi_ref(1,1,II), psi_non_ref(1,1,i), phase, N_int)
    get_dij_index = get_dij(psi_ref(1,1,II), psi_non_ref(1,1,i), s, Nint) * phase
    get_dij_index = get_dij_index * rho_mrcc(i,s) 
  else if(lambda_type == 1) then
    call i_h_j(psi_ref(1,1,II), psi_non_ref(1,1,i), Nint, HIi)
    get_dij_index = HIi * lambda_mrcc(s, i)
  else if(lambda_type == 2) then
    call get_phase(psi_ref(1,1,II), psi_non_ref(1,1,i), phase, N_int)
    get_dij_index = get_dij(psi_ref(1,1,II), psi_non_ref(1,1,i), s, Nint) * phase
  end if
end function


double precision function get_dij(det1, det2, s, Nint)
  use bitmasks
  implicit none
  integer, intent(in) :: s, Nint
  integer(bit_kind) :: det1(Nint, 2), det2(Nint, 2)
  integer :: degree, f, exc(0:2, 2, 2), t
  integer*2 :: h1, h2, p1, p2, s1, s2
  integer, external :: searchExc
  logical, external :: excEq
  double precision :: phase
  integer*2 :: tmp_array(4)
  
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
    tmp_array = (/s1, h1, s2, h2/)
  else
    tmp_array = (/s2, h2, s1, h1/)
  end if
  f = searchExc(hh_exists(1,1), tmp_array,  hh_shortcut(0))

  if(f == -1) return
  
  if(p1 + (s1-1)*mo_tot_num < p2 + (s2-1)*mo_tot_num) then
    tmp_array = (/s1, p1, s2, p2/)
  else
    tmp_array = (/s2, p2, s1, p1/)
  end if
  t = searchExc(pp_exists(1,hh_shortcut(f)), tmp_array, hh_shortcut(f+1)-hh_shortcut(f))

  if(t /= -1) then
    get_dij = dIj_unique(t - 1 + hh_shortcut(f), s)
  end if
end function


 BEGIN_PROVIDER [ integer*2, hh_exists, (4, N_hh_exists) ]
&BEGIN_PROVIDER [ integer*2, pp_exists, (4, N_pp_exists) ]
&BEGIN_PROVIDER [ integer, hh_shortcut, (0:N_hh_exists + 1) ]
&BEGIN_PROVIDER [ integer, hh_nex ]
  implicit none
  BEGIN_DOC
  !
  ! hh_exists : 
  !
  ! pp_exists : 
  !
  ! hh_shortcut : 
  !
  ! hh_nex : Total number of excitation operators
  !
  END_DOC
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
  hh_nex = hh_shortcut(hh_shortcut(0)+1)-1
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


subroutine apply_hole_local(det, exc, res, ok, Nint)
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
    pos = iand(h1-1,bit_kind_size-1) ! mod 64
    if(iand(det(ii, s1), ishft(1_bit_kind, pos)) == 0_8) then
      return
    endif
    res(ii, s1) = ibclr(res(ii, s1), pos)
  end if
  
  ii = (h2-1)/bit_kind_size + 1 
  pos = iand(h2-1,bit_kind_size-1) ! mod 64
  if(iand(det(ii, s2), ishft(1_bit_kind, pos)) == 0_8) then
    return
  endif
  res(ii, s2) = ibclr(res(ii, s2), pos)
  ok = .true.
end subroutine


subroutine apply_particle_local(det, exc, res, ok, Nint)
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
    pos = iand(p1-1,bit_kind_size-1)
    if(iand(det(ii, s1), ishft(1_bit_kind, pos)) /= 0_8) then
      return
    endif
    res(ii, s1) = ibset(res(ii, s1), pos)
  end if

  ii = (p2-1)/bit_kind_size + 1 
  pos = iand(p2-1,bit_kind_size-1)
  if(iand(det(ii, s2), ishft(1_bit_kind, pos)) /= 0_8) then
    return
  endif
  res(ii, s2) = ibset(res(ii, s2), pos)

  
  ok = .true.
end subroutine





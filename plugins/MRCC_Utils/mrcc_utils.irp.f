use bitmasks

 BEGIN_PROVIDER [ integer, mrmode ]
  mrmode = 0
END_PROVIDER
 
 
 BEGIN_PROVIDER [ double precision, lambda_mrcc, (N_states, N_det_non_ref) ]
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
  f = searchExc(pp_exists(1,hh_shortcut(f)), tmp_array, hh_shortcut(f+1)-hh_shortcut(f))

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



 BEGIN_PROVIDER [ integer(bit_kind), psi_non_ref_sorted, (N_int, 2, N_det_non_ref) ]
&BEGIN_PROVIDER [ integer, psi_non_ref_sorted_idx, (N_det_non_ref) ]
  implicit none
  psi_non_ref_sorted = psi_non_ref
  call sort_det(psi_non_ref_sorted, psi_non_ref_sorted_idx, N_det_non_ref, N_int)
END_PROVIDER


 BEGIN_PROVIDER [ double precision, dIj_unique, (hh_shortcut(hh_shortcut(0)+1)-1, N_states) ]
&BEGIN_PROVIDER [ double precision, rho_mrcc, (N_det_non_ref, N_states) ]
   implicit none
   logical                        :: ok
   integer                        :: i, j, k, s, II, pp, hh, ind, wk, nex, a_col, at_row
   integer, external              :: searchDet, unsortedSearchDet
   integer(bit_kind)              :: myDet(N_int, 2), myMask(N_int, 2)
   integer                        :: N, INFO, AtA_size, r1, r2
   double precision , allocatable :: AtB(:), AtA_val(:), A_val(:,:), x(:), x_new(:), A_val_mwen(:)
   double precision               :: t, norm, cx, res
   integer, allocatable           :: A_ind(:,:), lref(:), AtA_ind(:), A_ind_mwen(:), col_shortcut(:), N_col(:)
   
   
   
   nex = hh_shortcut(hh_shortcut(0)+1)-1
   print *, "TI", nex, N_det_non_ref
   allocate(A_ind(N_det_ref+1, nex), A_val(N_det_ref+1, nex))
   allocate(AtA_ind(N_det_ref * nex), AtA_val(N_det_ref * nex)) !!!!! MAY BE TOO SMALL ? !!!!!!!!
   allocate(x(nex), AtB(nex))
   allocate(A_val_mwen(nex), A_ind_mwen(nex))
   allocate(N_col(nex), col_shortcut(nex))
   allocate (x_new(nex))
   
   do s = 1, N_states
     
     A_val = 0d0
     A_ind = 0
     AtA_ind = 0
     AtA_val = 0d0
     x = 0d0
     A_val_mwen = 0d0
     A_ind_mwen = 0
     N_col = 0
     col_shortcut = 0
     
     !$OMP PARALLEL DO schedule(static,10) default(none) shared(psi_non_ref, hh_exists, pp_exists, N_int, A_val, A_ind)&
         !$OMP shared(s, hh_shortcut, psi_ref_coef, N_det_non_ref, psi_non_ref_sorted, psi_non_ref_sorted_idx, psi_ref, N_det_ref)&
         !$OMP private(lref, pp, II, ok, myMask, myDet, ind, wk)
     do hh = 1, hh_shortcut(0)
       do pp = hh_shortcut(hh), hh_shortcut(hh+1)-1
         allocate(lref(N_det_non_ref))
         lref = 0
         do II = 1, N_det_ref
           call apply_hole(psi_ref(1,1,II), hh_exists(1, hh), myMask, ok, N_int)
           if(.not. ok) cycle
           call apply_particle(myMask, pp_exists(1, pp), myDet, ok, N_int)
           if(.not. ok) cycle
           ind = searchDet(psi_non_ref_sorted(1,1,1), myDet(1,1), N_det_non_ref, N_int)
           if(ind /= -1) then
             lref(psi_non_ref_sorted_idx(ind)) = II
           end if
         end do
         wk = 0
         do i=1, N_det_non_ref
           if(lref(i) /= 0) then
             wk += 1
             A_val(wk, pp) = psi_ref_coef(lref(i), s)
             A_ind(wk, pp) = i
           end if
         end do
         deallocate(lref)
       end do
     end do
     !$OMP END PARALLEL DO
     
     AtB = 0d0
     AtA_size = 0
     wk = 0
     col_shortcut = 0
     N_col = 0
     !$OMP PARALLEL DO schedule(dynamic, 100) default(none) shared(k, psi_non_ref_coef, A_ind, A_val, x, N_det_ref, nex, N_det_non_ref)&
         !$OMP private(at_row, a_col, t, i, r1, r2, wk, A_ind_mwen, A_val_mwen)&
         !$OMP shared(col_shortcut, N_col, AtB, AtA_size, AtA_val, AtA_ind, s)
     do at_row = 1, nex
       wk = 0
       if(mod(at_row, 10000) == 0) print *, "AtA", at_row, "/", nex
       do i=1,N_det_ref
         if(A_ind(i, at_row) == 0) exit
         AtB(at_row) = AtB(at_row) + psi_non_ref_coef(A_ind(i, at_row), s) * A_val(i, at_row)
       end do
       do a_col = 1, nex
         t = 0d0
         r1 = 1
         r2 = 1
         do while ((A_ind(r1, at_row) /= 0).and.(A_ind(r2, a_col) /= 0))
           if(A_ind(r1, at_row) < A_ind(r2, a_col)) then
             r1 += 1
           else if(A_ind(r1, at_row) > A_ind(r2, a_col)) then
             r2 += 1
           else
             t = t - A_val(r1, at_row) * A_val(r2, a_col)
             r1 += 1
             r2 += 1
           end if
         end do
         
         if(a_col == at_row) then
           t = (t + 1d0)
         end if
         if(t /= 0d0) then
           wk += 1
           A_ind_mwen(wk) = a_col
           A_val_mwen(wk) = t
         end if
       end do
       
       if(wk /= 0) then
         !$OMP CRITICAL
         col_shortcut(at_row) = AtA_size+1
         N_col(at_row) = wk
         AtA_ind(AtA_size+1:AtA_size+wk) = A_ind_mwen(:wk)
         AtA_val(AtA_size+1:AtA_size+wk) = A_val_mwen(:wk)
         AtA_size += wk
         !$OMP END CRITICAL
       end if
     end do
     
     if(AtA_size > size(AtA_val)) stop "SIZA"
     print *, "ATA SIZE", ata_size
     do i=1,nex
       x(i) = AtB(i)
     enddo
     
     do k=0,100000
       !$OMP PARALLEL default(shared) private(cx, i, j, a_col)
       
       !$OMP DO
       do i=1,N_det_non_ref
         rho_mrcc(i,s) = 0.d0
       enddo
       !$OMP END DO
       
       !$OMP DO
       do a_col = 1, nex
         cx = 0d0
         do i=col_shortcut(a_col), col_shortcut(a_col) + N_col(a_col) - 1
           cx = cx + x(AtA_ind(i)) * AtA_val(i)
         end do
         x_new(a_col) = AtB(a_col) + cx
       end do
       !$OMP END DO
       
       !$OMP END PARALLEL
       
       res = 0.d0
       do a_col=1,nex
         do j=1,N_det_non_ref
           i = A_ind(j,a_col)
           if (i==0) exit
           rho_mrcc(i,s) = rho_mrcc(i,s) + A_val(j,a_col) * X_new(a_col)
         enddo
         res = res + (X_new(a_col) - X(a_col))**2
         X(a_col) = X_new(a_col)
       end do
       
       if(mod(k, 100) == 0) then
         print *, "residu ", k, res
       end if
       
       if(res < 1d-10) exit
     end do
     
     norm = 0.d0
     do i=1,N_det_non_ref
       norm = norm + rho_mrcc(i,s)*rho_mrcc(i,s)
     enddo
     do i=1,N_det_ref
       norm = norm + psi_ref_coef(i,s)*psi_ref_coef(i,s)
     enddo
     
     print *, k, "res : ", res, "norm : ", sqrt(norm)
     
     dIj_unique(:size(X), s) = X(:)
     
     do i=1,N_det_non_ref
       rho_mrcc(i,s) = psi_non_ref_coef(i,s) / rho_mrcc(i,s)
     enddo
     
   end do
   
   print *, "done"
END_PROVIDER


BEGIN_PROVIDER [ double precision, dij, (N_det_ref, N_det_non_ref, N_states) ]
  integer :: s,i,j
  double precision, external :: get_dij_index
  print *, "computing amplitudes..."
  do s=1, N_states
    do i=1, N_det_non_ref
      do j=1, N_det_ref
        dij(j, i, s) = get_dij_index(j, i, s, N_int)
      end do
    end do
  end do
  print *, "done computing amplitudes"
END_PROVIDER




double precision function get_dij_index(II, i, s, Nint)
  integer, intent(in) :: II, i, s, Nint
  double precision, external :: get_dij
  double precision :: HIi

  if(lambda_type == 0) then
    get_dij_index = get_dij(psi_ref(1,1,II), psi_non_ref(1,1,i), s, Nint)
    get_dij_index = get_dij_index * rho_mrcc(i,s)
  else
    call i_h_j(psi_ref(1,1,II), psi_non_ref(1,1,i), Nint, HIi)
    get_dij_index = HIi * lambda_mrcc(s, i)
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


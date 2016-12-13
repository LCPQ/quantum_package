 BEGIN_PROVIDER [ integer, n_exc_active ]
&BEGIN_PROVIDER [ integer, active_pp_idx, (hh_nex) ]
&BEGIN_PROVIDER [ integer, active_hh_idx, (hh_nex) ]
&BEGIN_PROVIDER [ logical, is_active_exc, (hh_nex) ]
 implicit none
 BEGIN_DOC
 ! is_active_exc : True if the excitation involves at least one active MO
 !
 ! n_exc_active : Number of active excitations : Number of excitations without the inactive ones.
 !
 ! active_hh_idx : 
 !
 ! active_pp_idx :
 END_DOC
 integer                        :: hh, pp, II
 integer                        :: ind
 logical                        :: ok
 integer(bit_kind)              :: myDet(N_int, 2), myMask(N_int, 2)

 integer, allocatable           :: pathTo(:)
 integer, external              :: searchDet

 allocate(pathTo(N_det_non_ref))

 pathTo(:) = 0
 is_active_exc(:) = .false.
 n_exc_active = 0

 do hh = 1, hh_shortcut(0)
   do pp = hh_shortcut(hh), hh_shortcut(hh+1)-1
     do II = 1, N_det_ref

       call apply_hole_local(psi_ref(1,1,II), hh_exists(1, hh), myMask, ok, N_int)
       if(.not. ok) cycle

       call apply_particle_local(myMask, pp_exists(1, pp), myDet, ok, N_int)
       if(.not. ok) cycle

       ind = searchDet(psi_non_ref_sorted(1,1,1), myDet(1,1), N_det_non_ref, N_int)
       if(ind == -1) cycle

       ind = psi_non_ref_sorted_idx(ind)
       if(pathTo(ind) == 0) then
         pathTo(ind) = pp
       else
         is_active_exc(pp) = .true.
         is_active_exc(pathTo(ind)) = .true.
       end if
     end do
   end do
 end do
!is_active_exc=.true.
 do hh = 1, hh_shortcut(0)
  do pp = hh_shortcut(hh), hh_shortcut(hh+1)-1
    if(is_active_exc(pp)) then
      n_exc_active = n_exc_active + 1
      active_hh_idx(n_exc_active) = hh
      active_pp_idx(n_exc_active) = pp
    end if
  end do
 end do

 deallocate(pathTo)

 print *, n_exc_active, "active excitations /", hh_nex

END_PROVIDER

BEGIN_PROVIDER [ integer, n_exc_active_sze ]
 implicit none
 BEGIN_DOC
 ! Dimension of arrays to avoid zero-sized arrays
 END_DOC
 n_exc_active_sze = max(n_exc_active,1)
END_PROVIDER



 BEGIN_PROVIDER [ integer, active_excitation_to_determinants_idx, (0:N_det_ref+1, n_exc_active_sze) ]
&BEGIN_PROVIDER [ double precision, active_excitation_to_determinants_val, (N_states,N_det_ref+1, n_exc_active_sze) ]
 implicit none
 BEGIN_DOC
 ! Sparse matrix A containing the matrix to transform the active excitations to
 ! determinants : A | \Psi_0 > = | \Psi_SD >
 END_DOC
 integer                        :: s, ppp, pp, hh, II, ind, wk, i
 integer, allocatable           :: lref(:) 
 integer(bit_kind)              :: myDet(N_int,2), myMask(N_int,2)
 double precision               :: phase
 logical                        :: ok
 integer, external              :: searchDet
 

 !$OMP PARALLEL default(none) shared(psi_non_ref, hh_exists, pp_exists, N_int,&
     !$OMP   active_excitation_to_determinants_val, active_excitation_to_determinants_idx)&
     !$OMP shared(hh_shortcut, psi_ref_coef, N_det_non_ref, psi_non_ref_sorted, &
     !$OMP   psi_non_ref_sorted_idx, psi_ref, N_det_ref, N_states)&
     !$OMP shared(is_active_exc, active_hh_idx, active_pp_idx, n_exc_active)&
     !$OMP private(lref, pp, II, ok, myMask, myDet, ind, phase, wk, ppp, hh, s)
 allocate(lref(N_det_non_ref))
 !$OMP DO schedule(dynamic)
 do ppp=1,n_exc_active
   active_excitation_to_determinants_val(:,:,ppp) = 0d0
   active_excitation_to_determinants_idx(:,ppp)   = 0
   pp = active_pp_idx(ppp)
   hh = active_hh_idx(ppp)
   lref = 0
   do II = 1, N_det_ref
     call apply_hole_local(psi_ref(1,1,II), hh_exists(1, hh), myMask, ok, N_int)
     if(.not. ok) cycle
     call apply_particle_local(myMask, pp_exists(1, pp), myDet, ok, N_int)
     if(.not. ok) cycle
     ind = searchDet(psi_non_ref_sorted(1,1,1), myDet(1,1), N_det_non_ref, N_int)
     if(ind /= -1) then
       call get_phase(myDet(1,1), psi_ref(1,1,II), phase, N_int)
       if (phase  > 0.d0) then
         lref(psi_non_ref_sorted_idx(ind)) =  II
       else
         lref(psi_non_ref_sorted_idx(ind)) = -II
       endif
     end if
   end do
   wk = 0
   do i=1, N_det_non_ref
     if(lref(i) > 0) then
       wk += 1
       do s=1,N_states
        active_excitation_to_determinants_val(s,wk, ppp) = psi_ref_coef(lref(i), s)
       enddo
       active_excitation_to_determinants_idx(wk, ppp) = i
     else if(lref(i) < 0) then
       wk += 1
       do s=1,N_states
        active_excitation_to_determinants_val(s,wk, ppp) = -psi_ref_coef(-lref(i), s)
       enddo
       active_excitation_to_determinants_idx(wk, ppp) = i
     end if
   end do
   active_excitation_to_determinants_idx(0,ppp) = wk
 end do
 !$OMP END DO
 deallocate(lref)
 !$OMP END PARALLEL

END_PROVIDER


 BEGIN_PROVIDER [ integer, mrcc_AtA_ind, (N_det_ref * n_exc_active_sze) ]
&BEGIN_PROVIDER [ double precision, mrcc_AtA_val, (N_states, N_det_ref * n_exc_active_sze) ]
&BEGIN_PROVIDER [ integer, mrcc_col_shortcut, (n_exc_active_sze) ]
&BEGIN_PROVIDER [ integer, mrcc_N_col, (n_exc_active_sze) ]
 implicit none
 BEGIN_DOC
 ! A is active_excitation_to_determinants in At.A
 END_DOC
 integer                        :: AtA_size, i,k
 integer                        :: at_roww, at_row, wk, a_coll, a_col, r1, r2, s
 double precision, allocatable  :: t(:), A_val_mwen(:,:), As2_val_mwen(:,:)
 integer, allocatable           :: A_ind_mwen(:)
 double precision               :: sij
 PROVIDE psi_non_ref 

 mrcc_AtA_ind(:) = 0
 mrcc_AtA_val(:,:) = 0.d0
 mrcc_col_shortcut(:) = 0
 mrcc_N_col(:) = 0
 AtA_size = 0


  !$OMP PARALLEL default(none) shared(k, active_excitation_to_determinants_idx,&
      !$OMP   active_excitation_to_determinants_val, hh_nex)         &
      !$OMP private(at_row, a_col, t, i, r1, r2, wk, A_ind_mwen, A_val_mwen,&
      !$OMP   As2_val_mwen, a_coll, at_roww,sij) &
      !$OMP shared(N_states,mrcc_col_shortcut, mrcc_N_col, AtA_size, mrcc_AtA_val, mrcc_AtA_ind, &
      !$OMP n_exc_active, active_pp_idx,psi_non_ref)
  allocate(A_val_mwen(N_states,hh_nex), As2_val_mwen(N_states,hh_nex), A_ind_mwen(hh_nex), t(N_states) )

  !$OMP DO schedule(dynamic, 100)
  do at_roww = 1, n_exc_active ! hh_nex
    at_row = active_pp_idx(at_roww)
    wk = 0

    do a_coll = 1, n_exc_active
      a_col = active_pp_idx(a_coll)
      t(:) = 0d0
      r1 = 1
      r2 = 1
      do while ((active_excitation_to_determinants_idx(r1, at_roww) /= 0).and.(active_excitation_to_determinants_idx(r2, a_coll) /= 0))
        if(active_excitation_to_determinants_idx(r1, at_roww) > active_excitation_to_determinants_idx(r2, a_coll)) then
          r2 = r2+1
        else if(active_excitation_to_determinants_idx(r1, at_roww) < active_excitation_to_determinants_idx(r2, a_coll)) then
          r1 = r1+1
        else
          do s=1,N_states
            t(s) = t(s) - active_excitation_to_determinants_val(s,r1, at_roww) * active_excitation_to_determinants_val(s,r2, a_coll)
          enddo
          r1 = r1+1
          r2 = r2+1
        end if
      end do

      if (a_col == at_row) then
        t(:) = t(:) + 1.d0
      endif
      if (sum(dabs(t(:))) > 0.d0) then
        wk = wk+1
        A_ind_mwen(wk) = a_col
        A_val_mwen(:,wk) = t(:)
      endif

    end do

    if(wk /= 0) then
      !$OMP CRITICAL
      mrcc_col_shortcut(at_roww) = AtA_size+1
      mrcc_N_col(at_roww) = wk
      if (AtA_size+wk > size(mrcc_AtA_ind,1)) then
        print *, AtA_size+wk , size(mrcc_AtA_ind,1)
        stop 'too small'
      endif
      do i=1,wk
        mrcc_AtA_ind(AtA_size+i) = A_ind_mwen(i)
        do s=1,N_states
          mrcc_AtA_val(s,AtA_size+i) = A_val_mwen(s,i)
        enddo
      enddo
      AtA_size += wk
      !$OMP END CRITICAL
    end if
  end do
  !$OMP END DO NOWAIT
  deallocate (A_ind_mwen, A_val_mwen, As2_val_mwen, t)
  !$OMP END PARALLEL

  print *, "At.A SIZE", ata_size

END_PROVIDER


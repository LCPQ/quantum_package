
program mrcc_sto
  implicit none
  BEGIN_DOC
! TODO
  END_DOC
  call dress_zmq()
  call ezfio_set_mrcc_sto_energy(ci_energy_dressed(1))
end


 BEGIN_PROVIDER [ double precision, hij_cache_, (N_det,Nproc) ]
&BEGIN_PROVIDER [ double precision, sij_cache_, (N_det,Nproc) ]
&BEGIN_PROVIDER [ double precision, dIa_hla_, (N_states,N_det,Nproc) ]
&BEGIN_PROVIDER [ double precision, dIa_sla_, (N_states,N_det,Nproc) ]
&BEGIN_PROVIDER [ integer, excs_ , (0:2,2,2,N_det,Nproc) ]
&BEGIN_PROVIDER [ double precision, phases_, (N_det, Nproc) ]
BEGIN_DOC
 ! temporay arrays for dress_with_alpha_buffer. Avoids reallocation.
END_DOC
END_PROVIDER



subroutine dress_with_alpha_buffer(delta_ij_loc, i_gen, minilist, det_minilist, n_minilist, alpha, iproc)
 use bitmasks
 implicit none
  BEGIN_DOC
  !delta_ij_loc(:,:,1) : dressing column for H
  !delta_ij_loc(:,:,2) : dressing column for S2
  !minilist : indices of determinants connected to alpha ( in psi_det_sorted )
  !n_minilist : size of minilist
  !alpha : alpha determinant
  END_DOC
  integer(bit_kind), intent(in)   :: alpha(N_int,2), det_minilist(N_int, 2, n_minilist)
  integer,intent(in)              :: minilist(n_minilist), n_minilist, iproc, i_gen
  double precision, intent(inout) :: delta_ij_loc(N_states,N_det,2)


  integer                        :: i,j,k,l,m
  integer                        :: degree1, degree2, degree

  double precision               :: hIk, hla, hIl, sla, dIk(N_states), dka(N_states), dIa(N_states), hka
  double precision               :: phase, phase2
  integer                        :: exc(0:2,2,2)
  integer                        :: h1,h2,p1,p2,s1,s2
  integer(bit_kind)              :: tmp_det(N_int,2), ctrl
  integer                        :: i_state, k_sd, l_sd, m_sd, ll_sd, i_I
  double precision :: Delta_E_inv(N_states)
  double precision :: sdress, hdress
  logical :: ok, ok2
  integer :: canbediamond
  PROVIDE mo_class

  
  if(n_minilist == 1) return
  
  do i=1,n_minilist
    if(idx_non_ref_rev(minilist(i)) == 0) return
  end do

  if (perturbative_triples) then
    PROVIDE one_anhil fock_virt_total fock_core_inactive_total one_creat
  endif

  canbediamond = 0
  do l_sd=1,n_minilist
    call get_excitation(det_minilist(1,1,l_sd),alpha,exc,degree1,phase,N_int)
    call decode_exc(exc,degree1,h1,p1,h2,p2,s1,s2)
   
    ok = (mo_class(h1)(1:1) == 'A' .or. mo_class(h1)(1:1) == 'I') .and. &
         (mo_class(p1)(1:1) == 'A' .or. mo_class(p1)(1:1) == 'V') 
    if(ok .and. degree1 == 2) then
            ok = (mo_class(h2)(1:1) == 'A' .or. mo_class(h2)(1:1) == 'I') .and. &
                 (mo_class(p2)(1:1) == 'A' .or. mo_class(p2)(1:1) == 'V') 
    end if
    
    if(ok) then
      canbediamond += 1
      excs_(:,:,:,l_sd,iproc) = exc(:,:,:)
      phases_(l_sd, iproc) = phase
    else
      phases_(l_sd, iproc) = 0d0
    end if
    !call i_h_j(alpha,det_minilist(1,1,l_sd),N_int,hij_cache_(l_sd,iproc))
    !call get_s2(alpha,det_minilist(1,1,l_sd),N_int,sij_cache_(l_sd,iproc))
    call i_h_j_s2(alpha,det_minilist(1,1,l_sd),N_int,hij_cache_(l_sd,iproc), sij_cache_(l_sd,iproc))
  enddo
  if(canbediamond <= 1) return

  do i_I=1,N_det_ref
    call get_excitation_degree(alpha,psi_ref(1,1,i_I),degree1,N_int)
    if (degree1 > 4) then
      cycle
    endif
    
    do i_state=1,N_states
      dIa(i_state) = 0.d0
    enddo

    do k_sd=1,n_minilist
      if(phases_(k_sd,iproc) == 0d0) cycle
      call get_excitation_degree(psi_ref(1,1,i_I),det_minilist(1,1,k_sd),degree,N_int)
      if (degree > 2) then
        cycle
      endif
      
      !call get_excitation(det_minilist(1,1,k_sd),alpha,exc,degree2,phase,N_int)
      phase = phases_(k_sd, iproc)
      exc(:,:,:) = excs_(:,:,:,k_sd,iproc)
      degree2 = exc(0,1,1) + exc(0,1,2)
      call apply_excitation(psi_ref(1,1,i_I), exc, tmp_det, ok, N_int)
      
      if((.not. ok) .and. (.not. perturbative_triples)) cycle

      do i_state=1,N_states
        dka(i_state) = 0.d0
      enddo
      
      ok2 = .false.
      !do i_state=1,N_states
      !  !if(dka(i_state) == 0) cycle
      !  dIk(i_state) = dij(i_I, idx_non_ref_rev(minilist(k_sd)), i_state)
      !  if(dIk(i_state) /= 0d0) then
      !    ok2 = .true.
      !  endif
      !enddo
      !if(.not. ok2) cycle

      if (ok) then
        phase2 = 0d0
        do l_sd=k_sd+1,n_minilist
          if(phases_(l_sd, iproc) == 0d0) cycle
          call get_excitation_degree(tmp_det,det_minilist(1,1,l_sd),degree,N_int)
          if (degree == 0) then
            do i_state=1,N_states
              dIk(i_state) = dij(i_I, idx_non_ref_rev(minilist(k_sd)), i_state)
              if(dIk(i_state) /= 0d0) then
                if(phase2 == 0d0) call get_excitation(psi_ref(1,1,i_I),det_minilist(1,1,l_sd),exc,degree,phase2,N_int)
                dka(i_state) = dij(i_I, idx_non_ref_rev(minilist(l_sd)), i_state) * phase * phase2
              end if
            end do

            !call get_excitation(psi_ref(1,1,i_I),det_minilist(1,1,l_sd),exc,degree,phase2,N_int)
            !do i_state=1,N_states
            !  if(dIk(i_state) /= 0d0) dka(i_state) = dij(i_I, idx_non_ref_rev(minilist(l_sd)), i_state) * phase * phase2
            !enddo
            exit

          endif
        enddo
      else if (perturbative_triples) then
        hka = hij_cache_(k_sd,iproc)
        if (dabs(hka) > 1.d-12) then
          call get_delta_e_dyall_general_mp(psi_ref(1,1,i_I),alpha,Delta_E_inv)

          do i_state=1,N_states
            ASSERT (Delta_E_inv(i_state) < 0.d0)
            dka(i_state) = hka / Delta_E_inv(i_state)
          enddo
        endif
      endif
      
           
      if (perturbative_triples.and. (degree2 == 1) ) then
          call i_h_j(psi_ref(1,1,i_I),tmp_det,N_int,hka)
          hka = hij_cache_(k_sd,iproc) - hka
          if (dabs(hka) > 1.d-12) then
            call get_delta_e_dyall_general_mp(psi_ref(1,1,i_I),alpha,Delta_E_inv)
            do i_state=1,N_states
              ASSERT (Delta_E_inv(i_state) < 0.d0)
              dka(i_state) = hka / Delta_E_inv(i_state)
            enddo
          endif
      endif
      do i_state=1,N_states
        dIa(i_state) = dIa(i_state) + dIk(i_state) * dka(i_state)
      enddo
    enddo
    
    ok2 = .false.
    do i_state=1,N_states
      if(dIa(i_state) /= 0d0) ok2 = .true.
    enddo
    if(.not. ok2) cycle

    do l_sd=1,n_minilist
      k_sd = minilist(l_sd)
      hla = hij_cache_(l_sd,iproc)
      sla = sij_cache_(l_sd,iproc)
      do i_state=1,N_states
        hdress =  dIa(i_state) * hla * psi_ref_coef(i_I,i_state)
        sdress =  dIa(i_state) * sla * psi_ref_coef(i_I,i_state)
        !!!$OMP ATOMIC
        delta_ij_loc(i_state,k_sd,1) += hdress
        !!!$OMP ATOMIC
        delta_ij_loc(i_state,k_sd,2) += sdress
      enddo
    enddo
  enddo
end subroutine





!! TESTS MINILIST
subroutine test_minilist(minilist, n_minilist, alpha)
  use bitmasks
  implicit none
  integer, intent(in)            :: n_minilist
  integer(bit_kind),intent(in)  :: alpha(N_int, 2)
  integer, intent(in) :: minilist(n_minilist)
  integer :: a, i, deg
  integer :: refc(N_det), testc(N_det)
  
  refc = 0
  testc = 0
  do i=1,N_det
    call get_excitation_degree(psi_det(1,1,i), alpha, deg, N_int) 
    if(deg <= 2) refc(i) = refc(i) + 1
  end do
  do i=1,n_minilist
    call get_excitation_degree(psi_det(1,1,minilist(i)), alpha, deg, N_int) 
    if(deg <= 2) then
      testc(minilist(i)) += 1
    else
      stop "NON LINKED IN MINILIST"
    end if
  end do
  
  do i=1,N_det
    if(refc(i) /= testc(i)) then
      print *, "MINILIST FAIL ", sum(refc), sum(testc), n_minilist
      exit
    end if
  end do
end subroutine



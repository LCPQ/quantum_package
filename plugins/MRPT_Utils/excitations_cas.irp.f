subroutine apply_exc_to_psi(orb,hole_particle,spin_exc, & 
           norm_out,psi_in_out,psi_in_out_coef, ndet,dim_psi_in,dim_psi_coef,N_states_in)
  use bitmasks
 implicit none
 integer, intent(in) :: orb, hole_particle,spin_exc,N_states_in,ndet,dim_psi_in,dim_psi_coef
 double precision, intent(out)  :: norm_out(N_states_in)
 integer(bit_kind), intent(inout) :: psi_in_out(N_int,2,dim_psi_in)
 double precision,  intent(inout) :: psi_in_out_coef(dim_psi_coef,N_states_in)
  BEGIN_DOC
  ! apply a contracted excitation to psi_in_out whose coefficients 
  ! are psi_in_out_coef
  ! hole_particle =  1  ===> creation     of an electron in psi_in_out
  !               = -1  ===> annhilation  of an electron in psi_in_out
  ! orb ===> is the index of orbital where you want wether to create or 
  !          annhilate an electron
  ! spin_exc ===> is the spin of the electron (1 == alpha) (2 == beta)
  ! the wave function gets out normalized to unity
  !
  ! norm_out is the sum of the squared of the coefficients 
  ! on which the excitation has been possible
  END_DOC

  integer :: elec_num_tab_local(2)
  integer :: i,j,accu_elec,k
  integer(bit_kind) :: det_tmp(N_int), det_tmp_bis(N_int)
  double precision :: phase
  double precision :: norm_factor

  elec_num_tab_local = 0
  do i = 1, ndet
   if( psi_in_out_coef (i,1) .ne. 0.d0)then
    do j = 1, N_int
     elec_num_tab_local(1) += popcnt(psi_in_out(j,1,i))
     elec_num_tab_local(2) += popcnt(psi_in_out(j,2,i))
    enddo
    exit
   endif
  enddo
  if(hole_particle == 1)then
   do i = 1, ndet
     call set_bit_to_integer(orb,psi_in_out(1,spin_exc,i),N_int) 
     accu_elec = 0
     do j = 1, N_int
      accu_elec += popcnt(psi_in_out(j,spin_exc,i))
     enddo 
     if(accu_elec .ne. elec_num_tab_local(spin_exc)+1)then
      do j = 1, N_int
       psi_in_out(j,1,i) = 0_bit_kind
       psi_in_out(j,2,i) = 0_bit_kind
      enddo
      do j = 1, N_states_in
       psi_in_out_coef(i,j) = 0.d0
      enddo
     endif
     phase = 1.d0
     do k = 1, orb
      do j = 1, N_int
       det_tmp(j) = 0_bit_kind
      enddo
      call set_bit_to_integer(k,det_tmp,N_int) 
      accu_elec = 0
      do j = 1, N_int
       det_tmp_bis(j) = iand(det_tmp(j),(psi_in_out(j,spin_exc,i)))
       accu_elec += popcnt(det_tmp_bis(j))
      enddo
      if(accu_elec == 1)then
       phase = -phase
      endif
     enddo
     do j = 1, N_states_in
      psi_in_out_coef(i,j) = psi_in_out_coef(i,j)  * phase
     enddo
   enddo

  else if (hole_particle == -1)then

   do i = 1, ndet
     call clear_bit_to_integer(orb,psi_in_out(1,spin_exc,i),N_int) 
     accu_elec = 0
     do j = 1, N_int
      accu_elec += popcnt(psi_in_out(j,spin_exc,i))
     enddo 
     if(accu_elec .ne. elec_num_tab_local(spin_exc)-1)then
      do j = 1, N_int
       psi_in_out(j,1,i) = 0_bit_kind
       psi_in_out(j,2,i) = 0_bit_kind
      enddo
      do j = 1, N_states_in
       psi_in_out_coef(i,j) = 0.d0
      enddo
     endif

     phase = 1.d0
     do k = 1, orb-1
      do j = 1, N_int
       det_tmp(j) = 0_bit_kind
      enddo
      call set_bit_to_integer(k,det_tmp,N_int) 
      accu_elec = 0
      do j = 1, N_int
       det_tmp_bis(j) = iand(det_tmp(j),(psi_in_out(j,spin_exc,i)))
       accu_elec += popcnt(det_tmp_bis(j)) 
      enddo
      if(accu_elec == 1)then
       phase = -phase 
      endif
     enddo
     do j = 1, N_states_in
      psi_in_out_coef(i,j) = psi_in_out_coef(i,j)  * phase
     enddo
   enddo
  endif


  norm_out = 0.d0
  do j = 1, N_states_in
   do i = 1, ndet
    norm_out(j) += psi_in_out_coef(i,j) * psi_in_out_coef(i,j)
   enddo
   if(norm_out(j).le.1.d-10)then
    norm_factor = 0.d0
   else 
    norm_factor = 1.d0/(dsqrt(norm_out(j)))
   endif
   do i = 1, ndet
    psi_in_out_coef(i,j) = psi_in_out_coef(i,j) * norm_factor
   enddo
  enddo
end


double precision function diag_H_mat_elem_no_elec_check(det_in,Nint)
  implicit none
  BEGIN_DOC
  ! Computes <i|H|i>
  END_DOC
  integer,intent(in)             :: Nint
  integer(bit_kind),intent(in)   :: det_in(Nint,2)
  
  integer                        :: i, j, iorb, jorb 
  integer                        :: occ(Nint*bit_kind_size,2)
  integer                        ::  elec_num_tab_local(2)

  double precision :: core_act
  double precision :: alpha_alpha
  double precision :: alpha_beta
  double precision :: beta_beta
  double precision :: mono_elec
  core_act = 0.d0
  alpha_alpha = 0.d0
  alpha_beta = 0.d0
  beta_beta = 0.d0
  mono_elec = 0.d0

  diag_H_mat_elem_no_elec_check = 0.d0
  call bitstring_to_list(det_in(1,1), occ(1,1), elec_num_tab_local(1), N_int)
  call bitstring_to_list(det_in(1,2), occ(1,2), elec_num_tab_local(2), N_int)
  ! alpha - alpha 
! print*, 'elec_num_tab_local(1)',elec_num_tab_local(1)
! print*, 'elec_num_tab_local(2)',elec_num_tab_local(2)
  do i = 1, elec_num_tab_local(1)
   iorb =  occ(i,1)
   diag_H_mat_elem_no_elec_check += mo_mono_elec_integral(iorb,iorb)
   mono_elec += mo_mono_elec_integral(iorb,iorb)
   do j = i+1, elec_num_tab_local(1)
    jorb = occ(j,1)
    diag_H_mat_elem_no_elec_check +=  mo_bielec_integral_jj_anti(jorb,iorb)
    alpha_alpha += mo_bielec_integral_jj_anti(jorb,iorb)
   enddo
  enddo 

  ! beta - beta   
  do i = 1, elec_num_tab_local(2)
   iorb =  occ(i,2)
   diag_H_mat_elem_no_elec_check += mo_mono_elec_integral(iorb,iorb)
   mono_elec += mo_mono_elec_integral(iorb,iorb)
   do j = i+1, elec_num_tab_local(2)
    jorb = occ(j,2)
    diag_H_mat_elem_no_elec_check +=  mo_bielec_integral_jj_anti(jorb,iorb)
    beta_beta +=  mo_bielec_integral_jj_anti(jorb,iorb)
   enddo
  enddo 
  

  ! alpha - beta   
  do i = 1, elec_num_tab_local(2)
   iorb =  occ(i,2)
   do j = 1, elec_num_tab_local(1)
    jorb = occ(j,1)
    diag_H_mat_elem_no_elec_check +=  mo_bielec_integral_jj(jorb,iorb)
    alpha_beta += mo_bielec_integral_jj(jorb,iorb)
   enddo
  enddo 
  

  ! alpha - core-act
  do i = 1, elec_num_tab_local(1)
   iorb =  occ(i,1)
   do j = 1, n_core_inact_orb
    jorb = list_core_inact(j)
    diag_H_mat_elem_no_elec_check +=  2.d0 * mo_bielec_integral_jj(jorb,iorb) - mo_bielec_integral_jj_exchange(jorb,iorb)
    core_act += 2.d0 * mo_bielec_integral_jj(jorb,iorb) - mo_bielec_integral_jj_exchange(jorb,iorb)
   enddo
  enddo 

  ! beta - core-act
  do i = 1, elec_num_tab_local(2)
   iorb =  occ(i,2)
   do j = 1, n_core_inact_orb
    jorb = list_core_inact(j)
    diag_H_mat_elem_no_elec_check +=  2.d0 * mo_bielec_integral_jj(jorb,iorb) - mo_bielec_integral_jj_exchange(jorb,iorb)
    core_act += 2.d0 * mo_bielec_integral_jj(jorb,iorb) - mo_bielec_integral_jj_exchange(jorb,iorb)
   enddo
  enddo 
! print*,'core_act    = ',core_act
! print*,'alpha_alpha = ',alpha_alpha
! print*,'alpha_beta  = ',alpha_beta
! print*,'beta_beta   = ',beta_beta
! print*,'mono_elec   = ',mono_elec

! do i = 1, n_core_inact_orb
!  iorb = list_core_inact(i)
!  diag_H_mat_elem_no_elec_check += 2.d0 * fock_core_inactive_total_spin_trace(iorb,1)
! enddo


!!!!!!!!!!!!
return
!!!!!!!!!!!!


  ! alpha - alpha
  do i = 1, n_core_inact_orb
   iorb = list_core_inact(i)
   diag_H_mat_elem_no_elec_check += 1.d0 * mo_mono_elec_integral(iorb,iorb)
   do j = i+1, n_core_inact_orb
    jorb = list_core_inact(j)
    diag_H_mat_elem_no_elec_check += 1.d0 * mo_bielec_integral_jj(jorb,iorb) - 1.d0 *  mo_bielec_integral_jj_exchange(jorb,iorb)
   enddo
  enddo

  do i = 1, n_core_inact_orb
   iorb = list_core_inact(i)
   diag_H_mat_elem_no_elec_check += 1.d0 * mo_mono_elec_integral(iorb,iorb)
   do j = i+1, n_core_inact_orb
    jorb = list_core_inact(j)
    diag_H_mat_elem_no_elec_check += 1.d0 * mo_bielec_integral_jj(jorb,iorb) - 1.d0 *  mo_bielec_integral_jj_exchange(jorb,iorb)
   enddo
  enddo

  do i = 1, n_core_inact_orb
   iorb = list_core_inact(i)
   do j = 1, n_core_inact_orb
    jorb = list_core_inact(j)
    diag_H_mat_elem_no_elec_check += 1.d0 * mo_bielec_integral_jj(jorb,iorb) 
   enddo
  enddo
  
end

subroutine i_H_j_dyall(key_i,key_j,Nint,hij)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Returns <i|H|j> where i and j are determinants
  END_DOC
  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: key_i(Nint,2), key_j(Nint,2)
  double precision, intent(out)  :: hij
  
  integer                        :: exc(0:2,2,2)
  integer                        :: degree
  double precision               :: get_mo_bielec_integral
  integer                        :: m,n,p,q
  integer                        :: i,j,k
  integer                        :: occ(Nint*bit_kind_size,2)
  double precision               :: diag_H_mat_elem_no_elec_check, phase,phase_2
  integer                        :: n_occ_ab(2)
  logical                        :: has_mipi(Nint*bit_kind_size)
  double precision               :: mipi(Nint*bit_kind_size), miip(Nint*bit_kind_size)
  PROVIDE mo_bielec_integrals_in_map mo_integrals_map
  
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  
  hij = 0.d0
  !DIR$ FORCEINLINE
  call get_excitation_degree(key_i,key_j,degree,Nint)
  select case (degree)
    case (2)
      call get_double_excitation(key_i,key_j,exc,phase,Nint)
      if (exc(0,1,1) == 1) then
        ! Mono alpha, mono beta
        hij = phase*get_mo_bielec_integral(                          &
            exc(1,1,1),                                              &
            exc(1,1,2),                                              &
            exc(1,2,1),                                              &
            exc(1,2,2) ,mo_integrals_map)
      else if (exc(0,1,1) == 2) then
        ! Double alpha
        hij = phase*(get_mo_bielec_integral(                         &
            exc(1,1,1),                                              &
            exc(2,1,1),                                              &
            exc(1,2,1),                                              &
            exc(2,2,1) ,mo_integrals_map) -                          &
            get_mo_bielec_integral(                                  &
            exc(1,1,1),                                              &
            exc(2,1,1),                                              &
            exc(2,2,1),                                              &
            exc(1,2,1) ,mo_integrals_map) )
      else if (exc(0,1,2) == 2) then
        ! Double beta
        hij = phase*(get_mo_bielec_integral(                         &
            exc(1,1,2),                                              &
            exc(2,1,2),                                              &
            exc(1,2,2),                                              &
            exc(2,2,2) ,mo_integrals_map) -                          &
            get_mo_bielec_integral(                                  &
            exc(1,1,2),                                              &
            exc(2,1,2),                                              &
            exc(2,2,2),                                              &
            exc(1,2,2) ,mo_integrals_map) )
      endif
    case (1)
      call get_mono_excitation(key_i,key_j,exc,phase,Nint)
      !DIR$ FORCEINLINE
      call bitstring_to_list_ab(key_i, occ, n_occ_ab, Nint)
      has_mipi = .False.
      if (exc(0,1,1) == 1) then
        ! Mono alpha
        m = exc(1,1,1)
        p = exc(1,2,1)
        do k = 1, n_occ_ab(1)
          i = occ(k,1)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral(m,i,p,i,mo_integrals_map)
            miip(i) = get_mo_bielec_integral(m,i,i,p,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        do k = 1, n_occ_ab(2)
          i = occ(k,2)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral(m,i,p,i,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        
        do k = 1, n_occ_ab(1)
          hij = hij + mipi(occ(k,1)) - miip(occ(k,1))
        enddo
        do k = 1, n_occ_ab(2)
          hij = hij + mipi(occ(k,2))
        enddo
        
      else
        ! Mono beta
        m = exc(1,1,2)
        p = exc(1,2,2)
        do k = 1, n_occ_ab(2)
          i = occ(k,2)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral(m,i,p,i,mo_integrals_map)
            miip(i) = get_mo_bielec_integral(m,i,i,p,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        do k = 1, n_occ_ab(1)
          i = occ(k,1)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral(m,i,p,i,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        
        do k = 1, n_occ_ab(1)
          hij = hij + mipi(occ(k,1))
        enddo
        do k = 1, n_occ_ab(2)
          hij = hij + mipi(occ(k,2)) - miip(occ(k,2))
        enddo
        
      endif
      hij = phase*(hij + mo_mono_elec_integral(m,p)  + fock_operator_active_from_core_inact(m,p) )
      
    case (0)
      hij = diag_H_mat_elem_no_elec_check(key_i,Nint)
  end select
end


subroutine u0_H_dyall_u0(energies,psi_in,psi_in_coef,ndet,dim_psi_in,dim_psi_coef,N_states_in,state_target)
  use bitmasks
 implicit none
 integer, intent(in) :: N_states_in,ndet,dim_psi_in,dim_psi_coef,state_target
 integer(bit_kind), intent(in) :: psi_in(N_int,2,dim_psi_in)
 double precision,  intent(in) :: psi_in_coef(dim_psi_coef,N_states_in)
 double precision,  intent(out) :: energies(N_states_in)
 
 integer :: i,j 
 double precision :: hij,accu
 energies = 0.d0
 accu = 0.d0
 double precision, allocatable :: psi_coef_tmp(:)
 allocate(psi_coef_tmp(ndet))
 
 do i = 1, ndet
  psi_coef_tmp(i) = psi_in_coef(i,state_target)
 enddo

 double precision :: hij_bis
 do i = 1, ndet
  if(psi_coef_tmp(i)==0.d0)cycle
  do j = 1, ndet
   if(psi_coef_tmp(j)==0.d0)cycle
   call i_H_j_dyall(psi_in(1,1,i),psi_in(1,1,j),N_int,hij)
   accu += psi_coef_tmp(i) * psi_coef_tmp(j) * hij
  enddo
 enddo
 energies(state_target) = accu
 deallocate(psi_coef_tmp)
end


double precision function coulomb_value_no_check(det_in,Nint)
  implicit none
  BEGIN_DOC
  ! Computes <i|H|i>
  END_DOC
  integer,intent(in)             :: Nint
  integer(bit_kind),intent(in)   :: det_in(Nint,2)
  
  integer                        :: i, j, iorb, jorb 
  integer                        :: occ(Nint*bit_kind_size,2)
  integer                        ::  elec_num_tab_local(2)

  double precision :: core_act
  double precision :: alpha_alpha
  double precision :: alpha_beta
  double precision :: beta_beta
  double precision :: mono_elec
  core_act = 0.d0
  alpha_alpha = 0.d0
  alpha_beta = 0.d0
  beta_beta = 0.d0
  mono_elec = 0.d0

  coulomb_value_no_check = 0.d0
  call bitstring_to_list(det_in(1,1), occ(1,1), elec_num_tab_local(1), N_int)
  call bitstring_to_list(det_in(1,2), occ(1,2), elec_num_tab_local(2), N_int)
  ! alpha - alpha 
  do i = 1, elec_num_tab_local(1)
   iorb =  occ(i,1)
   do j = i+1, elec_num_tab_local(1)
    jorb = occ(j,1)
    coulomb_value_no_check +=  mo_bielec_integral_jj_anti(jorb,iorb)
    alpha_alpha += mo_bielec_integral_jj_anti(jorb,iorb)
   enddo
  enddo 

  ! beta - beta   
  do i = 1, elec_num_tab_local(2)
   iorb =  occ(i,2)
   do j = i+1, elec_num_tab_local(2)
    jorb = occ(j,2)
    coulomb_value_no_check +=  mo_bielec_integral_jj_anti(jorb,iorb)
    beta_beta +=  mo_bielec_integral_jj_anti(jorb,iorb)
   enddo
  enddo 
  

  ! alpha - beta   
  do i = 1, elec_num_tab_local(2)
   iorb =  occ(i,2)
   do j = 1, elec_num_tab_local(1)
    jorb = occ(j,1)
    coulomb_value_no_check +=  mo_bielec_integral_jj(jorb,iorb)
    alpha_beta += mo_bielec_integral_jj(jorb,iorb)
   enddo
  enddo 
  

end

subroutine i_H_j_dyall_no_exchange(key_i,key_j,Nint,hij)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Returns <i|H|j> where i and j are determinants
  END_DOC
  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: key_i(Nint,2), key_j(Nint,2)
  double precision, intent(out)  :: hij
  
  integer                        :: exc(0:2,2,2)
  integer                        :: degree
  double precision               :: get_mo_bielec_integral
  integer                        :: m,n,p,q
  integer                        :: i,j,k
  integer                        :: occ(Nint*bit_kind_size,2)
  double precision               :: diag_H_mat_elem_no_elec_check_no_exchange, phase,phase_2
  integer                        :: n_occ_ab(2)
  logical                        :: has_mipi(Nint*bit_kind_size)
  double precision               :: mipi(Nint*bit_kind_size)
  PROVIDE mo_bielec_integrals_in_map mo_integrals_map
  
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  
  hij = 0.d0
  !DIR$ FORCEINLINE
  call get_excitation_degree(key_i,key_j,degree,Nint)
  select case (degree)
    case (2)
      call get_double_excitation(key_i,key_j,exc,phase,Nint)
      if (exc(0,1,1) == 1) then
        ! Mono alpha, mono beta
        if(exc(1,1,1) == exc(1,2,2) .and. exc(1,2,1) == exc(1,1,2))then
         hij = 0.d0
        else 
         hij = phase*get_mo_bielec_integral(                          &
            exc(1,1,1),                                              &
            exc(1,1,2),                                              &
            exc(1,2,1),                                              &
            exc(1,2,2) ,mo_integrals_map)
        endif
      else if (exc(0,1,1) == 2) then
        ! Double alpha
        hij = phase*get_mo_bielec_integral(                &
            exc(1,1,1),                                              &
            exc(2,1,1),                                              &
            exc(1,2,1),                                              &
            exc(2,2,1) ,mo_integrals_map) 
      else if (exc(0,1,2) == 2) then
        ! Double beta
        hij = phase*get_mo_bielec_integral(                         &
            exc(1,1,2),                                              &
            exc(2,1,2),                                              &
            exc(1,2,2),                                              &
            exc(2,2,2) ,mo_integrals_map) 
      endif
    case (1)
      call get_mono_excitation(key_i,key_j,exc,phase,Nint)
      !DIR$ FORCEINLINE
      call bitstring_to_list_ab(key_i, occ, n_occ_ab, Nint)
      has_mipi = .False.
      if (exc(0,1,1) == 1) then
        ! Mono alpha
        m = exc(1,1,1)
        p = exc(1,2,1)
        do k = 1, n_occ_ab(1)
          i = occ(k,1)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral(m,i,p,i,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        do k = 1, n_occ_ab(2)
          i = occ(k,2)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral(m,i,p,i,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        
        do k = 1, n_occ_ab(1)
          hij = hij + mipi(occ(k,1)) 
        enddo
        do k = 1, n_occ_ab(2)
          hij = hij + mipi(occ(k,2))
        enddo
        
      else
        ! Mono beta
        m = exc(1,1,2)
        p = exc(1,2,2)
        do k = 1, n_occ_ab(2)
          i = occ(k,2)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral(m,i,p,i,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        do k = 1, n_occ_ab(1)
          i = occ(k,1)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral(m,i,p,i,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        
        do k = 1, n_occ_ab(1)
          hij = hij + mipi(occ(k,1))
        enddo
        do k = 1, n_occ_ab(2)
          hij = hij + mipi(occ(k,2)) 
        enddo
        
      endif
      hij = phase*(hij + mo_mono_elec_integral(m,p)  + fock_operator_active_from_core_inact(m,p) )
      
    case (0)
      hij = diag_H_mat_elem_no_elec_check_no_exchange(key_i,Nint)
  end select
end


double precision function diag_H_mat_elem_no_elec_check_no_exchange(det_in,Nint)
  implicit none
  BEGIN_DOC
  ! Computes <i|H|i>
  END_DOC
  integer,intent(in)             :: Nint
  integer(bit_kind),intent(in)   :: det_in(Nint,2)
  
  integer                        :: i, j, iorb, jorb 
  integer                        :: occ(Nint*bit_kind_size,2)
  integer                        ::  elec_num_tab_local(2)

  double precision :: core_act_exchange(2)
  core_act_exchange = 0.d0
  diag_H_mat_elem_no_elec_check_no_exchange = 0.d0
  call bitstring_to_list(det_in(1,1), occ(1,1), elec_num_tab_local(1), N_int)
  call bitstring_to_list(det_in(1,2), occ(1,2), elec_num_tab_local(2), N_int)
  ! alpha - alpha 
  do i = 1, elec_num_tab_local(1)
   iorb =  occ(i,1)
   diag_H_mat_elem_no_elec_check_no_exchange += mo_mono_elec_integral(iorb,iorb)
   do j = i+1, elec_num_tab_local(1)
    jorb = occ(j,1)
    diag_H_mat_elem_no_elec_check_no_exchange +=  mo_bielec_integral_jj(jorb,iorb)
   enddo
  enddo 

  ! beta - beta   
  do i = 1, elec_num_tab_local(2)
   iorb =  occ(i,2)
   diag_H_mat_elem_no_elec_check_no_exchange += mo_mono_elec_integral(iorb,iorb)
   do j = i+1, elec_num_tab_local(2)
    jorb = occ(j,2)
    diag_H_mat_elem_no_elec_check_no_exchange +=  mo_bielec_integral_jj(jorb,iorb)
   enddo
  enddo 
  

  ! alpha - beta   
  do i = 1, elec_num_tab_local(2)
   iorb =  occ(i,2)
   do j = 1, elec_num_tab_local(1)
    jorb = occ(j,1)
    diag_H_mat_elem_no_elec_check_no_exchange +=  mo_bielec_integral_jj(jorb,iorb)
   enddo
  enddo 
  

  ! alpha - core-act
  do i = 1, elec_num_tab_local(1)
   iorb =  occ(i,1)
   do j = 1, n_core_inact_orb
    jorb = list_core_inact(j)
    diag_H_mat_elem_no_elec_check_no_exchange +=  2.d0 * mo_bielec_integral_jj(jorb,iorb) 
    core_act_exchange(1) += - mo_bielec_integral_jj_exchange(jorb,iorb)
   enddo
  enddo 

  ! beta - core-act
  do i = 1, elec_num_tab_local(2)
   iorb =  occ(i,2)
   do j = 1, n_core_inact_orb
    jorb = list_core_inact(j)
    diag_H_mat_elem_no_elec_check_no_exchange +=  2.d0 * mo_bielec_integral_jj(jorb,iorb) 
    core_act_exchange(2) += - mo_bielec_integral_jj_exchange(jorb,iorb)
   enddo
  enddo 
  
end

subroutine u0_H_dyall_u0_no_exchange(energies,psi_in,psi_in_coef,ndet,dim_psi_in,dim_psi_coef,N_states_in,state_target)
  use bitmasks
 implicit none
 integer, intent(in) :: N_states_in,ndet,dim_psi_in,dim_psi_coef,state_target
 integer(bit_kind), intent(in) :: psi_in(N_int,2,dim_psi_in)
 double precision,  intent(in) :: psi_in_coef(dim_psi_coef,N_states_in)
 double precision,  intent(out) :: energies(N_states_in)
 
 integer :: i,j 
 double precision :: hij,accu
 energies = 0.d0
 accu = 0.d0
 double precision, allocatable :: psi_coef_tmp(:)
 allocate(psi_coef_tmp(ndet))
 
 do i = 1, ndet
  psi_coef_tmp(i) = psi_in_coef(i,state_target)
 enddo

 double precision :: hij_bis
 do i = 1, ndet
  if(psi_coef_tmp(i)==0.d0)cycle
  do j = 1, ndet
   if(psi_coef_tmp(j)==0.d0)cycle
   call i_H_j_dyall_no_exchange(psi_in(1,1,i),psi_in(1,1,j),N_int,hij)
   accu += psi_coef_tmp(i) * psi_coef_tmp(j) * hij
  enddo
 enddo
 energies(state_target) = accu
 deallocate(psi_coef_tmp)
end

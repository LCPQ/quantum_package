

subroutine i_H_j_no_k_operators_from_act(key_i,key_j,Nint,hij)
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
  double precision               :: get_mo_bielec_integral, phase
  integer                        :: m,n,p,q
  integer                        :: i,j,k
  integer                        :: occ(Nint*bit_kind_size,2)
  double precision               :: diag_H_mat_elem
  integer                        :: n_occ_ab(2)
  logical                        :: has_mipi(Nint*bit_kind_size)
  double precision               :: mipi(Nint*bit_kind_size), miip(Nint*bit_kind_size), miip_other(Nint*bit_kind_size)
  PROVIDE mo_bielec_integrals_in_map mo_integrals_map

  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (sum(popcnt(key_i(:,1))) == elec_alpha_num)
  ASSERT (sum(popcnt(key_i(:,2))) == elec_beta_num)
  ASSERT (sum(popcnt(key_j(:,1))) == elec_alpha_num)
  ASSERT (sum(popcnt(key_j(:,2))) == elec_beta_num)

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
      logical :: is_i_in_active
      double precision :: accu_a, accu_b, accu_core
      accu_a = 0.d0
      accu_b = 0.d0
      if (exc(0,1,1) == 1) then
        ! Mono alpha
        m = exc(1,1,1)
        p = exc(1,2,1)
        do k = 1, elec_alpha_num
          i = occ(k,1)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral(m,i,p,i,mo_integrals_map)
            has_mipi(i) = .True.
          endif
          if(.not.is_i_in_active(i))then
           miip(i) = get_mo_bielec_integral(m,i,i,p,mo_integrals_map)
          else 
!          print*, i,get_mo_bielec_integral(m,i,i,p,mo_integrals_map)
           miip(i) = 1.0d0 * get_mo_bielec_integral(m,i,i,p,mo_integrals_map)
           accu_a += miip(i)
          endif
        enddo
        do k = 1, elec_beta_num
          i = occ(k,2)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral(m,i,p,i,mo_integrals_map)
            has_mipi(i) = .True.
          endif
          if(.not.is_i_in_active(i))then
           miip_other(i) = 0.d0
          else 
!          print*, i,get_mo_bielec_integral(m,i,i,p,mo_integrals_map)
           miip_other(i) = 1.0d0 * get_mo_bielec_integral(m,i,i,p,mo_integrals_map)
           accu_b += miip(i)
          endif
        enddo
!       print*, accu_a,accu_b,accu_a + accu_b
        accu_a = 0.d0
        accu_b = 0.d0
        accu_core = mo_mono_elec_integral(m,p)

        do k = 1, elec_alpha_num
          hij = hij + mipi(occ(k,1))  - miip(occ(k,1))
          accu_Core += mipi(occ(k,1))
          if(is_i_in_active(occ(k,1)))then
           accu_a += miip(occ(k,1))
          else 
           accu_Core -= miip(occ(k,1))
          endif
        enddo
!       print*, hij,accu_core
        do k = 1, elec_beta_num
          hij = hij + mipi(occ(k,2))  - miip_other(occ(k,2))
          accu_Core += mipi(occ(k,2))
          if(is_i_in_active(occ(k,2)))then
           accu_b += miip_other(occ(k,2))
          else 
           accu_Core -= miip_other(occ(k,2))
          endif
        enddo
!       print*, hij,accu_core,accu_core - accu_a - accu_b
!       print*, accu_a,accu_b,accu_a + accu_b
!       pause

      else
        ! Mono beta
        m = exc(1,1,2)
        p = exc(1,2,2)
        do k = 1, elec_beta_num
          i = occ(k,2)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral(m,i,p,i,mo_integrals_map)
            has_mipi(i) = .True.
          endif
          if(.not.is_i_in_active(i))then
           miip(i) = get_mo_bielec_integral(m,i,i,p,mo_integrals_map)
          else 
           miip(i) = 1.0d0 * get_mo_bielec_integral(m,i,i,p,mo_integrals_map)
          endif
        enddo
        do k = 1, elec_alpha_num
          i = occ(k,1)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral(m,i,p,i,mo_integrals_map)
            has_mipi(i) = .True.
          endif
          if(.not.is_i_in_active(i))then
           miip_other(i) = 0.d0
          else 
           miip_other(i) = 1.0d0 * get_mo_bielec_integral(m,i,i,p,mo_integrals_map)
          endif
        enddo
        do k = 1, elec_alpha_num
          hij = hij + mipi(occ(k,1)) - miip_other(occ(k,1))
        enddo
        do k = 1, elec_beta_num
          hij = hij + mipi(occ(k,2)) - miip(occ(k,2))
        enddo

      endif
      hij = phase*(hij + mo_mono_elec_integral(m,p))

    case (0)
      hij = diag_H_mat_elem(key_i,Nint)
  end select
end




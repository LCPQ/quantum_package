subroutine get_excitation_degree(key1,key2,degree,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Returns the excitation degree between two determinants
  END_DOC
  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: key1(Nint,2)
  integer(bit_kind), intent(in)  :: key2(Nint,2)
  integer, intent(out)           :: degree
  
  integer                        :: l
  
  ASSERT (Nint > 0)
  
  degree = popcnt(xor( key1(1,1), key2(1,1))) +                      &
      popcnt(xor( key1(1,2), key2(1,2)))
  !DIR$ NOUNROLL
  do l=2,Nint
    degree = degree+ popcnt(xor( key1(l,1), key2(l,1))) +            &
        popcnt(xor( key1(l,2), key2(l,2)))
  enddo
  ASSERT (degree >= 0)
  degree = ishft(degree,-1)
  
end



subroutine get_excitation(det1,det2,exc,degree,phase,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Returns the excitation operators between two determinants and the phase
  END_DOC
  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: det1(Nint,2)
  integer(bit_kind), intent(in)  :: det2(Nint,2)
  integer, intent(out)           :: exc(0:2,2,2)
  integer, intent(out)           :: degree
  double precision, intent(out)  :: phase
  ! exc(number,hole/particle,spin)
  ! ex :
  ! exc(0,1,1) = number of holes alpha
  ! exc(0,2,1) = number of particle alpha
  ! exc(0,2,2) = number of particle beta
  ! exc(1,2,1) = first particle alpha
  ! exc(1,1,1) = first hole     alpha
  ! exc(1,2,2) = first particle beta
  ! exc(1,1,2) = first hole     beta
  
  ASSERT (Nint > 0)
  
  !DIR$ FORCEINLINE
  call get_excitation_degree(det1,det2,degree,Nint)
  select case (degree)
      
    case (3:)
      degree = -1
      return
      
    case (2)
      call get_double_excitation(det1,det2,exc,phase,Nint)
      return
      
    case (1)
      call get_mono_excitation(det1,det2,exc,phase,Nint)
      return
      
    case(0)
      return
      
  end select
end

subroutine decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Decodes the exc arrays returned by get_excitation.
  ! h1,h2 : Holes
  ! p1,p2 : Particles
  ! s1,s2 : Spins (1:alpha, 2:beta)
  ! degree : Degree of excitation
  END_DOC
  integer, intent(in)            :: exc(0:2,2,2),degree
  integer, intent(out)           :: h1,h2,p1,p2,s1,s2
  ASSERT (degree > 0)
  ASSERT (degree < 3)
  
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
  end select
end


subroutine get_double_excitation(det1,det2,exc,phase,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Returns the two excitation operators between two doubly excited determinants and the phase
  END_DOC
  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: det1(Nint,2)
  integer(bit_kind), intent(in)  :: det2(Nint,2)
  integer, intent(out)           :: exc(0:2,2,2)
  double precision, intent(out)  :: phase
  integer                        :: tz
  integer                        :: l, ispin, idx_hole, idx_particle, ishift
  integer                        :: nperm
  integer                        :: i,j,k,m,n
  integer                        :: high, low
  integer                        :: a,b,c,d
  integer(bit_kind)              :: hole, particle, tmp
  double precision, parameter    :: phase_dble(0:1) = (/ 1.d0, -1.d0 /)
  
  ASSERT (Nint > 0)
  nperm = 0
  exc(0,1,1) = 0
  exc(0,2,1) = 0
  exc(0,1,2) = 0
  exc(0,2,2) = 0
  do ispin = 1,2
    idx_particle = 0
    idx_hole = 0
    ishift = 1-bit_kind_size
    do l=1,Nint
      ishift = ishift + bit_kind_size
      if (det1(l,ispin) == det2(l,ispin)) then
        cycle
      endif
      tmp = xor( det1(l,ispin), det2(l,ispin) )
      particle = iand(tmp, det2(l,ispin))
      hole     = iand(tmp, det1(l,ispin))
      do while (particle /= 0_bit_kind)
        tz = trailz(particle)
        idx_particle = idx_particle + 1
        exc(0,2,ispin) = exc(0,2,ispin) + 1
        exc(idx_particle,2,ispin) = tz+ishift
        particle = iand(particle,particle-1_bit_kind)
      enddo
      if (iand(exc(0,1,ispin),exc(0,2,ispin))==2) then  ! exc(0,1,ispin)==2 or exc(0,2,ispin)==2
        exit
      endif
      do while (hole /= 0_bit_kind)
        tz = trailz(hole)
        idx_hole = idx_hole + 1
        exc(0,1,ispin) = exc(0,1,ispin) + 1
        exc(idx_hole,1,ispin) = tz+ishift
        hole = iand(hole,hole-1_bit_kind)
      enddo
      if (iand(exc(0,1,ispin),exc(0,2,ispin))==2) then ! exc(0,1,ispin)==2 or exc(0,2,ispin)
        exit
      endif
    enddo
    
    select case (exc(0,1,ispin))
      case(0)
        cycle
        
      case(1)
        low  = min(exc(1,1,ispin), exc(1,2,ispin))
        high = max(exc(1,1,ispin), exc(1,2,ispin))
        
        ASSERT (low > 0)
        j = ishft(low-1,-bit_kind_shift)+1   ! Find integer in array(Nint)
        n = iand(low-1,bit_kind_size-1)+1        ! mod(low,bit_kind_size)
        ASSERT (high > 0)
        k = ishft(high-1,-bit_kind_shift)+1
        m = iand(high-1,bit_kind_size-1)+1
        
        if (j==k) then
          nperm = nperm + popcnt(iand(det1(j,ispin),                 &
              iand( ibset(0_bit_kind,m-1)-1_bit_kind,                &
              ibclr(-1_bit_kind,n)+1_bit_kind ) ))
        else
          nperm = nperm + popcnt(iand(det1(k,ispin),                 &
              ibset(0_bit_kind,m-1)-1_bit_kind)) 
          if (n < bit_kind_size) then
              nperm = nperm + popcnt(iand(det1(j,ispin), ibclr(-1_bit_kind,n) +1_bit_kind))
          endif
          do i=j+1,k-1
            nperm = nperm + popcnt(det1(i,ispin))
          end do
        endif
        
      case (2)
        
        do i=1,2
          low  = min(exc(i,1,ispin), exc(i,2,ispin))
          high = max(exc(i,1,ispin), exc(i,2,ispin))
          
          ASSERT (low > 0)
          j = ishft(low-1,-bit_kind_shift)+1   ! Find integer in array(Nint)
          n = iand(low-1,bit_kind_size-1)+1        ! mod(low,bit_kind_size)
          ASSERT (high > 0)
          k = ishft(high-1,-bit_kind_shift)+1
          m = iand(high-1,bit_kind_size-1)+1
          
          if (j==k) then
            nperm = nperm + popcnt(iand(det1(j,ispin),               &
                iand( ibset(0_bit_kind,m-1)-1_bit_kind,              &
                ibclr(-1_bit_kind,n)+1_bit_kind ) ))
          else
            nperm = nperm + popcnt(iand(det1(k,ispin),               &
                ibset(0_bit_kind,m-1)-1_bit_kind)) 
            if (n < bit_kind_size) then
               nperm = nperm + popcnt(iand(det1(j,ispin), ibclr(-1_bit_kind,n) +1_bit_kind))
            endif
            do l=j+1,k-1
              nperm = nperm + popcnt(det1(l,ispin))
            end do
          endif
          
        enddo
        
        a = min(exc(1,1,ispin), exc(1,2,ispin))
        b = max(exc(1,1,ispin), exc(1,2,ispin))
        c = min(exc(2,1,ispin), exc(2,2,ispin))
        d = max(exc(2,1,ispin), exc(2,2,ispin))
        if (c>a .and. c<b .and. d>b) then
          nperm = nperm + 1
        endif
        exit
    end select
    
  enddo
  phase = phase_dble(iand(nperm,1))
  
end

subroutine get_mono_excitation(det1,det2,exc,phase,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Returns the excitation operator between two singly excited determinants and the phase
  END_DOC
  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: det1(Nint,2)
  integer(bit_kind), intent(in)  :: det2(Nint,2)
  integer, intent(out)           :: exc(0:2,2,2)
  double precision, intent(out)  :: phase
  integer                        :: tz
  integer                        :: l, ispin, idx_hole, idx_particle, ishift
  integer                        :: nperm
  integer                        :: i,j,k,m,n
  integer                        :: high, low
  integer                        :: a,b,c,d
  integer(bit_kind)              :: hole, particle, tmp
  double precision, parameter    :: phase_dble(0:1) = (/ 1.d0, -1.d0 /)
  
  ASSERT (Nint > 0)
  nperm = 0
  exc(0,1,1) = 0
  exc(0,2,1) = 0
  exc(0,1,2) = 0
  exc(0,2,2) = 0
  do ispin = 1,2
    ishift = 1-bit_kind_size
    do l=1,Nint
      ishift = ishift + bit_kind_size
      if (det1(l,ispin) == det2(l,ispin)) then
        cycle
      endif
      tmp = xor( det1(l,ispin), det2(l,ispin) )
      particle = iand(tmp, det2(l,ispin))
      hole     = iand(tmp, det1(l,ispin))
      if (particle /= 0_bit_kind) then
        tz = trailz(particle)
        exc(0,2,ispin) = 1
        exc(1,2,ispin) = tz+ishift
      endif
      if (hole /= 0_bit_kind) then
        tz = trailz(hole)
        exc(0,1,ispin) = 1
        exc(1,1,ispin) = tz+ishift
      endif
      
      if ( iand(exc(0,1,ispin),exc(0,2,ispin)) /= 1) then  ! exc(0,1,ispin)/=1 and exc(0,2,ispin) /= 1
        cycle
      endif
      
      low = min(exc(1,1,ispin),exc(1,2,ispin))
      high = max(exc(1,1,ispin),exc(1,2,ispin))
      
      ASSERT (low > 0)
      j = ishft(low-1,-bit_kind_shift)+1   ! Find integer in array(Nint)
      n = iand(low-1,bit_kind_size-1)+1      ! mod(low,bit_kind_size)
      ASSERT (high > 0)
      k = ishft(high-1,-bit_kind_shift)+1
      m = iand(high-1,bit_kind_size-1)+1
      if (j==k) then
        nperm = popcnt(iand(det1(j,ispin),                           &
            iand(ibset(0_bit_kind,m-1)-1_bit_kind,ibclr(-1_bit_kind,n)+1_bit_kind)))
      else
        nperm = nperm + popcnt(iand(det1(k,ispin),ibset(0_bit_kind,m-1)-1_bit_kind))
        if (n < bit_kind_size) then
            nperm = nperm + popcnt(iand(det1(j,ispin),ibclr(-1_bit_kind,n)+1_bit_kind))
        endif
        do i=j+1,k-1
          nperm = nperm + popcnt(det1(i,ispin))
        end do
      endif
      phase = phase_dble(iand(nperm,1))
      return
      
    enddo
  enddo
end

subroutine bitstring_to_list_ab( string, list, n_elements, Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Gives the inidices(+1) of the bits set to 1 in the bit string
  ! For alpha/beta determinants
  END_DOC
  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: string(Nint,2)
  integer, intent(out)           :: list(Nint*bit_kind_size,2)
  integer, intent(out)           :: n_elements(2)

  integer                        :: i, j, ishift
  integer(bit_kind)              :: l

  n_elements(1) = 0
  n_elements(2) = 0
  ishift = 1
  do i=1,Nint
    l = string(i,1)
    do while (l /= 0_bit_kind)
      j = trailz(l)
      n_elements(1) = n_elements(1)+1
      l = ibclr(l,j)
      list(n_elements(1),1) = ishift+j
    enddo
    l = string(i,2)
    do while (l /= 0_bit_kind)
      j = trailz(l)
      n_elements(2) = n_elements(2)+1
      l = ibclr(l,j)
      list(n_elements(2),2) = ishift+j
    enddo
    ishift = ishift + bit_kind_size
  enddo

end

subroutine bitstring_to_list_ab_old( string, list, n_elements, Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Gives the inidices(+1) of the bits set to 1 in the bit string
  ! For alpha/beta determinants
  END_DOC
  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: string(Nint,2)
  integer, intent(out)           :: list(Nint*bit_kind_size,2)
  integer, intent(out)           :: n_elements(2)

  integer                        :: i, ishift
  integer(bit_kind)              :: l

  n_elements(1) = 0
  n_elements(2) = 0
  ishift = 2
  do i=1,Nint
    l = string(i,1)
    do while (l /= 0_bit_kind)
      n_elements(1) = n_elements(1)+1
      list(n_elements(1),1) = ishift+popcnt(l-1_bit_kind) - popcnt(l)
      l = iand(l,l-1_bit_kind)
    enddo
    l = string(i,2)
    do while (l /= 0_bit_kind)
      n_elements(2) = n_elements(2)+1
      list(n_elements(2),2) = ishift+popcnt(l-1_bit_kind) - popcnt(l)
      l = iand(l,l-1_bit_kind)
    enddo
    ishift = ishift + bit_kind_size
  enddo

end





subroutine i_H_j(key_i,key_j,Nint,hij)
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
  double precision               :: get_mo_bielec_integral_schwartz
  integer                        :: m,n,p,q
  integer                        :: i,j,k
  integer                        :: occ(Nint*bit_kind_size,2)
  double precision               :: diag_H_mat_elem, phase,phase_2
  integer                        :: n_occ_ab(2)
  logical                        :: has_mipi(Nint*bit_kind_size)
  double precision               :: mipi(Nint*bit_kind_size), miip(Nint*bit_kind_size)
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
        hij = phase*get_mo_bielec_integral_schwartz(                          &
            exc(1,1,1),                                              &
            exc(1,1,2),                                              &
            exc(1,2,1),                                              &
            exc(1,2,2) ,mo_integrals_map)
      else if (exc(0,1,1) == 2) then
        ! Double alpha
        hij = phase*(get_mo_bielec_integral_schwartz(                         &
            exc(1,1,1),                                              &
            exc(2,1,1),                                              &
            exc(1,2,1),                                              &
            exc(2,2,1) ,mo_integrals_map) -                          &
            get_mo_bielec_integral_schwartz(                                  &
            exc(1,1,1),                                              &
            exc(2,1,1),                                              &
            exc(2,2,1),                                              &
            exc(1,2,1) ,mo_integrals_map) )
      else if (exc(0,1,2) == 2) then
        ! Double beta
        hij = phase*(get_mo_bielec_integral_schwartz(                         &
            exc(1,1,2),                                              &
            exc(2,1,2),                                              &
            exc(1,2,2),                                              &
            exc(2,2,2) ,mo_integrals_map) -                          &
            get_mo_bielec_integral_schwartz(                                  &
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
        do k = 1, elec_alpha_num
          i = occ(k,1)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral_schwartz(m,i,p,i,mo_integrals_map)
            miip(i) = get_mo_bielec_integral_schwartz(m,i,i,p,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        do k = 1, elec_beta_num
          i = occ(k,2)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral_schwartz(m,i,p,i,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        
        do k = 1, elec_alpha_num
          hij = hij + mipi(occ(k,1)) - miip(occ(k,1))
        enddo
        do k = 1, elec_beta_num
          hij = hij + mipi(occ(k,2))
        enddo
        
      else
        ! Mono beta
        m = exc(1,1,2)
        p = exc(1,2,2)
        do k = 1, elec_beta_num
          i = occ(k,2)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral_schwartz(m,i,p,i,mo_integrals_map)
            miip(i) = get_mo_bielec_integral_schwartz(m,i,i,p,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        do k = 1, elec_alpha_num
          i = occ(k,1)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral_schwartz(m,i,p,i,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        
        do k = 1, elec_alpha_num
          hij = hij + mipi(occ(k,1))
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



subroutine i_H_j_phase_out(key_i,key_j,Nint,hij,phase,exc,degree)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Returns <i|H|j> where i and j are determinants
  END_DOC
  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: key_i(Nint,2), key_j(Nint,2)
  double precision, intent(out)  :: hij, phase

  integer,intent(out)            :: exc(0:2,2,2)
  integer,intent(out)            :: degree
  double precision               :: get_mo_bielec_integral_schwartz
  integer                        :: m,n,p,q
  integer                        :: i,j,k
  integer                        :: occ(Nint*bit_kind_size,2)
  double precision               :: diag_H_mat_elem
  integer                        :: n_occ_ab(2)
  logical                        :: has_mipi(Nint*bit_kind_size)
  double precision               :: mipi(Nint*bit_kind_size), miip(Nint*bit_kind_size)
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
        hij = phase*get_mo_bielec_integral_schwartz(                          &
            exc(1,1,1),                                              &
            exc(1,1,2),                                              &
            exc(1,2,1),                                              &
            exc(1,2,2) ,mo_integrals_map)
      else if (exc(0,1,1) == 2) then
        ! Double alpha
        hij = phase*(get_mo_bielec_integral_schwartz(                         &
            exc(1,1,1),                                              &
            exc(2,1,1),                                              &
            exc(1,2,1),                                              &
            exc(2,2,1) ,mo_integrals_map) -                          &
            get_mo_bielec_integral_schwartz(                                  &
            exc(1,1,1),                                              &
            exc(2,1,1),                                              &
            exc(2,2,1),                                              &
            exc(1,2,1) ,mo_integrals_map) )
      else if (exc(0,1,2) == 2) then
        ! Double beta
        hij = phase*(get_mo_bielec_integral_schwartz(                         &
            exc(1,1,2),                                              &
            exc(2,1,2),                                              &
            exc(1,2,2),                                              &
            exc(2,2,2) ,mo_integrals_map) -                          &
            get_mo_bielec_integral_schwartz(                                  &
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
        do k = 1, elec_alpha_num
          i = occ(k,1)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral_schwartz(m,i,p,i,mo_integrals_map)
            miip(i) = get_mo_bielec_integral_schwartz(m,i,i,p,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        do k = 1, elec_beta_num
          i = occ(k,2)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral_schwartz(m,i,p,i,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo

        do k = 1, elec_alpha_num
          hij = hij + mipi(occ(k,1)) - miip(occ(k,1))
        enddo
        do k = 1, elec_beta_num
          hij = hij + mipi(occ(k,2))
        enddo

      else
        ! Mono beta
        m = exc(1,1,2)
        p = exc(1,2,2)
        do k = 1, elec_beta_num
          i = occ(k,2)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral_schwartz(m,i,p,i,mo_integrals_map)
            miip(i) = get_mo_bielec_integral_schwartz(m,i,i,p,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        do k = 1, elec_alpha_num
          i = occ(k,1)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral_schwartz(m,i,p,i,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo

        do k = 1, elec_alpha_num
          hij = hij + mipi(occ(k,1))
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



subroutine i_H_j_verbose(key_i,key_j,Nint,hij,hmono,hdouble)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Returns <i|H|j> where i and j are determinants
  END_DOC
  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: key_i(Nint,2), key_j(Nint,2)
  double precision, intent(out)  :: hij,hmono,hdouble
  
  integer                        :: exc(0:2,2,2)
  integer                        :: degree
  double precision               :: get_mo_bielec_integral_schwartz
  integer                        :: m,n,p,q
  integer                        :: i,j,k
  integer                        :: occ(Nint*bit_kind_size,2)
  double precision               :: diag_H_mat_elem, phase,phase_2
  integer                        :: n_occ_ab(2)
  logical                        :: has_mipi(Nint*bit_kind_size)
  double precision               :: mipi(Nint*bit_kind_size), miip(Nint*bit_kind_size)
  PROVIDE mo_bielec_integrals_in_map mo_integrals_map
  
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (sum(popcnt(key_i(:,1))) == elec_alpha_num)
  ASSERT (sum(popcnt(key_i(:,2))) == elec_beta_num)
  ASSERT (sum(popcnt(key_j(:,1))) == elec_alpha_num)
  ASSERT (sum(popcnt(key_j(:,2))) == elec_beta_num)
  
  hij = 0.d0
  hmono = 0.d0
  hdouble = 0.d0
  !DIR$ FORCEINLINE
  call get_excitation_degree(key_i,key_j,degree,Nint)
  select case (degree)
    case (2)
      call get_double_excitation(key_i,key_j,exc,phase,Nint)
      if (exc(0,1,1) == 1) then
        ! Mono alpha, mono beta
        hij = phase*get_mo_bielec_integral_schwartz(                          &
            exc(1,1,1),                                              &
            exc(1,1,2),                                              &
            exc(1,2,1),                                              &
            exc(1,2,2) ,mo_integrals_map)
      else if (exc(0,1,1) == 2) then
        ! Double alpha
        print*,'phase hij = ',phase 
        hij = phase*(get_mo_bielec_integral_schwartz(                         &
            exc(1,1,1),                                              &
            exc(2,1,1),                                              &
            exc(1,2,1),                                              &
            exc(2,2,1) ,mo_integrals_map) -                          &
            get_mo_bielec_integral_schwartz(                                  &
            exc(1,1,1),                                              &
            exc(2,1,1),                                              &
            exc(2,2,1),                                              &
            exc(1,2,1) ,mo_integrals_map) )
            print*,get_mo_bielec_integral_schwartz(                  &
            exc(1,1,1),                                              &
            exc(2,1,1),                                              &
            exc(1,2,1),                                              &
            exc(2,2,1) ,mo_integrals_map) 
            print*,get_mo_bielec_integral_schwartz(                  &
            exc(1,1,1),                                              &
            exc(2,1,1),                                              &
            exc(2,2,1),                                              &
            exc(1,2,1) ,mo_integrals_map) 

      else if (exc(0,1,2) == 2) then
        ! Double beta
        print*,'phase hij = ',phase 
        print*, get_mo_bielec_integral_schwartz(                         &
            exc(1,1,2),                                              &
            exc(2,1,2),                                              &
            exc(1,2,2),                                              &
            exc(2,2,2) ,mo_integrals_map )
        print*,     get_mo_bielec_integral_schwartz(                                  &
            exc(1,1,2),                                              &
            exc(2,1,2),                                              &   
            exc(2,2,2),                                              &
            exc(1,2,2) ,mo_integrals_map)                    

        hij = phase*(get_mo_bielec_integral_schwartz(                         &
            exc(1,1,2),                                              &
            exc(2,1,2),                                              &
            exc(1,2,2),                                              &
            exc(2,2,2) ,mo_integrals_map) -                          &
            get_mo_bielec_integral_schwartz(                                  &
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
        do k = 1, elec_alpha_num
          i = occ(k,1)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral_schwartz(m,i,p,i,mo_integrals_map)
            miip(i) = get_mo_bielec_integral_schwartz(m,i,i,p,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        do k = 1, elec_beta_num
          i = occ(k,2)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral_schwartz(m,i,p,i,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        
        do k = 1, elec_alpha_num
          hdouble = hdouble + mipi(occ(k,1)) - miip(occ(k,1))
        enddo
        do k = 1, elec_beta_num
          hdouble = hdouble + mipi(occ(k,2))
        enddo
        
      else
        ! Mono beta
        m = exc(1,1,2)
        p = exc(1,2,2)
        do k = 1, elec_beta_num
          i = occ(k,2)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral_schwartz(m,i,p,i,mo_integrals_map)
            miip(i) = get_mo_bielec_integral_schwartz(m,i,i,p,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        do k = 1, elec_alpha_num
          i = occ(k,1)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral_schwartz(m,i,p,i,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        
        do k = 1, elec_alpha_num
          hdouble = hdouble + mipi(occ(k,1))
        enddo
        do k = 1, elec_beta_num
          hdouble = hdouble + mipi(occ(k,2)) - miip(occ(k,2))
        enddo
        
      endif
      hmono = mo_mono_elec_integral(m,p)
      hij = phase*(hdouble + hmono)
      
    case (0)
      hij = diag_H_mat_elem(key_i,Nint)
  end select
end


subroutine create_minilist(key_mask, fullList, miniList, idx_miniList, N_fullList, N_miniList, Nint)
  use bitmasks
  implicit none
  
  integer, intent(in)                      :: N_fullList
  integer, intent(in)                      :: Nint
  integer(bit_kind), intent(in)            :: fullList(Nint, 2, N_fullList)
  integer(bit_kind),intent(out)            :: miniList(Nint, 2, N_fullList)
  integer,intent(out)                      :: idx_miniList(N_fullList), N_miniList
  integer(bit_kind)                        :: key_mask(Nint, 2)
  integer                                  :: ni, k, i, n_a, n_b, e_a, e_b
  
  
  n_a = popcnt(key_mask(1,1))
  n_b = popcnt(key_mask(1,2))
  do ni=2,nint
    n_a = n_a + popcnt(key_mask(ni,1))
    n_b = n_b + popcnt(key_mask(ni,2))
  end do
  
  if(n_a == 0) then
    N_miniList = N_fullList
    do k=1,N_fullList
      do ni=1,Nint
        miniList(ni,1,k) = fullList(ni,1,k)
        miniList(ni,2,k) = fullList(ni,2,k)
      enddo
    enddo
    do i=1,N_fullList
      idx_miniList(i) = i
    end do
    return
  end if
  
  N_miniList = 0
  
  do i=1,N_fullList
    e_a = n_a - popcnt(iand(fullList(1, 1, i), key_mask(1, 1)))
    e_b = n_b - popcnt(iand(fullList(1, 2, i), key_mask(1, 2)))
    do ni=2,nint
      e_a -= popcnt(iand(fullList(ni, 1, i), key_mask(ni, 1)))
      e_b -= popcnt(iand(fullList(ni, 2, i), key_mask(ni, 2)))
    end do
    
    if(e_a + e_b <= 2) then
      N_miniList = N_miniList + 1
      do ni=1,Nint
        miniList(ni,1,N_miniList) = fullList(ni,1,i)
        miniList(ni,2,N_miniList) = fullList(ni,2,i)
      enddo
      idx_miniList(N_miniList) = i
    end if
  end do
end subroutine

subroutine create_minilist_find_previous(key_mask, fullList, miniList, N_fullList, N_miniList, fullMatch, Nint)
  use bitmasks
  implicit none
  
  integer, intent(in)                      :: N_fullList
  integer, intent(in)                      :: Nint
  integer(bit_kind), intent(in)            :: fullList(Nint, 2, N_fullList)
  integer(bit_kind),intent(out)            :: miniList(Nint, 2, N_fullList)
  integer(bit_kind)                        :: subList(Nint, 2, N_fullList)
  logical,intent(out)                      :: fullMatch
  integer,intent(out)                      :: N_miniList
  integer(bit_kind)                        :: key_mask(Nint, 2)
  integer                                  :: ni, i, k, l, N_subList
  
  
  fullMatch = .false.
  N_miniList = 0
  N_subList = 0
    
  l = popcnt(key_mask(1,1)) + popcnt(key_mask(1,2))
  do ni = 2,Nint
    l = l + popcnt(key_mask(ni,1)) + popcnt(key_mask(ni,2))
  end do
  
  if(l == 0) then
    N_miniList = N_fullList
    do k=1,N_fullList
      do ni=1,Nint
        miniList(ni,1,k) = fullList(ni,1,k)
        miniList(ni,2,k) = fullList(ni,2,k)
      enddo
    enddo
  else
    do i=N_fullList,1,-1
      k = l
      do ni=1,nint
        k -= popcnt(iand(key_mask(ni,1), fullList(ni,1,i))) + popcnt(iand(key_mask(ni,2), fullList(ni,2,i)))
      end do
      if(k == 2) then
        N_subList += 1
        do ni=1,Nint
          subList(ni,1,N_subList) = fullList(ni,1,i)
          subList(ni,2,N_subList) = fullList(ni,2,i)
        enddo
      else if(k == 1) then
        N_minilist += 1
        do ni=1,Nint
          miniList(ni,1,N_minilist) = fullList(ni,1,i)
          miniList(ni,2,N_minilist) = fullList(ni,2,i)
        enddo
      else if(k == 0) then
        fullMatch = .true.
        return
      end if
    end do
  end if
  
  if(N_subList > 0) then
    do k=1,N_subList
      do ni=1,Nint
        miniList(ni,1,N_minilist+k) = sublist(ni,1,k)
        miniList(ni,2,N_minilist+k) = sublist(ni,2,k)
      enddo
    enddo
    N_minilist = N_minilist + N_subList
  end if
end subroutine


subroutine i_H_psi(key,keys,coef,Nint,Ndet,Ndet_max,Nstate,i_H_psi_array)
  use bitmasks
  implicit none
  BEGIN_DOC
! Computes <i|H|Psi> = \sum_J c_J <i|H|J>.
!
! Uses filter_connected_i_H_psi0 to get all the |J> to which |i>
! is connected.
! The i_H_psi_minilist is much faster but requires to build the
! minilists
  END_DOC
  integer, intent(in)            :: Nint, Ndet,Ndet_max,Nstate
  integer(bit_kind), intent(in)  :: keys(Nint,2,Ndet)
  integer(bit_kind), intent(in)  :: key(Nint,2)
  double precision, intent(in)   :: coef(Ndet_max,Nstate)
  double precision, intent(out)  :: i_H_psi_array(Nstate)
  
  integer                        :: i, ii,j
  double precision               :: phase
  integer                        :: exc(0:2,2,2)
  double precision               :: hij
  integer                        :: idx(0:Ndet)
  
  ASSERT (Nint > 0)
  ASSERT (N_int == Nint)
  ASSERT (Nstate > 0)
  ASSERT (Ndet > 0)
  ASSERT (Ndet_max >= Ndet)
  i_H_psi_array = 0.d0
  
  call filter_connected_i_H_psi0(keys,key,Nint,Ndet,idx)
  if (Nstate == 1) then

    do ii=1,idx(0)
      i = idx(ii)
      !DIR$ FORCEINLINE
      call i_H_j(keys(1,1,i),key,Nint,hij)
      i_H_psi_array(1) = i_H_psi_array(1) + coef(i,1)*hij
    enddo

  else

    do ii=1,idx(0)
      i = idx(ii)
      !DIR$ FORCEINLINE
      call i_H_j(keys(1,1,i),key,Nint,hij)
      do j = 1, Nstate
        i_H_psi_array(j) = i_H_psi_array(j) + coef(i,j)*hij
      enddo
    enddo

  endif

end


subroutine i_H_psi_minilist(key,keys,idx_key,N_minilist,coef,Nint,Ndet,Ndet_max,Nstate,i_H_psi_array)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint, Ndet,Ndet_max,Nstate,idx_key(Ndet), N_minilist
  integer(bit_kind), intent(in)  :: keys(Nint,2,Ndet)
  integer(bit_kind), intent(in)  :: key(Nint,2)
  double precision, intent(in)   :: coef(Ndet_max,Nstate)
  double precision, intent(out)  :: i_H_psi_array(Nstate)
  
  integer                        :: i, ii,j, i_in_key, i_in_coef
  double precision               :: phase
  integer                        :: exc(0:2,2,2)
  double precision               :: hij
  integer                        :: idx(0:Ndet)
  BEGIN_DOC
! Computes <i|H|Psi> = \sum_J c_J <i|H|J>.
!
! Uses filter_connected_i_H_psi0 to get all the |J> to which |i>
! is connected. The |J> are searched in short pre-computed lists.
  END_DOC
  
  ASSERT (Nint > 0)
  ASSERT (N_int == Nint)
  ASSERT (Nstate > 0)
  ASSERT (Ndet > 0)
  ASSERT (Ndet_max >= Ndet)
  i_H_psi_array = 0.d0
  
  call filter_connected_i_H_psi0(keys,key,Nint,N_minilist,idx)
  if (Nstate == 1) then

    do ii=1,idx(0)
      i_in_key = idx(ii)
      i_in_coef = idx_key(idx(ii))
      !DIR$ FORCEINLINE
      call i_H_j(keys(1,1,i_in_key),key,Nint,hij)
      i_H_psi_array(1) = i_H_psi_array(1) + coef(i_in_coef,1)*hij
    enddo

  else

    do ii=1,idx(0)
      i_in_key = idx(ii)
      i_in_coef = idx_key(idx(ii))
      !DIR$ FORCEINLINE
      call i_H_j(keys(1,1,i_in_key),key,Nint,hij)
      do j = 1, Nstate
        i_H_psi_array(j) = i_H_psi_array(j) + coef(i_in_coef,j)*hij
      enddo
    enddo

  endif

end


subroutine i_H_psi_sec_ord(key,keys,coef,Nint,Ndet,Ndet_max,Nstate,i_H_psi_array,idx_interaction,interactions)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint, Ndet,Ndet_max,Nstate
  integer(bit_kind), intent(in)  :: keys(Nint,2,Ndet)
  integer(bit_kind), intent(in)  :: key(Nint,2)
  double precision, intent(in)   :: coef(Ndet_max,Nstate)
  double precision, intent(out)  :: i_H_psi_array(Nstate)
  double precision, intent(out)  :: interactions(Ndet)
  integer,intent(out)            :: idx_interaction(0:Ndet)
  
  integer                        :: i, ii,j
  double precision               :: phase
  integer                        :: exc(0:2,2,2)
  double precision               :: hij
  integer                        :: idx(0:Ndet),n_interact
  BEGIN_DOC
  ! <key|H|psi> for the various Nstates
  END_DOC
  
  ASSERT (Nint > 0)
  ASSERT (N_int == Nint)
  ASSERT (Nstate > 0)
  ASSERT (Ndet > 0)
  ASSERT (Ndet_max >= Ndet)
  i_H_psi_array = 0.d0
  call filter_connected_i_H_psi0(keys,key,Nint,Ndet,idx)
  n_interact = 0
  do ii=1,idx(0)
    i = idx(ii)
    !DIR$ FORCEINLINE
    call i_H_j(keys(1,1,i),key,Nint,hij)
    if(dabs(hij).ge.1.d-8)then
     if(i.ne.1)then
      n_interact += 1
      interactions(n_interact) = hij
      idx_interaction(n_interact) = i
     endif
    endif
    do j = 1, Nstate
      i_H_psi_array(j) = i_H_psi_array(j) + coef(i,j)*hij
    enddo
  enddo
  idx_interaction(0) = n_interact
end


subroutine i_H_psi_SC2(key,keys,coef,Nint,Ndet,Ndet_max,Nstate,i_H_psi_array,idx_repeat)
  use bitmasks
  BEGIN_DOC
  ! <key|H|psi> for the various Nstate
  !
  ! returns in addition
  !
  ! the array of the index of the non connected determinants to key1
  !
  ! in order to know what double excitation can be repeated on key1
  !
  ! idx_repeat(0) is the number of determinants that can be used
  !
  ! to repeat the excitations
  END_DOC
  implicit none
  integer, intent(in)            :: Nint, Ndet,Ndet_max,Nstate
  integer(bit_kind), intent(in)  :: keys(Nint,2,Ndet)
  integer(bit_kind), intent(in)  :: key(Nint,2)
  double precision, intent(in)   :: coef(Ndet_max,Nstate)
  double precision, intent(out)  :: i_H_psi_array(Nstate)
  integer         , intent(out)  :: idx_repeat(0:Ndet)
  
  integer                        :: i, ii,j
  double precision               :: phase
  integer                        :: exc(0:2,2,2)
  double precision               :: hij
  integer                        :: idx(0:Ndet)
  
  ASSERT (Nint > 0)
  ASSERT (N_int == Nint)
  ASSERT (Nstate > 0)
  ASSERT (Ndet > 0)
  ASSERT (Ndet_max >= Ndet)
  i_H_psi_array = 0.d0
  call filter_connected_i_H_psi0_SC2(keys,key,Nint,Ndet,idx,idx_repeat)
  do ii=1,idx(0)
    i = idx(ii)
    !DIR$ FORCEINLINE
    call i_H_j(keys(1,1,i),key,Nint,hij)
    do j = 1, Nstate
      i_H_psi_array(j) = i_H_psi_array(j) + coef(i,j)*hij
    enddo
  enddo
end


subroutine i_H_psi_SC2_verbose(key,keys,coef,Nint,Ndet,Ndet_max,Nstate,i_H_psi_array,idx_repeat)
  use bitmasks
  BEGIN_DOC
  ! <key|H|psi> for the various Nstate
  !
  ! returns in addition
  !
  ! the array of the index of the non connected determinants to key1
  !
  ! in order to know what double excitation can be repeated on key1
  !
  ! idx_repeat(0) is the number of determinants that can be used
  !
  ! to repeat the excitations
  END_DOC
  implicit none
  integer, intent(in)            :: Nint, Ndet,Ndet_max,Nstate
  integer(bit_kind), intent(in)  :: keys(Nint,2,Ndet)
  integer(bit_kind), intent(in)  :: key(Nint,2)
  double precision, intent(in)   :: coef(Ndet_max,Nstate)
  double precision, intent(out)  :: i_H_psi_array(Nstate)
  integer         , intent(out)  :: idx_repeat(0:Ndet)
  
  integer                        :: i, ii,j
  double precision               :: phase
  integer                        :: exc(0:2,2,2)
  double precision               :: hij
  integer                        :: idx(0:Ndet)
  
  ASSERT (Nint > 0)
  ASSERT (N_int == Nint)
  ASSERT (Nstate > 0)
  ASSERT (Ndet > 0)
  ASSERT (Ndet_max >= Ndet)
  i_H_psi_array = 0.d0
  call filter_connected_i_H_psi0_SC2(keys,key,Nint,Ndet,idx,idx_repeat)
  print*,'--------'
  do ii=1,idx(0)
    print*,'--'
    i = idx(ii)
    !DIR$ FORCEINLINE
    call i_H_j(keys(1,1,i),key,Nint,hij)
    if (i==1)then
     print*,'i==1 !!'
    endif
    print*,coef(i,1) * hij,coef(i,1),hij
    do j = 1, Nstate
      i_H_psi_array(j) = i_H_psi_array(j) + coef(i,j)*hij
    enddo
    print*,i_H_psi_array(1)
  enddo
  print*,'------'
end

subroutine get_excitation_degree_vector_mono(key1,key2,degree,Nint,sze,idx)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Applies get_excitation_degree to an array of determinants and return only the mono excitations
  END_DOC
  integer, intent(in)            :: Nint, sze
  integer(bit_kind), intent(in)  :: key1(Nint,2,sze)
  integer(bit_kind), intent(in)  :: key2(Nint,2)
  integer, intent(out)           :: degree(sze)
  integer, intent(out)           :: idx(0:sze)
  
  integer                        :: i,l,d,m
  
  ASSERT (Nint > 0)
  ASSERT (sze > 0)
  
  l=1
  if (Nint==1) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      d = popcnt(xor( key1(1,1,i), key2(1,1))) +       &
          popcnt(xor( key1(1,2,i), key2(1,2)))
      if (d > 2) then
        cycle
      else
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
      endif
    enddo
    
  else if (Nint==2) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      d = popcnt(xor( key1(1,1,i), key2(1,1))) +                     &
          popcnt(xor( key1(1,2,i), key2(1,2))) +                     &
          popcnt(xor( key1(2,1,i), key2(2,1))) +                     &
          popcnt(xor( key1(2,2,i), key2(2,2)))
      if (d > 2) then
        cycle
      else
        degree(l) = ishft(d,-1)
        idx(l)    = i
        l         = l+1
      endif
    enddo
    
  else if (Nint==3) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      d = popcnt(xor( key1(1,1,i), key2(1,1))) +                     &
          popcnt(xor( key1(1,2,i), key2(1,2))) +                     &
          popcnt(xor( key1(2,1,i), key2(2,1))) +                     &
          popcnt(xor( key1(2,2,i), key2(2,2))) +                     &
          popcnt(xor( key1(3,1,i), key2(3,1))) +                     &
          popcnt(xor( key1(3,2,i), key2(3,2)))
      if (d > 2) then
        cycle
      else
        degree(l) = ishft(d,-1)
        idx(l)    = i
        l         = l+1
      endif
    enddo
    
  else
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      d = 0
      !DIR$ LOOP COUNT MIN(4)
      do m=1,Nint
        d = d + popcnt(xor( key1(m,1,i), key2(m,1)))                 &
              + popcnt(xor( key1(m,2,i), key2(m,2)))
      enddo
      if (d > 2) then
        cycle
      else
        degree(l) = ishft(d,-1)
        idx(l)    = i
        l         = l+1
      endif
    enddo
    
  endif
  idx(0) = l-1
end

subroutine get_excitation_degree_vector_mono_or_exchange(key1,key2,degree,Nint,sze,idx)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Applies get_excitation_degree to an array of determinants and return only the mono excitations
  ! and the connections through exchange integrals 
  END_DOC
  integer, intent(in)            :: Nint, sze
  integer(bit_kind), intent(in)  :: key1(Nint,2,sze)
  integer(bit_kind), intent(in)  :: key2(Nint,2)
  integer, intent(out)           :: degree(sze)
  integer, intent(out)           :: idx(0:sze)
  
  integer                        :: i,l,d,m
  integer                        :: exchange_1,exchange_2
  
  ASSERT (Nint > 0)
  ASSERT (sze > 0)
  
  l=1
  if (Nint==1) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      d = popcnt(xor( key1(1,1,i), key2(1,1))) +       &
          popcnt(xor( key1(1,2,i), key2(1,2)))
      exchange_1 = popcnt(xor(ior(key1(1,1,i),key1(1,2,i)),ior(key2(1,1),key2(1,2))))
      exchange_2 = popcnt(ior(xor(key1(1,1,i),key2(1,1)),xor(key1(1,2,i),key2(1,2))))
      if (d > 4)cycle
      if (d ==4)then  
       if(exchange_1 .eq. 0 ) then
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
       else if (exchange_1 .eq. 2 .and. exchange_2.eq.2)then
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
       else 
        cycle
       endif
!      pause
      else 
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
      endif
    enddo
  else if (Nint==2) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      d = popcnt(xor( key1(1,1,i), key2(1,1))) +                     &
          popcnt(xor( key1(1,2,i), key2(1,2))) +                     &
          popcnt(xor( key1(2,1,i), key2(2,1))) +                     &
          popcnt(xor( key1(2,2,i), key2(2,2)))
      exchange_1 = popcnt(xor(ior(key1(1,1,i),key1(1,2,i)),ior(key2(1,2),key2(1,2))))   +  & 
                   popcnt(xor(ior(key1(2,1,i),key1(2,2,i)),ior(key2(2,2),key2(2,2))))
      exchange_2 = popcnt(ior(xor(key1(1,1,i),key2(1,1)),xor(key1(1,2,i),key2(1,2))))    +  & 
                   popcnt(ior(xor(key1(2,1,i),key2(2,1)),xor(key1(2,2,i),key2(2,2))))
      if (d > 4)cycle
      if (d ==4)then  
       if(exchange_1 .eq. 0 ) then
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
       else if (exchange_1 .eq. 2 .and. exchange_2.eq.2)then
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
       else 
        cycle
       endif
!      pause
      else 
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
      endif
    enddo
    
  else if (Nint==3) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      d = popcnt(xor( key1(1,1,i), key2(1,1))) +                     &
          popcnt(xor( key1(1,2,i), key2(1,2))) +                     &
          popcnt(xor( key1(2,1,i), key2(2,1))) +                     &
          popcnt(xor( key1(2,2,i), key2(2,2))) +                     &
          popcnt(xor( key1(3,1,i), key2(3,1))) +                     &
          popcnt(xor( key1(3,2,i), key2(3,2)))
      exchange_1 = popcnt(xor(ior(key1(1,1,i),key1(1,2,i)),ior(key2(1,1),key2(1,2))))   +  & 
                   popcnt(xor(ior(key1(2,1,i),key1(2,2,i)),ior(key2(2,1),key2(2,2))))   +  &
                   popcnt(xor(ior(key1(3,1,i),key1(3,2,i)),ior(key2(3,1),key2(3,2))))
      exchange_2 = popcnt(ior(xor(key1(1,1,i),key2(1,1)),xor(key1(1,2,i),key2(1,2))))    +  & 
                   popcnt(ior(xor(key1(2,1,i),key2(2,1)),xor(key1(2,2,i),key2(2,2))))    +  &
                   popcnt(ior(xor(key1(3,1,i),key2(3,1)),xor(key1(3,2,i),key2(3,2))))
      if (d > 4)cycle
      if (d ==4)then  
       if(exchange_1 .eq. 0 ) then
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
       else if (exchange_1 .eq. 2 .and. exchange_2.eq.2)then
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
       else 
        cycle
       endif
!      pause
      else 
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
      endif
    enddo
    
  else
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      d = 0
      exchange_1 = 0
      !DIR$ LOOP COUNT MIN(4)
      do m=1,Nint
        d = d + popcnt(xor( key1(m,1,i), key2(m,1)))                 &
              + popcnt(xor( key1(m,2,i), key2(m,2)))
        exchange_1 = popcnt(xor(ior(key1(m,1,i),key1(m,2,i)),ior(key2(m,1),key2(m,2))))  
        exchange_2 = popcnt(ior(xor(key1(m,1,i),key2(m,1)),xor(key1(m,2,i),key2(m,2))))   
      enddo
      if (d > 4)cycle
      if (d ==4)then  
       if(exchange_1 .eq. 0 ) then
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
       else if (exchange_1 .eq. 2 .and. exchange_2.eq.2)then
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
       else 
        cycle
       endif
!      pause
      else 
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
      endif
    enddo
    
  endif
  idx(0) = l-1
end

subroutine get_excitation_degree_vector_mono_or_exchange_verbose(key1,key2,degree,Nint,sze,idx)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Applies get_excitation_degree to an array of determinants and return only the mono excitations
  ! and the connections through exchange integrals 
  END_DOC
  integer, intent(in)            :: Nint, sze
  integer(bit_kind), intent(in)  :: key1(Nint,2,sze)
  integer(bit_kind), intent(in)  :: key2(Nint,2)
  integer, intent(out)           :: degree(sze)
  integer, intent(out)           :: idx(0:sze)
  
  integer                        :: i,l,d,m
  integer                        :: exchange_1,exchange_2
  
  ASSERT (Nint > 0)
  ASSERT (sze > 0)
  
  l=1
  if (Nint==1) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      d = popcnt(xor( key1(1,1,i), key2(1,1))) +       &
          popcnt(xor( key1(1,2,i), key2(1,2)))
      exchange_1 = popcnt(xor(ior(key1(1,1,i),key1(1,2,i)),ior(key2(1,1),key2(1,2))))
      exchange_2 = popcnt(ior(xor(key1(1,1,i),key2(1,1)),xor(key1(1,2,i),key2(1,2))))
      if(i==99)then
      integer(bit_kind) :: key_test(N_int,2)
      key_test(1,2) = 0_bit_kind
       call debug_det(key2,N_int)
      key_test(1,1) = ior(key2(1,1),key2(1,2))
       call debug_det(key_test,N_int)
      key_test(1,1) = ior(key1(1,1,i),key1(1,2,i))
       call debug_det(key1(1,1,i),N_int)
       call debug_det(key_test,N_int)
       key_test(1,1) = xor(ior(key1(1,1,i),key1(1,2,i)),ior(key2(1,1),key2(1,2)))
       call debug_det(key_test,N_int)
       print*, exchange_1 , exchange_2
       stop
      endif
      if (d > 4)cycle
      if (d ==4)then  
       if(exchange_1 .eq. 0 ) then
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
       else if (exchange_1 .eq. 2 .and. exchange_2.eq.2)then
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
       else 
        cycle
       endif
!      pause
      else 
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
      endif
    enddo
  else if (Nint==2) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      d = popcnt(xor( key1(1,1,i), key2(1,1))) +                     &
          popcnt(xor( key1(1,2,i), key2(1,2))) +                     &
          popcnt(xor( key1(2,1,i), key2(2,1))) +                     &
          popcnt(xor( key1(2,2,i), key2(2,2)))
      exchange_1 = popcnt(xor(iand(key1(1,1,i),key1(1,2,i)),iand(key2(1,2),key2(1,2))))   +  & 
                   popcnt(xor(iand(key1(2,1,i),key1(2,2,i)),iand(key2(2,2),key2(2,2))))
      exchange_2 = popcnt(iand(xor(key1(1,1,i),key2(1,1)),xor(key1(1,2,i),key2(1,2))))    +  & 
                   popcnt(iand(xor(key1(2,1,i),key2(2,1)),xor(key1(2,2,i),key2(2,2))))
      if (d > 4)cycle
      if (d ==4)then  
       if(exchange_1 .eq. 0 ) then
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
       else if (exchange_1 .eq. 2 .and. exchange_2.eq.2)then
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
       else 
        cycle
       endif
!      pause
      else 
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
      endif
    enddo
    
  else if (Nint==3) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      d = popcnt(xor( key1(1,1,i), key2(1,1))) +                     &
          popcnt(xor( key1(1,2,i), key2(1,2))) +                     &
          popcnt(xor( key1(2,1,i), key2(2,1))) +                     &
          popcnt(xor( key1(2,2,i), key2(2,2))) +                     &
          popcnt(xor( key1(3,1,i), key2(3,1))) +                     &
          popcnt(xor( key1(3,2,i), key2(3,2)))
      exchange_1 = popcnt(xor(iand(key1(1,1,i),key1(1,2,i)),iand(key2(1,1),key2(1,2))))   +  & 
                   popcnt(xor(iand(key1(2,1,i),key1(2,2,i)),iand(key2(2,1),key2(2,2))))   +  &
                   popcnt(xor(iand(key1(3,1,i),key1(3,2,i)),iand(key2(3,1),key2(3,2))))
      exchange_2 = popcnt(iand(xor(key1(1,1,i),key2(1,1)),xor(key1(1,2,i),key2(1,2))))    +  & 
                   popcnt(iand(xor(key1(2,1,i),key2(2,1)),xor(key1(2,2,i),key2(2,2))))    +  &
                   popcnt(iand(xor(key1(3,1,i),key2(3,1)),xor(key1(3,2,i),key2(3,2))))
      if (d > 4)cycle
      if (d ==4)then  
       if(exchange_1 .eq. 0 ) then
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
       else if (exchange_1 .eq. 2 .and. exchange_2.eq.2)then
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
       else 
        cycle
       endif
!      pause
      else 
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
      endif
    enddo
    
  else
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      d = 0
      exchange_1 = 0
      !DIR$ LOOP COUNT MIN(4)
      do m=1,Nint
        d = d + popcnt(xor( key1(m,1,i), key2(m,1)))                 &
              + popcnt(xor( key1(m,2,i), key2(m,2)))
        exchange_1 = popcnt(xor(iand(key1(m,1,i),key1(m,2,i)),iand(key2(m,1),key2(m,2))))  
        exchange_2 = popcnt(iand(xor(key1(m,1,i),key2(m,1)),xor(key1(m,2,i),key2(m,2))))   
      enddo
      if (d > 4)cycle
      if (d ==4)then  
       if(exchange_1 .eq. 0 ) then
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
       else if (exchange_1 .eq. 2 .and. exchange_2.eq.2)then
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
       else 
        cycle
       endif
!      pause
      else 
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
      endif
    enddo
    
  endif
  idx(0) = l-1
end


subroutine get_excitation_degree_vector(key1,key2,degree,Nint,sze,idx)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Applies get_excitation_degree to an array of determinants
  END_DOC
  integer, intent(in)            :: Nint, sze
  integer(bit_kind), intent(in)  :: key1(Nint,2,sze)
  integer(bit_kind), intent(in)  :: key2(Nint,2)
  integer, intent(out)           :: degree(sze)
  integer, intent(out)           :: idx(0:sze)
  
  integer                        :: i,l,d,m
  
  ASSERT (Nint > 0)
  ASSERT (sze > 0)
  
  l=1
  if (Nint==1) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      d = popcnt(xor( key1(1,1,i), key2(1,1))) +       &
          popcnt(xor( key1(1,2,i), key2(1,2)))
      if (d > 4) then
        cycle
      else
        degree(l) = ishft(d,-1)
        idx(l) = i
        l = l+1
      endif
    enddo
    
  else if (Nint==2) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      d = popcnt(xor( key1(1,1,i), key2(1,1))) +                     &
          popcnt(xor( key1(1,2,i), key2(1,2))) +                     &
          popcnt(xor( key1(2,1,i), key2(2,1))) +                     &
          popcnt(xor( key1(2,2,i), key2(2,2)))
      if (d > 4) then
        cycle
      else
        degree(l) = ishft(d,-1)
        idx(l)    = i
        l         = l+1
      endif
    enddo
    
  else if (Nint==3) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      d = popcnt(xor( key1(1,1,i), key2(1,1))) +                     &
          popcnt(xor( key1(1,2,i), key2(1,2))) +                     &
          popcnt(xor( key1(2,1,i), key2(2,1))) +                     &
          popcnt(xor( key1(2,2,i), key2(2,2))) +                     &
          popcnt(xor( key1(3,1,i), key2(3,1))) +                     &
          popcnt(xor( key1(3,2,i), key2(3,2)))
      if (d > 4) then
        cycle
      else
        degree(l) = ishft(d,-1)
        idx(l)    = i
        l         = l+1
      endif
    enddo
    
  else
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      d = 0
      !DIR$ LOOP COUNT MIN(4)
      do m=1,Nint
        d = d + popcnt(xor( key1(m,1,i), key2(m,1)))                 &
              + popcnt(xor( key1(m,2,i), key2(m,2)))
      enddo
      if (d > 4) then
        cycle
      else
        degree(l) = ishft(d,-1)
        idx(l)    = i
        l         = l+1
      endif
    enddo
    
  endif
  idx(0) = l-1
end




double precision function diag_H_mat_elem_fock(det_ref,det_pert,fock_diag_tmp,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes <i|H|i> when i is at most a double excitation from
  ! a reference.
  END_DOC
  integer,intent(in)             :: Nint
  integer(bit_kind),intent(in)   :: det_ref(Nint,2), det_pert(Nint,2)
  double precision, intent(in)   :: fock_diag_tmp(2,mo_tot_num+1)

  integer                        :: degree
  double precision               :: phase, E0
  integer                        :: exc(0:2,2,2)
  integer                        :: h1, p1, h2, p2, s1, s2

  call get_excitation_degree(det_ref,det_pert,degree,Nint)
  E0 = fock_diag_tmp(1,mo_tot_num+1)
  if (degree == 2) then
    call get_double_excitation(det_ref,det_pert,exc,phase,Nint)
    call decode_exc(exc,2,h1,p1,h2,p2,s1,s2)

    if ( (s1 == 1).and.(s2 == 1) ) then      ! alpha/alpha
      diag_H_mat_elem_fock = E0 &
        - fock_diag_tmp(1,h1) &
        + ( fock_diag_tmp(1,p1) - mo_bielec_integral_jj_anti(h1,p1) ) &
        - ( fock_diag_tmp(1,h2) - mo_bielec_integral_jj_anti(h1,h2)   &
            + mo_bielec_integral_jj_anti(p1,h2) )                     &
        + ( fock_diag_tmp(1,p2) - mo_bielec_integral_jj_anti(h1,p2)   &
            + mo_bielec_integral_jj_anti(p1,p2) - mo_bielec_integral_jj_anti(h2,p2) )

    else if ( (s1 == 2).and.(s2 == 2) ) then ! beta/beta
      diag_H_mat_elem_fock = E0 &
        - fock_diag_tmp(2,h1) &
        + ( fock_diag_tmp(2,p1) - mo_bielec_integral_jj_anti(h1,p1) ) &
        - ( fock_diag_tmp(2,h2) - mo_bielec_integral_jj_anti(h1,h2)   &
            + mo_bielec_integral_jj_anti(p1,h2) )                     &
        + ( fock_diag_tmp(2,p2) - mo_bielec_integral_jj_anti(h1,p2)   &
            + mo_bielec_integral_jj_anti(p1,p2) - mo_bielec_integral_jj_anti(h2,p2) )

    else                                    ! alpha/beta
      diag_H_mat_elem_fock = E0 &
        - fock_diag_tmp(1,h1) &
        + ( fock_diag_tmp(1,p1) - mo_bielec_integral_jj_anti(h1,p1) ) &
        - ( fock_diag_tmp(2,h2) - mo_bielec_integral_jj(h1,h2)        &
            + mo_bielec_integral_jj(p1,h2) )                          &
        + ( fock_diag_tmp(2,p2) - mo_bielec_integral_jj(h1,p2)        &
            + mo_bielec_integral_jj(p1,p2) - mo_bielec_integral_jj_anti(h2,p2) )

    endif

  else if (degree == 1) then
    call get_mono_excitation(det_ref,det_pert,exc,phase,Nint)
    call decode_exc(exc,1,h1,p1,h2,p2,s1,s2)
    if (s1 == 1) then
      diag_H_mat_elem_fock = E0 - fock_diag_tmp(1,h1) &
        + ( fock_diag_tmp(1,p1) - mo_bielec_integral_jj_anti(h1,p1) ) 
    else 
      diag_H_mat_elem_fock = E0 - fock_diag_tmp(2,h1) &
        + ( fock_diag_tmp(2,p1) - mo_bielec_integral_jj_anti(h1,p1) ) 
    endif

  else if (degree == 0) then
    diag_H_mat_elem_fock = E0
  else
    STOP 'Bug in diag_H_mat_elem_fock'
  endif
end

double precision function diag_H_mat_elem(det_in,Nint)
  implicit none
  BEGIN_DOC
  ! Computes <i|H|i>
  END_DOC
  integer,intent(in)             :: Nint
  integer(bit_kind),intent(in)   :: det_in(Nint,2)
  
  integer(bit_kind)              :: hole(Nint,2)
  integer(bit_kind)              :: particle(Nint,2)
  integer                        :: i, nexc(2), ispin
  integer                        :: occ_particle(Nint*bit_kind_size,2)
  integer                        :: occ_hole(Nint*bit_kind_size,2)
  integer(bit_kind)              :: det_tmp(Nint,2)
  integer                        :: na, nb
  
  ASSERT (Nint > 0)
  ASSERT (sum(popcnt(det_in(:,1))) == elec_alpha_num)
  ASSERT (sum(popcnt(det_in(:,2))) == elec_beta_num)
  
  nexc(1) = 0
  nexc(2) = 0
  do i=1,Nint
    hole(i,1)     = xor(det_in(i,1),ref_bitmask(i,1))
    hole(i,2)     = xor(det_in(i,2),ref_bitmask(i,2))
    particle(i,1) = iand(hole(i,1),det_in(i,1))
    particle(i,2) = iand(hole(i,2),det_in(i,2))
    hole(i,1)     = iand(hole(i,1),ref_bitmask(i,1))
    hole(i,2)     = iand(hole(i,2),ref_bitmask(i,2))
    nexc(1)       = nexc(1) + popcnt(hole(i,1))
    nexc(2)       = nexc(2) + popcnt(hole(i,2))
  enddo
  
  diag_H_mat_elem = ref_bitmask_energy
  if (nexc(1)+nexc(2) == 0) then
    return
  endif
  
  !call debug_det(det_in,Nint)
  integer                        :: tmp(2)
  !DIR$ FORCEINLINE
  call bitstring_to_list_ab(particle, occ_particle, tmp, Nint)
  ASSERT (tmp(1) == nexc(1))
  ASSERT (tmp(2) == nexc(2))
  !DIR$ FORCEINLINE
  call bitstring_to_list_ab(hole, occ_hole, tmp, Nint)
  ASSERT (tmp(1) == nexc(1))
  ASSERT (tmp(2) == nexc(2))
  
  det_tmp = ref_bitmask
  do ispin=1,2
    na = elec_num_tab(ispin)
    nb = elec_num_tab(iand(ispin,1)+1)
    do i=1,nexc(ispin)
      !DIR$ FORCEINLINE
      call ac_operator( occ_particle(i,ispin), ispin, det_tmp, diag_H_mat_elem, Nint,na,nb)
      !DIR$ FORCEINLINE
      call a_operator ( occ_hole    (i,ispin), ispin, det_tmp, diag_H_mat_elem, Nint,na,nb)
    enddo
  enddo
end

subroutine a_operator(iorb,ispin,key,hjj,Nint,na,nb)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Needed for diag_H_mat_elem
  END_DOC
  integer, intent(in)            :: iorb, ispin, Nint
  integer, intent(inout)         :: na, nb
  integer(bit_kind), intent(inout) :: key(Nint,2)
  double precision, intent(inout) :: hjj
  
  integer                        :: occ(Nint*bit_kind_size,2)
  integer                        :: other_spin
  integer                        :: k,l,i
  integer                        :: tmp(2)
  
  ASSERT (iorb > 0)
  ASSERT (ispin > 0)
  ASSERT (ispin < 3)
  ASSERT (Nint > 0)
  
  k = ishft(iorb-1,-bit_kind_shift)+1
  ASSERT (k > 0)
  l = iorb - ishft(k-1,bit_kind_shift)-1
  key(k,ispin) = ibclr(key(k,ispin),l)
  other_spin = iand(ispin,1)+1
  
  !DIR$ FORCEINLINE
  call bitstring_to_list_ab(key, occ, tmp, Nint)
  na = na-1
  
  hjj = hjj - mo_mono_elec_integral(iorb,iorb)
  
  ! Same spin
  do i=1,na
    hjj = hjj - mo_bielec_integral_jj_anti(occ(i,ispin),iorb)
  enddo
  
  ! Opposite spin
  do i=1,nb
    hjj = hjj - mo_bielec_integral_jj(occ(i,other_spin),iorb)
  enddo
  
end


subroutine ac_operator(iorb,ispin,key,hjj,Nint,na,nb)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Needed for diag_H_mat_elem
  END_DOC
  integer, intent(in)            :: iorb, ispin, Nint
  integer, intent(inout)         :: na, nb
  integer(bit_kind), intent(inout) :: key(Nint,2)
  double precision, intent(inout) :: hjj
  
  integer                        :: occ(Nint*bit_kind_size,2)
  integer                        :: other_spin
  integer                        :: k,l,i
  
  ASSERT (iorb > 0)
  ASSERT (ispin > 0)
  ASSERT (ispin < 3)
  ASSERT (Nint > 0)
  
  integer                        :: tmp(2)
  !DIR$ FORCEINLINE
  call bitstring_to_list_ab(key, occ, tmp, Nint)
  ASSERT (tmp(1) == elec_alpha_num)
  ASSERT (tmp(2) == elec_beta_num)
  
  k = ishft(iorb-1,-bit_kind_shift)+1
  ASSERT (k > 0)
  l = iorb - ishft(k-1,bit_kind_shift)-1
  key(k,ispin) = ibset(key(k,ispin),l)
  other_spin = iand(ispin,1)+1
  
  hjj = hjj + mo_mono_elec_integral(iorb,iorb)
  
  ! Same spin
  do i=1,na
    hjj = hjj + mo_bielec_integral_jj_anti(occ(i,ispin),iorb)
  enddo
  
  ! Opposite spin
  do i=1,nb
    hjj = hjj + mo_bielec_integral_jj(occ(i,other_spin),iorb)
  enddo
  na = na+1
end

subroutine get_occ_from_key(key,occ,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Returns a list of occupation numbers from a bitstring
  END_DOC
  integer          , intent(in)  :: Nint
  integer(bit_kind), intent(in)  :: key(Nint,2)
  integer         , intent(out)  :: occ(Nint*bit_kind_size,2)
  integer                        :: tmp(2)
  
  !DIR$ FORCEINLINE
  call bitstring_to_list_ab(key, occ, tmp, Nint)
  
end

subroutine u0_H_u_0(e_0,u_0,n,keys_tmp,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes e_0 = <u_0|H|u_0>/<u_0|u_0>
  !
  ! n : number of determinants
  !
  END_DOC
  integer, intent(in)            :: n,Nint
  double precision, intent(out)  :: e_0
  double precision, intent(in)   :: u_0(n)
  integer(bit_kind),intent(in)   :: keys_tmp(Nint,2,n)
  
  double precision               :: H_jj(n)
  double precision               :: v_0(n)
  double precision               :: u_dot_u,u_dot_v,diag_H_mat_elem
  integer :: i,j
  do i = 1, n
   H_jj(i) = diag_H_mat_elem(keys_tmp(1,1,i),Nint)
  enddo
  
  call H_u_0(v_0,u_0,H_jj,n,keys_tmp,Nint)
  e_0 = u_dot_v(v_0,u_0,n)/u_dot_u(u_0,n)
end


subroutine H_u_0(v_0,u_0,H_jj,n,keys_tmp,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes v_0 = H|u_0>
  !
  ! n : number of determinants
  !
  ! H_jj : array of <j|H|j>
  END_DOC
  integer, intent(in)            :: n,Nint
  double precision, intent(out)  :: v_0(n)
  double precision, intent(in)   :: u_0(n)
  double precision, intent(in)   :: H_jj(n)
  integer(bit_kind),intent(in)   :: keys_tmp(Nint,2,n)
  integer, allocatable           :: idx(:)
  double precision               :: hij
  double precision, allocatable  :: vt(:)
  integer                        :: i,j,k,l, jj,ii
  integer                        :: i0, j0
  
  integer, allocatable           :: shortcut(:), sort_idx(:)
  integer(bit_kind), allocatable :: sorted(:,:), version(:,:)
  integer(bit_kind)              :: sorted_i(Nint)
  
  integer                        :: sh, sh2, ni, exa, ext, org_i, org_j, endi
  double precision               :: local_threshold
  

  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (n>0)
  PROVIDE ref_bitmask_energy davidson_criterion

  allocate (shortcut(0:n+1), sort_idx(n), sorted(Nint,n), version(Nint,n))
  v_0 = 0.d0

  call sort_dets_ab_v(keys_tmp, sorted, sort_idx, shortcut, version, n, Nint)
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP PRIVATE(i,hij,j,k,jj,vt,ii,sh,sh2,ni,exa,ext,org_i,org_j,endi,local_threshold,sorted_i)&
      !$OMP SHARED(n,H_jj,u_0,keys_tmp,Nint,v_0,threshold_davidson,sorted,shortcut,sort_idx,version)
  allocate(vt(n))
  Vt = 0.d0
  
  !$OMP DO SCHEDULE(dynamic)
  do sh=1,shortcut(0)
    do sh2=1,sh
      exa = 0
      do ni=1,Nint
        exa = exa + popcnt(xor(version(ni,sh), version(ni,sh2)))
      end do
      if(exa > 2) then
        cycle
      end if
      
      do i=shortcut(sh),shortcut(sh+1)-1
        org_i = sort_idx(i)
        local_threshold = threshold_davidson - dabs(u_0(org_i))
        if(sh==sh2) then
          endi = i-1
        else
          endi = shortcut(sh2+1)-1
        end if
        do ni=1,Nint
          sorted_i(ni) = sorted(ni,i)
        enddo
        
        do j=shortcut(sh2),endi
          org_j = sort_idx(j)
          if ( dabs(u_0(org_j)) > local_threshold ) then
            ext = exa
            do ni=1,Nint
              ext = ext + popcnt(xor(sorted_i(ni), sorted(ni,j)))
            end do
            if(ext <= 4) then
              call i_H_j(keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),Nint,hij)
              vt (org_i) = vt (org_i) + hij*u_0(org_j)
              vt (org_j) = vt (org_j) + hij*u_0(org_i)
            endif
          endif
        enddo
      enddo
    enddo
  enddo
  !$OMP END DO
  
  !$OMP CRITICAL
  do i=1,n
    v_0(i) = v_0(i) + vt(i)
  enddo
  !$OMP END CRITICAL
  
  deallocate(vt)
  !$OMP END PARALLEL
  
  call sort_dets_ba_v(keys_tmp, sorted, sort_idx, shortcut, version, n, Nint)
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP PRIVATE(i,hij,j,k,jj,vt,ii,sh,sh2,ni,exa,ext,org_i,org_j,endi,local_threshold)&
      !$OMP SHARED(n,H_jj,u_0,keys_tmp,Nint,v_0,threshold_davidson,sorted,shortcut,sort_idx,version)
  allocate(vt(n))
  Vt = 0.d0
  
  !$OMP DO SCHEDULE(dynamic)
  do sh=1,shortcut(0)
    do i=shortcut(sh),shortcut(sh+1)-1
      org_i = sort_idx(i)
      local_threshold = threshold_davidson - dabs(u_0(org_i))
      do j=shortcut(sh),i-1
        org_j = sort_idx(j)
        if ( dabs(u_0(org_j)) > local_threshold ) then
          ext = 0
          do ni=1,Nint
            ext = ext + popcnt(xor(sorted(ni,i), sorted(ni,j)))
          end do
          if(ext == 4) then
            call i_H_j(keys_tmp(1,1,org_j),keys_tmp(1,1,org_i),Nint,hij)
            vt (org_i) = vt (org_i) + hij*u_0(org_j)
            vt (org_j) = vt (org_j) + hij*u_0(org_i)
          end if
        end if
      end do
    end do
  enddo
  !$OMP END DO
  
  !$OMP CRITICAL
  do i=1,n
    v_0(i) = v_0(i) + vt(i)
  enddo
  !$OMP END CRITICAL
  deallocate(vt)
  !$OMP END PARALLEL
  
  do i=1,n
    v_0(i) += H_jj(i) * u_0(i)
  enddo
  deallocate (shortcut, sort_idx, sorted, version)
end


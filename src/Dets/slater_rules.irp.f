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
  !DEC$ NOUNROLL
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
    
    ! TODO : Voir si il faut sortir i,n,k,m du case.
    
    select case (exc(0,1,ispin))
      case(0)
        cycle
        
      case(1)
        low  = min(exc(1,1,ispin), exc(1,2,ispin))
        high = max(exc(1,1,ispin), exc(1,2,ispin))
        
        ASSERT (low > 0)
        j = ishft(low-1,-bit_kind_shift)+1   ! Find integer in array(Nint)
        n = iand(low,bit_kind_size-1)        ! mod(low,bit_kind_size)
        ASSERT (high > 0)
        k = ishft(high-1,-bit_kind_shift)+1
        m = iand(high,bit_kind_size-1)
        
        if (j==k) then
          nperm = nperm + popcnt(iand(det1(j,ispin),                 &
              iand( ibset(0_bit_kind,m-1)-1_bit_kind,                &
              ibclr(-1_bit_kind,n)+1_bit_kind ) ))
        else
          nperm = nperm + popcnt(iand(det1(k,ispin),                 &
              ibset(0_bit_kind,m-1)-1_bit_kind)) +                   &
              popcnt(iand(det1(j,ispin), ibclr(-1_bit_kind,n) +1_bit_kind))
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
          n = iand(low,bit_kind_size-1)        ! mod(low,bit_kind_size)
          ASSERT (high > 0)
          k = ishft(high-1,-bit_kind_shift)+1
          m = iand(high,bit_kind_size-1)
          
          if (j==k) then
            nperm = nperm + popcnt(iand(det1(j,ispin),               &
                iand( ibset(0_bit_kind,m-1)-1_bit_kind,              &
                ibclr(-1_bit_kind,n)+1_bit_kind ) ))
          else
            nperm = nperm + popcnt(iand(det1(k,ispin),               &
                ibset(0_bit_kind,m-1)-1_bit_kind)) +                 &
                popcnt(iand(det1(j,ispin), ibclr(-1_bit_kind,n) +1_bit_kind))
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
      n = iand(low,bit_kind_size-1)        ! mod(low,bit_kind_size)
      ASSERT (high > 0)
      k = ishft(high-1,-bit_kind_shift)+1
      m = iand(high,bit_kind_size-1)
      if (j==k) then
        nperm = popcnt(iand(det1(j,ispin),                           &
            iand(ibset(0_bit_kind,m-1)-1_bit_kind,ibclr(-1_bit_kind,n)+1_bit_kind)))
      else
        nperm = nperm + popcnt(iand(det1(k,ispin),ibset(0_bit_kind,m-1)-1_bit_kind)) +&
            popcnt(iand(det1(j,ispin),ibclr(-1_bit_kind,n)+1_bit_kind))
        do i=j+1,k-1
          nperm = nperm + popcnt(det1(i,ispin))
        end do
      endif
      phase = phase_dble(iand(nperm,1))
      return
      
    enddo
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
  double precision               :: get_mo_bielec_integral
  integer                        :: m,n,p,q
  integer                        :: i,j,k
  integer                        :: occ(Nint*bit_kind_size,2)
  double precision               :: diag_H_mat_elem, phase,phase_2
  integer                        :: n_occ_alpha, n_occ_beta
  logical                        :: has_mipi(Nint*bit_kind_size)
  double precision               :: mipi(Nint*bit_kind_size), miip(Nint*bit_kind_size)
  PROVIDE mo_bielec_integrals_in_map
  
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (sum(popcnt(key_i(:,1))) == elec_alpha_num)
  ASSERT (sum(popcnt(key_i(:,2))) == elec_beta_num)
  ASSERT (sum(popcnt(key_j(:,1))) == elec_alpha_num)
  ASSERT (sum(popcnt(key_j(:,2))) == elec_beta_num)
  
  hij = 0.d0
  !DEC$ FORCEINLINE
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
      call bitstring_to_list(key_i(1,1), occ(1,1), n_occ_alpha, Nint)
      call bitstring_to_list(key_i(1,2), occ(1,2), n_occ_beta, Nint)
      has_mipi = .False.
      if (exc(0,1,1) == 1) then
        ! Mono alpha
        m = exc(1,1,1)
        p = exc(1,2,1)
        do k = 1, elec_alpha_num
          i = occ(k,1)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral(m,i,p,i,mo_integrals_map)
            miip(i) = get_mo_bielec_integral(m,i,i,p,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        do k = 1, elec_beta_num
          i = occ(k,2)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral(m,i,p,i,mo_integrals_map)
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
            mipi(i) = get_mo_bielec_integral(m,i,p,i,mo_integrals_map)
            miip(i) = get_mo_bielec_integral(m,i,i,p,mo_integrals_map)
            has_mipi(i) = .True.
          endif
        enddo
        do k = 1, elec_alpha_num
          i = occ(k,1)
          if (.not.has_mipi(i)) then
            mipi(i) = get_mo_bielec_integral(m,i,p,i,mo_integrals_map)
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



subroutine i_H_psi(key,keys,coef,Nint,Ndet,Ndet_max,Nstate,i_H_psi_array)
  use bitmasks
  implicit none
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
  do ii=1,idx(0)
    i = idx(ii)
    !DEC$ FORCEINLINE
    call i_H_j(keys(1,1,i),key,Nint,hij)
    do j = 1, Nstate
      i_H_psi_array(j) = i_H_psi_array(j) + coef(i,j)*hij
    enddo
  enddo
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
    !DEC$ FORCEINLINE
    call i_H_j(keys(1,1,i),key,Nint,hij)
    do j = 1, Nstate
      i_H_psi_array(j) = i_H_psi_array(j) + coef(i,j)*hij
    enddo
  enddo
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
  
  integer                        :: i,l
  
  ASSERT (Nint > 0)
  ASSERT (sze > 0)
  
  l=1
  if (Nint==1) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      degree(l) = ishft(popcnt(xor( key1(1,1,i), key2(1,1))) +       &
          popcnt(xor( key1(1,2,i), key2(1,2))),-1)
      if (degree(l) < 3) then
        idx(l) = i
        l = l+1
      endif
    enddo
    
  else if (Nint==2) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      degree(l) = ishft(popcnt(xor( key1(1,1,i), key2(1,1))) +       &
          popcnt(xor( key1(1,2,i), key2(1,2))) +                     &
          popcnt(xor( key1(2,1,i), key2(2,1))) +                     &
          popcnt(xor( key1(2,2,i), key2(2,2))),-1)
      if (degree(l) < 3) then
        idx(l) = i
        l = l+1
      endif
    enddo
    
  else if (Nint==3) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      degree(l) = ishft( popcnt(xor( key1(1,1,i), key2(1,1))) +      &
          popcnt(xor( key1(1,2,i), key2(1,2))) +                     &
          popcnt(xor( key1(2,1,i), key2(2,1))) +                     &
          popcnt(xor( key1(2,2,i), key2(2,2))) +                     &
          popcnt(xor( key1(3,1,i), key2(3,1))) +                     &
          popcnt(xor( key1(3,2,i), key2(3,2))),-1)
      if (degree(l) < 3) then
        idx(l) = i
        l = l+1
      endif
    enddo
    
  else
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      degree(l) = 0
      !DEC$ LOOP COUNT MIN(4)
      do l=1,Nint
        degree(l) = degree(l)+ popcnt(xor( key1(l,1,i), key2(l,1))) +&
            popcnt(xor( key1(l,2,i), key2(l,2)))
      enddo
      degree(l) = ishft(degree(l),-1)
      if (degree(l) < 3) then
        idx(l) = i
        l = l+1
      endif
    enddo
    
  endif
  idx(0) = l-1
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
    hole(i,1)     =  xor(det_in(i,1),ref_bitmask(i,1))
    hole(i,2)     =  xor(det_in(i,2),ref_bitmask(i,2))
    particle(i,1) = iand(hole(i,1),det_in(i,1))
    particle(i,2) = iand(hole(i,2),det_in(i,2))
    hole(i,1)     = iand(hole(i,1),ref_bitmask(i,1))
    hole(i,2)     = iand(hole(i,2),ref_bitmask(i,2))
    nexc(1)      += popcnt(hole(i,1))
    nexc(2)      += popcnt(hole(i,2))
  enddo
  
  diag_H_mat_elem = ref_bitmask_energy
  if (nexc(1)+nexc(2) == 0) then
    return
  endif
  
  !call debug_det(det_in,Nint)
  integer                        :: tmp
  call bitstring_to_list(particle(1,1), occ_particle(1,1), tmp, Nint)
  ASSERT (tmp == nexc(1))
  call bitstring_to_list(particle(1,2), occ_particle(1,2), tmp, Nint)
  ASSERT (tmp == nexc(2))
  call bitstring_to_list(hole(1,1), occ_hole(1,1), tmp, Nint)
  ASSERT (tmp == nexc(1))
  call bitstring_to_list(hole(1,2), occ_hole(1,2), tmp, Nint)
  ASSERT (tmp == nexc(2))
  
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
  call get_occ_from_key(key,occ,Nint)
  na -= 1
  
  hjj -= mo_mono_elec_integral(iorb,iorb)
  
  ! Same spin
  do i=1,na
    hjj -= mo_bielec_integral_jj_anti(occ(i,ispin),iorb)
  enddo
  
  ! Opposite spin
  do i=1,nb
    hjj -= mo_bielec_integral_jj(occ(i,other_spin),iorb)
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
  
  integer                        :: tmp
  !DIR$ FORCEINLINE
  call bitstring_to_list(key(1,1), occ(1,1), tmp, Nint)
  ASSERT (tmp == elec_alpha_num)
  !DIR$ FORCEINLINE
  call bitstring_to_list(key(1,2), occ(1,2), tmp, Nint)
  ASSERT (tmp == elec_beta_num)
  
  k = ishft(iorb-1,-bit_kind_shift)+1
  ASSERT (k > 0)
  l = iorb - ishft(k-1,bit_kind_shift)-1
  key(k,ispin) = ibset(key(k,ispin),l)
  other_spin = iand(ispin,1)+1
  
  hjj += mo_mono_elec_integral(iorb,iorb)
  
  ! Same spin
  do i=1,na
    hjj += mo_bielec_integral_jj_anti(occ(i,ispin),iorb)
  enddo
  
  ! Opposite spin
  do i=1,nb
    hjj += mo_bielec_integral_jj(occ(i,other_spin),iorb)
  enddo
  na += 1
end

subroutine get_occ_from_key(key,occ,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Returns a list of occupation numbers from a bitstring
  END_DOC
  integer(bit_kind), intent(in)  :: key(Nint,2)
  integer          , intent(in)  :: Nint
  integer         , intent(out)  :: occ(Nint*bit_kind_size,2)
  integer                        :: tmp
  
  call bitstring_to_list(key(1,1), occ(1,1), tmp, Nint)
  call bitstring_to_list(key(1,2), occ(1,2), tmp, Nint)
  
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
  integer                        :: i,j,k,l, jj
  integer                        :: i0, j0
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (n>0)
  PROVIDE ref_bitmask_energy
  integer, parameter             :: block_size = 157
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP PRIVATE(i,hij,j,k,idx,jj,vt)                             &
      !$OMP SHARED(n,H_jj,u_0,keys_tmp,Nint,v_0)
  !$OMP DO SCHEDULE(static)
  do i=1,n
    v_0(i) = H_jj(i) * u_0(i)
  enddo
  !$OMP END DO
  allocate(idx(0:n), vt(n))
  Vt = 0.d0
  !$OMP DO SCHEDULE(guided)
  do i=1,n
    idx(0) = i
    call filter_connected_davidson(keys_tmp,keys_tmp(1,1,i),Nint,i-1,idx)
    do jj=1,idx(0)
      j = idx(jj)
      if ( (dabs(u_0(j)) > 1.d-7).or.((dabs(u_0(i)) > 1.d-7)) ) then
        call i_H_j(keys_tmp(1,1,j),keys_tmp(1,1,i),Nint,hij)
        vt (i) = vt (i) + hij*u_0(j)
        vt (j) = vt (j) + hij*u_0(i)
      endif
    enddo
  enddo
  !$OMP END DO
  !$OMP CRITICAL
  do i=1,n
    v_0(i) = v_0(i) + vt(i)
  enddo
  !$OMP END CRITICAL
  deallocate(idx,vt)
  !$OMP END PARALLEL
end



BEGIN_PROVIDER [ integer, N_con_int ]
  implicit none
  BEGIN_DOC
  ! Number of integers to represent the connections between determinants
  END_DOC
  N_con_int = 1 + ishft(N_det-1,-11)
END_PROVIDER

BEGIN_PROVIDER [ integer*8, det_connections, (N_con_int,N_det) ]
  implicit none
  BEGIN_DOC
  !
  END_DOC
  integer                        :: i,j
  integer                        :: degree
  integer                        :: j_int, j_k, j_l
  integer, allocatable           :: idx(:)
  
  select case(N_int)
      
    case(1)
      
      !$OMP PARALLEL DEFAULT (NONE)                                  &
          !$OMP SHARED(N_det, N_con_int, psi_det,N_int, det_connections)&
          !$OMP PRIVATE(i,j_int,j_k,j_l,j,degree,idx)
      allocate (idx(0:N_det))
      !$OMP DO SCHEDULE(guided)
      do i=1,N_det
        do j_int=1,N_con_int
          det_connections(j_int,i) = 0_8
          j_k = ishft(j_int-1,11)
          do j_l = j_k,min(j_k+2047,N_det), 32
            do j = j_l+1,min(j_l+32,i)
              degree = popcnt(xor( psi_det(1,1,i),psi_det(1,1,j))) + &
                  popcnt(xor( psi_det(1,2,i),psi_det(1,2,j)))
              if (degree < 5) then
                det_connections(j_int,i) = ibset( det_connections(j_int,i), iand(63,ishft(j_l,-5)) )
                exit
              endif
            enddo
          enddo
        enddo
      enddo
      !$OMP ENDDO
      deallocate(idx)
      !$OMP END PARALLEL
      
    case(2)
      
      !$OMP PARALLEL DEFAULT (NONE)                                  &
          !$OMP SHARED(N_det, N_con_int, psi_det,N_int, det_connections)&
          !$OMP PRIVATE(i,j_int,j_k,j_l,j,degree,idx)
      allocate (idx(0:N_det))
      !$OMP DO SCHEDULE(guided)
      do i=1,N_det
        do j_int=1,N_con_int
          det_connections(j_int,i) = 0_8
          j_k = ishft(j_int-1,11)
          do j_l = j_k,min(j_k+2047,N_det), 32
            do j = j_l+1,min(j_l+32,i)
              degree = popcnt(xor( psi_det(1,1,i),psi_det(1,1,j))) + &
                  popcnt(xor( psi_det(1,2,i),psi_det(1,2,j))) +      &
                  popcnt(xor( psi_det(2,1,i),psi_det(2,1,j))) +      &
                  popcnt(xor( psi_det(2,2,i),psi_det(2,2,j)))
              if (degree < 5) then
                det_connections(j_int,i) = ibset( det_connections(j_int,i), iand(63,ishft(j_l,-5)) )
                exit
              endif
            enddo
          enddo
        enddo
      enddo
      !$OMP ENDDO
      deallocate(idx)
      !$OMP END PARALLEL
      
    case(3)
      
      !$OMP PARALLEL DEFAULT (NONE)                                  &
          !$OMP SHARED(N_det, N_con_int, psi_det,N_int, det_connections)&
          !$OMP PRIVATE(i,j_int,j_k,j_l,j,degree,idx)
      allocate (idx(0:N_det))
      !$OMP DO SCHEDULE(guided)
      do i=1,N_det
        do j_int=1,N_con_int
          det_connections(j_int,i) = 0_8
          j_k = ishft(j_int-1,11)
          do j_l = j_k,min(j_k+2047,N_det), 32
            do j = j_l+1,min(j_l+32,i)
              degree = popcnt(xor( psi_det(1,1,i),psi_det(1,1,j))) + &
                  popcnt(xor( psi_det(1,2,i),psi_det(1,2,j))) +      &
                  popcnt(xor( psi_det(2,1,i),psi_det(2,1,j))) +      &
                  popcnt(xor( psi_det(2,2,i),psi_det(2,2,j))) +      &
                  popcnt(xor( psi_det(3,1,i),psi_det(3,1,j))) +      &
                  popcnt(xor( psi_det(3,2,i),psi_det(3,2,j)))
              if (degree < 5) then
                det_connections(j_int,i) = ibset( det_connections(j_int,i), iand(63,ishft(j_l,-5)) )
                exit
              endif
            enddo
          enddo
        enddo
      enddo
      !$OMP ENDDO
      deallocate(idx)
      !$OMP END PARALLEL
      
    case default
      
      
      !$OMP PARALLEL DEFAULT (NONE)                                  &
          !$OMP SHARED(N_det, N_con_int, psi_det,N_int, det_connections)&
          !$OMP PRIVATE(i,j_int,j_k,j_l,j,degree,idx)
      allocate (idx(0:N_det))
      !$OMP DO SCHEDULE(guided)
      do i=1,N_det
        do j_int=1,N_con_int
          det_connections(j_int,i) = 0_8
          j_k = ishft(j_int-1,11)
          do j_l = j_k,min(j_k+2047,N_det), 32
            do j = j_l+1,min(j_l+32,i)
              !DIR$ FORCEINLINE
              call get_excitation_degree(psi_det(1,1,i),psi_det(1,1,j),degree,N_int)
              if (degree < 3) then
                det_connections(j_int,i) = ibset( det_connections(j_int,i), iand(63,ishft(j_l,-5)) )
                exit
              endif
            enddo
          enddo
        enddo
      enddo
      !$OMP ENDDO
      deallocate(idx)
      !$OMP END PARALLEL
      
  end select

END_PROVIDER


logical function is_in_wavefunction(key,Nint,Ndet)
  implicit none

  integer, intent(in)            :: Nint, Ndet
  integer(bit_kind), intent(in)  :: key(Nint,2)

  integer                        :: i, ibegin, iend, istep, l
  integer*8                      :: det_ref, det_search
  integer*8, external            :: det_search_key
  
  is_in_wavefunction = .False.
  ibegin = 1
  iend   = N_det+1
  
  det_ref = det_search_key(key,Nint)
  det_search = det_search_key(psi_det_sorted_bit(1,1,1),Nint)
  
  istep = ishft(iend-ibegin,-1)
  i=ibegin+istep
  do while (istep > 0)
    det_search = det_search_key(psi_det_sorted_bit(1,1,i),Nint)
    if ( det_search > det_ref ) then
      iend = i
    else if ( det_search == det_ref ) then
      exit
    else
      ibegin = i
    endif
    istep = ishft(iend-ibegin,-1)
    i = ibegin + istep 
  end do

  do while (det_search_key(psi_det_sorted_bit(1,1,i),Nint) == det_ref)
    i = i-1
    if (i == 0) then
      exit
    endif
  enddo
  i += 1
  if (i > N_det) then
    return
  endif

  do while (det_search_key(psi_det_sorted_bit(1,1,i),Nint) == det_ref)
    if ( (key(1,1) /= psi_det_sorted_bit(1,1,i)).or.                               &
          (key(1,2) /= psi_det_sorted_bit(1,2,i)) ) then
      continue
    else
      is_in_wavefunction = .True.
      !DEC$ LOOP COUNT MIN(3)
      do l=2,Nint
        if ( (key(l,1) /= psi_det_sorted_bit(l,1,i)).or.                           &
              (key(l,2) /= psi_det_sorted_bit(l,2,i)) ) then
          is_in_wavefunction = .False.
          exit
        endif
      enddo
      if (is_in_wavefunction) then
        exit
      endif
    endif
    i += 1
    if (i > N_det) then
      return
 !    exit
    endif
    
  enddo
  

! DEBUG is_in_wf
! if (is_in_wavefunction) then
!   degree = 1
!   do i=1,N_det
!     integer                        :: degree
!     call get_excitation_degree(key,psi_det(1,1,i),degree,N_int)
!     if (degree == 0) then
!       exit
!     endif
!   enddo
!   if (degree /=0) then
!     stop 'pouet 1'
!   endif
! else
!   do i=1,N_det
!     call get_excitation_degree(key,psi_det(1,1,i),degree,N_int)
!     if (degree == 0) then
!       stop 'pouet 2'
!     endif
!   enddo
! endif
! END DEBUG is_in_wf
end

integer function connected_to_ref(key,keys,Nint,N_past_in,Ndet)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint, N_past_in, Ndet
  integer(bit_kind), intent(in)  :: keys(Nint,2,Ndet)
  integer(bit_kind), intent(in)  :: key(Nint,2)
  
  integer                        :: N_past
  integer                        :: i, l
  integer                        :: degree_x2
  logical                        :: det_is_not_or_may_be_in_ref, t
  double precision               :: hij_elec
  
  ! output :   0 : not connected
  !            i : connected to determinant i of the past
  !           -i : is the ith determinant of the refernce wf keys
  
  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  
  connected_to_ref = 0
  N_past = max(1,N_past_in)
  if (Nint == 1) then
    
    do i=1,N_past-1
      degree_x2 = popcnt(xor( key(1,1), keys(1,1,i))) +              &
          popcnt(xor( key(1,2), keys(1,2,i)))
      if (degree_x2 > 4) then
        cycle
      else
        connected_to_ref = i
        return
      endif
    enddo
    
    return

    
  else if (Nint==2) then
    
    do i=1,N_past-1
      degree_x2 = popcnt(xor( key(1,1), keys(1,1,i))) +              &
          popcnt(xor( key(1,2), keys(1,2,i))) +                      &
          popcnt(xor( key(2,1), keys(2,1,i))) +                      &
          popcnt(xor( key(2,2), keys(2,2,i)))
      if (degree_x2 > 4) then
        cycle
      else
        connected_to_ref = i
        return
      endif
    enddo
    
    return
    
  else if (Nint==3) then
    
    do i=1,N_past-1
      degree_x2 = popcnt(xor( key(1,1), keys(1,1,i))) +              &
          popcnt(xor( key(1,2), keys(1,2,i))) +                      &
          popcnt(xor( key(2,1), keys(2,1,i))) +                      &
          popcnt(xor( key(2,2), keys(2,2,i))) +                      &
          popcnt(xor( key(3,1), keys(3,1,i))) +                      &
          popcnt(xor( key(3,2), keys(3,2,i)))
      if (degree_x2 > 4) then
        cycle
      else
        connected_to_ref = i
        return
      endif
    enddo
    
    return
    
  else
    
    do i=1,N_past-1
      degree_x2 = popcnt(xor( key(1,1), keys(1,1,i))) +              &
          popcnt(xor( key(1,2), keys(1,2,i)))
      !DEC$ LOOP COUNT MIN(3)
      do l=2,Nint
        degree_x2 = degree_x2 + popcnt(xor( key(l,1), keys(l,1,i))) +&
            popcnt(xor( key(l,2), keys(l,2,i)))
      enddo
      if (degree_x2 > 4) then
        cycle
      else
        connected_to_ref = i
        return
      endif
    enddo
    
  endif
  
end

logical function det_is_not_or_may_be_in_ref(key,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! If true, det is not in ref
  ! If false, det may be in ref
  END_DOC
  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: key(Nint,2)
  integer(bit_kind)              :: key_int
  integer*1                      :: key_short(bit_kind)
  !DIR$ ATTRIBUTES ALIGN : 32    :: key_short
  equivalence  (key_int,key_short)
  
  integer                        :: i, ispin, k
  
  det_is_not_or_may_be_in_ref = .False.
  do ispin=1,2
    do i=1,Nint
      key_int = key(i,ispin)
      do k=1,bit_kind
        det_is_not_or_may_be_in_ref =                                &
            det_is_not_or_may_be_in_ref .or.                         &
            key_pattern_not_in_ref(key_short(k), i, ispin)
      enddo
      if(det_is_not_or_may_be_in_ref) then
        return
      endif
    enddo
  enddo
  
end


BEGIN_PROVIDER [ logical, key_pattern_not_in_ref, (-128:127,N_int,2) ]
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Min and max values of the integers of the keys of the reference
  END_DOC
  
  integer                        :: i, j, ispin
  integer(bit_kind)              :: key
  integer*1                      :: key_short(bit_kind)
  equivalence  (key,key_short)
  integer                        :: idx, k
  
  key_pattern_not_in_ref = .True.
  
  do j=1,N_det
    do ispin=1,2
      do i=1,N_int
        key = psi_det(i,ispin,j)
        do k=1,bit_kind
          key_pattern_not_in_ref( key_short(k), i, ispin ) = .False.
        enddo
      enddo
    enddo
  enddo
  
END_PROVIDER


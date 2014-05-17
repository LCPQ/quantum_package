integer function connected_to_ref(key,keys,Nint,N_past_in,Ndet,thresh)
 use bitmasks
 implicit none
 integer, intent(in)  :: Nint, N_past_in, Ndet
 integer(bit_kind), intent(in)  :: keys(ishft(Nint,-1),2,Ndet)
 integer(bit_kind), intent(in)  :: key(ishft(Nint,-1),2)
 double precision, intent(in) :: thresh

 integer :: N_past
 integer :: i, l
 integer :: degree_x2
 logical :: det_is_not_or_may_be_in_ref, t
 double precision :: hij_elec

 ! output :   0 : not connected
 !            i : connected to determinant i of the past
 !           -i : is the ith determinant of the refernce wf keys

 ASSERT (Nint == N_int)

 connected_to_ref = 0
 N_past = max(1,N_past_in)
 if (Nint == 1) then

   do i=N_past-1,1,-1
     degree_x2 = popcnt(xor( key(1,1), keys(1,1,i))) + &
                 popcnt(xor( key(1,2), keys(1,2,i)))
     if(degree_x2 == 0)then
      connected_to_ref = -i
      return
     endif
     if (degree_x2 > 5) then
       cycle
     else
       call i_H_j(keys(1,1,i),key,N_int,hij_elec)
       if(dabs(hij_elec).lt.thresh)cycle
       connected_to_ref = i
       return
     endif
   enddo

   !DIR$ FORCEINLINE
   t = det_is_not_or_may_be_in_ref(key,N_int) 
   if ( t ) then
     return
   endif

   do i=N_past,Ndet
     if ( (key(1,1) /= keys(1,1,i)).or. &
          (key(1,2) /= keys(1,2,i)) ) then
          cycle
     endif
     connected_to_ref = -i
     return
   enddo
   return

 else if (Nint==2) then

   do i=N_past-1,1,-1
     degree_x2 = popcnt(xor( key(1,1), keys(1,1,i))) + &
                 popcnt(xor( key(1,2), keys(1,2,i))) + &
                 popcnt(xor( key(2,1), keys(2,1,i))) + &
                 popcnt(xor( key(2,2), keys(2,2,i)))
     if(degree_x2 == 0)then
      connected_to_ref = -i
      return
     endif
     if (degree_x2 > 5) then
       cycle
     else
       call i_H_j(keys(1,1,i),key,N_int,hij_elec)
       if(dabs(hij_elec).lt.thresh)cycle
       connected_to_ref = i
       return
     endif
   enddo

   !DIR$ FORCEINLINE
   t = det_is_not_or_may_be_in_ref(key,N_int) 
   if ( t ) then
     return
   endif

   !DIR$ LOOP COUNT (1000)
   do i=N_past+1,Ndet
     if ( (key(1,1) /= keys(1,1,i)).or. &
          (key(1,2) /= keys(1,2,i)).or. &
          (key(2,1) /= keys(2,1,i)).or. &
          (key(2,2) /= keys(2,2,i)) ) then
          cycle
     endif
     connected_to_ref = -i
     return
   enddo
   return

 else if (Nint==3) then

   do i=N_past-1,1,-1
     degree_x2 = popcnt(xor( key(1,1), keys(1,1,i))) + &
                 popcnt(xor( key(1,2), keys(1,2,i))) + &
                 popcnt(xor( key(2,1), keys(2,1,i))) + &
                 popcnt(xor( key(2,2), keys(2,2,i))) + &
                 popcnt(xor( key(3,1), keys(3,1,i))) + &
                 popcnt(xor( key(3,2), keys(3,2,i)))
     if(degree_x2 == 0)then
      connected_to_ref = -i
      return
     endif
     if (degree_x2 > 5) then
       cycle
     else
       call i_H_j(keys(1,1,i),key,N_int,hij_elec)
       if(dabs(hij_elec).lt.thresh)cycle
       connected_to_ref = i
       return
     endif
   enddo

   !DIR$ FORCEINLINE
   t = det_is_not_or_may_be_in_ref(key,N_int) 
   if ( t ) then
     return
   endif

   do i=N_past+1,Ndet
     if ( (key(1,1) /= keys(1,1,i)).or. &
          (key(1,2) /= keys(1,2,i)).or. &
          (key(2,1) /= keys(2,1,i)).or. &
          (key(2,2) /= keys(2,2,i)).or. &
          (key(3,1) /= keys(3,1,i)).or. &
          (key(3,2) /= keys(3,2,i)) ) then
          cycle
     endif
     connected_to_ref = -i
     return
   enddo
   return

 else

   do i=N_past-1,1,-1
     degree_x2 = popcnt(xor( key(1,1), keys(1,1,i))) + &
                 popcnt(xor( key(1,2), keys(1,2,i)))
     !DEC$ LOOP COUNT MIN(3) 
     do l=2,Nint
      degree_x2 = degree_x2 + popcnt(xor( key(l,1), keys(l,1,i))) + &
                              popcnt(xor( key(l,2), keys(l,2,i)))
     enddo
     if(degree_x2 == 0)then
      connected_to_ref = -i
      return
     endif
     if (degree_x2 > 5) then
       cycle
     else
       call i_H_j(keys(1,1,i),key,N_int,hij_elec)
       if(dabs(hij_elec).lt.thresh)cycle
       connected_to_ref = i
       return
     endif
   enddo

   !DIR$ FORCEINLINE
   t = det_is_not_or_may_be_in_ref(key,N_int) 
   if ( t ) then
     return
   endif

   do i=N_past+1,Ndet
     if ( (key(1,1) /= keys(1,1,i)).or. &
          (key(1,2) /= keys(1,2,i)) ) then
       cycle
     else
       connected_to_ref = -1
       !DEC$ LOOP COUNT MIN(3) 
       do l=2,Nint
         if ( (key(l,1) /= keys(l,1,i)).or. &
              (key(l,2) /= keys(l,2,i)) ) then
           connected_to_ref = 0
           exit
         endif
       enddo
       if (connected_to_ref /= 0) then
         return
       endif
     endif
   enddo

 endif

end

logical function det_is_not_or_may_be_in_ref(key,Nint)
 implicit none
 ! If true, det is not in ref
 ! If false, det may be in ref

 integer, intent(in) :: key(Nint,2), Nint
 integer :: key_int
 integer*1 :: key_short(4)
 !DIR$ ATTRIBUTES ALIGN : 32 :: key_short
 equivalence  (key_int,key_short)

 integer :: i, ispin

 det_is_not_or_may_be_in_ref = .False.
 do ispin=1,2
  do i=1,Nint
   key_int = key(i,ispin)
   if ( &
        key_pattern_not_in_ref(key_short(1), i, ispin) &
    .or.key_pattern_not_in_ref(key_short(2), i, ispin) &
    .or.key_pattern_not_in_ref(key_short(3), i, ispin) &
    .or.key_pattern_not_in_ref(key_short(4), i, ispin) &
        ) then
        det_is_not_or_may_be_in_ref = .True.
        return
   endif
  enddo
 enddo

end


BEGIN_PROVIDER [ logical, key_pattern_not_in_ref, (-128:127,N_int,2) ]
 implicit none
 BEGIN_DOC  
! Min and max values of the integers of the keys of the reference
 END_DOC

 integer :: i, j, ispin
 integer :: key
 integer*1 :: key_short(4)
 equivalence  (key,key_short)
 integer :: idx

 key_pattern_not_in_ref = .True.

 do j=1,N_det
  do ispin=1,2
   do i=1,N_int
     key = psi_det(i,ispin,j)
     key_pattern_not_in_ref( key_short(1), i, ispin ) = .False.
     key_pattern_not_in_ref( key_short(2), i, ispin ) = .False.
     key_pattern_not_in_ref( key_short(3), i, ispin ) = .False.
     key_pattern_not_in_ref( key_short(4), i, ispin ) = .False.
   enddo
  enddo
 enddo

END_PROVIDER


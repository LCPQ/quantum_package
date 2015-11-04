
subroutine filter_connected(key1,key2,Nint,sze,idx)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Filters out the determinants that are not connected by H
  !
  ! returns the array idx which contains the index of the 
  !
  ! determinants in the array key1 that interact 
  !
  ! via the H operator with key2.
  !
  ! idx(0) is the number of determinants that interact with key1
  END_DOC
  integer, intent(in)            :: Nint, sze
  integer(bit_kind), intent(in)  :: key1(Nint,2,sze)
  integer(bit_kind), intent(in)  :: key2(Nint,2)
  integer, intent(out)           :: idx(0:sze)
  
  integer                        :: i,j,l
  integer                        :: degree_x2
  
  ASSERT (Nint > 0)
  ASSERT (sze >= 0)

  l=1
  
  if (Nint==1) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      degree_x2 = popcnt(    xor( key1(1,1,i), key2(1,1))) &
                + popcnt(    xor( key1(1,2,i), key2(1,2)))
      if (degree_x2 > 4) then
        cycle
      else
        idx(l) = i
        l = l+1
      endif
    enddo
    
  else if (Nint==2) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      degree_x2 =  popcnt(xor( key1(1,1,i), key2(1,1))) +            &
          popcnt(xor( key1(2,1,i), key2(2,1))) +                     &
          popcnt(xor( key1(1,2,i), key2(1,2))) +                     &
          popcnt(xor( key1(2,2,i), key2(2,2)))
      if (degree_x2 > 4) then
        cycle
      else
        idx(l) = i
        l = l+1
      endif
    enddo
    
  else if (Nint==3) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      degree_x2 = popcnt(xor( key1(1,1,i), key2(1,1))) +             &
          popcnt(xor( key1(1,2,i), key2(1,2))) +                     &
          popcnt(xor( key1(2,1,i), key2(2,1))) +                     &
          popcnt(xor( key1(2,2,i), key2(2,2))) +                     &
          popcnt(xor( key1(3,1,i), key2(3,1))) +                     &
          popcnt(xor( key1(3,2,i), key2(3,2)))
      if (degree_x2 > 4) then
        cycle
      else
        idx(l) = i
        l = l+1
      endif
    enddo
    
  else
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      degree_x2 = 0
      !DEC$ LOOP COUNT MIN(4)
      do j=1,Nint
        degree_x2 = degree_x2+ popcnt(xor( key1(j,1,i), key2(j,1))) +&
            popcnt(xor( key1(j,2,i), key2(j,2)))
        if (degree_x2 > 4) then
          exit
        endif
      enddo
      if (degree_x2 <= 5) then
        idx(l) = i
        l = l+1
      endif
    enddo
    
  endif
  idx(0) = l-1
end



subroutine filter_connected_davidson_warp(key1,warp,key2,Nint,sze,idx)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Filters out the determinants that are not connected by H
  ! returns the array idx which contains the index of the 
  ! determinants in the array key1 that interact 
  ! via the H operator with key2.
  !
  ! idx(0) is the number of determinants that interact with key1
  ! key1 should come from psi_det_sorted_ab.
  END_DOC
  integer, intent(in)            :: Nint, sze
  integer(bit_kind), intent(in)  :: key1(Nint,2,sze)
  integer(bit_kind), intent(in)  :: key2(Nint,2)
  integer, intent(out)           :: idx(0:sze)
  
  integer,intent(in)             :: warp(2,0:sze+1)
  
  integer                        :: i,j,k,l
  integer                        :: degree_x2
  integer :: i_alpha, i_beta, exc_a, exc_b, endloop, ni
  integer(bit_kind) :: tmp1, tmp2
  
  ASSERT (Nint > 0)
  ASSERT (sze >= 0)

  l=1
  i_alpha = 0
  
  
  if (Nint /= 1) then
    do while(i_alpha < warp(1,0) .and. warp(1,i_alpha+1) <= sze)
      i_alpha = i_alpha + 1
      exc_a = 0
      do ni=1,Nint
        exc_a += popcnt(xor(key1(ni,1,warp(1,i_alpha)), key2(ni,1)))
      end do
      endloop = min(warp(2,i_alpha), sze)
      if(exc_a == 4) then
        beta_loop : do i_beta=warp(1,i_alpha),endloop
          do ni=1,Nint
            if(key1(ni,2,i_beta) /= key2(ni,2)) then
              cycle beta_loop
            end if
          end do
          idx(l) = i_beta
          l = l + 1
          exit beta_loop
        end do beta_loop
      else
        do i_beta=warp(1,i_alpha),endloop
          exc_b = 0
          do ni=1,Nint
            exc_b += popcnt(xor(key1(ni,2,i_beta), key2(ni,2)))
          end do
          if(exc_b + exc_a <= 4) then
            idx(l) = i_beta
            l = l + 1
          end if                          
        end do
      end if
    end do
  else
    do while(i_alpha < warp(1,0) .and. warp(1,i_alpha+1) <= sze)
      i_alpha = i_alpha + 1
      exc_a = popcnt(xor(key1(1,1,warp(1,i_alpha)), key2(1,1)))
      endloop = min(warp(2,i_alpha), sze)
      if(exc_a == 4) then
        do i_beta=warp(1,i_alpha),endloop
          if(key1(1,2,i_beta) == key2(1,2)) then
            idx(l) = i_beta
            l = l + 1
            exit
          end if
        end do
      else
        do i_beta=warp(1,i_alpha),endloop
          exc_b = popcnt(xor(key1(1,2,i_beta), key2(1,2)))
          if(exc_b + exc_a <= 4) then
            idx(l) = i_beta
            l = l + 1
          end if                          
        end do
      end if
    end do
  end if
    
  idx(0) = l-1
end


! subroutine filter_connected_davidson_shortcut(key1,shortcut,key2,Nint,sze,idx)
!   use bitmasks
!   implicit none
!   BEGIN_DOC
!   ! Filters out the determinants that are not connected by H
!   ! returns the array idx which contains the index of the 
!   ! determinants in the array key1 that interact 
!   ! via the H operator with key2.
!   !
!   ! idx(0) is the number of determinants that interact with key1
!   ! key1 should come from psi_det_sorted_ab.
!   END_DOC
!   integer, intent(in)            :: Nint, sze
!   integer(bit_kind), intent(in)  :: key1(Nint,2,sze)
!   integer(bit_kind), intent(in)  :: key2(Nint,2)
!   integer, intent(out)           :: idx(0:sze)
!   
!   integer,intent(in)             :: shortcut(0:sze+1)
!   
!   integer                        :: i,j,k,l
!   integer                        :: degree_x2
!   integer :: i_alpha, i_beta, exc_a, exc_b, endloop
!   integer(bit_kind) :: tmp1, tmp2
!   
!   ASSERT (Nint > 0)
!   ASSERT (sze >= 0)
! 
!   l=1
!   i_alpha = 0
!   
!   if (Nint==1) then
!     do while(shortcut(i_alpha+1) < sze)
!       i_alpha = i_alpha + 1
!       exc_a = popcnt(xor(key1(1,1,shortcut(i_alpha)), key2(1,1)))
!       if(exc_a > 4) then
!         cycle
!       end if
!       endloop = min(shortcut(i_alpha+1)-1, sze)
!       if(exc_a == 4) then
!         do i_beta = shortcut(i_alpha), endloop
!           if(key1(1,2,i_beta) == key2(1,2)) then
!             idx(l) = i_beta
!             l = l + 1
!             exit
!           end if
!         end do
!       else
!         do i_beta = shortcut(i_alpha), endloop
!           exc_b = popcnt(xor(key1(1,2,i_beta), key2(1,2)))
!           if(exc_b + exc_a <= 4) then
!             idx(l) = i_beta
!             l = l + 1
!           end if
!         end do
!       end if
!     end do
!   else
!     print *, "TBD : filter_connected_davidson_shortcut Nint>1"
!     stop
!   end if
!     
!   idx(0) = l-1
! end
! 
! subroutine filter_connected_davidson(key1,key2,Nint,sze,idx)
!   use bitmasks
!   implicit none
!   BEGIN_DOC
!   ! Filters out the determinants that are not connected by H
!   ! returns the array idx which contains the index of the 
!   ! determinants in the array key1 that interact 
!   ! via the H operator with key2.
!   !
!   ! idx(0) is the number of determinants that interact with key1
!   ! key1 should come from psi_det_sorted_ab.
!   END_DOC
!   integer, intent(in)            :: Nint, sze
!   integer(bit_kind), intent(in)  :: key1(Nint,2,sze)
!   integer(bit_kind), intent(in)  :: key2(Nint,2)
!   integer, intent(inout)           :: idx(0:sze)
!   
!   integer                        :: i,j,k,l
!   integer                        :: degree_x2
!   integer :: j_int, j_start
!   integer*8 :: itmp
!   
!   PROVIDE N_con_int det_connections
!   
!   
!   ASSERT (Nint > 0)
!   ASSERT (sze >= 0)
! 
!   l=1
!   
!   if (Nint==1) then
! 
!     i = idx(0) ! lecture dans un intent(out) ?
!     do j_int=1,N_con_int
!       itmp = det_connections(j_int,i)
!       do while (itmp /= 0_8)
!         j_start = ishft(j_int-1,11) + ishft(trailz(itmp),5)
!         do j = j_start+1, min(j_start+32,i-1)
!           degree_x2 = popcnt(xor( key1(1,1,j), key2(1,1))) +             &
!               popcnt(xor( key1(1,2,j), key2(1,2)))
!           if (degree_x2 > 4) then
!             cycle
!           else
!             idx(l) = j
!             l = l+1
!           endif
!         enddo
!         itmp = iand(itmp-1_8,itmp)
!       enddo
!     enddo
!     
!   else if (Nint==2) then
!     
!     
!     i = idx(0)
!     do j_int=1,N_con_int
!       itmp = det_connections(j_int,i)
!       do while (itmp /= 0_8)
!         j_start = ishft(j_int-1,11) + ishft(trailz(itmp),5)
!         do j = j_start+1, min(j_start+32,i-1)
!           degree_x2 = popcnt(xor( key1(1,1,j), key2(1,1))) +         &
!               popcnt(xor( key1(2,1,j), key2(2,1))) +                 &
!               popcnt(xor( key1(1,2,j), key2(1,2))) +                 &
!               popcnt(xor( key1(2,2,j), key2(2,2)))
!           if (degree_x2 > 4) then
!             cycle
!           else
!             idx(l) = j
!             l = l+1
!           endif
!         enddo
!         itmp = iand(itmp-1_8,itmp)
!       enddo
!     enddo
!     
!   else if (Nint==3) then
!     
!     i = idx(0)
!     !DIR$ LOOP COUNT (1000)
!     do j_int=1,N_con_int
!       itmp = det_connections(j_int,i)
!       do while (itmp /= 0_8)
!         j_start = ishft(j_int-1,11) + ishft(trailz(itmp),5)
!         do j = j_start+1, min(j_start+32,i-1)
!           degree_x2 = popcnt(xor( key1(1,1,j), key2(1,1))) +         &
!               popcnt(xor( key1(1,2,j), key2(1,2))) +                 &
!               popcnt(xor( key1(2,1,j), key2(2,1))) +                 &
!               popcnt(xor( key1(2,2,j), key2(2,2))) +                 &
!               popcnt(xor( key1(3,1,j), key2(3,1))) +                 &
!               popcnt(xor( key1(3,2,j), key2(3,2)))
!           if (degree_x2 > 4) then
!             cycle
!           else
!             idx(l) = j
!             l = l+1
!           endif
!         enddo
!         itmp = iand(itmp-1_8,itmp)
!       enddo
!     enddo
!     
!   else
!     
!     i = idx(0)
!     !DIR$ LOOP COUNT (1000)
!     do j_int=1,N_con_int
!       itmp = det_connections(j_int,i)
!       do while (itmp /= 0_8)
!         j_start = ishft(j_int-1,11) + ishft(trailz(itmp),5)
!         do j = j_start+1, min(j_start+32,i-1)
!           degree_x2 = 0
!           !DEC$ LOOP COUNT MIN(4)
!           do k=1,Nint
!             degree_x2 = degree_x2+ popcnt(xor( key1(k,1,j), key2(k,1))) +&
!                 popcnt(xor( key1(k,2,j), key2(k,2)))
!             if (degree_x2 > 4) then
!               exit
!             endif
!           enddo
!           if (degree_x2 <= 5) then
!             idx(l) = j
!             l = l+1
!           endif
!         enddo
!         itmp = iand(itmp-1_8,itmp)
!       enddo
!     enddo
! 
!   endif
!   idx(0) = l-1
! end

subroutine filter_connected_i_H_psi0(key1,key2,Nint,sze,idx)
  use bitmasks
  BEGIN_DOC
  ! returns the array idx which contains the index of the 
  !
  ! determinants in the array key1 that interact 
  !
  ! via the H operator with key2.
  !
  ! idx(0) is the number of determinants that interact with key1
  END_DOC
  implicit none
  integer, intent(in)            :: Nint, sze
  integer(bit_kind), intent(in)  :: key1(Nint,2,sze)
  integer(bit_kind), intent(in)  :: key2(Nint,2)
  integer, intent(out)           :: idx(0:sze)
  
  integer                        :: i,l,m
  integer                        :: degree_x2

  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (sze > 0)
  
  l=1
  
  if (Nint==1) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      degree_x2 = popcnt(xor( key1(1,1,i), key2(1,1))) +             &
          popcnt(xor( key1(1,2,i), key2(1,2)))
      if (degree_x2 > 4) then
        cycle
      else if(degree_x2 .ne. 0)then
        idx(l) = i
        l = l+1
      endif
    enddo
    
  else if (Nint==2) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      degree_x2 =  popcnt(xor( key1(1,1,i), key2(1,1))) +            &
          popcnt(xor( key1(2,1,i), key2(2,1))) +                     &
          popcnt(xor( key1(1,2,i), key2(1,2))) +                     &
          popcnt(xor( key1(2,2,i), key2(2,2)))
      if (degree_x2 > 4) then
        cycle
      else if(degree_x2 .ne. 0)then
        idx(l) = i
        l = l+1
      endif
    enddo
    
  else if (Nint==3) then
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      degree_x2 = popcnt(xor( key1(1,1,i), key2(1,1))) +             &
          popcnt(xor( key1(1,2,i), key2(1,2))) +                     &
          popcnt(xor( key1(2,1,i), key2(2,1))) +                     &
          popcnt(xor( key1(2,2,i), key2(2,2))) +                     &
          popcnt(xor( key1(3,1,i), key2(3,1))) +                     &
          popcnt(xor( key1(3,2,i), key2(3,2)))
      if (degree_x2 > 4) then
        cycle
      else if(degree_x2 .ne. 0)then
        idx(l) = i
        l = l+1
      endif
    enddo
    
  else
    
    !DIR$ LOOP COUNT (1000)
    do i=1,sze
      degree_x2 = 0
      !DEC$ LOOP COUNT MIN(4)
      do m=1,Nint
        degree_x2 = degree_x2+ popcnt(xor( key1(m,1,i), key2(m,1))) +&
            popcnt(xor( key1(m,2,i), key2(m,2)))
        if (degree_x2 > 4) then
          exit
        endif
      enddo
      if (degree_x2 > 4) then
        cycle
      else if(degree_x2 .ne. 0)then
          idx(l) = i
          l = l+1
      endif
    enddo
    
  endif
  idx(0) = l-1
end

subroutine filter_connected_i_H_psi0_SC2(key1,key2,Nint,sze,idx,idx_repeat)
  use bitmasks
  BEGIN_DOC
  ! standard filter_connected_i_H_psi but returns in addition
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
  integer, intent(in)            :: Nint, sze
  integer(bit_kind), intent(in)  :: key1(Nint,2,sze)
  integer(bit_kind), intent(in)  :: key2(Nint,2)
  integer, intent(out)           :: idx(0:sze)
  integer, intent(out)           :: idx_repeat(0:sze)
  
  integer                        :: i,l,l_repeat,m
  integer                        :: degree_x2

  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (sze > 0)
  
  integer :: degree
  degree = popcnt(xor( ref_bitmask(1,1), key2(1,1))) +                      &
      popcnt(xor( ref_bitmask(1,2), key2(1,2)))
  !DEC$ NOUNROLL
  do m=2,Nint
    degree = degree+ popcnt(xor( ref_bitmask(m,1), key2(m,1))) +            &
        popcnt(xor( ref_bitmask(m,2), key2(m,2)))
  enddo
  degree = ishft(degree,-1)
  
  l_repeat=1
  l=1
  if(degree == 2)then
   if (Nint==1) then
 
      !DIR$ LOOP COUNT (1000)
      do i=1,sze
        degree_x2 = popcnt(xor( key1(1,1,i), key2(1,1))) +             &
            popcnt(xor( key1(1,2,i), key2(1,2)))
        if (degree_x2 < 5) then
          if(degree_x2 .ne. 0)then
            idx(l) = i
            l = l+1
          endif
        elseif(degree_x2>6)then
         idx_repeat(l_repeat) = i
         l_repeat = l_repeat + 1
        endif
      enddo
     
   else if (Nint==2) then
     
     !DIR$ LOOP COUNT (1000)
     do i=1,sze
       degree_x2 =  popcnt(xor( key1(1,1,i), key2(1,1))) +            &
           popcnt(xor( key1(2,1,i), key2(2,1))) +                     &
           popcnt(xor( key1(1,2,i), key2(1,2))) +                     &
           popcnt(xor( key1(2,2,i), key2(2,2)))
       if (degree_x2 < 5) then
         if(degree_x2 .ne. 0)then
           idx(l) = i
           l = l+1
         endif
       elseif(degree_x2>6)then
         idx_repeat(l_repeat) = i
         l_repeat = l_repeat + 1
       endif
     enddo
     
   else if (Nint==3) then
     
     !DIR$ LOOP COUNT (1000)
     do i=1,sze
       degree_x2 = popcnt(xor( key1(1,1,i), key2(1,1))) +             &
           popcnt(xor( key1(1,2,i), key2(1,2))) +                     &
           popcnt(xor( key1(2,1,i), key2(2,1))) +                     &
           popcnt(xor( key1(2,2,i), key2(2,2))) +                     &
           popcnt(xor( key1(3,1,i), key2(3,1))) +                     &
           popcnt(xor( key1(3,2,i), key2(3,2)))
       if(degree_x2>6)then
         idx_repeat(l_repeat) = i
         l_repeat = l_repeat + 1
       else if (degree_x2 < 5) then
         if(degree_x2 .ne. 0)then
           idx(l) = i
           l = l+1
         endif
       endif
     enddo
     
   else
     
     !DIR$ LOOP COUNT (1000)
     do i=1,sze
       degree_x2 = 0
       !DEC$ LOOP COUNT MIN(4)
       do m=1,Nint
         degree_x2 = degree_x2+ popcnt(xor( key1(m,1,i), key2(m,1))) +&
             popcnt(xor( key1(m,2,i), key2(m,2)))
         if (degree_x2 > 4) then
           exit
         endif
       enddo
       if (degree_x2 <= 5) then
         if(degree_x2 .ne. 0)then
           idx(l) = i
           l = l+1
         endif
       elseif(degree_x2>6)then
         idx_repeat(l_repeat) = i
         l_repeat = l_repeat + 1
       endif
     enddo
     
   endif
  elseif(degree==1)then
   if (Nint==1) then
     
 
      !DIR$ LOOP COUNT (1000)
      do i=1,sze
        degree_x2 = popcnt(xor( key1(1,1,i), key2(1,1))) +             &
            popcnt(xor( key1(1,2,i), key2(1,2)))
        if (degree_x2 < 5) then
          if(degree_x2 .ne. 0)then
            idx(l) = i
            l = l+1
          endif
        else
         idx_repeat(l_repeat) = i
         l_repeat = l_repeat + 1
        endif
      enddo
     
   else if (Nint==2) then
     
     !DIR$ LOOP COUNT (1000)
     do i=1,sze
       degree_x2 =  popcnt(xor( key1(1,1,i), key2(1,1))) +            &
           popcnt(xor( key1(2,1,i), key2(2,1))) +                     &
           popcnt(xor( key1(1,2,i), key2(1,2))) +                     &
           popcnt(xor( key1(2,2,i), key2(2,2)))
       if (degree_x2 < 5) then
         if(degree_x2 .ne. 0)then
           idx(l) = i
           l = l+1
         endif
       else
         idx_repeat(l_repeat) = i
         l_repeat = l_repeat + 1
       endif
     enddo
     
   else if (Nint==3) then
     
     !DIR$ LOOP COUNT (1000)
     do i=1,sze
       degree_x2 = popcnt(xor( key1(1,1,i), key2(1,1))) +             &
           popcnt(xor( key1(1,2,i), key2(1,2))) +                     &
           popcnt(xor( key1(2,1,i), key2(2,1))) +                     &
           popcnt(xor( key1(2,2,i), key2(2,2))) +                     &
           popcnt(xor( key1(3,1,i), key2(3,1))) +                     &
           popcnt(xor( key1(3,2,i), key2(3,2)))
       if (degree_x2 < 5) then
         if(degree_x2 .ne. 0)then
           idx(l) = i
           l = l+1
         endif
        else
         idx_repeat(l_repeat) = i
         l_repeat = l_repeat + 1
       endif
     enddo
     
   else
     
     !DIR$ LOOP COUNT (1000)
     do i=1,sze
       degree_x2 = 0
       !DEC$ LOOP COUNT MIN(4)
       do m=1,Nint
         degree_x2 = degree_x2+ popcnt(xor( key1(m,1,i), key2(m,1))) +&
             popcnt(xor( key1(m,2,i), key2(m,2)))
         if (degree_x2 > 4) then
           exit
         endif
       enddo
       if (degree_x2 <= 5) then
         if(degree_x2 .ne. 0)then
           idx(l) = i
           l = l+1
         endif
        else
         idx_repeat(l_repeat) = i
         l_repeat = l_repeat + 1
       endif
     enddo
     
   endif

  else 
!  print*,'more than a double excitation, can not apply the '
!  print*,'SC2 dressing of the diagonal element .....'
!  print*,'stop !!'
!  print*,'degree = ',degree
!  stop
  idx(0) = 0
  idx_repeat(0) = 0
  endif
  idx(0) = l-1
  idx_repeat(0) = l_repeat-1
end


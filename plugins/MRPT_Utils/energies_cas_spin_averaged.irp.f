
BEGIN_PROVIDER [ double precision, one_creat_spin_trace, (n_act_orb)]
 implicit none
 integer :: i
 do i = 1, n_act_orb
  one_creat_spin_trace(i) = one_creat(i,1) + one_creat(i,2)
  one_creat_spin_trace(i) = 0.5d0 * one_creat_spin_trace(i)
 enddo
END_PROVIDER 


BEGIN_PROVIDER [ double precision, one_anhil_spin_trace, (n_act_orb)]
 implicit none
 integer :: i
 do i = 1, n_act_orb
  one_anhil_spin_trace(i) = one_anhil(i,1) + one_anhil(i,2)
  one_anhil_spin_trace(i) = 0.5d0 * one_anhil_spin_trace(i)
 enddo

END_PROVIDER 

BEGIN_PROVIDER [ double precision, two_creat_spin_trace, (n_act_orb,n_act_orb)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin
 double precision :: counting
 do i = 1, n_act_orb
  do j = 1, n_act_orb 
   two_creat_spin_trace(j,i) = 0.d0
   counting = 0.d0
   do ispin = 1, 2
    do jspin = 1,2 
     two_creat_spin_trace(j,i) += two_creat(j,i,ispin,jspin)
     counting += 1.d0 
    enddo
   enddo 
   two_creat_spin_trace(j,i) = two_creat_spin_trace(j,i) / counting
  enddo
 enddo
END_PROVIDER 

BEGIN_PROVIDER [ double precision, two_anhil_spin_trace, (n_act_orb,n_act_orb)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin
 double precision :: counting
 do i = 1, n_act_orb
  do j = 1, n_act_orb 
   two_anhil_spin_trace(j,i) = 0.d0
   counting = 0.d0
   do ispin = 1, 2
    do jspin = 1,2 
     two_anhil_spin_trace(j,i) += two_anhil(j,i,ispin,jspin)
     counting += 1.d0 
    enddo
   enddo 
   two_anhil_spin_trace(j,i) = two_anhil_spin_trace(j,i) / counting
  enddo
 enddo
END_PROVIDER 


BEGIN_PROVIDER [ double precision, one_anhil_one_creat_spin_trace, (n_act_orb,n_act_orb)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin
 double precision :: counting
 do i = 1, n_act_orb
  do j = 1, n_act_orb 
   one_anhil_one_creat_spin_trace(j,i) = 0.d0
   counting = 0.d0
   do ispin = 1, 2
    do jspin = 1,2 
     one_anhil_one_creat_spin_trace(j,i) += one_anhil_one_creat(j,i,jspin,ispin)
     counting += 1.d0 
    enddo
   enddo
   one_anhil_one_creat_spin_trace(j,i) = one_anhil_one_creat_spin_trace(j,i) / counting
  enddo
 enddo
  
END_PROVIDER
 

BEGIN_PROVIDER [ double precision, two_anhil_one_creat_spin_trace, (n_act_orb,n_act_orb,n_act_orb)]
 implicit none
 integer :: i,j,k
 integer :: ispin,jspin,kspin
 double precision :: counting
 
 do i = 1, n_act_orb 
  do j = 1, n_act_orb 
   do k = 1, n_act_orb
    two_anhil_one_creat_spin_trace(k,j,i) = 0.d0
    counting = 0.d0
    do ispin = 1, 2 
     do jspin = 1,2
      do kspin = 1,2
       two_anhil_one_creat_spin_trace(k,j,i)  += two_anhil_one_creat(k,j,i,kspin,jspin,ispin)
       counting += 1.d0
      enddo 
     enddo
     two_anhil_one_creat_spin_trace(k,j,i) = two_anhil_one_creat_spin_trace(k,j,i) / counting
    enddo
   enddo
  enddo
 enddo

END_PROVIDER 

BEGIN_PROVIDER [ double precision, two_creat_one_anhil_spin_trace, (n_act_orb,n_act_orb,n_act_orb)]
 implicit none
 integer :: i,j,k
 integer :: ispin,jspin,kspin
 double precision :: counting
 
 do i = 1, n_act_orb 
  do j = 1, n_act_orb 
   do k = 1, n_act_orb
    two_creat_one_anhil_spin_trace(k,j,i) = 0.d0
    counting = 0.d0
    do ispin = 1, 2 
     do jspin = 1,2
      do kspin = 1,2
       two_creat_one_anhil_spin_trace(k,j,i)  += two_creat_one_anhil(k,j,i,kspin,jspin,ispin)
       counting += 1.d0
      enddo 
     enddo
     two_creat_one_anhil_spin_trace(k,j,i) = two_creat_one_anhil_spin_trace(k,j,i) / counting
    enddo
   enddo
  enddo
 enddo

END_PROVIDER 


BEGIN_PROVIDER [ double precision, three_creat_spin_trace, (n_act_orb,n_act_orb,n_act_orb)]
 implicit none
 integer :: i,j,k
 integer :: ispin,jspin,kspin
 double precision :: counting
 
 do i = 1, n_act_orb 
  do j = 1, n_act_orb 
   do k = 1, n_act_orb
    three_creat_spin_trace(k,j,i) = 0.d0
    counting = 0.d0
    do ispin = 1, 2 
     do jspin = 1,2
      do kspin = 1,2
       three_creat_spin_trace(k,j,i)  += three_creat(k,j,i,kspin,jspin,ispin)
       counting += 1.d0
      enddo 
     enddo
     three_creat_spin_trace(k,j,i) = three_creat_spin_trace(k,j,i) / counting
    enddo
   enddo
  enddo
 enddo

END_PROVIDER 


BEGIN_PROVIDER [ double precision, three_anhil_spin_trace, (n_act_orb,n_act_orb,n_act_orb)]
 implicit none
 integer :: i,j,k
 integer :: ispin,jspin,kspin
 double precision :: counting
 
 do i = 1, n_act_orb 
  do j = 1, n_act_orb 
   do k = 1, n_act_orb
    three_anhil_spin_trace(k,j,i) = 0.d0
    counting = 0.d0
    do ispin = 1, 2 
     do jspin = 1,2
      do kspin = 1,2
       three_anhil_spin_trace(k,j,i)  += three_anhil(k,j,i,kspin,jspin,ispin)
       counting += 1.d0
      enddo 
     enddo
     three_anhil_spin_trace(k,j,i) = three_anhil_spin_trace(k,j,i) / counting
    enddo
   enddo
  enddo
 enddo

END_PROVIDER 


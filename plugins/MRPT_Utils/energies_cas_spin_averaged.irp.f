
BEGIN_PROVIDER [ double precision, one_creation_spin_averaged, (n_act_orb)]
 implicit none
 integer :: i
 do i = 1, n_act_orb
  one_creation_spin_averaged(i) = one_creation(i,1) + one_creation(i,2)
  one_creation_spin_averaged(i) = 0.5d0 * one_creation_spin_averaged(i)
 enddo
END_PROVIDER 


BEGIN_PROVIDER [ double precision, one_anhilation_spin_averaged, (n_act_orb)]
 implicit none
 integer :: i
 do i = 1, n_act_orb
  one_anhilation_spin_averaged(i) = one_anhilation(i,1) + one_anhilation(i,2)
  one_anhilation_spin_averaged(i) = 0.5d0 * one_anhilation_spin_averaged(i)
 enddo

END_PROVIDER 

BEGIN_PROVIDER [ double precision, two_creation_spin_averaged, (n_act_orb,n_act_orb)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin
 double precision :: counting
 do i = 1, n_act_orb
  do j = 1, n_act_orb 
   two_creation_spin_averaged(j,i) = 0.d0
   counting = 0.d0
   do ispin = 1, 2
    do jspin = 1,2 
     two_creation_spin_averaged(j,i) += two_creation(j,i,ispin,jspin)
     counting += 1.d0 
    enddo
   enddo 
   two_creation_spin_averaged(j,i) = two_creation_spin_averaged(j,i) / counting
  enddo
 enddo
END_PROVIDER 

BEGIN_PROVIDER [ double precision, two_anhilation_spin_averaged, (n_act_orb,n_act_orb)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin
 double precision :: counting
 do i = 1, n_act_orb
  do j = 1, n_act_orb 
   two_anhilation_spin_averaged(j,i) = 0.d0
   counting = 0.d0
   do ispin = 1, 2
    do jspin = 1,2 
     two_anhilation_spin_averaged(j,i) += two_anhilation(j,i,ispin,jspin)
     counting += 1.d0 
    enddo
   enddo 
   two_anhilation_spin_averaged(j,i) = two_anhilation_spin_averaged(j,i) / counting
  enddo
 enddo
END_PROVIDER 


BEGIN_PROVIDER [ double precision, one_anhilation_one_creation_spin_averaged, (n_act_orb,n_act_orb)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin
 double precision :: counting
 do i = 1, n_act_orb
  do j = 1, n_act_orb 
   one_anhilation_one_creation_spin_averaged(j,i) = 0.d0
   counting = 0.d0
   do ispin = 1, 2
    do jspin = 1,2 
     one_anhilation_one_creation_spin_averaged(j,i) += one_anhilation_one_creation(j,i,jspin,ispin)
     counting += 1.d0 
    enddo
   enddo
   one_anhilation_one_creation_spin_averaged(j,i) = one_anhilation_one_creation_spin_averaged(j,i) / counting
  enddo
 enddo
  
END_PROVIDER
 

BEGIN_PROVIDER [ double precision, two_anhilation_one_creation_spin_averaged, (n_act_orb,n_act_orb,n_act_orb)]
 implicit none
 integer :: i,j,k
 integer :: ispin,jspin,kspin
 double precision :: counting
 
 do i = 1, n_act_orb 
  do j = 1, n_act_orb 
   do k = 1, n_act_orb
    two_anhilation_one_creation_spin_averaged(k,j,i) = 0.d0
    counting = 0.d0
    do ispin = 1, 2 
     do jspin = 1,2
      do kspin = 1,2
       two_anhilation_one_creation_spin_averaged(k,j,i)  += two_anhilation_one_creation(k,j,i,kspin,jspin,ispin)
       counting += 1.d0
      enddo 
     enddo
     two_anhilation_one_creation_spin_averaged(k,j,i) = two_anhilation_one_creation_spin_averaged(k,j,i) / counting
    enddo
   enddo
  enddo
 enddo

END_PROVIDER 

BEGIN_PROVIDER [ double precision, two_creation_one_anhilation_spin_averaged, (n_act_orb,n_act_orb,n_act_orb)]
 implicit none
 integer :: i,j,k
 integer :: ispin,jspin,kspin
 double precision :: counting
 
 do i = 1, n_act_orb 
  do j = 1, n_act_orb 
   do k = 1, n_act_orb
    two_creation_one_anhilation_spin_averaged(k,j,i) = 0.d0
    counting = 0.d0
    do ispin = 1, 2 
     do jspin = 1,2
      do kspin = 1,2
       two_creation_one_anhilation_spin_averaged(k,j,i)  += two_creation_one_anhilation(k,j,i,kspin,jspin,ispin)
       counting += 1.d0
      enddo 
     enddo
     two_creation_one_anhilation_spin_averaged(k,j,i) = two_creation_one_anhilation_spin_averaged(k,j,i) / counting
    enddo
   enddo
  enddo
 enddo

END_PROVIDER 


BEGIN_PROVIDER [ double precision, three_creation_spin_averaged, (n_act_orb,n_act_orb,n_act_orb)]
 implicit none
 integer :: i,j,k
 integer :: ispin,jspin,kspin
 double precision :: counting
 
 do i = 1, n_act_orb 
  do j = 1, n_act_orb 
   do k = 1, n_act_orb
    three_creation_spin_averaged(k,j,i) = 0.d0
    counting = 0.d0
    do ispin = 1, 2 
     do jspin = 1,2
      do kspin = 1,2
       three_creation_spin_averaged(k,j,i)  += three_creation(k,j,i,kspin,jspin,ispin)
       counting += 1.d0
      enddo 
     enddo
     three_creation_spin_averaged(k,j,i) = three_creation_spin_averaged(k,j,i) / counting
    enddo
   enddo
  enddo
 enddo

END_PROVIDER 


BEGIN_PROVIDER [ double precision, three_anhilation_spin_averaged, (n_act_orb,n_act_orb,n_act_orb)]
 implicit none
 integer :: i,j,k
 integer :: ispin,jspin,kspin
 double precision :: counting
 
 do i = 1, n_act_orb 
  do j = 1, n_act_orb 
   do k = 1, n_act_orb
    three_anhilation_spin_averaged(k,j,i) = 0.d0
    counting = 0.d0
    do ispin = 1, 2 
     do jspin = 1,2
      do kspin = 1,2
       three_anhilation_spin_averaged(k,j,i)  += three_anhilation(k,j,i,kspin,jspin,ispin)
       counting += 1.d0
      enddo 
     enddo
     three_anhilation_spin_averaged(k,j,i) = three_anhilation_spin_averaged(k,j,i) / counting
    enddo
   enddo
  enddo
 enddo

END_PROVIDER 


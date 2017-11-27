subroutine i_O1_j(array,key_i,key_j,Nint,hij)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Returns <i|O1|j> where i and j are determinants
  ! and O1 is a ONE BODY OPERATOR
  ! array  is the array of the mono electronic operator 
  ! on the MO basis
  END_DOC
  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: key_i(Nint,2), key_j(Nint,2)
  double precision, intent(out)  :: hij
  double precision, intent(in)   :: array(mo_tot_num,mo_tot_num)
  
  integer                        :: exc(0:2,2,2)
  integer                        :: degree
  integer                        :: m,p
  double precision               :: diag_O1_mat_elem, phase
  
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
     hij = 0.d0
    case (1)
      call get_mono_excitation(key_i,key_j,exc,phase,Nint)
      if (exc(0,1,1) == 1) then
        ! Mono alpha
        m = exc(1,1,1)
        p = exc(1,2,1)
      else
        ! Mono beta
        m = exc(1,1,2)
        p = exc(1,2,2)
      endif
      hij = phase* array(m,p)
      
    case (0)
      hij = diag_O1_mat_elem(array,key_i,Nint)
  end select
end


subroutine i_O1_psi(array,key,keys,coef,Nint,Ndet,Ndet_max,Nstate,i_H_psi_array)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint, Ndet,Ndet_max,Nstate
  double precision, intent(in)   :: array(mo_tot_num,mo_tot_num)
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
  ! <key|O1|psi> for the various Nstates
  ! and O1 is a ONE BODY OPERATOR
  ! array  is the array of the mono electronic operator 
  ! on the MO basis
  END_DOC
  
  ASSERT (Nint > 0)
  ASSERT (N_int == Nint)
  ASSERT (Nstate > 0)
  ASSERT (Ndet > 0)
  ASSERT (Ndet_max >= Ndet)
  i_H_psi_array = 0.d0
  call filter_connected_mono(keys,key,Nint,Ndet,idx)
  do ii=1,idx(0)
    i = idx(ii)
    !DIR$ FORCEINLINE
    call i_O1_j(array,keys(1,1,i),key,Nint,hij)
    do j = 1, Nstate
      i_H_psi_array(j) = i_H_psi_array(j) + coef(i,j)*hij
    enddo
  enddo
end

double precision function diag_O1_mat_elem(array,det_in,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes <i|O1|i>
  END_DOC
  integer,intent(in)             :: Nint
  integer(bit_kind),intent(in)   :: det_in(Nint,2)
  double precision, intent(in)   :: array(mo_tot_num,mo_tot_num)
  
  integer                        :: i, ispin,tmp
  integer                        :: occ_det(Nint*bit_kind_size,2)
  
  ASSERT (Nint > 0)
  ASSERT (sum(popcnt(det_in(:,1))) == elec_alpha_num)
  ASSERT (sum(popcnt(det_in(:,2))) == elec_beta_num)
  
  call bitstring_to_list(det_in(1,1), occ_det(1,1), tmp, Nint)
  call bitstring_to_list(det_in(1,2), occ_det(1,2), tmp, Nint)
  diag_O1_mat_elem = 0.d0
  do ispin = 1, 2
   do i = 1, elec_num_tab(ispin)
    diag_O1_mat_elem += array(occ_det(i,ispin),occ_det(i,ispin))
   enddo
  enddo
end


subroutine i_O1_psi_alpha_beta(array,key,keys,coef,Nint,Ndet,Ndet_max,Nstate,i_H_psi_array)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint, Ndet,Ndet_max,Nstate
  double precision, intent(in)   :: array(mo_tot_num,mo_tot_num)
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
  ! <key|O1(alpha) - O1(beta)|psi> for the various Nstates
  ! and O1 is a ONE BODY OPERATOR
  ! array  is the array of the mono electronic operator 
  ! on the MO basis
  END_DOC
  
  ASSERT (Nint > 0)
  ASSERT (N_int == Nint)
  ASSERT (Nstate > 0)
  ASSERT (Ndet > 0)
  ASSERT (Ndet_max >= Ndet)
  i_H_psi_array = 0.d0
  call filter_connected_mono(keys,key,Nint,Ndet,idx)
  do ii=1,idx(0)
    i = idx(ii)
    !DIR$ FORCEINLINE
    call i_O1_j_alpha_beta(array,keys(1,1,i),key,Nint,hij)
    do j = 1, Nstate
      i_H_psi_array(j) = i_H_psi_array(j) + coef(i,j)*hij
    enddo
  enddo
end

subroutine i_O1_j_alpha_beta(array,key_i,key_j,Nint,hij)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Returns <i|O1(alpha) - O1(beta)|j> where i and j are determinants
  ! and O1 is a ONE BODY OPERATOR
  ! array  is the array of the mono electronic operator 
  ! on the MO basis
  END_DOC
  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: key_i(Nint,2), key_j(Nint,2)
  double precision, intent(out)  :: hij
  double precision, intent(in)   :: array(mo_tot_num,mo_tot_num)
  
  integer                        :: exc(0:2,2,2)
  integer                        :: degree
  integer                        :: m,p
  double precision               :: diag_O1_mat_elem_alpha_beta, phase
  
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
     hij = 0.d0
    case (1)
      call get_mono_excitation(key_i,key_j,exc,phase,Nint)
      if (exc(0,1,1) == 1) then
        ! Mono alpha
        m = exc(1,1,1)
        p = exc(1,2,1)
        hij = phase* array(m,p)
      else
        ! Mono beta
        m = exc(1,1,2)
        p = exc(1,2,2)
        hij = -phase* array(m,p)
      endif
      
    case (0)
      hij = diag_O1_mat_elem_alpha_beta(array,key_i,Nint)
  end select
end


double precision function diag_O1_mat_elem_alpha_beta(array,det_in,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Computes <i|O1(alpha) -O1(beta)|i>
  END_DOC
  integer,intent(in)             :: Nint
  integer(bit_kind),intent(in)   :: det_in(Nint,2)
  double precision, intent(in)   :: array(mo_tot_num,mo_tot_num)
  
  integer                        :: i, ispin,tmp
  integer                        :: occ_det(Nint*bit_kind_size,2)
  
  ASSERT (Nint > 0)
  ASSERT (sum(popcnt(det_in(:,1))) == elec_alpha_num)
  ASSERT (sum(popcnt(det_in(:,2))) == elec_beta_num)
  
  call bitstring_to_list(det_in(1,1), occ_det(1,1), tmp, Nint)
  call bitstring_to_list(det_in(1,2), occ_det(1,2), tmp, Nint)
  diag_O1_mat_elem_alpha_beta = 0.d0
  ispin = 1
   do i = 1, elec_num_tab(ispin)
    diag_O1_mat_elem_alpha_beta += array(occ_det(i,ispin),occ_det(i,ispin))
   enddo
  ispin = 2
   do i = 1, elec_num_tab(ispin)
    diag_O1_mat_elem_alpha_beta -= array(occ_det(i,ispin),occ_det(i,ispin))
   enddo
end

subroutine filter_connected_mono(key1,key2,Nint,sze,idx)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Filters out the determinants that are not connected through PURE 
  !
  ! MONO EXCITATIONS OPERATORS (a^{\dagger}j a_i)
  !
  ! returns the array idx which contains the index of the 
  !
  ! determinants in the array key1 that interact 
  !
  ! via some PURE MONO EXCITATIONS OPERATORS
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
      if (degree_x2 > 3) then
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
      if (degree_x2 > 3) then
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
      if (degree_x2 > 3) then
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
      !DIR$ LOOP COUNT MIN(4)
      do j=1,Nint
        degree_x2 = degree_x2+ popcnt(xor( key1(j,1,i), key2(j,1))) +&
            popcnt(xor( key1(j,2,i), key2(j,2)))
        if (degree_x2 > 3) then
          exit
        endif
      enddo
      if (degree_x2 <= 3) then
        idx(l) = i
        l = l+1
      endif
    enddo
    
  endif
  idx(0) = l-1
end



 use bitmasks
 BEGIN_PROVIDER [integer, max_number_ionic]
&BEGIN_PROVIDER [integer, min_number_ionic]
 BEGIN_DOC
 ! Maximum and minimum number of ionization in psi_ref 
 END_DOC
 implicit none
 integer :: i,j
 integer :: n_closed_shell_cas
 max_number_ionic = 0
 min_number_ionic = 100000
 do i = 1, N_det_ref
  j = n_closed_shell_cas(psi_ref(1,1,i),n_int)
  if(j> max_number_ionic)then
   max_number_ionic = j
  endif
  if(j< min_number_ionic)then
   min_number_ionic = j
  endif
  
 enddo
 print*,'max_number_ionic = ',max_number_ionic
 print*,'min_number_ionic = ',min_number_ionic
END_PROVIDER 

 BEGIN_PROVIDER [integer, ionic_index, (min_number_ionic:max_number_ionic,0:N_det_ref)]
&BEGIN_PROVIDER [double precision, normalization_factor_ionic, (min_number_ionic:max_number_ionic, N_states)]
 BEGIN_DOC
 ! Index of the various determinants in psi_ref according to their level of ionicity
 ! ionic_index(i,0) = number of determinants in psi_ref having the degree of ionicity "i"
 ! ionic_index(i,j) = index of the determinants having the degree of ionicity "i" 
 END_DOC
 implicit none
 integer :: i,j,k
 integer :: n_closed_shell_cas
 double precision :: accu
 ionic_index = 0
 do i = 1, N_det_ref
  j = n_closed_shell_cas(psi_ref(1,1,i),n_int)
  ionic_index(j,0) +=1
  ionic_index(j,ionic_index(j,0)) = i
 enddo
 do i = min_number_ionic,max_number_ionic 
  accu = 0.d0
  do j = 1, N_states
   do k = 1, ionic_index(i,0)
    accu += psi_ref_coef_diagonalized(ionic_index(i,k),j)   *  psi_ref_coef_diagonalized(ionic_index(i,k),j)
   enddo
   normalization_factor_ionic(i,j) = 1.d0/dsqrt(accu)
  enddo
 enddo

END_PROVIDER


 BEGIN_PROVIDER [double precision, H_OVB_naked, (min_number_ionic:max_number_ionic, min_number_ionic:max_number_ionic, n_states)]
 BEGIN_DOC 
 ! Hamiltonian matrix expressed in the basis of contracted forms in terms of ionic structures
 END_DOC
 implicit none
 integer :: i,j,istate,k,l
 double precision :: accu,hij
 do i = min_number_ionic,max_number_ionic
  do j = min_number_ionic,max_number_ionic
   do istate = 1, N_states
    accu = 0.d0
    do k = 1,  ionic_index(i,0)
     do l = 1,  ionic_index(j,0)
      hij = ref_hamiltonian_matrix(ionic_index(i,k),ionic_index(j,l))
      accu += psi_ref_coef_diagonalized(ionic_index(i,k),istate) * normalization_factor_ionic(i,istate) * & 
              psi_ref_coef_diagonalized(ionic_index(j,l),istate) * normalization_factor_ionic(j,istate) * hij
     enddo
    enddo
    H_OVB_naked(i,j,istate) = accu
   enddo
  enddo
 enddo
 END_PROVIDER

 BEGIN_PROVIDER [integer, n_couples_act_orb]
 implicit none
 n_couples_act_orb = 3
 END_PROVIDER 

 BEGIN_PROVIDER [integer, couples_act_orb, (n_couples_act_orb,2) ]
 implicit none
 
 couples_act_orb(1,1) = 20
 couples_act_orb(1,2) = 21
 couples_act_orb(2,1) = 22
 couples_act_orb(2,2) = 23
 couples_act_orb(3,1) = 24
 couples_act_orb(3,2) = 25

 END_PROVIDER 


 BEGIN_PROVIDER [double precision, H_matrix_between_ionic_on_given_atom , (n_act_orb,n_act_orb)]
 implicit none
 BEGIN_DOC
! Hamiltonian matrix elements between the various contracted functions 
! that have a negative charge on a given active orbital
 END_DOC
 integer :: i,j,k,l,jj,ii
 integer(bit_kind), allocatable :: key_1(:,:),key_2(:,:)
 double precision :: accu,hij
 double precision :: norm
 allocate (key_1(N_int,2),key_2(N_int,2))
 do i = 1, n_act_orb
  j = i           ! Diagonal part 
  norm = 0.d0
  accu = 0.d0
  do k = 1,  n_det_ionic_on_given_atom(i)
   norm += psi_coef_mono_ionic_on_given_atom(k,i) **2
   do ii = 1, N_int
    key_1(ii,1) = psi_det_mono_ionic_on_given_atom(ii,1,k,i)
    key_1(ii,2) = psi_det_mono_ionic_on_given_atom(ii,2,k,i)
   enddo 
   do l = 1,  n_det_ionic_on_given_atom(j)
    do jj = 1, N_int
     key_2(jj,1) = psi_det_mono_ionic_on_given_atom(jj,1,l,j)
     key_2(jj,2) = psi_det_mono_ionic_on_given_atom(jj,2,l,j)
    enddo 
    call i_H_j(key_1,key_2,N_int,hij)
    accu += psi_coef_mono_ionic_on_given_atom(l,j) * psi_coef_mono_ionic_on_given_atom(k,i) * hij
   enddo
  enddo
  H_matrix_between_ionic_on_given_atom(i,j) = accu


  do j = i+1, n_act_orb   ! Extra diagonal part 
   accu = 0.d0
   do k = 1,  n_det_ionic_on_given_atom(i)
    do jj = 1, N_int
     key_1(jj,1) = psi_det_mono_ionic_on_given_atom(jj,1,k,i)
     key_1(jj,2) = psi_det_mono_ionic_on_given_atom(jj,2,k,i)
    enddo 
    do l = 1,  n_det_ionic_on_given_atom(j)
     do jj = 1, N_int
      key_2(jj,1) = psi_det_mono_ionic_on_given_atom(jj,1,l,j)
      key_2(jj,2) = psi_det_mono_ionic_on_given_atom(jj,2,l,j)
     enddo 
     call i_H_j(key_1,key_2,N_int,hij)
     accu += psi_coef_mono_ionic_on_given_atom(l,j) * psi_coef_mono_ionic_on_given_atom(k,i) * hij
    enddo
   enddo
   H_matrix_between_ionic_on_given_atom(i,j) = accu
   H_matrix_between_ionic_on_given_atom(j,i) = accu
  enddo
 enddo
 
 END_PROVIDER 

 BEGIN_PROVIDER [double precision, H_matrix_between_ionic_on_given_atom_and_others , (n_act_orb,min_number_ionic:max_number_ionic)]
 implicit none
 use bitmasks
 BEGIN_DOC
! Hamiltonian matrix elements between the various contracted functions 
! that have a negative charge on a given active orbital 
! and all the other fully contracted OVB structures 
 END_DOC
 integer :: i,j,k,l,jj,ii
 integer(bit_kind), allocatable :: key_1(:,:),key_2(:,:)
 double precision :: accu,hij
 double precision :: norm
 allocate (key_1(N_int,2),key_2(N_int,2))
 
 do i = 1, n_act_orb
  do j = min_number_ionic,max_number_ionic
   if(j==1)then
    H_matrix_between_ionic_on_given_atom_and_others(i,j) = 0.d0
   endif
   accu = 0.d0
   do k = 1,  n_det_ionic_on_given_atom(i)
    do jj = 1, N_int
     key_1(jj,1) = psi_det_mono_ionic_on_given_atom(jj,1,k,i)
     key_1(jj,2) = psi_det_mono_ionic_on_given_atom(jj,2,k,i)
    enddo 
    do l = 1,  ionic_index(j,0)
     do ii = 1, N_int
      key_2(ii,1) = psi_det_ovb(ii,1,l,j)
      key_2(ii,2) = psi_det_ovb(ii,2,l,j)
     enddo 
     call i_H_j(key_1,key_2,N_int,hij)
     accu += psi_coef_ovb(l,j) * psi_coef_mono_ionic_on_given_atom(k,i) * hij
    enddo
   enddo
   H_matrix_between_ionic_on_given_atom_and_others(i,j) = accu
  enddo
 enddo

 print*,'H_matrix_between_ionic_on_given_atom_and_others'
 print*,''
 do i = 1, n_act_orb
  write(*,'(I3,X,100(F16.7))'),H_matrix_between_ionic_on_given_atom_and_others(i,:)
 enddo



END_PROVIDER 

 BEGIN_PROVIDER [integer, n_det_ionic_on_given_atom, (n_act_orb)]
&BEGIN_PROVIDER [double precision, normalization_factor_ionic_on_given_atom, (n_act_orb) ]
&BEGIN_PROVIDER [double precision, psi_coef_mono_ionic_on_given_atom, (N_det_ref,n_act_orb) ]
&BEGIN_PROVIDER [integer(bit_kind), psi_det_mono_ionic_on_given_atom, (N_int,2,N_det_ref,n_act_orb)]
 implicit none
 use bitmasks
 BEGIN_DOC
! number of determinants that are mono ionic with the negative charge 
! on a given atom, normalization_factor, array of determinants,and coefficients
 END_DOC
 integer :: i,j,k,l
 ionicity_level = 1
 integer :: ionicity_level
 logical :: doubly_occupied_array(n_act_orb)
 n_det_ionic_on_given_atom = 0
 normalization_factor_ionic_on_given_atom = 0.d0
 do i = 1,  ionic_index(ionicity_level,0)
  call give_index_of_doubly_occ_in_active_space(psi_det(1,1,ionic_index(ionicity_level,i)),doubly_occupied_array)
  do j = 1, n_act_orb
   if(doubly_occupied_array(j))then
    n_det_ionic_on_given_atom(j) += 1
    normalization_factor_ionic_on_given_atom(j) += psi_ref_coef_diagonalized(ionic_index(1,i),1) **2
    do k = 1, N_int
     psi_det_mono_ionic_on_given_atom(k,1,n_det_ionic_on_given_atom(j),j) = psi_det(k,1,ionic_index(ionicity_level,i))
     psi_det_mono_ionic_on_given_atom(k,2,n_det_ionic_on_given_atom(j),j) = psi_det(k,2,ionic_index(ionicity_level,i))
    enddo
    psi_coef_mono_ionic_on_given_atom(n_det_ionic_on_given_atom(j),j) = psi_ref_coef_diagonalized(ionic_index(1,i),1) 
   endif
  enddo
 enddo
 integer :: i_count
 i_count = 0
 do j = 1, n_act_orb
  i_count += n_det_ionic_on_given_atom(j)
  normalization_factor_ionic_on_given_atom(j) = 1.d0/dsqrt(normalization_factor_ionic_on_given_atom(j))
 enddo
 if(i_count.ne.ionic_index(ionicity_level,0))then
   print*,'PB with n_det_ionic_on_given_atom'
   print*,'i_count = ',i_count
   print*,'ionic_index(ionicity_level,0)',ionic_index(ionicity_level,0)
   stop
 endif
 do j = 1, n_act_orb 
  do i = 1, n_det_ionic_on_given_atom(j) 
   psi_coef_mono_ionic_on_given_atom(i,j) =  psi_coef_mono_ionic_on_given_atom(i,j) * normalization_factor_ionic_on_given_atom(j)
  enddo
 enddo
 

 END_PROVIDER 

 BEGIN_PROVIDER [integer(bit_kind), psi_det_ovb, (N_int,2,N_det_ref,min_number_ionic:max_number_ionic)]
&BEGIN_PROVIDER [double precision, psi_coef_ovb, (N_det_ref,min_number_ionic:max_number_ionic)  ]
 implicit none
 BEGIN_DOC
! Array of the determinants belonging to each ovb structures (neutral, mono ionic, bi ionic etc ...)
! together with the arrays of coefficients
 END_DOC
 integer :: i,j,k,l
 use bitmasks
 integer :: ionicity_level,i_count
 double precision :: accu
 
 do ionicity_level = min_number_ionic,max_number_ionic
  accu = 0.d0
  do i = 1,  ionic_index(ionicity_level,0)
   do j = 1, N_int
    psi_det_ovb(j,1,i,ionicity_level)  = psi_det(j,1,ionic_index(ionicity_level,i))
    psi_det_ovb(j,2,i,ionicity_level)  = psi_det(j,2,ionic_index(ionicity_level,i))
   enddo
   psi_coef_ovb(i,ionicity_level) = psi_ref_coef_diagonalized(ionic_index(ionicity_level,i),1) * normalization_factor_ionic(ionicity_level,1)
   accu +=  psi_coef_ovb(i,ionicity_level)**2
  enddo
  accu = 1.d0/dsqrt(accu)
  do i = 1,  ionic_index(ionicity_level,0)
   psi_coef_ovb(i,ionicity_level) =  psi_coef_ovb(i,ionicity_level) * accu
  enddo
  accu = 0.d0
  do i = 1,  ionic_index(ionicity_level,0)
   accu += psi_coef_ovb(i,ionicity_level) **2
  enddo
 enddo
 
END_PROVIDER

 BEGIN_PROVIDER [double precision, H_matrix_psi_det_ovb, (min_number_ionic:max_number_ionic,min_number_ionic:max_number_ionic)]
 implicit none
 BEGIN_DOC
! H matrix between the fully contracted OVB forms
 END_DOC
 integer :: i,j,k,l,jj,ii
 integer(bit_kind), allocatable :: key_1(:,:),key_2(:,:)
 use bitmasks
 double precision :: accu,hij
 double precision :: norm
 allocate (key_1(N_int,2),key_2(N_int,2))
 do i = min_number_ionic,max_number_ionic
  do j = min_number_ionic,max_number_ionic
   accu = 0.d0
   do k = 1, ionic_index(i,0)
    do ii = 1, N_int
     key_1(ii,1) = psi_det_ovb(ii,1,k,i)
     key_1(ii,2) = psi_det_ovb(ii,2,k,i)
    enddo
    do l = 1, ionic_index(j,0)
     do ii = 1, N_int
      key_2(ii,1) = psi_det_ovb(ii,1,l,j)
      key_2(ii,2) = psi_det_ovb(ii,2,l,j)
     enddo
     call i_H_j(key_1,key_2,N_int,hij)
     accu += psi_coef_ovb(l,j) * psi_coef_ovb(k,i) * hij
    enddo
   enddo 
   H_matrix_psi_det_ovb(i,j) = accu
  enddo
 enddo

 END_PROVIDER 

 BEGIN_PROVIDER [integer, number_first_ionic_couples]
&BEGIN_PROVIDER [logical , is_a_first_ionic_couple, (N_det_ref)]
&BEGIN_PROVIDER [double precision, normalization_factor_special_first_ionic, (2)]
 implicit none
 BEGIN_DOC
  ! Number of determinants belonging to the class of first ionic
  ! AND that have a couple of positive/negative charge belonging 
  ! to a couple of orbital couples_act_orb
  ! If is_a_first_ionic_couple(i) = .True. then this determinant is a first ionic
  ! and have a couple of positive/negative charge belonging
  ! to a couple of orbital couples_act_orb
  ! normalization factor (1) = 1/(sum c_i^2   .with. is_a_first_ionic_couple(i) = .True.)
  ! normalization factor (2) = 1/(sum c_i^2   .with. is_a_first_ionic_couple(i) = .False.)
 END_DOC
 integer :: i,j
 use bitmasks
 number_first_ionic_couples = 0
 integer :: ionicity_level
 logical :: couples_out(0:n_couples_act_orb)
 integer(bit_kind) :: key_tmp(N_int,2)
 ionicity_level = 1
 normalization_factor_special_first_ionic = 0.d0
 do i = 1,  ionic_index(ionicity_level,0)
  do j = 1, N_int
   key_tmp(j,1) = psi_det(j,1,ionic_index(ionicity_level,i))
   key_tmp(j,2) = psi_det(j,2,ionic_index(ionicity_level,i))
  enddo
  call doubly_occ_empty_in_couple(key_tmp,n_couples_act_orb,couples_act_orb,couples_out)
  if(couples_out(0))then
   number_first_ionic_couples +=1
   is_a_first_ionic_couple(i) = .True.
   normalization_factor_special_first_ionic(1) += psi_ref_coef_diagonalized(ionic_index(1,i),1) **2 
  else 
   is_a_first_ionic_couple(i) = .False.
   normalization_factor_special_first_ionic(2) += psi_ref_coef_diagonalized(ionic_index(1,i),1) **2
  endif
 enddo
 normalization_factor_special_first_ionic(1) = 1.d0/dsqrt(normalization_factor_special_first_ionic(1))
 normalization_factor_special_first_ionic(2) = 1.d0/dsqrt(normalization_factor_special_first_ionic(2))
 print*,'number_first_ionic_couples = ',number_first_ionic_couples
 END_PROVIDER
 

 BEGIN_PROVIDER [integer, number_neutral_no_hund_couples]
&BEGIN_PROVIDER [logical , is_a_neutral_no_hund_couple, (N_det_ref)]
&BEGIN_PROVIDER [double precision, normalization_factor_neutra_no_hund_couple, (2)]
&BEGIN_PROVIDER [double precision, ratio_hund_no_hund ]
 implicit none
 BEGIN_DOC
  ! Number of determinants belonging to the class of neutral determinants 
  ! AND that have a couple of alpha beta electrons in couple of orbital couples_act_orb
  ! If is_a_neutral_no_hund_couple(i) = .True. then this determinant is a neutral determinants 
  ! and have a a couple of alpha beta electrons in couple of orbital couples_act_orb
  ! normalization factor (1) = 1/sqrt(sum c_i^2   .with. is_a_neutral_no_hund_couple(i) = .True.)
  ! normalization factor (2) = 1/sqrt(sum c_i^2   .with. is_a_neutral_no_hund_couple(i) = .False.)
 END_DOC
 integer :: i,j
 use bitmasks
 number_neutral_no_hund_couples = 0
 integer :: ionicity_level
 logical :: couples_out(0:n_couples_act_orb)
 integer(bit_kind) :: key_tmp(N_int,2)
 integer :: ifirst_hund,ifirst_no_hund
 double precision :: coef_ref_hund,coef_ref_no_hund
 ifirst_hund = 0
 ifirst_no_hund = 0
 ionicity_level = 0
 normalization_factor_neutra_no_hund_couple = 0.d0
 do i = 1,  ionic_index(ionicity_level,0)
  do j = 1, N_int
   key_tmp(j,1) = psi_det(j,1,ionic_index(ionicity_level,i))
   key_tmp(j,2) = psi_det(j,2,ionic_index(ionicity_level,i))
  enddo
  call neutral_no_hund_in_couple(key_tmp,n_couples_act_orb,couples_act_orb,couples_out)
  if(couples_out(0))then
   if(ifirst_no_hund == 0)then
     coef_ref_no_hund = psi_ref_coef_diagonalized(ionic_index(ionicity_level,i),1)
     ifirst_no_hund = 1
   endif
   number_neutral_no_hund_couples +=1
   is_a_neutral_no_hund_couple(i) = .True.
   normalization_factor_neutra_no_hund_couple(1) += psi_ref_coef_diagonalized(ionic_index(ionicity_level,i),1) **2 
  else 
   if(ifirst_hund == 0)then
     coef_ref_hund = psi_ref_coef_diagonalized(ionic_index(ionicity_level,i),1)
     ifirst_hund = 1
   endif
   is_a_neutral_no_hund_couple(i) = .False.
   normalization_factor_neutra_no_hund_couple(2) += psi_ref_coef_diagonalized(ionic_index(ionicity_level,i),1) **2
  endif
 enddo
 ratio_hund_no_hund = coef_ref_no_hund/coef_ref_hund

 normalization_factor_neutra_no_hund_couple(1) = 1.d0/dsqrt(normalization_factor_neutra_no_hund_couple(1))
 normalization_factor_neutra_no_hund_couple(2) = 1.d0/dsqrt(normalization_factor_neutra_no_hund_couple(2))
 print*,'number_neutral_no_hund_couples = ',number_neutral_no_hund_couples
 END_PROVIDER
 
 BEGIN_PROVIDER [double precision, H_OVB_naked_first_ionic, (2,min_number_ionic:max_number_ionic,n_states)]
&BEGIN_PROVIDER [double precision, H_OVB_naked_first_ionic_between_ionic, (2,2,n_states)]
 BEGIN_DOC 
 ! H_OVB_naked_first_ionic(1,i) = H_matrix element between the first ionic determinants belonging to is_a_first_ionic_couple = True
 ! and the contracted ith ionic form
 ! if i == 1 not defined
 ! H_OVB_naked_first_ionic(2,i) = H_matrix element between the first ionic determinants belonging to is_a_first_ionic_couple = False
 ! and the contracted ith ionic form
 ! if i == 1 not defined
 ! H_OVB_naked_first_ionic_between_ionic(1,1) = H_matrix element between the first ionic determinants belonging to is_a_first_ionic_couple = True
 ! and the first ionic determinants belonging to is_a_first_ionic_couple = True
 ! H_OVB_naked_first_ionic_between_ionic(1,2) = H_matrix element between the first ionic determinants belonging to is_a_first_ionic_couple = True
 ! and the first ionic determinants belonging to is_a_first_ionic_couple = False
 ! H_OVB_naked_first_ionic_between_ionic(2,2) = H_matrix element between the first ionic determinants belonging to is_a_first_ionic_couple = False
 ! and the first ionic determinants belonging to is_a_first_ionic_couple = False
 END_DOC
 implicit none
 integer :: i,j,istate,k,l
 double precision :: accu_1,accu_2,hij
  H_OVB_naked_first_ionic = 0.d0
  H_OVB_naked_first_ionic_between_ionic = 0.d0
  i = 1
  do j = min_number_ionic,max_number_ionic
   if(j==1)cycle
   do istate = 1, N_states
    accu_1 = 0.d0
    accu_2 = 0.d0
    do k = 1,  ionic_index(i,0)
     if(is_a_first_ionic_couple(k))then   
      do l = 1,  ionic_index(j,0)
       hij = ref_hamiltonian_matrix(ionic_index(i,k),ionic_index(j,l))
       accu_1 += psi_ref_coef_diagonalized(ionic_index(i,k),istate) * normalization_factor_special_first_ionic(1) * & 
                 psi_ref_coef_diagonalized(ionic_index(j,l),istate) * normalization_factor_ionic(j,istate) * hij
      enddo
     else 
      do l = 1,  ionic_index(j,0)
       hij = ref_hamiltonian_matrix(ionic_index(i,k),ionic_index(j,l))
       accu_2 += psi_ref_coef_diagonalized(ionic_index(i,k),istate) * normalization_factor_special_first_ionic(2) * & 
                 psi_ref_coef_diagonalized(ionic_index(j,l),istate) * normalization_factor_ionic(j,istate) * hij
      enddo
     endif
    enddo
    H_OVB_naked_first_ionic(1,j,istate) = accu_1
    H_OVB_naked_first_ionic(2,j,istate) = accu_2
   enddo
  enddo


   do istate = 1, N_states
    accu_1 = 0.d0
    accu_2 = 0.d0
    integer :: i_count
    i_count = 0
    do k = 1,  ionic_index(1,0)
      do l = 1,  ionic_index(1,0)
        hij = ref_hamiltonian_matrix(ionic_index(1,k),ionic_index(1,l))
        accu_1 = hij * psi_ref_coef_diagonalized(ionic_index(1,k),istate) * psi_ref_coef_diagonalized(ionic_index(1,l),istate)
        if(is_a_first_ionic_couple(k).and. is_a_first_ionic_couple(l))then   
         H_OVB_naked_first_ionic_between_ionic(1,1,istate) += accu_1 *  normalization_factor_special_first_ionic(1) **2 
        elseif(is_a_first_ionic_couple(k).and. .not.is_a_first_ionic_couple(l))then
         i_count += 1
         H_OVB_naked_first_ionic_between_ionic(1,2,istate) += accu_1 *  & 
                                       normalization_factor_special_first_ionic(1) *normalization_factor_special_first_ionic(2)
!       elseif(is_a_first_ionic_couple(l).and. .not.is_a_first_ionic_couple(k))then
!        i_count += 1
!        H_OVB_naked_first_ionic_between_ionic(1,2,istate) += accu_1 *  & 
!                                      normalization_factor_special_first_ionic(1) *normalization_factor_special_first_ionic(2)
        elseif(.not.is_a_first_ionic_couple(k).and. .not.is_a_first_ionic_couple(l))then
         H_OVB_naked_first_ionic_between_ionic(2,2,istate) +=  accu_1 *  normalization_factor_special_first_ionic(2) **2
        endif
      enddo
   enddo
  enddo
  print*,'i_count                       = ',i_count
  print*,'number_first_ionic_couples**2 = ',ionic_index(1,0) * number_first_ionic_couples
  
 double precision :: convert_hartree_ev
 convert_hartree_ev = 27.211399d0
   print*,'Special H matrix'
   do i = 1,2
    write(*,'(I4,X,10(F16.8 ,4X))')i, H_OVB_naked_first_ionic(i,:,1)
   enddo

   print*,'Special H matrix bis'
   do i = 1,2
    write(*,'(I4,X,10(F16.8 ,4X))')i, H_OVB_naked_first_ionic_between_ionic(i,:,1)
   enddo


 END_PROVIDER 


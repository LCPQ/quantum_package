
integer function n_open_shell(det_in,nint)
 implicit none
 use bitmasks
 integer, intent(in) :: nint
 integer(bit_kind), intent(in) :: det_in(nint,2)
 integer :: i
 n_open_shell = 0
 do i=1,Nint
   n_open_shell += popcnt(iand(xor(det_in(i,1),det_in(i,2)),det_in(i,1)))
 enddo
end

integer function n_closed_shell(det_in,nint)
 implicit none
 use bitmasks
 integer, intent(in) :: nint
 integer(bit_kind), intent(in) :: det_in(nint,2)
 integer :: i
 n_closed_shell = 0
 do i=1,Nint
   n_closed_shell += popcnt(iand(det_in(i,1),det_in(i,2)))
 enddo
end

integer function n_closed_shell_cas(det_in,nint)
 implicit none
 use bitmasks
 integer, intent(in) :: nint
 integer(bit_kind), intent(in) :: det_in(nint,2)
 integer(bit_kind) :: det_tmp(nint,2)
 integer :: i
 n_closed_shell_cas = 0
 do i=1,Nint
   det_tmp(i,1) = xor(det_in(i,1),reunion_of_core_inact_bitmask(i,1))
   det_tmp(i,2) = xor(det_in(i,2),reunion_of_core_inact_bitmask(i,2))
 enddo
!call debug_det(det_tmp,nint)
 do i=1,Nint
   n_closed_shell_cas += popcnt(iand(det_tmp(i,1),det_tmp(i,2)))
 enddo
end

subroutine doubly_occ_empty_in_couple(det_in,n_couples,couples,couples_out)
 implicit none
  use bitmasks
 integer, intent(in) :: n_couples,couples(n_couples,2)
 integer(bit_kind),intent(in) :: det_in(N_int,2)
 logical, intent(out) :: couples_out(0:n_couples)
 integer(bit_kind) :: det_tmp(N_int)
 integer(bit_kind) :: det_tmp_bis(N_int)
 BEGIN_DOC
 ! n_couples is the number of couples of orbitals to be checked
 ! couples(i,1) = first orbital of the ith couple
 ! couples(i,2) = second orbital of the ith couple
 ! returns the array couples_out
 ! couples_out(i) = .True. if det_in contains 
 !                   an orbital empty in the ith couple  AND
 !                   an orbital doubly occupied in the ith couple
 END_DOC
 integer :: i,j,k,l
 
 ! det_tmp tells you if the orbitals are occupied or not
 do j = 1, N_int
  det_tmp(j) = ior(det_in(j,1),det_in(j,2))
 enddo
 
 couples_out(0) = .False.
 do i = 1, n_couples
  do j = 1, N_int
   det_tmp_bis(j) = 0_bit_kind
  enddo
  call set_bit_to_integer(couples(i,1),det_tmp_bis,N_int) ! first orb
  call set_bit_to_integer(couples(i,2),det_tmp_bis,N_int) ! second orb
  ! det_tmp is zero except for the two orbitals of the couple
  integer :: i_count
  i_count = 0
  do j = 1, N_int
    i_count += popcnt(iand(det_tmp(j),det_tmp_bis(j)))   ! check if the two orbitals are both occupied 
  enddo
  if(i_count .ne. 1)then
   couples_out(i) = .False.  
   cycle  
  endif

  ! test if orbital there are two electrons or not
  i_count = 0
  do j = 1, N_int
   i_count += popcnt(iand(iand(det_in(j,1),det_in(j,2)),det_tmp_bis(j)))
  enddo
  if(i_count.ne.1)then
   couples_out(i) = .False.
  else  
   couples_out(i) = .True.
   couples_out(0) = .True.
  endif
 enddo
end

subroutine give_index_of_doubly_occ_in_active_space(det_in,doubly_occupied_array)
 implicit none
  use bitmasks
 integer(bit_kind), intent(in) :: det_in(N_int,2)
 logical, intent(out) :: doubly_occupied_array(n_act_orb)
 integer(bit_kind) :: det_tmp(N_int)
 integer(bit_kind) :: det_tmp_bis(N_int)
 BEGIN_DOC
 END_DOC
 integer :: i,j,k,l
 
 ! det_tmp tells you if the orbitals are occupied or not
 do j = 1, N_int
  det_tmp(j) = ior(det_in(j,1),det_in(j,2))
 enddo
 
 do i = 1, n_act_orb 
  do j = 1, N_int
   det_tmp_bis(j) = 0_bit_kind
  enddo
  i_bite = list_act(i)
  call set_bit_to_integer(i_bite,det_tmp_bis,N_int) ! act orb
  ! det_tmp is zero except for the orbital "ith" active orbital
  integer :: i_count,i_bite

  ! test if orbital there are two electrons or not
  i_count = 0
  do j = 1, N_int
   i_count += popcnt(iand(iand(det_in(j,1),det_in(j,2)),det_tmp_bis(j)))
  enddo
  if(i_count.ne.1)then
   doubly_occupied_array(i) = .False.
  else  
   doubly_occupied_array(i) = .True.
  endif
 enddo
end

subroutine doubly_occ_empty_in_couple_and_no_hund_elsewhere(det_in,n_couple_no_hund,couple_ion,couple_no_hund,is_ok)
 implicit none
  use bitmasks
 integer, intent(in) :: n_couple_no_hund,couple_ion(2),couple_no_hund(n_couple_no_hund,2)
 integer(bit_kind),intent(in) :: det_in(N_int,2)
 logical, intent(out) :: is_ok
 integer(bit_kind) :: det_tmp(N_int)
 integer(bit_kind) :: det_tmp_bis(N_int)
 BEGIN_DOC
 ! n_couples is the number of couples of orbitals to be checked
 ! couples(i,1) = first orbital of the ith couple
 ! couples(i,2) = second orbital of the ith couple
 ! returns the array couples_out
 ! couples_out(i) = .True. if det_in contains 
 !                   an orbital empty in the ith couple  AND
 !                   an orbital doubly occupied in the ith couple
 END_DOC
 integer :: i,j,k,l
 
 ! det_tmp tells you if the orbitals are occupied or not
 do j = 1, N_int
  det_tmp(j) = ior(det_in(j,1),det_in(j,2))
 enddo
 
  is_ok = .False.
  do j = 1, N_int
   det_tmp_bis(j) = 0_bit_kind
  enddo
  call set_bit_to_integer(couple_ion(1),det_tmp_bis,N_int) ! first orb
  call set_bit_to_integer(couple_ion(2),det_tmp_bis,N_int) ! second orb
  ! det_tmp is zero except for the two orbitals of the couple
  integer :: i_count
  i_count = 0
  do j = 1, N_int
    i_count += popcnt(iand(det_tmp(j),det_tmp_bis(j)))   ! check if the two orbitals are both occupied 
  enddo
  if(i_count .ne. 1)then
   is_ok = .False.  
   return
  endif

  ! test if orbital there are two electrons or not
  i_count = 0
  do j = 1, N_int
   i_count += popcnt(iand(iand(det_in(j,1),det_in(j,2)),det_tmp_bis(j)))
  enddo
  if(i_count.ne.1)then
   is_ok = .False.  
   return
  else  
   do i = 1, n_couple_no_hund
    do j = 1, N_int
     det_tmp_bis(j) = 0_bit_kind
    enddo
    call set_bit_to_integer(couple_no_hund (i,1),det_tmp_bis,N_int) ! first orb
    call set_bit_to_integer(couple_no_hund (i,2),det_tmp_bis,N_int) ! second orb
    ! det_tmp_bis is zero except for the two orbitals of the couple
    i_count = 0
    do j = 1, N_int
      i_count += popcnt(iand(det_tmp(j),det_tmp_bis(j)))   ! check if the two orbitals are both occupied 
    enddo
    if(i_count .ne. 2)then
     is_ok = .False.  
     return
    endif
    ! test if orbital there are one alpha and one beta
    integer :: i_count_alpha,i_count_beta
    i_count_alpha = 0
    i_count_beta = 0
    do j = 1, N_int
     i_count_alpha += popcnt(iand(det_in(j,1),det_tmp_bis(j)))
     i_count_beta  += popcnt(iand(det_in(j,2),det_tmp_bis(j)))
    enddo
    if(i_count_alpha==1.and.i_count_beta==1)then
     is_ok = .True.
    else
     is_ok = .False.
     return
    endif
   enddo
   is_ok = .True.
  endif
end


subroutine neutral_no_hund_in_couple(det_in,n_couples,couples,couples_out)
 implicit none
  use bitmasks
 integer, intent(in) :: n_couples,couples(n_couples,2)
 integer(bit_kind),intent(in) :: det_in(N_int,2)
 logical, intent(out) :: couples_out(0:n_couples)
 integer(bit_kind) :: det_tmp(N_int)
 integer(bit_kind) :: det_tmp_bis(N_int)
 BEGIN_DOC
 ! n_couples is the number of couples of orbitals to be checked
 ! couples(i,1) = first orbital of the ith couple
 ! couples(i,2) = second orbital of the ith couple
 ! returns the array couples_out
 ! couples_out(i) = .True. if det_in contains 
 !                   an orbital empty in the ith couple  AND
 !                   an orbital doubly occupied in the ith couple
 END_DOC
 integer :: i,j,k,l
 
 ! det_tmp tells you if the orbitals are occupied or not
 do j = 1, N_int
  det_tmp(j) = ior(det_in(j,1),det_in(j,2))
 enddo
 
 couples_out(0) = .True.
 do i = 1, n_couples
  do j = 1, N_int
   det_tmp_bis(j) = 0_bit_kind
  enddo
  call set_bit_to_integer(couples(i,1),det_tmp_bis,N_int) ! first orb
  call set_bit_to_integer(couples(i,2),det_tmp_bis,N_int) ! second orb
  ! det_tmp_bis is zero except for the two orbitals of the couple
  integer :: i_count
  i_count = 0
  do j = 1, N_int
    i_count += popcnt(iand(det_tmp(j),det_tmp_bis(j)))   ! check if the two orbitals are both occupied 
  enddo
  if(i_count .ne. 2)then
   couples_out(i) = .False.  
   cycle  
  endif

  ! test if orbital there are one alpha and one beta
  integer :: i_count_alpha,i_count_beta
  i_count_alpha = 0
  i_count_beta = 0
  do j = 1, N_int
   i_count_alpha += popcnt(iand(det_in(j,1),det_tmp_bis(j)))
   i_count_beta  += popcnt(iand(det_in(j,2),det_tmp_bis(j)))
  enddo
  if(i_count_alpha==1.and.i_count_beta==1)then
   couples_out(i) = .True.
  else
   couples_out(i) = .False.
  endif
 enddo
 do i = 1, n_couples
  if(.not.couples_out(i))then
   couples_out(0) = .False.
  endif
 enddo
end



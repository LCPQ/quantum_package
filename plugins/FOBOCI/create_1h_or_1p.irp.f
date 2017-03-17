subroutine create_restart_and_1h(i_hole)
 implicit none 
 use bitmasks
 integer, intent(in) :: i_hole
 integer(bit_kind) :: key_tmp(N_int,2)
 integer :: i,j,i_part_act,ispin,k,l,i_ok
 integer :: n_new_det
 integer(bit_kind), allocatable :: new_det(:,:,:)
 integer(bit_kind), allocatable :: old_psi_det(:,:,:)
 allocate (old_psi_det(N_int,2,n_det))
 do i = 1, N_det
  do j = 1, N_int
   old_psi_det(j,1,i) = psi_det(j,1,i)
   old_psi_det(j,2,i) = psi_det(j,2,i)
  enddo
 enddo
 n_new_det = 0
  do j = 1, n_act_orb
   i_part_act = list_act(j)  ! index of the particle in the active space
   do i = 1, N_det
    do ispin = 1,2
     do k = 1, N_int
      key_tmp(k,1) = psi_det(k,1,i)
      key_tmp(k,2) = psi_det(k,2,i)
     enddo
     call do_mono_excitation(key_tmp,i_hole,i_part_act,ispin,i_ok)
     if(i_ok .ne. 1)cycle
     n_new_det +=1
    enddo
   enddo
  enddo
 

 integer :: N_det_old
 N_det_old = N_det

 logical, allocatable :: duplicate(:)
 allocate (new_det(N_int,2,n_new_det),duplicate(n_new_det))

 n_new_det = 0
  do j = 1, n_act_orb
   i_part_act = list_act(j)  ! index of the particle in the active space
   do i = 1, N_det_old
    do ispin = 1,2
     do k = 1, N_int
      key_tmp(k,1) = psi_det(k,1,i)
      key_tmp(k,2) = psi_det(k,2,i)
     enddo
     call do_mono_excitation(key_tmp,i_hole,i_part_act,ispin,i_ok)
     if(i_ok .ne. 1)cycle
     n_new_det +=1
     do k = 1, N_int
      new_det(k,1,n_new_det) = key_tmp(k,1) 
      new_det(k,2,n_new_det) = key_tmp(k,2) 
     enddo
    enddo
   enddo
  enddo

 integer :: i_test
 duplicate = .False. 
 do i = 1, n_new_det
  if(duplicate(i))cycle
  do j = i+1, n_new_det
   i_test = 0
   do ispin =1 ,2
    do k = 1, N_int
     i_test += popcnt(xor(new_det(k,ispin,i),new_det(k,ispin,j)))
    enddo
   enddo
   if(i_test.eq.0)then
    duplicate(j) = .True.
   endif
  enddo
 enddo

 integer :: n_new_det_unique
 n_new_det_unique = 0
 print*, 'uniq det'
 do i = 1, n_new_det
  if(.not.duplicate(i))then
   n_new_det_unique += 1
  endif
 enddo
 print*, n_new_det_unique
 N_det += n_new_det_unique
 if (psi_det_size < N_det) then
   psi_det_size = N_det
   TOUCH psi_det_size
 endif
 do i = 1, n_new_det_unique
  do ispin = 1, 2
   do k = 1, N_int
    psi_det(k,ispin,N_det_old+i) = new_det(k,ispin,i)
   enddo
  enddo
  psi_coef(N_det_old+i,:) = 0.d0
 enddo


  SOFT_TOUCH N_det psi_det psi_coef
  deallocate (new_det,duplicate)
end

subroutine create_restart_and_1p(i_particle)
 implicit none 
 integer, intent(in) :: i_particle
 use bitmasks
 integer(bit_kind) :: key_tmp(N_int,2)
 integer :: i,j,i_hole_act,ispin,k,l,i_ok
 integer :: n_new_det
 integer(bit_kind), allocatable :: new_det(:,:,:)
 integer(bit_kind), allocatable :: old_psi_det(:,:,:)
 allocate (old_psi_det(N_int,2,n_det))
 do i = 1, N_det
  do j = 1, N_int
   old_psi_det(j,1,i) = psi_det(j,1,i)
   old_psi_det(j,2,i) = psi_det(j,2,i)
  enddo
 enddo
 n_new_det = 0
  do j = 1, n_act_orb
   i_hole_act = list_act(j)  ! index of the particle in the active space
   do i = 1, N_det
    do ispin = 1,2
     do k = 1, N_int
      key_tmp(k,1) = psi_det(k,1,i)
      key_tmp(k,2) = psi_det(k,2,i)
     enddo
     call do_mono_excitation(key_tmp,i_hole_act,i_particle,ispin,i_ok)
     if(i_ok .ne. 1)cycle
     n_new_det +=1
    enddo
   enddo
  enddo

 integer :: N_det_old
 N_det_old = N_det
 logical, allocatable :: duplicate(:)
 allocate (new_det(N_int,2,n_new_det),duplicate(n_new_det))

 n_new_det = 0
  do j = 1, n_act_orb
   i_hole_act = list_act(j)  ! index of the particle in the active space
   do i = 1, N_det_old
    do ispin = 1,2
     do k = 1, N_int
      key_tmp(k,1) = psi_det(k,1,i)
      key_tmp(k,2) = psi_det(k,2,i)
     enddo
     call do_mono_excitation(key_tmp,i_hole_act,i_particle,ispin,i_ok)
     if(i_ok .ne. 1)cycle
     n_new_det +=1
     do k = 1, N_int
      new_det(k,1,n_new_det) = key_tmp(k,1) 
      new_Det(k,2,n_new_det) = key_tmp(k,2) 
     enddo
    enddo
   enddo
  enddo

 integer :: i_test
 duplicate = .False. 
 do i = 1, n_new_det
  if(duplicate(i))cycle
  call debug_det(new_det(1,1,i),N_int)
  do j = i+1, n_new_det
   i_test = 0
   call debug_det(new_det(1,1,j),N_int)
   do ispin =1 ,2
    do k = 1, N_int
     i_test += popcnt(xor(new_det(k,ispin,i),new_det(k,ispin,j)))
    enddo
   enddo
   if(i_test.eq.0)then
    duplicate(j) = .True.
   endif
  enddo
 enddo

 integer :: n_new_det_unique
 n_new_det_unique = 0
 print*, 'uniq det'
 do i = 1, n_new_det
  if(.not.duplicate(i))then
   n_new_det_unique += 1
  endif
 enddo
 print*, n_new_det_unique

 N_det += n_new_det_unique
 if (psi_det_size < N_det) then
   psi_det_size = N_det
   TOUCH psi_det_size
 endif
 do i = 1, n_new_det_unique
  do ispin = 1, 2
   do k = 1, N_int
    psi_det(k,ispin,N_det_old+i) = new_det(k,ispin,i)
   enddo
  enddo
  psi_coef(N_det_old+i,:) = 0.d0
 enddo

  SOFT_TOUCH N_det psi_det psi_coef
  deallocate (new_det,duplicate)

end

subroutine create_restart_1h_1p(i_hole,i_part)
 implicit none 
 use bitmasks
 integer, intent(in) :: i_hole
 integer, intent(in) :: i_part

 integer :: i,j,i_part_act,ispin,k,l,i_ok
 integer(bit_kind) :: key_tmp(N_int,2)
 integer :: n_new_det
 integer(bit_kind), allocatable :: new_det(:,:,:)
 integer(bit_kind), allocatable :: old_psi_det(:,:,:)

 allocate (old_psi_det(N_int,2,n_det))
 do i = 1, N_det
  do j = 1, N_int
   old_psi_det(j,1,i) = psi_det(j,1,i)
   old_psi_det(j,2,i) = psi_det(j,2,i)
  enddo
 enddo
 n_new_det = 0
 i_part_act = i_part  ! index of the particle in the active space
 do i = 1, N_det
  do ispin = 1,2
   do k = 1, N_int
    key_tmp(k,1) = psi_det(k,1,i)
    key_tmp(k,2) = psi_det(k,2,i)
   enddo
   call do_mono_excitation(key_tmp,i_hole,i_part_act,ispin,i_ok)
   if(i_ok .ne. 1)cycle
   n_new_det +=1
  enddo
 enddo

 integer :: N_det_old
 N_det_old = N_det
 N_det += n_new_det
 allocate (new_det(N_int,2,n_new_det))
 if (psi_det_size < N_det) then
   psi_det_size = N_det
   TOUCH psi_det_size
 endif
 do i = 1, N_det_old 
  do k = 1, N_int
   psi_det(k,1,i) = old_psi_det(k,1,i)
   psi_det(k,2,i) = old_psi_det(k,2,i)
  enddo
 enddo

 n_new_det = 0
 i_part_act = i_part  ! index of the particle in the active space
 do i = 1, N_det_old
  do ispin = 1,2
   do k = 1, N_int
    key_tmp(k,1) = psi_det(k,1,i)
    key_tmp(k,2) = psi_det(k,2,i)
   enddo
   call do_mono_excitation(key_tmp,i_hole,i_part_act,ispin,i_ok)
   if(i_ok .ne. 1)cycle
   n_new_det +=1
   do k = 1, N_int
    psi_det(k,1,n_det_old+n_new_det) = key_tmp(k,1) 
    psi_det(k,2,n_det_old+n_new_det) = key_tmp(k,2) 
   enddo
   psi_coef(n_det_old+n_new_det,:) = 0.d0
  enddo
 enddo

  SOFT_TOUCH N_det psi_det psi_coef
  logical :: found_duplicates
  if(n_act_orb.gt.1)then
  call remove_duplicates_in_psi_det(found_duplicates)
  endif

end

use bitmasks

! BEGIN_PROVIDER [ double precision, integral8, (mo_tot_num,  mo_tot_num, mo_tot_num, mo_tot_num) ]
!   use bitmasks
!   implicit none
!   
!   integer :: h1, h2
!   
!   integral8 = 0d0
!   do h1=1, mo_tot_num
!     do h2=1, mo_tot_num
!       call get_mo_bielec_integrals_ij(h1, h2 ,mo_tot_num,integral8(1,1,h1,h2),mo_integrals_map)
!     end do
!   end do
! END_PROVIDER


double precision function integral8(i,j,k,l)
  implicit none
  
  integer, intent(in) :: i,j,k,l
  double precision, external :: get_mo_bielec_integral
  
  integral8 = get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
end function


BEGIN_PROVIDER [ integer(1), psi_phasemask, (N_int*bit_kind_size, 2, N_det)]
  use bitmasks
  implicit none
  
  integer :: i
  do i=1, N_det
    call get_mask_phase(psi_det_sorted(1,1,i), psi_phasemask(1,1,i))
  end do
END_PROVIDER


subroutine assert(cond, msg)
  character(*), intent(in) :: msg
  logical, intent(in) :: cond
  
  if(.not. cond) then
    print *, "assert fail: "//msg
    stop
  end if
end subroutine


subroutine get_mask_phase(det, phasemask)
  use bitmasks
  implicit none

  integer(bit_kind), intent(in) :: det(N_int, 2)
  integer(1), intent(out) :: phasemask(N_int*bit_kind_size, 2)
  integer :: s, ni, i
  logical :: change

!   phasemask = 0_8
  phasemask = 0_1
  do s=1,2
    change = .false.
    do ni=1,N_int
      do i=0,bit_kind_size-1
        if(BTEST(det(ni, s), i)) change = .not. change
!         if(change) phasemask(ni, s) = ibset(phasemask(ni, s), i)
          if(change) phasemask((ni-1)*bit_kind_size + i + 1, s) = 1_1
      end do
    end do
  end do
end subroutine


subroutine select_connected(i_generator,E0,pt2,b)
  use bitmasks
  use selection_types
  implicit none
  integer, intent(in)            :: i_generator
  type(selection_buffer), intent(inout) :: b
  double precision, intent(inout)  :: pt2(N_states)
  integer :: k,l
  double precision, intent(in)   :: E0(N_states)

  integer(bit_kind)              :: hole_mask(N_int,2), particle_mask(N_int,2)
  double precision               :: fock_diag_tmp(2,mo_tot_num+1)
  
  call build_fock_tmp(fock_diag_tmp,psi_det_generators(1,1,i_generator),N_int)

  do l=1,N_generators_bitmask
    do k=1,N_int
      hole_mask(k,1) = iand(generators_bitmask(k,1,s_hole,l), psi_det_generators(k,1,i_generator))
      hole_mask(k,2) = iand(generators_bitmask(k,2,s_hole,l), psi_det_generators(k,2,i_generator))
      particle_mask(k,1) = iand(generators_bitmask(k,1,s_part,l), not(psi_det_generators(k,1,i_generator)) )
      particle_mask(k,2) = iand(generators_bitmask(k,2,s_part,l), not(psi_det_generators(k,2,i_generator)) )

!      hole_mask(k,1) = ior(generators_bitmask(k,1,s_hole,l), generators_bitmask(k,1,s_part,l))
!      hole_mask(k,2) = ior(generators_bitmask(k,2,s_hole,l), generators_bitmask(k,2,s_part,l))
!      particle_mask(k,1) = hole_mask(k,1)
!      particle_mask(k,2) = hole_mask(k,2)
    enddo
    print *, 'det'
    call debug_det(psi_det_generators(1,1,i_generator),N_int)
    print *, 'hole'
    call debug_det(hole_mask,N_int)
    print *, 'particle_mask'
    call debug_det(particle_mask,N_int)
    print *,  ''
    pause
    call select_doubles(i_generator,hole_mask,particle_mask,fock_diag_tmp,E0,pt2,b)
    call select_singles(i_generator,hole_mask,particle_mask,fock_diag_tmp,E0,pt2,b)
  enddo
end subroutine


subroutine spot_occupied(mask, bannedOrb)
  use bitmasks
  implicit none

  integer(bit_kind),intent(in) :: mask(N_int)
  logical, intent(inout) :: bannedOrb(mo_tot_num)
  integer :: i, ne, list(mo_tot_num)

  call bitstring_to_list(mask, list, ne, N_int)
  do i=1, ne
    bannedOrb(list(i)) = .true.
  end do
end subroutine


double precision function get_phase_bi(phasemask, s1, s2, h1, p1, h2, p2)
  use bitmasks
  implicit none

  integer(1), intent(in) :: phasemask(N_int*bit_kind_size, 2)
  integer, intent(in) :: s1, s2, h1, h2, p1, p2
  logical :: change
  integer(1) :: np
  double precision, parameter :: res(0:1) = (/1d0, -1d0/)

!   call assert(s1 /= s2 .or. (h1 <= h2 .and. p1 <= p2), irp_here)
!   np = 0
!   change = btest(phasemask(1+ishft(h1, -6), s1), iand(h1-1, 63))
!   change = xor(change, btest(phasemask(1+ishft(p1, -6), s1), iand(p1-1, 63)))
!   if(xor(change, p1 < h1)) np = 1
! 
!   change = btest(phasemask(1+ishft(h2, -6), s2), iand(h2-1, 63))
!   change = xor(change, btest(phasemask(1+ishft(p2, -6), s2), iand(p2-1, 63)))
!   if(xor(change, p2 < h2)) np = np + 1
  
  np = phasemask(h1,s1) + phasemask(p1,s1) + phasemask(h2,s2) + phasemask(p2,s2)
  if(p1 < h1) np = np + 1_1
  if(p2 < h2) np = np + 1_1
  
  if(s1 == s2 .and. max(h1, p1) > min(h2, p2)) np = np + 1_1
  get_phase_bi = res(iand(np,1_1))
end subroutine




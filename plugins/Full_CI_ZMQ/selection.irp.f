use bitmasks


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

  phasemask = 0_1
  do s=1,2
    change = .false.
    do ni=1,N_int
      do i=0,bit_kind_size-1
        if(BTEST(det(ni, s), i)) change = .not. change
          if(change) phasemask((ni-1)*bit_kind_size + i + 1, s) = 1_1
      end do
    end do
  end do
end subroutine


subroutine select_connected(i_generator,E0,pt2,b,subset)
  use bitmasks
  use selection_types
  implicit none
  integer, intent(in)            :: i_generator, subset
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

    enddo
    call select_doubles(i_generator,hole_mask,particle_mask,fock_diag_tmp,E0,pt2,b,subset)
  enddo
end subroutine


double precision function get_phase_bi(phasemask, s1, s2, h1, p1, h2, p2)
  use bitmasks
  implicit none

  integer(1), intent(in) :: phasemask(N_int*bit_kind_size, 2)
  integer, intent(in) :: s1, s2, h1, h2, p1, p2
  logical :: change
  integer(1) :: np
  double precision, parameter :: res(0:1) = (/1d0, -1d0/)

  np = phasemask(h1,s1) + phasemask(p1,s1) + phasemask(h2,s2) + phasemask(p2,s2)
  if(p1 < h1) np = np + 1_1
  if(p2 < h2) np = np + 1_1
  
  if(s1 == s2 .and. max(h1, p1) > min(h2, p2)) np = np + 1_1
  get_phase_bi = res(iand(np,1_1))
end subroutine




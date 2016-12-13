
subroutine give_active_part_determinant(det_in,det_out)
 implicit none
 use bitmasks
 integer(bit_kind),intent(in)  :: det_in(N_int,2)
 integer(bit_kind),intent(out) :: det_out(N_int,2)
 integer :: i
 do i = 1,N_int
  det_out(i,1) = iand(det_in(i,1),cas_bitmask(i,1,1))
  det_out(i,2) = iand(det_in(i,2),cas_bitmask(i,1,1))
 enddo
end

subroutine give_core_inactive_part_determinant(det_in,det_out)
 implicit none
 use bitmasks
 integer(bit_kind),intent(in)  :: det_in(N_int,2)
 integer(bit_kind),intent(out) :: det_out(N_int,2)
 integer :: i
 do i = 1,N_int
  det_out(i,1) = iand(det_in(i,1),reunion_of_core_inact_bitmask(i,1))
  det_out(i,2) = iand(det_in(i,2),reunion_of_core_inact_bitmask(i,1))
 enddo
end

subroutine give_virt_part_determinant(det_in,det_out)
 implicit none
 use bitmasks
 integer(bit_kind),intent(in)  :: det_in(N_int,2)
 integer(bit_kind),intent(out) :: det_out(N_int,2)
 integer :: i
 do i = 1,N_int
  det_out(i,1) = iand(det_in(i,1),virt_bitmask(i,1))
  det_out(i,2) = iand(det_in(i,2),virt_bitmask(i,1))
 enddo
end

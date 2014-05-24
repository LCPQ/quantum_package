BEGIN_SHELL [ /bin/bash ]
./h_apply.py
END_SHELL


subroutine fill_H_apply_buffer(n_selected,det_buffer,Nint,iproc)
  use bitmasks
  implicit none
  BEGIN_DOC
  !  Fill the H_apply buffer with determiants for CISD
  END_DOC
  
  integer, intent(in)            :: n_selected, Nint, iproc
  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,n_selected)
  integer                        :: i,j,k
  integer                        :: new_size
  PROVIDE H_apply_buffer_allocated
  new_size = H_apply_buffer(iproc)%N_det + n_selected
  if (new_size > H_apply_buffer(iproc)%sze) then
    call resize_h_apply_buffer(max(2*H_apply_buffer(iproc)%sze,new_size),iproc)
  endif
  do i=1,H_apply_buffer(iproc)%N_det
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,1,i)) )== elec_alpha_num)
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,2,i))) == elec_beta_num)
  enddo
  do i=1,n_selected
    do j=1,N_int
      H_apply_buffer(iproc)%det(j,1,i+H_apply_buffer(iproc)%N_det) = det_buffer(j,1,i)
      H_apply_buffer(iproc)%det(j,2,i+H_apply_buffer(iproc)%N_det) = det_buffer(j,2,i)
    enddo
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,1,i+H_apply_buffer(iproc)%N_det)) )== elec_alpha_num)
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,2,i+H_apply_buffer(iproc)%N_det))) == elec_beta_num)
    H_apply_buffer(iproc)%coef(i,:) = 0.d0
  enddo
  H_apply_buffer(iproc)%N_det = new_size
  do i=1,H_apply_buffer(iproc)%N_det
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,1,i)) )== elec_alpha_num)
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,2,i))) == elec_beta_num)
  enddo
end


subroutine H_apply_cisd
  implicit none
  BEGIN_DOC
  ! Calls H_apply on the HF determinant and selects all connected single and double
  ! excitations (of the same symmetry).
  END_DOC
  
  integer(bit_kind)              :: hole_mask(N_int,2)
  integer(bit_kind)              :: particle_mask(N_int,2)
  hole_mask(:,1) = HF_bitmask(:,1)
  hole_mask(:,2) = HF_bitmask(:,2)
  particle_mask(:,1) = iand(not(HF_bitmask(:,1)),full_ijkl_bitmask(:,1))
  particle_mask(:,2) = iand(not(HF_bitmask(:,2)),full_ijkl_bitmask(:,2))

  PROVIDE N_det_generators

  call H_apply_cisd_OpenMP_monoexc(HF_bitmask,                              &
      hole_mask, particle_mask)
  call H_apply_cisd_OpenMP_diexc(HF_bitmask,                                &
      hole_mask, particle_mask,                                      &
      hole_mask, particle_mask )

  call copy_h_apply_buffer_to_wf
end



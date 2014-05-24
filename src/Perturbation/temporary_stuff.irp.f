use bitmasks
BEGIN_SHELL [ /bin/bash ]
./h_apply.py
END_SHELL

BEGIN_PROVIDER [ double precision, reference_energy, (N_states) ]
 implicit none
 BEGIN_DOC
 ! Reference energy
 END_DOC
 integer :: i

 call diagonalize(psi_det,psi_coef,reference_energy,size(psi_coef,1),N_det,N_states,N_int,output_CISD)
 SOFT_TOUCH psi_det psi_coef 
END_PROVIDER


subroutine diagonalize(psi_det_in,psi_coef_in,eigvalues_out,psi_coef_dim,Ndet,N_st,Nint,iunit)
  use bitmasks
  implicit none
  integer,intent(in) :: Nint,Ndet,N_st,psi_coef_dim, iunit
  integer(bit_kind), intent(in) :: psi_det_in(Nint,2,Ndet)
  double precision, intent(in) :: psi_coef_in(Ndet,N_st)
  double precision, intent(out) :: eigvalues_out(N_st)

  ASSERT (Nint == N_int)

  call davidson_diag(psi_det_in,psi_coef_in,eigvalues_out,psi_coef_dim,Ndet,N_st,Nint,iunit)
end


BEGIN_PROVIDER [ integer, psi_ref_size ]
 implicit none
 psi_ref_size = psi_det_size
END_PROVIDER
BEGIN_PROVIDER [ integer, N_det_ref]
 implicit none
  N_det_ref = N_det
END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), psi_ref, (N_int,2,psi_ref_size) ]
&BEGIN_PROVIDER [ double precision, psi_ref_coef, (psi_ref_size,N_states) ]
  implicit none
  BEGIN_DOC
  ! On what we apply <i|H|psi> for perturbation. If selection, it may be 0.9 of the norm.
  END_DOC
  integer                        :: i,k
  
  do i=1,N_det_ref
    do k=1,N_int
      psi_ref(k,1,i) = psi_det(k,1,i)
      psi_ref(k,2,i) = psi_det(k,2,i)
    enddo
  enddo
  do k=1,N_states
    do i=1,N_det_ref
      psi_ref_coef(i,k) = psi_coef(i,k)
    enddo
  enddo
END_PROVIDER


subroutine H_apply_cisd_pt2(pt2, norm_pert, H_pert_diag,  N_st)
  implicit none
  BEGIN_DOC
  ! Calls H_apply on the HF determinant and selects all connected single and double
  ! excitations (of the same symmetry).
  END_DOC
  
  integer, intent(in)            :: N_st
  double precision, intent(inout):: pt2(N_st)
  double precision, intent(inout):: norm_pert(N_st)
  double precision, intent(inout):: H_pert_diag
  integer(bit_kind)              :: hole_mask(N_int,2)
  integer(bit_kind)              :: particle_mask(N_int,2)
  hole_mask(:,1) = HF_bitmask(:,1)
  hole_mask(:,2) = HF_bitmask(:,2)
  particle_mask(:,1) = iand(not(HF_bitmask(:,1)),full_ijkl_bitmask(:,1))
  particle_mask(:,2) = iand(not(HF_bitmask(:,2)),full_ijkl_bitmask(:,2))


  PROVIDE reference_energy N_det_generators key_pattern_not_in_ref
  PROVIDE mo_bielec_integrals_in_map
  PROVIDE H_apply_buffer_allocated
  pt2 = 0.d0
  call H_apply_cisd_pt2_OpenMP_monoexc(HF_bitmask,                       &
      hole_mask, particle_mask,                                      &
      pt2,norm_pert,H_pert_diag,N_st,N_int )
  call H_apply_cisd_pt2_OpenMP_diexc(HF_bitmask,                         &
      hole_mask, particle_mask,                                      &
      hole_mask, particle_mask,                                      &
      pt2,norm_pert,H_pert_diag,N_st,N_int )
  
end


subroutine H_apply_cisd_selection(pt2, norm_pert, H_pert_diag,  N_st)
  implicit none
  BEGIN_DOC
  ! Calls H_apply on the HF determinant and selects all connected single and double
  ! excitations (of the same symmetry).
  END_DOC
  
  integer, intent(in)            :: N_st
  double precision, intent(inout):: pt2(N_st)
  double precision, intent(inout):: norm_pert(N_st)
  double precision, intent(inout):: H_pert_diag
  integer(bit_kind)              :: hole_mask(N_int,2)
  integer(bit_kind)              :: particle_mask(N_int,2)
  hole_mask(:,1) = HF_bitmask(:,1)
  hole_mask(:,2) = HF_bitmask(:,2)
  particle_mask(:,1) = iand(not(HF_bitmask(:,1)),full_ijkl_bitmask(:,1))
  particle_mask(:,2) = iand(not(HF_bitmask(:,2)),full_ijkl_bitmask(:,2))


  PROVIDE reference_energy N_det_generators key_pattern_not_in_ref
  PROVIDE mo_bielec_integrals_in_map selection_criterion
  PROVIDE H_apply_buffer_allocated
  pt2 = 0.d0
  norm_pert = 0.d0
  H_pert_diag = 0.d0
  call H_apply_cisd_selection_OpenMP_monoexc(HF_bitmask,                       &
      hole_mask, particle_mask,                                      &
      pt2,norm_pert,H_pert_diag,N_st,N_int )
  call H_apply_cisd_selection_OpenMP_diexc(HF_bitmask,                         &
      hole_mask, particle_mask,                                      &
      hole_mask, particle_mask,                                      &
      pt2,norm_pert,H_pert_diag,N_st,N_int )
  call copy_h_apply_buffer_to_wf  
  selection_criterion_min = selection_criterion_min*0.1d0
  selection_criterion = selection_criterion_min
end



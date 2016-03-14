subroutine all_single_split(psi_det_generators_input,psi_coef_generators_input,Ndet_generators_input,dressing_matrix)
 implicit none
 use bitmasks
 integer, intent(in) :: Ndet_generators_input
 integer(bit_kind), intent(in) :: psi_det_generators_input(N_int,2,Ndet_generators_input)
 double precision, intent(inout) :: dressing_matrix(Ndet_generators_input,Ndet_generators_input)
 double precision, intent(in)    :: psi_coef_generators_input(ndet_generators_input,n_states)
 integer :: i,i_hole
 n_det_max_jacobi = 50
 soft_touch n_det_max_jacobi
 do i = 1, n_inact_orb
   i_hole = list_inact(i)
   print*,''
   print*,'Doing all the single excitations from the orbital '
   print*,i_hole
   print*,''
   print*,''
   threshold_davidson = 1.d-4
   soft_touch threshold_davidson davidson_criterion
   call modify_bitmasks_for_hole(i_hole)
   call set_bitmask_particl_as_input(reunion_of_bitmask)
   call set_generators_as_input_psi(ndet_generators_input,psi_det_generators_input,psi_coef_generators_input)
   call set_psi_det_as_input_psi(ndet_generators_input,psi_det_generators_input,psi_coef_generators_input)
   call all_single
   threshold_davidson = 1.d-10
   soft_touch threshold_davidson davidson_criterion
   call diagonalize_CI
   call provide_matrix_dressing(dressing_matrix,ndet_generators_input,psi_det_generators_input)
 enddo
 n_det_max_jacobi = 1000
 soft_touch n_det_max_jacobi
end


subroutine all_single_for_1h(dressing_matrix_1h1p,dressing_matrix_2h1p)
 implicit none
 use bitmasks
 double precision, intent(inout) :: dressing_matrix_1h1p(N_det_generators,N_det_generators)
 double precision, intent(inout) :: dressing_matrix_2h1p(N_det_generators,N_det_generators)
 integer :: i,i_hole
 n_det_max_jacobi = 50
 soft_touch n_det_max_jacobi

 integer :: n_det_1h1p,n_det_2h1p
 integer(bit_kind), allocatable :: psi_ref_out(:,:,:)
 integer(bit_kind), allocatable :: psi_1h1p(:,:,:)
 integer(bit_kind), allocatable :: psi_2h1p(:,:,:)
 double precision, allocatable :: psi_ref_coef_out(:,:)
 double precision, allocatable :: psi_coef_1h1p(:,:)
 double precision, allocatable :: psi_coef_2h1p(:,:)
 call all_single_no_1h_or_1p
 
 threshold_davidson = 1.d-12
 soft_touch threshold_davidson davidson_criterion
 call diagonalize_CI
 call give_n_1h1p_and_n_2h1p_in_psi_det(n_det_1h1p,n_det_2h1p)
 allocate(psi_ref_out(N_int,2,N_det_generators))
 allocate(psi_1h1p(N_int,2,n_det_1h1p))
 allocate(psi_2h1p(N_int,2,n_det_2h1p))
 allocate(psi_ref_coef_out(N_det_generators,N_states))
 allocate(psi_coef_1h1p(n_det_1h1p,N_states))
 allocate(psi_coef_2h1p(n_det_2h1p,N_states))
 call split_wf_generators_and_1h1p_and_2h1p(n_det_1h1p,n_det_2h1p,psi_ref_out,psi_ref_coef_out,psi_1h1p,psi_coef_1h1p,psi_2h1p,psi_coef_2h1p)
 call provide_matrix_dressing_general(dressing_matrix_1h1p,psi_ref_out,psi_ref_coef_out,N_det_generators, &
                                                        psi_1h1p,psi_coef_1h1p,n_det_1h1p)
 call provide_matrix_dressing_general(dressing_matrix_2h1p,psi_ref_out,psi_ref_coef_out,N_det_generators, &
                                                        psi_2h1p,psi_coef_2h1p,n_det_2h1p)

 deallocate(psi_ref_out)
 deallocate(psi_1h1p)
 deallocate(psi_2h1p)
 deallocate(psi_ref_coef_out)
 deallocate(psi_coef_1h1p)
 deallocate(psi_coef_2h1p)

end




subroutine all_single_split_for_1h(dressing_matrix_1h1p,dressing_matrix_2h1p)
 implicit none
 use bitmasks
 double precision, intent(inout) :: dressing_matrix_1h1p(N_det_generators,N_det_generators)
 double precision, intent(inout) :: dressing_matrix_2h1p(N_det_generators,N_det_generators)
 integer :: i,i_hole
 n_det_max_jacobi = 50
 soft_touch n_det_max_jacobi

 integer :: n_det_1h1p,n_det_2h1p
 integer(bit_kind), allocatable :: psi_ref_out(:,:,:)
 integer(bit_kind), allocatable :: psi_1h1p(:,:,:)
 integer(bit_kind), allocatable :: psi_2h1p(:,:,:)
 double precision, allocatable :: psi_ref_coef_out(:,:)
 double precision, allocatable :: psi_coef_1h1p(:,:)
 double precision, allocatable :: psi_coef_2h1p(:,:)
 do i = 1, n_inact_orb
   i_hole = list_inact(i)
   print*,''
   print*,'Doing all the single excitations from the orbital '
   print*,i_hole
   print*,''
   print*,''
   threshold_davidson = 1.d-4
   soft_touch threshold_davidson davidson_criterion
   selection_criterion_factor = 1.d-4
   soft_touch selection_criterion_factor selection_criterion selection_criterion_min
   call modify_bitmasks_for_hole(i_hole)
   call set_bitmask_particl_as_input(reunion_of_bitmask)
   call set_generators_as_input_psi(n_det_generators,psi_det_generators,psi_coef_generators)
   call set_psi_det_as_input_psi(n_det_generators,psi_det_generators,psi_coef_generators)
   call all_single_no_1h_or_1p
   threshold_davidson = 1.d-10
   soft_touch threshold_davidson davidson_criterion
   call diagonalize_CI
   call give_n_1h1p_and_n_2h1p_in_psi_det(n_det_1h1p,n_det_2h1p)
   allocate(psi_ref_out(N_int,2,N_det_generators))
   allocate(psi_1h1p(N_int,2,n_det_1h1p))
   allocate(psi_2h1p(N_int,2,n_det_2h1p))
   allocate(psi_ref_coef_out(N_det_generators,N_states))
   allocate(psi_coef_1h1p(n_det_1h1p,N_states))
   allocate(psi_coef_2h1p(n_det_2h1p,N_states))
   call split_wf_generators_and_1h1p_and_2h1p(n_det_1h1p,n_det_2h1p,psi_ref_out,psi_ref_coef_out,psi_1h1p,psi_coef_1h1p,psi_2h1p,psi_coef_2h1p)
   call provide_matrix_dressing_general(dressing_matrix_1h1p,psi_ref_out,psi_ref_coef_out,N_det_generators, &
                                                          psi_1h1p,psi_coef_1h1p,n_det_1h1p)
   call provide_matrix_dressing_general(dressing_matrix_2h1p,psi_ref_out,psi_ref_coef_out,N_det_generators, &
                                                        psi_2h1p,psi_coef_2h1p,n_det_2h1p)

   deallocate(psi_ref_out)
   deallocate(psi_1h1p)
   deallocate(psi_2h1p)
   deallocate(psi_ref_coef_out)
   deallocate(psi_coef_1h1p)
   deallocate(psi_coef_2h1p)
 enddo
 n_det_max_jacobi = 1000
 soft_touch n_det_max_jacobi
end


subroutine all_single_split_for_1p(dressing_matrix_1h1p,dressing_matrix_1h2p)
 implicit none
 use bitmasks
 double precision, intent(inout) :: dressing_matrix_1h1p(N_det_generators,N_det_generators)
 double precision, intent(inout) :: dressing_matrix_1h2p(N_det_generators,N_det_generators)
 integer :: i,i_hole
 n_det_max_jacobi = 50
 soft_touch n_det_max_jacobi

 integer :: n_det_1h1p,n_det_1h2p
 integer(bit_kind), allocatable :: psi_ref_out(:,:,:)
 integer(bit_kind), allocatable :: psi_1h1p(:,:,:)
 integer(bit_kind), allocatable :: psi_1h2p(:,:,:)
 double precision, allocatable :: psi_ref_coef_out(:,:)
 double precision, allocatable :: psi_coef_1h1p(:,:)
 double precision, allocatable :: psi_coef_1h2p(:,:)
 do i = 1, n_inact_orb
   i_hole = list_inact(i)
   print*,''
   print*,'Doing all the single excitations from the orbital '
   print*,i_hole
   print*,''
   print*,''
   threshold_davidson = 1.d-4
   soft_touch threshold_davidson davidson_criterion
   selection_criterion_factor = 1.d-4
   soft_touch selection_criterion_factor selection_criterion selection_criterion_min
   call modify_bitmasks_for_hole(i_hole)
   call set_bitmask_particl_as_input(reunion_of_bitmask)
   call set_generators_as_input_psi(n_det_generators,psi_det_generators,psi_coef_generators)
   call set_psi_det_as_input_psi(n_det_generators,psi_det_generators,psi_coef_generators)
   call all_single_no_1h_or_1p
   threshold_davidson = 1.d-10
   soft_touch threshold_davidson davidson_criterion
   call diagonalize_CI
   call give_n_1h1p_and_n_1h2p_in_psi_det(n_det_1h1p,n_det_1h2p)
   allocate(psi_ref_out(N_int,2,N_det_generators))
   allocate(psi_1h1p(N_int,2,n_det_1h1p))
   allocate(psi_1h2p(N_int,2,n_det_1h2p))
   allocate(psi_ref_coef_out(N_det_generators,N_states))
   allocate(psi_coef_1h1p(n_det_1h1p,N_states))
   allocate(psi_coef_1h2p(n_det_1h2p,N_states))
   call split_wf_generators_and_1h1p_and_1h2p(n_det_1h1p,n_det_1h2p,psi_ref_out,psi_ref_coef_out,psi_1h1p,psi_coef_1h1p,psi_1h2p,psi_coef_1h2p)
   call provide_matrix_dressing_general(dressing_matrix_1h1p,psi_ref_out,psi_ref_coef_out,N_det_generators, &
                                                          psi_1h1p,psi_coef_1h1p,n_det_1h1p)
   call provide_matrix_dressing_general(dressing_matrix_1h2p,psi_ref_out,psi_ref_coef_out,N_det_generators, &
                                                          psi_1h2p,psi_coef_1h2p,n_det_1h2p)

   deallocate(psi_ref_out)
   deallocate(psi_1h1p)
   deallocate(psi_1h2p)
   deallocate(psi_ref_coef_out)
   deallocate(psi_coef_1h1p)
   deallocate(psi_coef_1h2p)
 enddo
 n_det_max_jacobi = 1000
 soft_touch n_det_max_jacobi
end

subroutine all_single_for_1p(dressing_matrix_1h1p,dressing_matrix_1h2p)
 implicit none
 use bitmasks
 double precision, intent(inout) :: dressing_matrix_1h1p(N_det_generators,N_det_generators)
 double precision, intent(inout) :: dressing_matrix_1h2p(N_det_generators,N_det_generators)
 integer :: i,i_hole
 n_det_max_jacobi = 50
 soft_touch n_det_max_jacobi

 integer :: n_det_1h1p,n_det_1h2p
 integer(bit_kind), allocatable :: psi_ref_out(:,:,:)
 integer(bit_kind), allocatable :: psi_1h1p(:,:,:)
 integer(bit_kind), allocatable :: psi_1h2p(:,:,:)
 double precision, allocatable :: psi_ref_coef_out(:,:)
 double precision, allocatable :: psi_coef_1h1p(:,:)
 double precision, allocatable :: psi_coef_1h2p(:,:)
 call all_single_no_1h_or_1p_or_2p
 
 threshold_davidson = 1.d-12
 soft_touch threshold_davidson davidson_criterion
 call diagonalize_CI
 call give_n_1h1p_and_n_1h2p_in_psi_det(n_det_1h1p,n_det_1h2p)
 allocate(psi_ref_out(N_int,2,N_det_generators))
 allocate(psi_1h1p(N_int,2,n_det_1h1p))
 allocate(psi_1h2p(N_int,2,n_det_1h2p))
 allocate(psi_ref_coef_out(N_det_generators,N_states))
 allocate(psi_coef_1h1p(n_det_1h1p,N_states))
 allocate(psi_coef_1h2p(n_det_1h2p,N_states))
 call split_wf_generators_and_1h1p_and_1h2p(n_det_1h1p,n_det_1h2p,psi_ref_out,psi_ref_coef_out,psi_1h1p,psi_coef_1h1p,psi_1h2p,psi_coef_1h2p)
 call provide_matrix_dressing_general(dressing_matrix_1h1p,psi_ref_out,psi_ref_coef_out,N_det_generators, &
                                                        psi_1h1p,psi_coef_1h1p,n_det_1h1p)
 call provide_matrix_dressing_general(dressing_matrix_1h2p,psi_ref_out,psi_ref_coef_out,N_det_generators, &
                                                        psi_1h2p,psi_coef_1h2p,n_det_1h2p)

 deallocate(psi_ref_out)
 deallocate(psi_1h1p)
 deallocate(psi_1h2p)
 deallocate(psi_ref_coef_out)
 deallocate(psi_coef_1h1p)
 deallocate(psi_coef_1h2p)

end



program save_for_qmc
 read_wf = .True.
 TOUCH read_wf
!  call save_dets_qmcchem
 call write_spindeterminants 
! call write_pseudopotential
! call save_aux_basis
end

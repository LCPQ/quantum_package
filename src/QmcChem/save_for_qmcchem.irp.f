program save_for_qmc
 read_wf = .True.
 TOUCH read_wf
 call write_spindeterminants 
 if (do_pseudo) then
   call write_pseudopotential
 endif
end

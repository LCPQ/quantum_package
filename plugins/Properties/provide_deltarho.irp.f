program pouet
 implicit none
 read_wf = .True.
 touch read_wf
 call routine
end

subroutine routine
 implicit none
 provide  integrated_delta_rho_all_points
end

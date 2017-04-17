program write_integrals
 implicit none
 read_wf = .true.
 touch read_wf
 disk_access_only_mo_one_integrals = "None"
 touch disk_access_only_mo_one_integrals
 disk_access_mo_integrals = "None"
 touch disk_access_mo_integrals
 call routine

end

subroutine routine
 implicit none
 call save_one_e_effective_potential
 call save_erf_bi_elec_integrals

end

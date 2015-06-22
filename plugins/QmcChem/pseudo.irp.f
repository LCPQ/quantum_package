subroutine write_pseudopotential
  implicit none
  BEGIN_DOC
!  Write the pseudo_potential into the EZFIO file
  END_DOC
  call ezfio_set_pseudo_mo_pseudo_grid(mo_pseudo_grid)
end


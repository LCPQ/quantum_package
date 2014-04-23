BEGIN_PROVIDER [ double precision,thresh_SCF ]
  implicit none
  BEGIN_DOC  
!  Threshold on the convergence of the Hartree Fock energy
  END_DOC

  logical :: has
  PROVIDE ezfio_filename
  call ezfio_has_Hartree_Fock_thresh_SCF(has)
  if (has) then
    call ezfio_get_Hartree_Fock_thresh_SCF(thresh_SCF)
  else
    thresh_SCF = 1.d-10
    call ezfio_set_Hartree_Fock_thresh_SCF(thresh_SCF)
  endif

END_PROVIDER

BEGIN_PROVIDER [ integer ,n_it_scf_max]
  implicit none
  BEGIN_DOC  
!  Maximum number of SCF iterations
  END_DOC

  logical :: has
  PROVIDE ezfio_filename
  call ezfio_has_Hartree_Fock_n_it_scf_max (has)
  if (has) then
    call ezfio_get_Hartree_Fock_n_it_scf_max(n_it_scf_max)
  else
    n_it_scf_max = 30
    call ezfio_set_Hartree_Fock_n_it_scf_max(n_it_scf_max)
  endif

END_PROVIDER


subroutine fci_iterations(n_determinants,energy,pt2)
  implicit none
  BEGIN_DOC
! Output the number of determinants, energy, and pt2 correction at each iteration 
  END_DOC
  integer            		 :: n_determinants
  double precision               :: energy, pt2
  integer                        :: N_iterations_before
  integer                        :: N_iterations
  logical                        :: hasIter
  integer, allocatable           :: n_determinants_list_before(:)
  real, allocatable              :: energy_list_before(:)
  real, allocatable              :: pt2_list_before(:)
  real, allocatable              :: energy_pt2_list_before(:)
  integer, allocatable           :: n_determinants_list(:)
  real, allocatable              :: energy_list(:)
  real, allocatable              :: pt2_list(:)
  real, allocatable              :: energy_pt2_list(:)



  !call ezfio_has_hartree_fock_energy(has)
  !call ezfio_get_hartree_fock_energy(hf_energy_ref)
  !call ezfio_set_full_ci_zmq_energy(CI_energy(1))
  
  ! IF THE ITERATION IS PAST 1
  ! GET THE ITERATION NUMBER
  ! AND INCREMENT BY 1
  call ezfio_has_fci_iterations_n_iter(hasIter)
  if (hasIter) then 
     call ezfio_get_fci_iterations_n_iter(N_iterations_before)
  ! OTHERWISE SET IT AT 1
  else
     N_iterations_before = 0
  endif

  
  
  !! IF THERE HAS ALREADY BEEN AN ITERATION
  !! GET THE ARRAYS
  if (hasIter) then
     allocate(n_determinants_list_before(N_iterations_before))
     allocate(energy_list_before(N_iterations_before))
     allocate(pt2_list_before(N_iterations_before))
     allocate(energy_pt2_list_before(N_iterations_before))

     call ezfio_get_fci_iterations_n_det(n_determinants_list_before)
     call ezfio_get_fci_iterations_energy(energy_list_before)
     call ezfio_get_fci_iterations_energy_pt2(energy_pt2_list_before)
     call ezfio_get_fci_iterations_pt2(pt2_list_before)
  endif
  
  N_iterations = N_iterations_before +1
  ! RESET THE ITERATION NUMBER 
  call ezfio_set_fci_iterations_n_iter(N_iterations)
  
  !!! NOW UPDATE ARRAY(N_iterations) = LATEST_UPDATE
  allocate(n_determinants_list(N_iterations))
  allocate(energy_list(N_iterations))
  allocate(pt2_list(N_iterations))
  allocate(energy_pt2_list(N_iterations))
 
  if (hasIter) then
     n_determinants_list(1:N_iterations_before) = n_determinants_list_before
     energy_list(1:N_iterations_before) = energy_list_before
     pt2_list(1:N_iterations_before) = pt2_list_before
     energy_pt2_list(1:N_iterations_before) = energy_pt2_list_before 

     deallocate(n_determinants_list_before)
     deallocate(energy_list_before)
     deallocate(pt2_list_before)
     deallocate(energy_pt2_list_before)
  endif
  n_determinants_list(N_iterations) = n_determinants
  energy_list(N_iterations) = energy
  pt2_list(N_iterations) = pt2
  energy_pt2_list(N_iterations) = energy+pt2

  !!!! NOW RESET THE EZFIO VALUES
  call ezfio_set_fci_iterations_n_det(n_determinants_list)
  call ezfio_set_fci_iterations_energy(energy_list)
  call ezfio_set_fci_iterations_pt2(pt2_list)
  call ezfio_set_fci_iterations_energy_pt2(energy_pt2_list)

  deallocate(n_determinants_list)
  deallocate(energy_list)
  deallocate(pt2_list)
  deallocate(energy_pt2_list)
end subroutine 

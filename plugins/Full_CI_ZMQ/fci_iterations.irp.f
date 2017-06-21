subroutine fci_iterations(n_determinants,energy,pt2)
  implicit none
  BEGIN_DOC
! Output the number of determinants, energy, and pt2 correction at each iteration 
  END_DOC
  integer            		 :: n_determinants
  double precision               :: energy, pt2,E_before
  integer                        :: N_iterations
  integer, allocatable           :: n_determinants_list(:)
  double precision, allocatable  :: energy_list(:)
  double precision, allocatable  :: energy_pt2_list(:)
  logical			 :: saveIterations, hasIter



  !!! CHECK TO ENSURE THAT WE SHOULD SAVE ITERATIONS (DEFAULT IS FALSE)
  call ezfio_get_full_ci_zmq_iterative(saveIterations)
  if (saveIterations) then


     !!! If THE ITERATION IS PAST 1
     !!! GET THE ITERATION NUMBER
     !!! MAKE SURE THE CALCULATION IS ALSO NOT STARTING FROM SCRATCH
     call ezfio_has_full_ci_zmq_n_iter(hasIter)
     if((hasIter).AND.(n_determinants>1)) then
         call ezfio_get_full_ci_zmq_n_iter(N_iterations)
     else 
         N_iterations = 0
         E_before =0
     endif

    !! NOW ALLOCATE THE ARRAY FOR ENTIRE SIZE NEEDED
    !! BUT FIRST CHECK TO ENSURE IT IS NOT STARTING FROM SCRATCH BY CHECKING IF THE NUM DETERMINANTS HAS INCREASED
    allocate(n_determinants_list(N_iterations+1))
    allocate(energy_list(N_iterations+1))
    allocate(energy_pt2_list(N_iterations+1))



     !!! PULL PREVIOUSLY WRITTEN DATA
     !!! UNLESS ITS A NEW CALCULATION
     if((hasIter).AND.(n_determinants>1)) then
        call ezfio_get_full_ci_zmq_n_det_iter(n_determinants_list(1:N_iterations))
        call ezfio_get_full_ci_zmq_energy_iter(energy_list(1:N_iterations))
        E_before = energy_list(N_iterations)
        call ezfio_get_full_ci_zmq_energyBefore_pt2_iter(energy_pt2_list(1:N_iterations))
    endif


    !! NOW INCREMENT TO THE CURRENT ITERATION
    N_iterations = N_iterations+1

  
    !! ADD THE DATA FROM LATEST ITERATION 
    n_determinants_list(N_iterations) = n_determinants
    energy_list(N_iterations) = energy
    energy_pt2_list(N_iterations) = E_before +pt2 

    ! RESET THE ITERATION NUMBER 
    call ezfio_set_full_ci_zmq_n_iter(N_iterations)
 
    !!!! NOW RESET THE EZFIO VALUES
    !!!! TO INCLUDE THE LATEST DATA
    call ezfio_set_full_ci_zmq_n_det_iter(n_determinants_list)
    call ezfio_set_full_ci_zmq_energy_iter(energy_list)
    call ezfio_set_full_ci_zmq_energyBefore_pt2_iter(energy_pt2_list)

    deallocate(n_determinants_list)
    deallocate(energy_list)
    deallocate(energy_pt2_list)

  endif
 end subroutine 

subroutine dump_fci_iterations_value(n_determinants,energy,pt2)
  implicit none

!  Not using an irp.f90 environment because the SAVE statement is needed for simpler code

!  BEGIN_DOC
!! Output the number of determinants, energy, and pt2 correction at each iteration 
!  END_DOC
  integer, intent(in)            :: n_determinants
  double precision, intent(in)   :: energy(N_states), pt2(N_states)
  integer                        :: N_iterations
  integer, allocatable           :: n_determinants_list(:)
  double precision, allocatable  :: energy_list(:,:)
  double precision, allocatable  :: pt2_list(:,:)
  integer                        :: saveMethod 
  logical                        :: hasIter
  logical,save                   :: firstAccess=.TRUE. !! every update of firstAccess will be saved
  double precision, allocatable  :: extrapolated_energy(:,:)
  integer                        :: i,k

  !!! Check to ensure that we should save iterations (default is Append)
  ! saveMethod: 1==Append, 2==Overwrite, 3==NoSave
  if (N_det < N_states) then
    return
  endif
  call ezfio_get_full_ci_zmq_iterative_save(saveMethod)

  !!! Check we are saving data
  if (saveMethod/=3) then

     !!! If the iteration number exists get it
     !!! otherwise set it to zero
     call ezfio_has_full_ci_zmq_n_iter(hasIter)
     if (hasIter) then
         call ezfio_get_full_ci_zmq_n_iter(N_iterations)
     else
        N_iterations=0 
     endif

     !!! If it is append we don't have to do any more checks
     !!! N_iterations will be correct now. 

     !!! If we should overwrite and this is the first time
     !!! Then we need to reset N_iterations to zero
     if ((saveMethod==2).AND.(firstAccess)) then
        N_iterations=0
     endif
     
     !! Now allocate the array for entire size needed
     allocate(extrapolated_energy(N_iterations+1,N_states))
     allocate(n_determinants_list(N_iterations+1))
     allocate(energy_list(N_states,N_iterations+1))
     allocate(pt2_list(N_states,N_iterations+1))

     !!! Pull previously written data
     !!! Unless it is at the beginning of a new/restarted calculation
     if((hasIter).AND.(N_iterations>0)) then 
        call ezfio_get_full_ci_zmq_n_det_iter(n_determinants_list)
        call ezfio_get_full_ci_zmq_energy_iter(energy_list)
        call ezfio_get_full_ci_zmq_pt2_iter(pt2_list)
     endif

     !! Now increment to the current iteration
     N_iterations = N_iterations+1

     !! Add the data from latest iteration 
     n_determinants_list(N_iterations) = n_determinants
     energy_list(:,N_iterations) = energy(:)
     pt2_list(:,N_iterations) = pt2(:) 

    if (N_iterations > 2) then
      write(*,'(A)') ''
      write(*,'(A)') 'Extrapolated energies'
      write(*,'(A)') '------------------------'
      write(*,'(A)') ''
      
      do i=1, min(N_states,N_det)
        call extrapolate_data(N_iterations, energy_list(i,1:N_iterations), pt2_list(i,1:N_iterations), extrapolated_energy(1:N_iterations,i))
      enddo
  
      print *,  ''
      print *,  'State ', 1
      print *,  ''
      write(*,*)  '=========== ', '==================='
      write(*,*)  'minimum PT2 ', 'Extrapolated energy'
      write(*,*)  '=========== ', '==================='
      do k=2,min(N_iterations,8)
        write(*,'(F11.4,2X,F18.8)') pt2_list(1,N_iterations+1-k), extrapolated_energy(k,1)
      enddo
      write(*,*)  '=========== ', '==================='

      do i=2, min(N_states,N_det)
        print *,  ''
        print *,  'State ', i
        print *,  ''
        write(*,*)  '=========== ', '=================== ', '=================== ', '==================='
        write(*,*)  'minimum PT2 ', 'Extrapolated energy ', '  Excitation (a.u)  ', '  Excitation (eV)  '
        write(*,*)  '=========== ', '=================== ', '=================== ', '==================='
        do k=2,min(N_iterations,8)
          write(*,'(F11.4,X,3(X,F18.8))') pt2_list(i,N_iterations+1-k), extrapolated_energy(k,i), &
              extrapolated_energy(k,i) - extrapolated_energy(k,1), &
              (extrapolated_energy(k,i) - extrapolated_energy(k,1) ) * 27.211396641308d0
        enddo
        write(*,*)  '=========== ', '=================== ', '=================== ', '==================='
      enddo

      print *,  ''
    endif
  

    ! Reset the iteration number 
    call ezfio_set_full_ci_zmq_n_iter(N_iterations)

    !!!! Now reset the ezfio values
    !!!! To include the latest data
    call ezfio_set_full_ci_zmq_n_det_iter(n_determinants_list)
    call ezfio_set_full_ci_zmq_energy_iter(energy_list)
    call ezfio_set_full_ci_zmq_pt2_iter(pt2_list)

    deallocate(n_determinants_list)
    deallocate(energy_list)
    deallocate(pt2_list)

  endif

  !!! set first access to false
  !!! it will be saved
  firstAccess=.FALSE.
 end subroutine 


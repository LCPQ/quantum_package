program fci_zmq
  implicit none
  integer                        :: i,j,k
  double precision, allocatable  :: pt2(:)
  integer                        :: degree
  integer                        :: n_det_before, to_select
  double precision               :: threshold_davidson_in
  
  allocate (pt2(N_states))

  double precision               :: hf_energy_ref
  logical                        :: has
  double precision               :: relative_error, absolute_error
  integer                        :: N_states_p
  character*(512)                :: fmt

  relative_error=PT2_relative_error
  absolute_error=PT2_absolute_error

  pt2 = -huge(1.e0)
  threshold_davidson_in = threshold_davidson
  threshold_davidson = threshold_davidson_in * 100.d0
  SOFT_TOUCH threshold_davidson

  call diagonalize_CI
  call save_wavefunction
  
  call ezfio_has_hartree_fock_energy(has)
  if (has) then
    call ezfio_get_hartree_fock_energy(hf_energy_ref)
  else
    hf_energy_ref = ref_bitmask_energy
  endif

  if (N_det > N_det_max) then
    psi_det = psi_det_sorted
    psi_coef = psi_coef_sorted
    N_det = N_det_max
    soft_touch N_det psi_det psi_coef
    call diagonalize_CI
    call save_wavefunction
    N_states_p = min(N_det,N_states)
  endif
  
  n_det_before = 0

  character*(8) :: pt2_string
  double precision :: correlation_energy_ratio
  double precision :: threshold_selectors_save, threshold_generators_save
  threshold_selectors_save  = threshold_selectors
  threshold_generators_save = threshold_generators
  double precision :: error(N_states)

  correlation_energy_ratio = 0.d0

  if (.True.) then ! Avoid pre-calculation of CI_energy
    do while (                                                         &
          (N_det < N_det_max) .and.                                    &
          (maxval(abs(pt2(1:N_states))) > pt2_max) .and.               &
          (correlation_energy_ratio <= correlation_energy_ratio_max)    &
          )
       write(*,'(A)')  '--------------------------------------------------------------------------------'


      if (do_pt2) then
        pt2_string = '        '
        pt2 = 0.d0
        threshold_selectors = 1.d0
        threshold_generators = 1d0 
        SOFT_TOUCH threshold_selectors threshold_generators
        call ZMQ_pt2(CI_energy, pt2,relative_error,absolute_error,error) ! Stochastic PT2
        threshold_selectors = threshold_selectors_save
        threshold_generators = threshold_generators_save
        SOFT_TOUCH threshold_selectors threshold_generators
      else
        pt2_string = '(approx)'
      endif


      correlation_energy_ratio = (CI_energy(1) - hf_energy_ref)  /     &
                      (CI_energy(1) + pt2(1) - hf_energy_ref)
      correlation_energy_ratio = min(1.d0,correlation_energy_ratio)

      N_states_p = min(N_det,N_states)

      print *, ''
      print '(A,I12)',  'Summary at N_det = ', N_det
      print '(A)',      '-----------------------------------'
      print *, ''
      call write_double(6,correlation_energy_ratio, 'Correlation ratio')
      print *, ''

      write(fmt,*) '(''# ============'',', N_states_p, '(1X,''=============================''))'
      write(*,fmt)
      write(fmt,*) '(12X,', N_states_p, '(6X,A7,1X,I6,10X))'
      write(*,fmt) ('State',k, k=1,N_states_p)
      write(fmt,*) '(''# ============'',', N_states_p, '(1X,''=============================''))'
      write(*,fmt)
      write(fmt,*) '(A12,', N_states_p, '(1X,F14.8,15X))'
      write(*,fmt) '# E          ', CI_energy(1:N_states_p)
      if (N_states_p > 1) then
        write(*,fmt) '# Excit. (au)', CI_energy(1:N_states_p)-CI_energy(1)
        write(*,fmt) '# Excit. (eV)', (CI_energy(1:N_states_p)-CI_energy(1))*27.211396641308d0
      endif
      write(fmt,*) '(A12,', 2*N_states_p, '(1X,F14.8))'
      write(*,fmt) '# PT2'//pt2_string, (pt2(k), error(k), k=1,N_states_p)
      write(*,'(A)') '#'
      write(*,fmt) '# E+PT2      ', (CI_energy(k)+pt2(k),error(k), k=1,N_states_p)
      if (N_states_p > 1) then
        write(*,fmt) '# Excit. (au)', ( (CI_energy(k)+pt2(k)-CI_energy(1)-pt2(1)), &
          dsqrt(error(k)*error(k)+error(1)*error(1)), k=1,N_states_p)
        write(*,fmt) '# Excit. (eV)', ( (CI_energy(k)+pt2(k)-CI_energy(1)-pt2(1))*27.211396641308d0, &
          dsqrt(error(k)*error(k)+error(1)*error(1))*27.211396641308d0, k=1,N_states_p)
      endif
      write(fmt,*) '(''# ============'',', N_states_p, '(1X,''=============================''))'
      write(*,fmt)
      print *,  ''

      print *,  'N_det             = ', N_det
      print *,  'N_states          = ', N_states
      print*,   'correlation_ratio = ', correlation_energy_ratio

      do k=1, N_states_p
        print*,'State ',k
        print *,  'PT2             = ', pt2(k)
        print *,  'E               = ', CI_energy(k)
        print *,  'E+PT2'//pt2_string//'   = ', CI_energy(k)+pt2(k), ' +/- ', error(k)
      enddo

      print *,  '-----'
      if(N_states.gt.1)then
        print *, 'Variational Energy difference (au | eV)'
        do i=2, N_states_p
          print*,'Delta E = ', (CI_energy(i) - CI_energy(1)), &
            (CI_energy(i) - CI_energy(1)) * 27.211396641308d0
        enddo
        print *,  '-----'
        print*, 'Variational + perturbative Energy difference (au | eV)'
        do i=2, N_states_p
          print*,'Delta E = ', (CI_energy(i)+ pt2(i) - (CI_energy(1) + pt2(1))), &
            (CI_energy(i)+ pt2(i) - (CI_energy(1) + pt2(1))) * 27.211396641308d0
        enddo
      endif
      call ezfio_set_full_ci_zmq_energy_pt2(CI_energy(1)+pt2(1))
      call dump_fci_iterations_value(N_det,CI_energy,pt2) 

      n_det_before = N_det
      if (s2_eig) then
        to_select = N_det/2+1
        to_select = max(N_det/2+1, to_select)
        to_select = min(to_select, N_det_max-n_det_before)
      else
        to_select = N_det
        to_select = max(N_det, to_select)
        to_select = min(to_select, N_det_max-n_det_before)
      endif
      call save_natural_mos
      call map_deinit(mo_integrals_map)
      FREE mo_integrals_map
      PROVIDE mo_integrals_map
      call four_index_transform_block(ao_integrals_map,mo_integrals_map,  &
         mo_coef, size(mo_coef,1),                                      &
         1, 1, 1, 1, ao_num, ao_num, ao_num, ao_num,                    &
         1, 1, 1, 1, mo_num, mo_num, mo_num, mo_num)
      
      call ZMQ_selection(to_select, pt2)
      
      PROVIDE  psi_coef
      PROVIDE  psi_det
      PROVIDE  psi_det_sorted

      if (N_det >= N_det_max) then
        threshold_davidson = threshold_davidson_in
      end if
      call diagonalize_CI
      call save_wavefunction
      call ezfio_set_full_ci_zmq_energy(CI_energy(1))
    enddo
  endif

  if (N_det < N_det_max) then
      threshold_davidson = threshold_davidson_in
      call diagonalize_CI
      call save_wavefunction
      call ezfio_set_full_ci_zmq_energy(CI_energy(1))
      call ezfio_set_full_ci_zmq_energy_pt2(CI_energy(1)+pt2(1))
  endif

  if (do_pt2) then
    pt2 = 0.d0
    threshold_selectors = 1.d0
    threshold_generators = 1d0 
    SOFT_TOUCH threshold_selectors threshold_generators
    call ZMQ_pt2(CI_energy, pt2,relative_error,absolute_error,error) ! Stochastic PT2
    threshold_selectors = threshold_selectors_save
    threshold_generators = threshold_generators_save
    SOFT_TOUCH threshold_selectors threshold_generators
    call ezfio_set_full_ci_zmq_energy(CI_energy(1))
    call ezfio_set_full_ci_zmq_energy_pt2(CI_energy(1)+pt2(1))
  endif
  print *,  'N_det             = ', N_det
  print *,  'N_states          = ', N_states
  print*,   'correlation_ratio = ', correlation_energy_ratio


  call dump_fci_iterations_value(N_det,CI_energy,pt2) 

  print *, ''
  print '(A,I12)',  'Summary at N_det = ', N_det
  print '(A)',      '-----------------------------------'
  print *, ''
  call write_double(6,correlation_energy_ratio, 'Correlation ratio')
  print *, ''


  N_states_p = min(N_det,N_states)
  print *,  ''
  write(fmt,*) '(''# ============'',', N_states_p, '(1X,''=============================''))'
  write(*,fmt)
  write(fmt,*) '(12X,', N_states_p, '(6X,A7,1X,I6,10X))'
  write(*,fmt) ('State',k, k=1,N_states_p)
  write(fmt,*) '(''# ============'',', N_states_p, '(1X,''=============================''))'
  write(*,fmt)
  write(fmt,*) '(A12,', N_states_p, '(1X,F14.8,15X))'
  write(*,fmt) '# E          ', CI_energy(1:N_states_p)
  if (N_states_p > 1) then
    write(*,fmt) '# Excit. (au)', CI_energy(1:N_states_p)-CI_energy(1)
    write(*,fmt) '# Excit. (eV)', (CI_energy(1:N_states_p)-CI_energy(1))*27.211396641308d0
  endif
  write(fmt,*) '(A12,', 2*N_states_p, '(1X,F14.8))'
  write(*,fmt) '# PT2'//pt2_string, (pt2(k), error(k), k=1,N_states_p)
  write(*,'(A)') '#'
  write(*,fmt) '# E+PT2      ', (CI_energy(k)+pt2(k),error(k), k=1,N_states_p)
  if (N_states_p > 1) then
    write(*,fmt) '# Excit. (au)', ( (CI_energy(k)+pt2(k)-CI_energy(1)-pt2(1)), &
      dsqrt(error(k)*error(k)+error(1)*error(1)), k=1,N_states_p)
    write(*,fmt) '# Excit. (eV)', ( (CI_energy(k)+pt2(k)-CI_energy(1)-pt2(1))*27.211396641308d0, &
      dsqrt(error(k)*error(k)+error(1)*error(1))*27.211396641308d0, k=1,N_states_p)
  endif
  write(fmt,*) '(''# ============'',', N_states_p, '(1X,''=============================''))'
  write(*,fmt)
  print *,  ''



end

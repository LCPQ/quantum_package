program analyze_wf
  implicit none
  BEGIN_DOC
! Wave function analyzis
  END_DOC
  read_wf = .True.
  SOFT_TOUCH read_wf
  call run()
end

subroutine run
  implicit none
  integer                        :: istate, i
  integer                        :: class(0:mo_tot_num,5)
  double precision               :: occupation(mo_tot_num)

  write(*,'(A)')  'Energy of 1st determinant'
  write(*,'(A)')  '========================='
  write(*,'(A)')  ''
  write(*,*) 'Total', ref_bitmask_energy + nuclear_repulsion
  write(*,*) 'Mono-electronic', mono_elec_ref_bitmask_energy
  write(*,*) 'Kinetic', kinetic_ref_bitmask_energy
  write(*,*) 'Electron-nucleus', nucl_elec_ref_bitmask_energy
  write(*,*) 'Two-electron', bi_elec_ref_bitmask_energy
  write(*,'(A)')  ''
  write(*,'(A)')  ''

  write(*,'(A)')  'MO Occupation'
  write(*,'(A)')  '============='
  write(*,'(A)')  ''
  do istate=1,N_states
    call get_occupation_from_dets(occupation,istate)
    write(*,'(A)')  ''
    write(*,'(A,I3)'),  'State ', istate
    write(*,'(A)')  '---------------'
    write(*,'(A)')  ''
    write (*,'(A)')  '======== ================'
    class = 0
    do i=1,mo_tot_num
      write (*,'(I8,X,F16.10)')  i, occupation(i)
      if (occupation(i) > 1.999d0) then
        class(0,1) += 1
        class( class(0,1), 1) = i
      else if (occupation(i) > 1.97d0) then
        class(0,2) += 1
        class( class(0,2), 2) = i
      else if (occupation(i) < 0.001d0) then
        class(0,5) += 1
        class( class(0,5), 5) = i
      else if (occupation(i) < 0.03d0) then
        class(0,4) += 1
        class( class(0,4), 4) = i
      else 
        class(0,3) += 1
        class( class(0,3), 3) = i
      endif
    enddo
    write (*,'(A)')  '======== ================'
    write (*,'(A)')  ''

    write (*,'(A)')  'Suggested classes'
    write (*,'(A)')  '-----------------'
    write (*,'(A)')  ''
    write (*,'(A)')  'Core :'
    write (*,*)      (class(i,1), ',', i=1,class(0,1))
    write (*,*)      ''
    write (*,'(A)')  'Inactive :'
    write (*,*)      (class(i,2), ',', i=1,class(0,2))
    write (*,'(A)')  ''
    write (*,'(A)')  'Active :'
    write (*,*)      (class(i,3), ',', i=1,class(0,3))
    write (*,'(A)')  ''
    write (*,'(A)')  'Virtual :'
    write (*,*)      (class(i,4), ',', i=1,class(0,4))
    write (*,'(A)')  ''
    write (*,'(A)')  'Deleted :'
    write (*,*)      (class(i,5), ',', i=1,class(0,5))
    write (*,'(A)')  ''
  enddo

end

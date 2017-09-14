program dump_one_body_mos
  implicit none
  BEGIN_DOC
! Output density matrices of all the states
  END_DOC
  read_wf = .True.
  TOUCH read_wf
  call run()
end

subroutine run
  implicit none
  integer                        :: istate
  integer, parameter             :: iunit = 66
  character*(64)                 :: filename, fmt
  integer                        :: i,j,k

  write(fmt,'(''('',I4.4,''(X,E20.14))'')') mo_tot_num
  do istate=1,N_states
    write(filename,'(''state.'',I2.2)') istate
    open(unit=iunit, form='formatted', file=filename)
    write(iunit,*) mo_tot_num
    do j=1,mo_tot_num
      write(iunit,fmt) one_body_dm_mo_alpha(1:mo_tot_num,j,istate) + one_body_dm_mo_beta(1:mo_tot_num,j,istate)
    enddo
  enddo


  call run2()
end
subroutine run2
  integer :: i,j, istate
  print *,  'Phi_S'
  do i=2,N_states
    print *,  i, Phi_S(i)
  enddo
end


BEGIN_PROVIDER [ logical, abort_all ]
 implicit none
 BEGIN_DOC
 ! If True, all the calculation is aborted
 END_DOC
 abort_all = .False.

END_PROVIDER

BEGIN_PROVIDER [ logical, abort_here ]
 implicit none
 BEGIN_DOC
 ! If True, all the calculation is aborted
 END_DOC
 abort_here = abort_all
END_PROVIDER

subroutine trap_signals
  use ifport
  implicit none
  BEGIN_DOC
  ! What to do when a signal is caught. Here, trap Ctrl-C and call the control_C subroutine.
  END_DOC
  integer, external              :: control_C
  integer                        :: err, flag
  flag = -1
  err = signal (sigint, control_C, flag)
  PROVIDE abort_all
  PROVIDE abort_here
end subroutine trap_signals

integer function control_C(signum)
  implicit none
  integer, intent(in) :: signum
  BEGIN_DOC
  ! What to do on Ctrl-C. If two Ctrl-C are pressed within 1 sec, the calculation if aborted.
  END_DOC
  double precision, save         :: last_time
  double precision               :: this_time
  control_C = 0
  call wall_time(this_time)
  if (this_time - last_time < 1.d0) then
    print *,  'Caught Ctrl-C'
    abort_all = .True.
  endif 
  last_time = this_time
  abort_here = .True.
end subroutine control_C


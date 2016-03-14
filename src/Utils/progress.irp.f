subroutine start_progress(max,title,progress_init)
  implicit none
  integer, intent(in) :: max
  double precision, intent(in) :: progress_init
  character*(*), intent(in) :: title
  BEGIN_DOC
! Starts the progress bar
  END_DOC

  progress_bar(1) = 0
  progress_bar(2) = max
  progress_title = title
  progress_active = .True.
  progress_value = progress_init
! call run_progress()

end

subroutine stop_progress
  implicit none
  BEGIN_DOC
! Stop the progress bar
  END_DOC
  progress_active = .False.
end

 BEGIN_PROVIDER [ real, progress_bar, (2) ]
&BEGIN_PROVIDER [ integer, progress_timeout ]
&BEGIN_PROVIDER [ logical, progress_active ]
&BEGIN_PROVIDER [ double precision, progress_value ]
&BEGIN_PROVIDER [ character*(20) , progress_title ]
 implicit none
 BEGIN_DOC
 ! Current status for displaying progress bars. Global variable.
 END_DOC
 progress_bar     = 0
 progress_value   = 0.d0
 progress_title   = ''
 progress_active  = .False.
 progress_timeout = 1
! open (unit=0, carriagecontrol='fortran')

END_PROVIDER

recursive subroutine run_progress
  implicit none
  BEGIN_DOC
! Display a progress bar with documentation of what is happening
  END_DOC
  character(75), parameter       :: bar0=                            &
  ' ---                      : ---                |                     | ---%'
  character(75)                  :: bar
  integer                        :: prog


  bar = bar0
  if (.not.progress_active) then
    call stop_progress
    write(unit=0,fmt="(a1,a1,a70)") '+',char(13), bar
  else
    prog = int( progress_bar(1)*100./progress_bar(2) )
    write(bar(1:25),'(A)') progress_title
    write(bar(29:47),'(G17.10)') progress_value
    write(bar(72:74),'(i3)') prog
   
    integer :: k,j
    j = int( progress_bar(1)*20./progress_bar(2) )
    do k=1, j
      bar(49+k:49+k)="="
    enddo
    write(unit=0,fmt="(a1,a1,a75)") '+',char(13), bar
    call alarm(progress_timeout,run_progress)
  endif

end



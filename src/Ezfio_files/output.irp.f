 BEGIN_PROVIDER [ double precision, output_wall_time_0 ]
&BEGIN_PROVIDER [ double precision, output_cpu_time_0 ]
  implicit none
  BEGIN_DOC
  ! Initial CPU and wall times when printing in the output files
  END_DOC
  call cpu_time(output_wall_time_0)
  call wall_time(output_wall_time_0)
END_PROVIDER
 
BEGIN_SHELL [ /bin/bash ]
 
 for NAME in $(\ls -d ${QP_ROOT}/src/*/)
 do
 NAME=$(basename ${NAME})
 cat << EOF
 BEGIN_PROVIDER [ integer, output_$NAME ]
  implicit none
  BEGIN_DOC
  ! Output file for $NAME
  END_DOC
  PROVIDE output_wall_time_0 output_cpu_time_0 ezfio_filename
!  integer                        :: getUnitAndOpen
!  call ezfio_set_output_empty(.False.)
IRP_IF COARRAY
  if (this_image() == 1) then
    output_$NAME = 6 !getUnitAndOpen(trim(ezfio_filename)//'/output/'//'$NAME.rst','a')
  else
    output_$NAME = getUnitAndOpen('/dev/null','w')
  endif
IRP_ELSE
  output_$NAME = 6 !getUnitAndOpen(trim(ezfio_filename)//'/output/'//'$NAME.rst','a')
IRP_ENDIF
  write(output_$NAME,'(A)')                                          &
      '--------------------------------------------------------------------------------'
 END_PROVIDER
EOF
 done
 
END_SHELL
 
subroutine write_time(iunit)
  implicit none
  BEGIN_DOC
  ! Write a time stamp in the output for chronological reconstruction
  END_DOC
  integer, intent(in)            :: iunit
  double precision               :: wt, ct
  call cpu_time(ct)
  call wall_time(wt)
  write(iunit,*)
  write(iunit,'(A,F15.6,A,F15.6,A)') &
    '.. >>>>> [ WALL TIME: ', wt-output_wall_time_0, &
        ' s ] [ CPU  TIME: ', ct-output_cpu_time_0, ' s ] <<<<< ..'
  write(iunit,*)
end
 
subroutine write_double(iunit,value,label)
  implicit none
  BEGIN_DOC
  ! Write a double precision value in output
  END_DOC
  integer, intent(in)            :: iunit
  double precision               :: value
  character*(*)                  :: label
  character*(64), parameter      :: f = '(A50,G24.16)'
  character*(50)                 :: newlabel
  write(newlabel,'(A,A)') '* ',trim(label)
  write(iunit,f) newlabel, value
end


subroutine write_int(iunit,value,label)
  implicit none
  BEGIN_DOC
  ! Write an integer value in output
  END_DOC
  integer, intent(in)            :: iunit
  integer                        :: value
  character*(*)                  :: label
  character*(64), parameter      :: f = '(A50,I16)'
  character*(50)                 :: newlabel
  write(newlabel,'(A,A)') '* ',trim(label)
  write(iunit,f) newlabel, value
end


subroutine write_bool(iunit,value,label)
  implicit none
  BEGIN_DOC
  ! Write an logical value in output
  END_DOC
  integer, intent(in)            :: iunit
  logical                        :: value
  character*(*)                  :: label
  character*(64), parameter      :: f = '(A50,L1)'
  character*(50)                 :: newlabel
  write(newlabel,'(A,A)') '* ',trim(label)
  write(iunit,f) newlabel, value
end



program prog_truncate_mo
  BEGIN_DOC
! Truncate MO set
  END_DOC
  implicit none
  integer :: n
  write(*,*) 'Number of MOs to keep'
  read (*,*) n
  call save_mos_truncated(n)
end

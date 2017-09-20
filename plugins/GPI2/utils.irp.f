 BEGIN_PROVIDER [ logical, GASPI_is_initialized ]
&BEGIN_PROVIDER [ logical, has_gaspi ]
  implicit none
  BEGIN_DOC
! This is true when GASPI_Init has been called
  END_DOC

  has_gaspi = .False.
  IRP_IF GASPI
      use GASPI
      integer(gaspi_return_t) :: res
      res = gaspi_proc_init(GASPI_BLOCK)
      if (res /= GASPI_SUCCESS) then
         print *, res
         print *, 'GASPI failed to initialize'
         stop -1
      endif
      has_gaspi = .True.
  IRP_ENDIF
  GASPI_is_initialized = .True.
END_PROVIDER


 BEGIN_PROVIDER [ integer, GASPI_rank ]
&BEGIN_PROVIDER [ integer, GASPI_size ]
&BEGIN_PROVIDER [ logical, is_GASPI_master ]
 implicit none
 BEGIN_DOC
! Usual GASPI variables
 END_DOC

 PROVIDE GASPI_is_initialized

 IRP_IF GASPI
   use GASPI
   integer(gaspi_return_t) :: res 
   integer(gaspi_rank_t)   :: n
   res = gaspi_proc_num(n)
   GASPI_size = n
   if (res  /= GASPI_SUCCESS) then
     print *, res 
     print *, 'Unable to get GASPI_size'
     stop -1
   endif
   res = gaspi_proc_rank(n)
   GASPI_rank = n
   if (res  /= GASPI_SUCCESS) then
     print *, res 
     print *, 'Unable to get GASPI_rank'
     stop -1
   endif
   is_GASPI_master = (GASPI_rank == 0)
 IRP_ELSE
   GASPI_rank = 0
   GASPI_size = 1
   is_GASPI_master = .True.
 IRP_ENDIF


END_PROVIDER

subroutine gaspi_finalize()
  implicit none
  PROVIDE GASPI_is_initialized
  IRP_IF GASPI
    use GASPI
    integer(gaspi_return_t) :: res 
    res = gaspi_proc_term(GASPI_BLOCK)
    if (res  /= GASPI_SUCCESS) then
      print *, res 
      print *, 'Unable to finalize GASPI'
      stop -1
    endif
  IRP_ENDIF
end subroutine


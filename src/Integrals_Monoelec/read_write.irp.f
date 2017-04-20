 BEGIN_PROVIDER [ logical, read_ao_one_integrals ]
&BEGIN_PROVIDER [ logical, read_mo_one_integrals ]
&BEGIN_PROVIDER [ logical, write_ao_one_integrals ]
&BEGIN_PROVIDER [ logical, write_mo_one_integrals ]
   
   BEGIN_DOC
   ! One level of abstraction for disk_access_ao_integrals and disk_access_mo_integrals
   END_DOC
   implicit none
   
   if (disk_access_ao_one_integrals.EQ.'Read') then
     read_ao_one_integrals =  .True.
     write_ao_one_integrals = .False.
     
   else if  (disk_access_ao_one_integrals.EQ.'Write') then
     read_ao_one_integrals = .False.
     write_ao_one_integrals =  .True.
     
   else if (disk_access_ao_one_integrals.EQ.'None') then
     read_ao_one_integrals = .False.
     write_ao_one_integrals = .False.
     
   else
     print *, 'bielec_integrals/disk_access_ao_integrals has a wrong type'
     stop 1
     
   endif
   
   if (disk_access_mo_one_integrals.EQ.'Read') then
     read_mo_one_integrals =  .True.
     write_mo_one_integrals = .False.
     
   else if  (disk_access_mo_one_integrals.EQ.'Write') then
     read_mo_one_integrals = .False.
     write_mo_one_integrals =  .True.
     
   else if (disk_access_mo_one_integrals.EQ.'None') then
     read_mo_one_integrals = .False.
     write_mo_one_integrals = .False.
     
   else
     print *, 'bielec_integrals/disk_access_mo_integrals has a wrong type'
     stop 1
     
   endif
   
END_PROVIDER

subroutine write_one_e_integrals(filename, A, m, n)
  implicit none
  BEGIN_DOC
! Write the 1-electron integrals stored in A(m,n) into file 'filename'
  END_DOC
  character(len=*), intent(in)   :: filename
  integer, intent(in)            :: m,n
  double precision, intent(in)   :: A(m,n)

  integer                        :: iunit
  integer, external              :: getUnitAndOpen
  character*(256)                :: f

  iunit = getUnitAndOpen( trim(ezfio_work_dir)//trim(filename), 'W' )
  write(iunit) A
  close(iunit)
end

subroutine read_one_e_integrals(filename, A, m, n)
  implicit none
  BEGIN_DOC
! Read the 1-electron integrals into in A(m,n) from file 'filename'
  END_DOC
  character(len=*), intent(in)   :: filename
  integer, intent(in)            :: m,n
  double precision, intent(out)  :: A(m,n)

  integer                        :: iunit
  integer, external              :: getUnitAndOpen
  character*(256)                :: f

  iunit = getUnitAndOpen( trim(ezfio_work_dir)//trim(filename), 'R' )
  read(iunit) A
  close(iunit)
end


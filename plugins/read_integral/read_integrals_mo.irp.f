program read_integrals
  BEGIN_DOC
! Reads the integrals from the following files:
! il for (Index Last value i j k l)
! - hcore_mo_il (kintec + nuclear + pseudo )
! - bielec_mo_il 

  END_DOC
  PROVIDE ezfio_filename
  call ezfio_set_integrals_monoelec_disk_access_mo_hcore("None")
  call run
end

subroutine run
  use map_module
  implicit none
  
  integer :: iunit
  integer :: getunitandopen

  integer ::i,j,k,l
  double precision :: integral
  double precision, allocatable :: A(:,:)

  integer             :: n_integrals 
  integer(key_kind), allocatable   :: buffer_i(:) 
  real(integral_kind), allocatable :: buffer_values(:)
  integer(key_kind)  :: key
   
  call ezfio_get_mo_basis_mo_tot_num(mo_tot_num)

  allocate (A(mo_tot_num,mo_tot_num))
  A = 0.d0
  
  iunit = getunitandopen('hcore_mo_il','r')
  do 
    read (iunit,*,end=10) integral, i,j
    A(i,j) = integral
  enddo
  10 continue
  close(iunit)
  call write_one_e_integrals('mo_mono_elec_integral', A, size(A,1), size(A,2))
  

  call ezfio_set_integrals_monoelec_disk_access_mo_one_integrals("Read")

  allocate(buffer_i(mo_tot_num**4), buffer_values(mo_tot_num**4))
   
  iunit = getunitandopen('bielec_mo_il','r')
  n_integrals=0
  do 
    read (iunit,*,end=13) integral, i,j,k,l
    n_integrals += 1
    call bielec_integrals_index(i, j, k, l, buffer_i(n_integrals) )
    buffer_values(n_integrals) = integral
  enddo
  13 continue
  close(iunit)
  
  call insert_into_mo_integrals_map(n_integrals,buffer_i,buffer_values,0.d0)

  call map_sort(mo_integrals_map)

  call map_save_to_disk(trim(ezfio_filename)//'/work/mo_ints',mo_integrals_map)
  call ezfio_set_integrals_bielec_disk_access_mo_integrals("Read")
  
end

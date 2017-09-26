program read_integrals

  PROVIDE ezfio_filename
  call ezfio_set_integrals_monoelec_disk_access_ao_one_integrals("None")
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
   
  allocate (A(ao_num,ao_num))
  A = 0.d0
  
  iunit = getunitandopen('kinetic_ao','r')
  do 
    read (iunit,*,end=10) i,j, integral
    A(i,j) = integral
    A(j,i) = integral
  enddo
  10 continue
  close(iunit)
  call write_one_e_integrals('ao_kinetic_integral', A, size(A,1), size(A,2))


  A = 0.d0
  iunit = getunitandopen('nuclear_ao','r')
  do 
    read (iunit,*,end=12) i,j, integral
    A(i,j) = integral
    A(j,i) = integral
  enddo
  12 continue
  close(iunit)
  call write_one_e_integrals('ao_ne_integral', A, size(A,1), size(A,2))

  call write_one_e_integrals('ao_pseudo_integral', ao_pseudo_integral,&
        size(ao_pseudo_integral,1), size(ao_pseudo_integral,2))


  call ezfio_set_integrals_monoelec_disk_access_ao_one_integrals("Read")

  allocate(buffer_i(ao_num**4), buffer_values(ao_num**4))
   
  iunit = getunitandopen('bielec_ao','r')
  n_integrals=0
  do 
    read (iunit,*,end=13) i,j,k,l, integral
    n_integrals += 1
    call bielec_integrals_index(i, j, k, l, buffer_i(n_integrals) )
    buffer_values(n_integrals) = integral
  enddo
  13 continue
  close(iunit)
  
  call insert_into_ao_integrals_map(n_integrals,buffer_i,buffer_values)

  call map_sort(ao_integrals_map)
  call map_unique(ao_integrals_map)

  call map_save_to_disk(trim(ezfio_filename)//'/work/ao_ints',ao_integrals_map)
  call ezfio_set_integrals_bielec_disk_access_ao_integrals('Read')

end

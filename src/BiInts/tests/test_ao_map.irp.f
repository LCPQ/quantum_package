program test_ao_map
  implicit none
  integer                        :: i,j,k,l
  double precision               :: get_ao_bielec_integral, tmp
  double precision, allocatable  :: buffer_value(:,:)
  double precision               :: ao_bielec_integral,tmp2
  integer                        :: non_zero_int
  double precision               :: s
  integer                        :: OK

  allocate(buffer_value(ao_num,2))
  
  OK = 1
  do l=1,ao_num,7
    s = 0.d0
    do k=1, ao_num
     do j=1,l
      call compute_ao_bielec_integrals(j,k,l,ao_num,buffer_value(1,1))
      call get_ao_bielec_integrals(j,k,l,ao_num,buffer_value(1,2))
      do i=1,k
        s += (buffer_value(i,1)-buffer_value(i,2))*(buffer_value(i,1)-buffer_value(i,2))
      enddo
     enddo
    enddo
    if (s > 1.d-20) then
      print *,  l, ' : ', s
      OK=0
    endif
  enddo

  deallocate(buffer_value)
  print *,  ' OK : ', OK
  
  
end

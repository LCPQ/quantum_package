subroutine bitstring_to_list( string, list, n_elements, Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Gives the inidices(+1) of the bits set to 1 in the bit string
  END_DOC
  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: string(Nint)
  integer, intent(out)           :: list(Nint*bit_kind_size)
  integer, intent(out)           :: n_elements
  
  integer                        :: i, ishift
  integer(bit_kind)              :: l
  
  n_elements = 0
  ishift = 2
  do i=1,Nint
    l = string(i)
    do while (l /= 0_bit_kind)
      n_elements = n_elements+1
      list(n_elements) = ishift+popcnt(l-1_bit_kind) - popcnt(l)
      l = iand(l,l-1_bit_kind)
    enddo
    ishift = ishift + bit_kind_size
  enddo
  
end

subroutine list_to_bitstring( string, list, n_elements, Nint)
  use bitmasks
  implicit none
  integer, intent(in)            :: Nint
  integer(bit_kind), intent(out) :: string(Nint)
  integer, intent(in)            :: list(Nint*bit_kind_size)
  integer, intent(in)            :: n_elements
  
  
  integer                        :: i, j
  integer                        :: ipos, iint
  BEGIN_DOC
  ! return the physical string "string(N_int,2)" from the array of occupations "list(N_int*bit_kind_size,2)
  !                              list
  !                                       <== ipos ==>
  !                                                  |
  !                                                  v
  !string :|------------------------|-------------------------|------------------------|
  !        <==== bit_kind_size ====> <==== bit_kind_size ====> <==== bit_kind_size ====>
  !        {        iint            } {         iint         } {         iint         }
  END_DOC
  
  string = 0_bit_kind
  
  do i=1,n_elements
    iint = ishft(list(i)-1,-bit_kind_shift) + 1
    ipos = list(i)-ishft((iint-1),bit_kind_shift)-1
    string(iint) = ibset( string(iint), ipos )
  enddo
  
end


subroutine write_bitstring( iunit, string, Nint )
  implicit none
   use bitmasks
  integer, intent(in)            :: iunit
  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: string(Nint)
  
  integer                        :: i, j, ibuf
  integer(bit_kind)              :: itemp
  character*(1)                  :: buffer(Nint*bit_kind_size+2)
  
  ibuf = 1
  buffer(ibuf) = '|'
  ibuf = ibuf+1
  do i=1,Nint
    itemp = 1_bit_kind
    do j=1,bit_kind_size
      if (iand(itemp,string(i)) == itemp) then
        buffer(ibuf) = '+'
      else
        buffer(ibuf) = '-'
      endif
      ibuf = ibuf+1
      itemp = ishft(itemp,1)
    enddo
  enddo
  buffer(ibuf) = '|'
  write(iunit,'(100A)') buffer(1:ibuf)
  
end




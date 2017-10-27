subroutine map_save_to_disk(filename,map)
  use map_module
  use mmap_module
  implicit none
  character*(*), intent(in)      :: filename
  type(map_type), intent(inout)  :: map
  type(c_ptr)                    :: c_pointer(3)
  integer                        :: fd(3)
  integer*8                      :: i,k
  integer                        :: j


  if (map % consolidated) then
    stop 'map already consolidated'
  endif

  call mmap(trim(filename)//'_consolidated_idx', (/ map % map_size + 2_8 /), 8, fd(1), .False., c_pointer(1))
  call c_f_pointer(c_pointer(1),map % consolidated_idx, (/ map % map_size +2_8/))

  call mmap(trim(filename)//'_consolidated_key', (/ map % n_elements /), cache_key_kind, fd(2), .False., c_pointer(2))
  call c_f_pointer(c_pointer(2),map % consolidated_key, (/ map % n_elements /))

  call mmap(trim(filename)//'_consolidated_value', (/ map % n_elements /), integral_kind, fd(3), .False., c_pointer(3))
  call c_f_pointer(c_pointer(3),map % consolidated_value, (/ map % n_elements /))

  if (.not.associated(map%consolidated_key)) then
    stop 'cannot consolidate map : consolidated_key not associated'
  endif

  if (.not.associated(map%consolidated_value)) then
    stop 'cannot consolidate map : consolidated_value not associated'
  endif

  if (.not.associated(map%consolidated_idx)) then
    stop 'cannot consolidate map : consolidated_idx not associated'
  endif

  call map_sort(map)
  k = 1_8
  do i=0_8, map % map_size
    map % consolidated_idx (i+1) = k
    do j=1, map % map(i) % n_elements
      map % consolidated_value(k) = map % map(i) % value(j)
      map % consolidated_key  (k) = map % map(i) % key(j)
      k = k+1_8
    enddo
    deallocate(map % map(i) % value)
    deallocate(map % map(i) % key)
    map % map(i) % value => map % consolidated_value ( map % consolidated_idx (i+1_8) :)
    map % map(i) % key   => map % consolidated_key   ( map % consolidated_idx (i+1_8) :)
  enddo
  map % consolidated_idx (map % map_size + 2_8) = k
  map % consolidated = .True.

  integer*8 :: n_elements
  n_elements = int(map % n_elements,8)

  print *,  'Writing data to disk...'
  call msync ( (/ map % map_size + 2_8 /),    8, fd(1), c_pointer(1))
  call msync ( (/ n_elements /), cache_key_kind, fd(2), c_pointer(2))
  call msync ( (/ n_elements /), integral_kind , fd(3), c_pointer(3))
  print *,  'Done'

end

subroutine map_load_from_disk(filename,map)
  use map_module
  use mmap_module
  implicit none
  character*(*), intent(in)      :: filename
  type(map_type), intent(inout)  :: map
  double precision               :: x
  type(c_ptr)                    :: c_pointer(3)
  integer                        :: fd(3)
  integer*8                      :: i,k,l
  integer*4                      :: j,n_elements

  if (map % consolidated) then
    stop 'map already consolidated'
  endif

  call mmap(trim(filename)//'_consolidated_idx', (/ map % map_size + 2_8 /), 8, fd(1), .True., c_pointer(1))
  call c_f_pointer(c_pointer(1),map % consolidated_idx, (/ map % map_size + 2_8/))

  map% n_elements = map % consolidated_idx (map % map_size+2_8)-1_8

  call mmap(trim(filename)//'_consolidated_key', (/ map % n_elements /), cache_key_kind, fd(2), .True., c_pointer(2))
  call c_f_pointer(c_pointer(2),map % consolidated_key, (/ map % n_elements /))

  call mmap(trim(filename)//'_consolidated_value', (/ map % n_elements /), integral_kind, fd(3), .True., c_pointer(3))
  call c_f_pointer(c_pointer(3),map % consolidated_value, (/ map % n_elements /))

  l = 0_8
  k = 1_8
  x = 0.d0
  do i=0_8, map % map_size
    deallocate(map % map(i) % value)
    deallocate(map % map(i) % key)
    map % map(i) % value => map % consolidated_value ( map % consolidated_idx (i+1_8) :)
    map % map(i) % key   => map % consolidated_key   ( map % consolidated_idx (i+1_8) :)
    map % map(i) % sorted = .True.
    n_elements = int( map % consolidated_idx (i+2_8) - k, 4)
    k = map % consolidated_idx (i+2_8)
    map % map(i) % map_size = n_elements
    map % map(i) % n_elements = n_elements
    ! Load memory from disk
    do j=1,n_elements
      x = x + map % map(i) % value(j)
      l = iand(l,int(map % map(i) % key(j),8))
      if (map % map(i) % value(j) > 1.e30) then
        stop 'Error in integrals file'
      endif
      if (map % map(i) % key(j) < 0) then
        stop 'Error in integrals file'
      endif
    enddo
  enddo
  map % sorted = x>0 .or. l == 0_8
  map % n_elements = k-1_8
  map % sorted = map % sorted .or. .True. 
  map % consolidated = .True.

end


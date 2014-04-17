use map_module

!! AO Map
!! ======

BEGIN_PROVIDER [ type(map_type), ao_integrals_map ]
  implicit none
  BEGIN_DOC
  ! AO integrals
  END_DOC
  integer*8                      :: sze
  call bielec_integrals_index(ao_num,ao_num,ao_num,ao_num,sze)
  call map_init(ao_integrals_map,sze)
  write(output_BiInts,*)  'AO map initialized'
END_PROVIDER

subroutine bielec_integrals_index(i,j,k,l,i1)
  implicit none
  integer, intent(in)            :: i,j,k,l
  integer*8, intent(out)         :: i1
  integer*8                      :: p,q,r,s,i2
  p = min(i,k)
  r = max(i,k)
  p = p+ishft(r*r-r,-1)
  q = min(j,l)
  s = max(j,l)
  q = q+ishft(s*s-s,-1)
  i1 = min(p,q)
  i2 = max(p,q)
  i1 = i1+ishft(i2*i2-i2,-1)
end

double precision function get_ao_bielec_integral(i,j,k,l,map)
  use map_module
  implicit none
  BEGIN_DOC
  ! Gets one AO bi-electronic integral from the AO map
  END_DOC
  integer, intent(in)            :: i,j,k,l
  integer*8                      :: idx
  type(map_type), intent(inout)  :: map
  real(integral_kind)            :: tmp
  PROVIDE ao_bielec_integrals_in_map
  !DIR$ FORCEINLINE
  call bielec_integrals_index(i,j,k,l,idx)
  call map_get(map,idx,tmp)
  get_ao_bielec_integral = tmp
end


subroutine get_ao_bielec_integrals(j,k,l,sze,out_val)
  use map_module
  BEGIN_DOC
  ! Gets multiple AO bi-electronic integral from the AO map .
  ! All i are retrieved for j,k,l fixed.
  END_DOC
  implicit none
  integer, intent(in)            :: j,k,l, sze
  real(integral_kind), intent(out) :: out_val(sze)
  
  integer                        :: i
  integer*8                      :: hash
  double precision               :: thresh
  PROVIDE ao_bielec_integrals_in_map
  thresh = ao_integrals_threshold
  
  if (ao_overlap_abs(j,l) < thresh) then
    out_val = 0.d0
    return
  endif
  
  do i=1,sze
    if (ao_overlap_abs(i,k)*ao_overlap_abs(j,l) < thresh ) then
      out_val(i) = 0.d0
    else
      !DIR$ FORCEINLINE
      call bielec_integrals_index(i,j,k,l,hash)
      call map_get(ao_integrals_map, hash, out_val(i))
    endif
  enddo
  
end

subroutine get_ao_bielec_integrals_non_zero(j,k,l,sze,out_val,out_val_index,non_zero_int)
  use map_module
  implicit none
  BEGIN_DOC
  ! Gets multiple AO bi-electronic integral from the AO map .
  ! All non-zero i are retrieved for j,k,l fixed.
  END_DOC
  integer, intent(in)            :: j,k,l, sze
  real(integral_kind), intent(out) :: out_val(sze)
  integer, intent(out)           :: out_val_index(sze),non_zero_int
  
  integer                        :: i
  integer*8                      :: hash
  double precision               :: thresh,tmp
  PROVIDE ao_bielec_integrals_in_map
  thresh = ao_integrals_threshold
  
  if (ao_overlap_abs(j,l) < thresh) then
    out_val = 0.d0
    return
  endif
  
  non_zero_int = 0
  do i=1,sze
    !DIR$ FORCEINLINE
    call bielec_integrals_index(i,j,k,l,hash)
    call map_get(ao_integrals_map, hash,tmp)
    if (dabs(tmp) < thresh ) cycle
    non_zero_int = non_zero_int+1
    out_val_index(non_zero_int) = i
    out_val(non_zero_int) = tmp
  enddo
  
end


integer*8 function get_ao_map_size()
  implicit none
  BEGIN_DOC
  ! Returns the number of elements in the AO map
  END_DOC
  get_ao_map_size = ao_integrals_map % n_elements
end

subroutine clear_ao_map
  implicit none
  BEGIN_DOC
  ! Frees the memory of the AO map
  END_DOC
  call map_deinit(ao_integrals_map)
  FREE ao_integrals_map
end



!! MO Map
!! ======

BEGIN_PROVIDER [ type(map_type), mo_integrals_map ]
  implicit none
  BEGIN_DOC
  ! MO integrals
  END_DOC
  integer*8                      :: sze
  call bielec_integrals_index(mo_tot_num,mo_tot_num,mo_tot_num,mo_tot_num,sze)
  call map_init(mo_integrals_map,sze)
  write(output_BiInts,*) 'MO map initialized'
END_PROVIDER

subroutine insert_into_ao_integrals_map(n_integrals,                 &
      buffer_i, buffer_values)
  use map_module
  implicit none
  BEGIN_DOC
  ! Create new entry into AO map
  END_DOC
  
  integer, intent(in)            :: n_integrals
  integer*8, intent(inout)       :: buffer_i(n_integrals)
  real(integral_kind), intent(inout) :: buffer_values(n_integrals)
  
  call map_append(ao_integrals_map, buffer_i, buffer_values, n_integrals)
end

subroutine insert_into_mo_integrals_map(n_integrals,                 &
      buffer_i, buffer_values, thr)
  use map_module
  implicit none
  
  BEGIN_DOC
  ! Create new entry into MO map, or accumulate in an existing entry
  END_DOC
  
  integer, intent(in)            :: n_integrals
  integer*8, intent(inout)       :: buffer_i(n_integrals)
  real(integral_kind), intent(inout) :: buffer_values(n_integrals)
  real(integral_kind), intent(in) :: thr
  call map_update(mo_integrals_map, buffer_i, buffer_values, n_integrals, thr)
end

double precision function get_mo_bielec_integral(i,j,k,l,map)
  use map_module
  implicit none
  BEGIN_DOC
  ! Returns one integral <ij|kl> in the MO basis
  END_DOC
  integer, intent(in)            :: i,j,k,l
  integer*8                      :: idx
  type(map_type), intent(inout)  :: map
  real(integral_kind)            :: tmp
  PROVIDE mo_bielec_integrals_in_map
  !DIR$ FORCEINLINE
  call bielec_integrals_index(i,j,k,l,idx)
  call map_get(map,idx,tmp)
  get_mo_bielec_integral = dble(tmp)
end

double precision function mo_bielec_integral(i,j,k,l)
  implicit none
  BEGIN_DOC
  ! Returns one integral <ij|kl> in the MO basis
  END_DOC
  integer, intent(in)            :: i,j,k,l
  double precision               :: get_mo_bielec_integral
  PROVIDE mo_bielec_integrals_in_map
  mo_bielec_integral = get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
  return
end

subroutine get_mo_bielec_integrals(j,k,l,sze,out_val,map)
  use map_module
  implicit none
  BEGIN_DOC
  ! Returns multiple integrals <ij|kl> in the MO basis, all
  ! i for j,k,l fixed.
  END_DOC
  integer, intent(in)            :: j,k,l, sze
  real(integral_kind), intent(out) :: out_val(sze)
  type(map_type), intent(inout)  :: map
  integer                        :: i
  integer*8                      :: hash(sze)
  PROVIDE mo_bielec_integrals_in_map
  
  do i=1,sze
    !DIR$ FORCEINLINE
    call bielec_integrals_index(i,j,k,l,hash(i))
  enddo
  
  call map_get_many(map, hash, out_val, sze)
end

integer*8 function get_mo_map_size()
  implicit none
  BEGIN_DOC
  ! Return the number of elements in the MO map
  END_DOC
  get_mo_map_size = mo_integrals_map % n_elements
end

subroutine clear_mo_map
  implicit none
  BEGIN_DOC
  ! Frees the memory of the MO map
  END_DOC
  call map_deinit(mo_integrals_map)
  FREE mo_integrals_map
end

BEGIN_TEMPLATE

subroutine dump_$ao_integrals(filename)
  use map_module
  implicit none
  BEGIN_DOC
  ! Save to disk the $ao integrals
  END_DOC
  character*(*), intent(in)      :: filename
  integer(cache_key_kind), pointer :: key(:)
  real(integral_kind), pointer   :: val(:)
  integer*8                      :: i,j, n
  open(unit=66,file=filename,FORM='unformatted')
  write(66) integral_kind, key_kind
  write(66) $ao_integrals_map%sorted, $ao_integrals_map%map_size,    &
      $ao_integrals_map%n_elements
  do i=0_8,$ao_integrals_map%map_size
    write(66) $ao_integrals_map%map(i)%sorted, $ao_integrals_map%map(i)%map_size,&
        $ao_integrals_map%map(i)%n_elements
  enddo
  do i=0_8,$ao_integrals_map%map_size
    key => $ao_integrals_map%map(i)%key
    val => $ao_integrals_map%map(i)%value
    n = $ao_integrals_map%map(i)%n_elements
    write(66) (key(j), j=1,n), (val(j), j=1,n)
  enddo
  close(66)
  
end


integer function load_$ao_integrals(filename)
  implicit none
  BEGIN_DOC
  ! Read from disk the $ao integrals
  END_DOC
  character*(*), intent(in)      :: filename
  integer*8                      :: i
  integer(cache_key_kind), pointer :: key(:)
  real(integral_kind), pointer   :: val(:)
  integer                        :: iknd, kknd, n, j
  load_$ao_integrals = 1
  open(unit=66,file=filename,FORM='unformatted',STATUS='UNKNOWN')
  read(66,err=98,end=98) iknd, kknd
  if (iknd /= integral_kind) then
    print *,  'Wrong integrals kind in file :', iknd
    stop 1
  endif
  if (kknd /= key_kind) then
    print *,  'Wrong key kind in file :', kknd
    stop 1
  endif
  read(66,err=98,end=98) $ao_integrals_map%sorted, $ao_integrals_map%map_size,&
      $ao_integrals_map%n_elements
  do i=0_8, $ao_integrals_map%map_size
    read(66,err=99,end=99) $ao_integrals_map%map(i)%sorted,          &
        $ao_integrals_map%map(i)%map_size, $ao_integrals_map%map(i)%n_elements
    call cache_map_reallocate($ao_integrals_map%map(i),$ao_integrals_map%map(i)%map_size)
  enddo
  do i=0_8, $ao_integrals_map%map_size
    key => $ao_integrals_map%map(i)%key
    val => $ao_integrals_map%map(i)%value
    n = $ao_integrals_map%map(i)%n_elements
    read(66,err=99,end=99) (key(j), j=1,n), (val(j), j=1,n)
  enddo
  call map_sort($ao_integrals_map)
  load_$ao_integrals = 0
  return
  99 continue
  call map_deinit($ao_integrals_map)
  FREE $ao_integrals_map
  if (.True.) then
    PROVIDE $ao_integrals_map
  endif
  stop 'Problem reading $ao_integrals_map file in work/'
  98 continue
  
end

SUBST [ ao_integrals_map, ao_integrals, ao_num , get_ao_bielec_integral ]
ao_integrals_map ; ao_integrals ; ao_num ; get_ao_bielec_integral ;;
mo_integrals_map ; mo_integrals ; n_act ; get_mo_bielec_integral ;;
END_TEMPLATE

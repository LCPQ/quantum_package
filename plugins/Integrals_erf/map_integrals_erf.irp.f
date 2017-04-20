use map_module

!! AO Map
!! ======

BEGIN_PROVIDER [ type(map_type), ao_integrals_erf_map ]
  implicit none
  BEGIN_DOC
  ! AO integrals
  END_DOC
  integer(key_kind)              :: key_max
  integer(map_size_kind)         :: sze
  call bielec_integrals_index(ao_num,ao_num,ao_num,ao_num,key_max)
  sze = key_max
  call map_init(ao_integrals_erf_map,sze)
  print*,  'AO map initialized : ', sze
END_PROVIDER

 BEGIN_PROVIDER [ integer, ao_integrals_erf_cache_min ]
&BEGIN_PROVIDER [ integer, ao_integrals_erf_cache_max ]
 implicit none
 BEGIN_DOC
 ! Min and max values of the AOs for which the integrals are in the cache
 END_DOC
 ao_integrals_erf_cache_min = max(1,ao_num - 63)
 ao_integrals_erf_cache_max = ao_num

END_PROVIDER

BEGIN_PROVIDER [ double precision, ao_integrals_erf_cache, (0:64*64*64*64) ]
  use map_module
 implicit none
 BEGIN_DOC
 ! Cache of AO integrals for fast access
 END_DOC
 PROVIDE ao_bielec_integrals_erf_in_map
 integer                        :: i,j,k,l,ii
 integer(key_kind)              :: idx
 real(integral_kind)            :: integral
 !$OMP PARALLEL DO PRIVATE (i,j,k,l,idx,ii,integral)
 do l=ao_integrals_erf_cache_min,ao_integrals_erf_cache_max
   do k=ao_integrals_erf_cache_min,ao_integrals_erf_cache_max
     do j=ao_integrals_erf_cache_min,ao_integrals_erf_cache_max
       do i=ao_integrals_erf_cache_min,ao_integrals_erf_cache_max
         !DIR$ FORCEINLINE
         call bielec_integrals_index(i,j,k,l,idx)
         !DIR$ FORCEINLINE
         call map_get(ao_integrals_erf_map,idx,integral)
         ii = l-ao_integrals_erf_cache_min
         ii = ior( ishft(ii,6), k-ao_integrals_erf_cache_min)
         ii = ior( ishft(ii,6), j-ao_integrals_erf_cache_min)
         ii = ior( ishft(ii,6), i-ao_integrals_erf_cache_min)
         ao_integrals_erf_cache(ii) = integral
       enddo
     enddo
   enddo
 enddo
 !$OMP END PARALLEL DO

END_PROVIDER


double precision function get_ao_bielec_integral_erf(i,j,k,l,map) result(result)
  use map_module
  implicit none
  BEGIN_DOC
  ! Gets one AO bi-electronic integral from the AO map
  END_DOC
  integer, intent(in)            :: i,j,k,l
  integer(key_kind)              :: idx
  type(map_type), intent(inout)  :: map
  integer                        :: ii
  real(integral_kind)            :: tmp
  PROVIDE ao_bielec_integrals_erf_in_map ao_integrals_erf_cache ao_integrals_erf_cache_min
  !DIR$ FORCEINLINE
  if (ao_overlap_abs(i,k)*ao_overlap_abs(j,l) < ao_integrals_threshold ) then
    tmp = 0.d0
  else if (ao_bielec_integral_erf_schwartz(i,k)*ao_bielec_integral_erf_schwartz(j,l) < ao_integrals_threshold) then
    tmp = 0.d0
  else
    ii = l-ao_integrals_erf_cache_min
    ii = ior(ii, k-ao_integrals_erf_cache_min)
    ii = ior(ii, j-ao_integrals_erf_cache_min)
    ii = ior(ii, i-ao_integrals_erf_cache_min)
    if (iand(ii, -64) /= 0) then
      !DIR$ FORCEINLINE
      call bielec_integrals_index(i,j,k,l,idx)
      !DIR$ FORCEINLINE
      call map_get(map,idx,tmp)
      tmp = tmp
    else
      ii = l-ao_integrals_erf_cache_min
      ii = ior( ishft(ii,6), k-ao_integrals_erf_cache_min)
      ii = ior( ishft(ii,6), j-ao_integrals_erf_cache_min)
      ii = ior( ishft(ii,6), i-ao_integrals_erf_cache_min)
      tmp = ao_integrals_erf_cache(ii)
    endif
  endif
  result = tmp
end


subroutine get_ao_bielec_integrals_erf(j,k,l,sze,out_val)
  use map_module
  BEGIN_DOC
  ! Gets multiple AO bi-electronic integral from the AO map .
  ! All i are retrieved for j,k,l fixed.
  END_DOC
  implicit none
  integer, intent(in)            :: j,k,l, sze
  real(integral_kind), intent(out) :: out_val(sze)
  
  integer                        :: i
  integer(key_kind)              :: hash
  double precision               :: thresh
  PROVIDE ao_bielec_integrals_erf_in_map ao_integrals_erf_map
  thresh = ao_integrals_threshold
  
  if (ao_overlap_abs(j,l) < thresh) then
    out_val = 0.d0
    return
  endif
  
  double precision :: get_ao_bielec_integral_erf
  do i=1,sze
    out_val(i) = get_ao_bielec_integral_erf(i,j,k,l,ao_integrals_erf_map)
  enddo
  
end

subroutine get_ao_bielec_integrals_erf_non_zero(j,k,l,sze,out_val,out_val_index,non_zero_int)
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
  integer(key_kind)              :: hash
  double precision               :: thresh,tmp
  PROVIDE ao_bielec_integrals_erf_in_map
  thresh = ao_integrals_threshold
  
  non_zero_int = 0
  if (ao_overlap_abs(j,l) < thresh) then
    out_val = 0.d0
    return
  endif
 
  non_zero_int = 0
  do i=1,sze
    integer, external :: ao_l4
    double precision, external :: ao_bielec_integral_erf
    !DIR$ FORCEINLINE
    if (ao_bielec_integral_erf_schwartz(i,k)*ao_bielec_integral_erf_schwartz(j,l) < thresh) then
      cycle
    endif
    call bielec_integrals_index(i,j,k,l,hash)
    call map_get(ao_integrals_erf_map, hash,tmp)
    if (dabs(tmp) < thresh ) cycle
    non_zero_int = non_zero_int+1
    out_val_index(non_zero_int) = i
    out_val(non_zero_int) = tmp
  enddo
  
end


function get_ao_erf_map_size()
  implicit none
  integer (map_size_kind) :: get_ao_erf_map_size
  BEGIN_DOC
  ! Returns the number of elements in the AO map
  END_DOC
  get_ao_erf_map_size = ao_integrals_erf_map % n_elements
end

subroutine clear_ao_erf_map
  implicit none
  BEGIN_DOC
  ! Frees the memory of the AO map
  END_DOC
  call map_deinit(ao_integrals_erf_map)
  FREE ao_integrals_erf_map
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
  call ezfio_set_work_empty(.False.)
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

IRP_IF COARRAY
subroutine communicate_$ao_integrals()
  use map_module
  implicit none
  BEGIN_DOC
  ! Communicate the $ao integrals with co-array
  END_DOC
  integer(cache_key_kind), pointer :: key(:)
  real(integral_kind), pointer   :: val(:)
  integer*8                      :: i,j, k, nmax
  integer*8, save                :: n[*]
  integer                        :: copy_n

  real(integral_kind), allocatable            :: buffer_val(:)[:]
  integer(cache_key_kind), allocatable        :: buffer_key(:)[:]
  real(integral_kind), allocatable            :: copy_val(:)
  integer(key_kind), allocatable              :: copy_key(:)

  n = 0_8
  do i=0_8,$ao_integrals_map%map_size
    n = max(n,$ao_integrals_map%map(i)%n_elements)
  enddo
  sync all
  nmax = 0_8
  do j=1,num_images()
    nmax = max(nmax,n[j])
  enddo
  allocate( buffer_key(nmax)[*], buffer_val(nmax)[*])
  allocate( copy_key(nmax), copy_val(nmax))
  do i=0_8,$ao_integrals_map%map_size
    key => $ao_integrals_map%map(i)%key
    val => $ao_integrals_map%map(i)%value
    n = $ao_integrals_map%map(i)%n_elements
    do j=1,n
      buffer_key(j) = key(j)
      buffer_val(j) = val(j)
    enddo
    sync all
    do j=1,num_images()
      if (j /= this_image()) then
        copy_n = n[j]
        do k=1,copy_n
          copy_val(k) = buffer_val(k)[j]
          copy_key(k) = buffer_key(k)[j]
          copy_key(k) = copy_key(k)+ishft(i,-map_shift)
        enddo
        call map_append($ao_integrals_map, copy_key, copy_val, copy_n )
      endif
    enddo
    sync all
  enddo
  deallocate( buffer_key, buffer_val, copy_val, copy_key)
  
end
IRP_ENDIF 


integer function load_$ao_integrals(filename)
  implicit none
  BEGIN_DOC
  ! Read from disk the $ao integrals
  END_DOC
  character*(*), intent(in)      :: filename
  integer*8                      :: i
  integer(cache_key_kind), pointer :: key(:)
  real(integral_kind), pointer   :: val(:)
  integer                        :: iknd, kknd
  integer*8                      :: n, j
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
  98 continue
  stop 'Problem reading $ao_integrals_map file in work/'
  
end

SUBST [ ao_integrals_map, ao_integrals, ao_num ]
ao_integrals_erf_map ; ao_integrals_erf ; ao_num ;;
mo_integrals_erf_map ; mo_integrals_erf ; mo_tot_num;;
END_TEMPLATE




BEGIN_PROVIDER [ type(map_type), mo_integrals_erf_map ]
  implicit none
  BEGIN_DOC
  ! MO integrals
  END_DOC
  integer(key_kind)              :: key_max
  integer(map_size_kind)         :: sze
  call bielec_integrals_index(mo_tot_num,mo_tot_num,mo_tot_num,mo_tot_num,key_max)
  sze = key_max
  call map_init(mo_integrals_erf_map,sze)
  print*, 'MO map initialized'
END_PROVIDER

subroutine insert_into_ao_integrals_erf_map(n_integrals,buffer_i, buffer_values)
  use map_module
  implicit none
  BEGIN_DOC
  ! Create new entry into AO map
  END_DOC
  
  integer, intent(in)                :: n_integrals
  integer(key_kind), intent(inout)   :: buffer_i(n_integrals)
  real(integral_kind), intent(inout) :: buffer_values(n_integrals)
  
  call map_append(ao_integrals_erf_map, buffer_i, buffer_values, n_integrals)
end

subroutine insert_into_mo_integrals_erf_map(n_integrals,                 &
      buffer_i, buffer_values, thr)
  use map_module
  implicit none
  
  BEGIN_DOC
  ! Create new entry into MO map, or accumulate in an existing entry
  END_DOC
  
  integer, intent(in)                :: n_integrals
  integer(key_kind), intent(inout)   :: buffer_i(n_integrals)
  real(integral_kind), intent(inout) :: buffer_values(n_integrals)
  real(integral_kind), intent(in)    :: thr
  call map_update(mo_integrals_erf_map, buffer_i, buffer_values, n_integrals, thr)
end

 BEGIN_PROVIDER [ integer, mo_integrals_erf_cache_min ]
&BEGIN_PROVIDER [ integer, mo_integrals_erf_cache_max ]
 implicit none
 BEGIN_DOC
 ! Min and max values of the MOs for which the integrals are in the cache
 END_DOC
 mo_integrals_erf_cache_min = max(1,elec_alpha_num - 31)
 mo_integrals_erf_cache_max = min(mo_tot_num,mo_integrals_erf_cache_min+63)

END_PROVIDER

BEGIN_PROVIDER [ double precision, mo_integrals_erf_cache, (0:64*64*64*64) ]
 implicit none
 BEGIN_DOC
 ! Cache of MO integrals for fast access
 END_DOC
 PROVIDE mo_bielec_integrals_erf_in_map
 integer                        :: i,j,k,l
 integer                        :: ii
 integer(key_kind)              :: idx
 real(integral_kind)            :: integral
 FREE ao_integrals_erf_cache
 !$OMP PARALLEL DO PRIVATE (i,j,k,l,idx,ii,integral)
 do l=mo_integrals_erf_cache_min,mo_integrals_erf_cache_max
   do k=mo_integrals_erf_cache_min,mo_integrals_erf_cache_max
     do j=mo_integrals_erf_cache_min,mo_integrals_erf_cache_max
       do i=mo_integrals_erf_cache_min,mo_integrals_erf_cache_max
         !DIR$ FORCEINLINE
         call bielec_integrals_index(i,j,k,l,idx)
         !DIR$ FORCEINLINE
         call map_get(mo_integrals_erf_map,idx,integral)
         ii = l-mo_integrals_erf_cache_min
         ii = ior( ishft(ii,6), k-mo_integrals_erf_cache_min)
         ii = ior( ishft(ii,6), j-mo_integrals_erf_cache_min)
         ii = ior( ishft(ii,6), i-mo_integrals_erf_cache_min)
         mo_integrals_erf_cache(ii) = integral
       enddo
     enddo
   enddo
 enddo
 !$OMP END PARALLEL DO

END_PROVIDER


double precision function get_mo_bielec_integral_erf(i,j,k,l,map)
  use map_module
  implicit none
  BEGIN_DOC
  ! Returns one integral <ij|kl> in the MO basis
  END_DOC
  integer, intent(in)            :: i,j,k,l
  integer(key_kind)              :: idx
  integer                        :: ii
  type(map_type), intent(inout)  :: map
  real(integral_kind)            :: tmp
  PROVIDE mo_bielec_integrals_erf_in_map mo_integrals_erf_cache
  ii = l-mo_integrals_erf_cache_min
  ii = ior(ii, k-mo_integrals_erf_cache_min)
  ii = ior(ii, j-mo_integrals_erf_cache_min)
  ii = ior(ii, i-mo_integrals_erf_cache_min)
  if (iand(ii, -64) /= 0) then
    !DIR$ FORCEINLINE
    call bielec_integrals_index(i,j,k,l,idx)
    !DIR$ FORCEINLINE
    call map_get(map,idx,tmp)
    get_mo_bielec_integral_erf = dble(tmp)
  else
    ii = l-mo_integrals_erf_cache_min
    ii = ior( ishft(ii,6), k-mo_integrals_erf_cache_min)
    ii = ior( ishft(ii,6), j-mo_integrals_erf_cache_min)
    ii = ior( ishft(ii,6), i-mo_integrals_erf_cache_min)
    get_mo_bielec_integral_erf = mo_integrals_erf_cache(ii)
  endif
end


double precision function mo_bielec_integral_erf(i,j,k,l)
  implicit none
  BEGIN_DOC
  ! Returns one integral <ij|kl> in the MO basis
  END_DOC
  integer, intent(in)            :: i,j,k,l
  double precision               :: get_mo_bielec_integral_erf
  PROVIDE mo_bielec_integrals_erf_in_map mo_integrals_erf_cache
  !DIR$ FORCEINLINE
  PROVIDE mo_bielec_integrals_erf_in_map
  mo_bielec_integral_erf = get_mo_bielec_integral_erf(i,j,k,l,mo_integrals_erf_map)
  return
end

subroutine get_mo_bielec_integrals_erf(j,k,l,sze,out_val,map)
  use map_module
  implicit none
  BEGIN_DOC
  ! Returns multiple integrals <ij|kl> in the MO basis, all
  ! i for j,k,l fixed.
  END_DOC
  integer, intent(in)            :: j,k,l, sze
  double precision, intent(out)  :: out_val(sze)
  type(map_type), intent(inout)  :: map
  integer                        :: i
  integer(key_kind)              :: hash(sze)
  real(integral_kind)            :: tmp_val(sze)
  PROVIDE mo_bielec_integrals_erf_in_map
  
  do i=1,sze
    !DIR$ FORCEINLINE
    call bielec_integrals_index(i,j,k,l,hash(i))
  enddo
  
  if (key_kind == 8) then
    call map_get_many(map, hash, out_val, sze)
  else
    call map_get_many(map, hash, tmp_val, sze)
    ! Conversion to double precision 
    do i=1,sze
      out_val(i) = dble(tmp_val(i))
    enddo
  endif
end

subroutine get_mo_bielec_integrals_erf_ij(k,l,sze,out_array,map)
  use map_module
  implicit none
  BEGIN_DOC
  ! Returns multiple integrals <ij|kl> in the MO basis, all
  ! i(1)j(2) 1/r12 k(1)l(2)
  ! i, j for k,l fixed.
  END_DOC
  integer, intent(in)            :: k,l, sze
  double precision, intent(out)  :: out_array(sze,sze)
  type(map_type), intent(inout)  :: map
  integer                        :: i,j,kk,ll,m
  integer(key_kind),allocatable  :: hash(:)
  integer  ,allocatable          :: pairs(:,:), iorder(:)
  real(integral_kind), allocatable :: tmp_val(:)

  PROVIDE mo_bielec_integrals_erf_in_map
  allocate (hash(sze*sze), pairs(2,sze*sze),iorder(sze*sze), &
  tmp_val(sze*sze))
  
  kk=0
  out_array = 0.d0
  do j=1,sze
   do i=1,sze
    kk += 1
    !DIR$ FORCEINLINE
    call bielec_integrals_index(i,j,k,l,hash(kk))
    pairs(1,kk) = i
    pairs(2,kk) = j
    iorder(kk) = kk
   enddo
  enddo

  logical :: integral_is_in_map
  if (key_kind == 8) then
    call i8radix_sort(hash,iorder,kk,-1)
  else if (key_kind == 4) then
    call iradix_sort(hash,iorder,kk,-1)
  else if (key_kind == 2) then
    call i2radix_sort(hash,iorder,kk,-1)
  endif

  call map_get_many(mo_integrals_erf_map, hash, tmp_val, kk)

  do ll=1,kk
    m = iorder(ll)
    i=pairs(1,m)
    j=pairs(2,m)
    out_array(i,j) = tmp_val(ll)
  enddo  

  deallocate(pairs,hash,iorder,tmp_val)
end

subroutine get_mo_bielec_integrals_erf_coulomb_ii(k,l,sze,out_val,map)
  use map_module
  implicit none
  BEGIN_DOC
  ! Returns multiple integrals <ki|li> 
  ! k(1)i(2) 1/r12 l(1)i(2) :: out_val(i1)
  ! for k,l fixed.
  END_DOC
  integer, intent(in)            :: k,l, sze
  double precision, intent(out)  :: out_val(sze)
  type(map_type), intent(inout)  :: map
  integer                        :: i
  integer(key_kind)              :: hash(sze)
  real(integral_kind)            :: tmp_val(sze)
  PROVIDE mo_bielec_integrals_erf_in_map
  
  integer :: kk
  do i=1,sze
    !DIR$ FORCEINLINE
    call bielec_integrals_index(k,i,l,i,hash(i))
  enddo
  
  if (key_kind == 8) then
    call map_get_many(map, hash, out_val, sze)
  else
    call map_get_many(map, hash, tmp_val, sze)
    ! Conversion to double precision 
    do i=1,sze
      out_val(i) = dble(tmp_val(i))
    enddo
  endif
end

subroutine get_mo_bielec_integrals_erf_exch_ii(k,l,sze,out_val,map)
  use map_module
  implicit none
  BEGIN_DOC
  ! Returns multiple integrals <ki|il> 
  ! k(1)i(2) 1/r12 i(1)l(2) :: out_val(i1)
  ! for k,l fixed.
  END_DOC
  integer, intent(in)            :: k,l, sze
  double precision, intent(out)  :: out_val(sze)
  type(map_type), intent(inout)  :: map
  integer                        :: i
  integer(key_kind)              :: hash(sze)
  real(integral_kind)            :: tmp_val(sze)
  PROVIDE mo_bielec_integrals_erf_in_map
  
  integer :: kk
  do i=1,sze
    !DIR$ FORCEINLINE
    call bielec_integrals_index(k,i,i,l,hash(i))
  enddo
  
  if (key_kind == 8) then
    call map_get_many(map, hash, out_val, sze)
  else
    call map_get_many(map, hash, tmp_val, sze)
    ! Conversion to double precision 
    do i=1,sze
      out_val(i) = dble(tmp_val(i))
    enddo
  endif
end


integer*8 function get_mo_erf_map_size()
  implicit none
  BEGIN_DOC
  ! Return the number of elements in the MO map
  END_DOC
  get_mo_erf_map_size = mo_integrals_erf_map % n_elements
end

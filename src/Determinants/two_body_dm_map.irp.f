
use map_module

BEGIN_PROVIDER [ type(map_type), two_body_dm_ab_map ]
  implicit none
  BEGIN_DOC
  ! Map of the two body density matrix elements for the alpha/beta elements
  END_DOC
  integer(key_kind)              :: key_max
  integer(map_size_kind)         :: sze
  call bielec_integrals_index(mo_tot_num,mo_tot_num,mo_tot_num,mo_tot_num,key_max)
  sze = key_max
  call map_init(two_body_dm_ab_map,sze)
  print*, 'two_body_dm_ab_map initialized'
END_PROVIDER

subroutine insert_into_two_body_dm_ab_map(n_product,buffer_i, buffer_values, thr)
  use map_module
  implicit none
  
  BEGIN_DOC
  ! Create new entry into two_body_dm_ab_map, or accumulate in an existing entry
  END_DOC
  
  integer, intent(in)                :: n_product 
  integer(key_kind), intent(inout)   :: buffer_i(n_product)
  real(integral_kind), intent(inout) :: buffer_values(n_product)
  real(integral_kind), intent(in)    :: thr
  call map_update(two_body_dm_ab_map, buffer_i, buffer_values, n_product, thr)
end

double precision function get_two_body_dm_ab_map_element(i,j,k,l,map)
  use map_module
  implicit none
  BEGIN_DOC
  ! Returns one value of the wo body density matrix \rho_{ijkl}^{\alpha \beta} defined as :
  ! \rho_{ijkl}^{\alpha \beta  } = <\Psi|a^{\dagger}_{i\alpha} a^{\dagger}_{j\beta} a_{k\beta} a_{l\alpha}|\Psi>
  END_DOC
  PROVIDE two_body_dm_ab_map

  integer, intent(in)            :: i,j,k,l
  integer(key_kind)              :: idx
  type(map_type), intent(inout)  :: map
  real(integral_kind)            :: tmp
  PROVIDE two_body_dm_in_map
  !DIR$ FORCEINLINE
  call bielec_integrals_index(i,j,k,l,idx)
  !DIR$ FORCEINLINE
  call map_get(two_body_dm_ab_map,idx,tmp)
  get_two_body_dm_ab_map_element = dble(tmp)
end

subroutine get_get_two_body_dm_ab_map_elements(j,k,l,sze,out_val,map)
  use map_module
  implicit none
  BEGIN_DOC
  ! Returns multiple elements of the \rho_{ijkl}^{\alpha \beta }, all
  ! i for j,k,l fixed.
  END_DOC
  integer, intent(in)            :: j,k,l, sze
  double precision, intent(out)  :: out_val(sze)
  type(map_type), intent(inout)  :: map
  integer                        :: i
  integer(key_kind)              :: hash(sze)
  real(integral_kind)            :: tmp_val(sze)
  PROVIDE two_body_dm_in_map
  
  do i=1,sze
    !DIR$ FORCEINLINE
    call bielec_integrals_index(i,j,k,l,hash(i))
  enddo
  
  if (key_kind == 8) then
    call map_get_many(two_body_dm_ab_map, hash, out_val, sze)
  else
    call map_get_many(two_body_dm_ab_map, hash, tmp_val, sze)
    ! Conversion to double precision 
    do i=1,sze
      out_val(i) = dble(tmp_val(i))
    enddo
  endif
end

BEGIN_PROVIDER [ logical, two_body_dm_in_map ]
  implicit none

  BEGIN_DOC
  ! If True, the map of the two body density matrix alpha/beta is provided
  END_DOC

  two_body_dm_in_map = .True.
  call add_values_to_two_body_dm_map(full_ijkl_bitmask_4)
END_PROVIDER

subroutine add_values_to_two_body_dm_map(mask_ijkl)
  use bitmasks
  use map_module
  implicit none

  BEGIN_DOC
  ! Adds values to the map of two_body_dm according to some bitmask
  END_DOC

  integer(bit_kind), intent(in)  :: mask_ijkl(N_int,4)
  integer                        :: degree

  PROVIDE mo_coef psi_coef psi_det

  integer                        :: exc(0:2,2,2)
  integer                        :: h1,h2,p1,p2,s1,s2
  double precision               :: phase
  double precision               :: contrib
  integer(key_kind),allocatable  :: buffer_i(:)
  double precision ,allocatable  :: buffer_value(:)
  integer                        :: size_buffer
  integer                        :: n_elements
  integer                        :: occ(N_int*bit_kind_size,2)
  integer                        :: n_occ_ab(2)
  integer :: i,j,k,l,m
  
  size_buffer = min(mo_tot_num*mo_tot_num*mo_tot_num,16000000)

  allocate(buffer_i(size_buffer),buffer_value(size_buffer))

  n_elements = 0
  do i = 1, N_det ! i == |I>
   call bitstring_to_list_ab(psi_det(1,1,i), occ, n_occ_ab, N_int)
   do j = i+1, N_det ! j == <J|
    call get_excitation_degree(psi_det(1,1,i),psi_det(1,1,j),degree,N_int)
    if(degree>2)cycle
    call get_excitation(psi_det(1,1,i),psi_det(1,1,j),exc,degree,phase,N_int)
    contrib = psi_coef(i,1) * psi_coef(j,1) * phase
    call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
    if(degree==2)then  ! case of the DOUBLE EXCITATIONS  ************************************

     if(s1==s2)cycle  ! Only the alpha/beta two body density matrix
     ! <J| a^{\dagger}_{p1 s1} a^{\dagger}_{p2 s2} a_{h2 s2} a_{h1 s1} |I> * c_I * c_J
     n_elements += 1
     buffer_value(n_elements) = contrib
     !DEC$ FORCEINLINE
     call mo_bielec_integrals_index(h1,h2,p1,p2,buffer_i(n_elements))
     if (n_elements == size_buffer) then
       call insert_into_two_body_dm_ab_map(n_elements,buffer_i,buffer_value,&
           real(mo_integrals_threshold,integral_kind))
       n_elements = 0
     endif

    else ! case of the SINGLE EXCITATIONS  ***************************************************

     if(s1==1)then  ! Mono alpha : 
      do k = 1, elec_beta_num  
       m = occ(k,2)
       n_elements += 1
       buffer_value(n_elements) = contrib
       ! <J| a^{\dagger}_{p1 \alpha} \hat{n}_{m \beta} a_{h1 \alpha} |I> * c_I * c_J
       call mo_bielec_integrals_index(h1,m,p1,m,buffer_i(n_elements))
       if (n_elements == size_buffer) then
         call insert_into_two_body_dm_ab_map(n_elements,buffer_i,buffer_value,&
             real(mo_integrals_threshold,integral_kind))
         n_elements = 0
       endif
      enddo
     else  ! Mono Beta : 
      do k = 1, elec_alpha_num
       m = occ(k,1)
       n_elements += 1
       buffer_value(n_elements) = contrib
       ! <J| a^{\dagger}_{p1 \beta} \hat{n}_{m \alpha} a_{h1 \beta} |I> * c_I * c_J
       call mo_bielec_integrals_index(h1,m,p1,m,buffer_i(n_elements))
       if (n_elements == size_buffer) then
         call insert_into_two_body_dm_ab_map(n_elements,buffer_i,buffer_value,&
             real(mo_integrals_threshold,integral_kind))
         n_elements = 0
       endif
      enddo
     endif

    endif
   enddo
  enddo
  print*,'n_elements = ',n_elements
  call insert_into_two_body_dm_ab_map(n_elements,buffer_i,buffer_value,&
      real(mo_integrals_threshold,integral_kind))

end

BEGIN_PROVIDER [double precision, two_body_dm_ab_diag, (mo_tot_num, mo_tot_num)]
 implicit none
 integer :: i,j,k,l,m
 integer                        :: occ(N_int*bit_kind_size,2)
 integer                        :: n_occ_ab(2)
 double precision :: contrib
 BEGIN_DOC 
 ! two_body_dm_ab_diag(k,m) = <\Psi | n_(k\alpha) n_(m\beta) | \Psi>

 END_DOC
 two_body_dm_ab_diag = 0.d0
  do i = 1, N_det ! i == |I>
   call bitstring_to_list_ab(psi_det(1,1,i), occ, n_occ_ab, N_int)
   contrib = psi_coef(i,1)**2
   do j = 1, elec_beta_num  
    k = occ(j,2)
    do l = 1, elec_beta_num  
     m = occ(l,1)
     two_body_dm_ab_diag(k,m) += contrib
    enddo
   enddo
  enddo
END_PROVIDER 


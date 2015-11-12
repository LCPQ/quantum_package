!==============================================================================!
!                                                                              !
!                          Independent alpha/beta parts                        !
!                                                                              !
!==============================================================================!

use bitmasks

integer*8 function spin_det_search_key(det,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
! Return an integer*8 corresponding to a determinant index for searching
  END_DOC
  integer, intent(in) :: Nint
  integer(bit_kind), intent(in) :: det(Nint)
  integer(bit_kind), parameter :: unsigned_shift = not(huge(1_bit_kind)) ! 100...00
  integer :: i
  spin_det_search_key = det(1)
  do i=2,Nint
    spin_det_search_key = ieor(spin_det_search_key,det(i))
  enddo
  spin_det_search_key = spin_det_search_key-unsigned_shift
end


BEGIN_PROVIDER [ integer(bit_kind), psi_det_alpha, (N_int,psi_det_size) ]
 implicit none
 BEGIN_DOC
! List of alpha determinants of psi_det
 END_DOC
 integer :: i,k

 do i=1,N_det
   do k=1,N_int
     psi_det_alpha(k,i) = psi_det(k,1,i)
   enddo
 enddo
END_PROVIDER

BEGIN_PROVIDER [ integer(bit_kind), psi_det_beta, (N_int,psi_det_size) ]
 implicit none
 BEGIN_DOC
! List of beta determinants of psi_det
 END_DOC
 integer :: i,k

 do i=1,N_det
   do k=1,N_int
     psi_det_beta(k,i) = psi_det(k,2,i)
   enddo
 enddo
END_PROVIDER


BEGIN_TEMPLATE

 BEGIN_PROVIDER [ integer(bit_kind), psi_det_$alpha_unique, (N_int,psi_det_size) ]
&BEGIN_PROVIDER [ integer, N_det_$alpha_unique ]
 implicit none
 BEGIN_DOC
 ! Unique $alpha determinants
 END_DOC

 integer                        :: i,j,k
 integer, allocatable           :: iorder(:)
 integer*8, allocatable         :: bit_tmp(:)
 integer*8                      :: last_key
 integer*8, external            :: spin_det_search_key
 logical,allocatable            :: duplicate(:)

 allocate ( iorder(N_det), bit_tmp(N_det), duplicate(N_det) )

 do i=1,N_det
   iorder(i) = i
   bit_tmp(i) = spin_det_search_key(psi_det_$alpha(1,i),N_int)
 enddo

 call i8sort(bit_tmp,iorder,N_det)

 N_det_$alpha_unique = 0
 last_key = 0_8
 do i=1,N_det
   last_key = bit_tmp(i)
   N_det_$alpha_unique += 1
   do k=1,N_int
     psi_det_$alpha_unique(k,N_det_$alpha_unique) = psi_det_$alpha(k,iorder(i))
   enddo
   duplicate(i) = .False.
 enddo

 j=1
 do i=1,N_det_$alpha_unique-1
   if (duplicate(i)) then
     cycle
   endif
   j = i+1
   do while (bit_tmp(j)==bit_tmp(i))
     if (duplicate(j)) then
       j += 1
       cycle
     endif
     duplicate(j) = .True.
     do k=1,N_int
       if (psi_det_$alpha_unique(k,i) /= psi_det_$alpha_unique(k,j)) then
         duplicate(j) = .False.
         exit
       endif
     enddo
     j+=1
     if (j > N_det_$alpha_unique) then
       exit
     endif
   enddo
 enddo

 j=1
 do i=2,N_det_$alpha_unique
   if (duplicate(i)) then
     cycle
  else
    j += 1
    psi_det_$alpha_unique(:,j) = psi_det_$alpha_unique(:,i)
  endif
 enddo
 N_det_$alpha_unique = j

 deallocate (iorder, bit_tmp, duplicate)
END_PROVIDER

SUBST [ alpha ]

alpha ;;
beta ;;

END_TEMPLATE




integer function get_index_in_psi_det_alpha_unique(key,Nint)
  use bitmasks
  BEGIN_DOC
! Returns the index of the determinant in the ``psi_det_alpha_unique`` array
  END_DOC
  implicit none

  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: key(Nint)

  integer                        :: i, ibegin, iend, istep, l
  integer*8                      :: det_ref, det_search
  integer*8, external            :: spin_det_search_key
  logical                        :: in_wavefunction

  in_wavefunction = .False.
  get_index_in_psi_det_alpha_unique = 0
  ibegin = 1
  iend   = N_det_alpha_unique + 1

  !DIR$ FORCEINLINE
  det_ref = spin_det_search_key(key,Nint)

  !DIR$ FORCEINLINE
  det_search = spin_det_search_key(psi_det_alpha_unique(1,1),Nint)

  istep = ishft(iend-ibegin,-1)
  i=ibegin+istep
  do while (istep > 0)
    !DIR$ FORCEINLINE
    det_search = spin_det_search_key(psi_det_alpha_unique(1,i),Nint)
    if ( det_search > det_ref ) then
      iend = i
    else if ( det_search == det_ref ) then
      exit
    else
      ibegin = i
    endif
    istep = ishft(iend-ibegin,-1)
    i = ibegin + istep
  end do

  !DIR$ FORCEINLINE
  do while (spin_det_search_key(psi_det_alpha_unique(1,i),Nint) == det_ref)
    i = i-1
    if (i == 0) then
      exit
    endif
  enddo
  i += 1

  if (i > N_det_alpha_unique) then
    return
  endif

  !DIR$ FORCEINLINE
  do while (spin_det_search_key(psi_det_alpha_unique(1,i),Nint) == det_ref)
    if (key(1) /= psi_det_alpha_unique(1,i)) then
      continue
    else
      in_wavefunction = .True.
      !DIR$ IVDEP
      !DIR$ LOOP COUNT MIN(3)
      do l=2,Nint
        if (key(l) /= psi_det_alpha_unique(l,i)) then
          in_wavefunction = .False.
        endif
      enddo
      if (in_wavefunction) then
        get_index_in_psi_det_alpha_unique = i
        return
      endif
    endif
    i += 1
    if (i > N_det_alpha_unique) then
      return
    endif

  enddo

end

integer function get_index_in_psi_det_beta_unique(key,Nint)
  use bitmasks
  BEGIN_DOC
! Returns the index of the determinant in the ``psi_det_beta_unique`` array
  END_DOC
  implicit none

  integer, intent(in)            :: Nint
  integer(bit_kind), intent(in)  :: key(Nint)

  integer                        :: i, ibegin, iend, istep, l
  integer*8                      :: det_ref, det_search
  integer*8, external            :: spin_det_search_key
  logical                        :: in_wavefunction

  in_wavefunction = .False.
  get_index_in_psi_det_beta_unique = 0
  ibegin = 1
  iend   = N_det_beta_unique + 1

  !DIR$ FORCEINLINE
  det_ref = spin_det_search_key(key,Nint)
  !DIR$ FORCEINLINE
  det_search = spin_det_search_key(psi_det_beta_unique(1,1),Nint)

  istep = ishft(iend-ibegin,-1)
  i=ibegin+istep
  do while (istep > 0)
    !DIR$ FORCEINLINE
    det_search = spin_det_search_key(psi_det_beta_unique(1,i),Nint)
    if ( det_search > det_ref ) then
      iend = i
    else if ( det_search == det_ref ) then
      exit
    else
      ibegin = i
    endif
    istep = ishft(iend-ibegin,-1)
    i = ibegin + istep
  end do

  !DIR$ FORCEINLINE
  do while (spin_det_search_key(psi_det_beta_unique(1,i),Nint) == det_ref)
    i = i-1
    if (i == 0) then
      exit
    endif
  enddo
  i += 1

  if (i > N_det_beta_unique) then
    return
  endif

  !DIR$ FORCEINLINE
  do while (spin_det_search_key(psi_det_beta_unique(1,i),Nint) == det_ref)
    if (key(1) /= psi_det_beta_unique(1,i)) then
      continue
    else
      in_wavefunction = .True.
      !DIR$ IVDEP
      !DIR$ LOOP COUNT MIN(3)
      do l=2,Nint
        if (key(l) /= psi_det_beta_unique(l,i)) then
          in_wavefunction = .False.
        endif
      enddo
      if (in_wavefunction) then
        get_index_in_psi_det_beta_unique = i
        return
      endif
    endif
    i += 1
    if (i > N_det_beta_unique) then
      return
    endif

  enddo

end


subroutine write_spindeterminants
  use bitmasks
  implicit none
  integer*8, allocatable         :: tmpdet(:,:)
  integer                        :: N_int2
  integer                        :: i,j,k
  integer*8                      :: det_8(100)
  integer(bit_kind)              :: det_bk((100*8)/bit_kind)
  equivalence (det_8, det_bk)

  N_int2 = (N_int*bit_kind)/8
  call ezfio_set_spindeterminants_n_det_alpha(N_det_alpha_unique)
  call ezfio_set_spindeterminants_n_det_beta(N_det_beta_unique)
  call ezfio_set_spindeterminants_n_det(N_det)
  call ezfio_set_spindeterminants_n_int(N_int)
  call ezfio_set_spindeterminants_bit_kind(bit_kind)
  call ezfio_set_spindeterminants_n_states(N_states)

  allocate(tmpdet(N_int2,N_det_alpha_unique))
  do i=1,N_det_alpha_unique
    do k=1,N_int
      det_bk(k) = psi_det_alpha_unique(k,i)
    enddo
    do k=1,N_int2
      tmpdet(k,i) = det_8(k)
    enddo
  enddo
  call ezfio_set_spindeterminants_psi_det_alpha(psi_det_alpha_unique)
  deallocate(tmpdet)
  
  allocate(tmpdet(N_int2,N_det_beta_unique))
  do i=1,N_det_beta_unique
    do k=1,N_int
      det_bk(k) = psi_det_beta_unique(k,i)
    enddo
    do k=1,N_int2
      tmpdet(k,i) = det_8(k)
    enddo
  enddo
  call ezfio_set_spindeterminants_psi_det_beta(psi_det_beta_unique)
  deallocate(tmpdet)
  
  call ezfio_set_spindeterminants_psi_coef_matrix_values(psi_bilinear_matrix_values)
  call ezfio_set_spindeterminants_psi_coef_matrix_rows(psi_bilinear_matrix_rows)
  call ezfio_set_spindeterminants_psi_coef_matrix_columns(psi_bilinear_matrix_columns)
  
end

 BEGIN_PROVIDER [ double precision, det_alpha_norm, (N_det_alpha_unique) ]
&BEGIN_PROVIDER [ double precision, det_beta_norm, (N_det_beta_unique) ]
 implicit none
 BEGIN_DOC
 ! Norm of the alpha and beta spin determinants in the wave function:
 !
 ! ||Da||_i \sum_j C_{ij}**2
 END_DOC

 integer :: i,j,k,l
 double precision :: f

 det_alpha_norm = 0.d0
 det_beta_norm  = 0.d0
 do k=1,N_det
   i = psi_bilinear_matrix_rows(k)
   j = psi_bilinear_matrix_columns(k)
   do l=1,N_states
    f = psi_bilinear_matrix_values(k,l)*psi_bilinear_matrix_values(k,l)
   enddo
   det_alpha_norm(i) += f
   det_beta_norm(j)  += f
 enddo
 det_alpha_norm = det_alpha_norm / dble(N_states)
 det_beta_norm = det_beta_norm / dble(N_states)

END_PROVIDER


!==============================================================================!
!                                                                              !
!                               Alpha x Beta Matrix                            !
!                                                                              !
!==============================================================================!

BEGIN_PROVIDER  [ double precision, psi_bilinear_matrix_values, (N_det,N_states) ]
&BEGIN_PROVIDER [ integer, psi_bilinear_matrix_rows, (N_det) ]
&BEGIN_PROVIDER [ integer, psi_bilinear_matrix_columns, (N_det) ]
  use bitmasks
  implicit none
  BEGIN_DOC
! Sparse coefficient matrix if the wave function is expressed in a bilinear form :
!  D_a^t C D_b
  END_DOC
  integer                        :: i,j,k, l
  integer(bit_kind)               :: tmp_det(N_int,2)
  integer                        :: idx
  integer, external              :: get_index_in_psi_det_sorted_bit


  PROVIDE psi_coef_sorted_bit

  integer, allocatable :: iorder(:), to_sort(:)
  integer, external :: get_index_in_psi_det_alpha_unique
  integer, external :: get_index_in_psi_det_beta_unique
  allocate(iorder(N_det), to_sort(N_det))
  do k=1,N_det
    i = get_index_in_psi_det_alpha_unique(psi_det(1,1,k),N_int)
    j = get_index_in_psi_det_beta_unique (psi_det(1,2,k),N_int)

    do l=1,N_states
      psi_bilinear_matrix_values(k,l) = psi_coef(k,l)
    enddo
    psi_bilinear_matrix_rows(k) = i
    psi_bilinear_matrix_columns(k) = j
    to_sort(k) = N_det_alpha_unique * (j-1) + i
    iorder(k) = k
  enddo
  call isort(to_sort, iorder, N_det)
  call iset_order(psi_bilinear_matrix_rows,iorder,N_det)
  call iset_order(psi_bilinear_matrix_columns,iorder,N_det)
  call dset_order(psi_bilinear_matrix_values,iorder,N_det)
  deallocate(iorder,to_sort)
END_PROVIDER

BEGIN_PROVIDER [ double precision, psi_bilinear_matrix, (N_det_alpha_unique,N_det_beta_unique,N_states) ]
  implicit none
  BEGIN_DOC
! Coefficient matrix if the wave function is expressed in a bilinear form :
!  D_a^t C D_b
  END_DOC
  integer :: i,j,k,istate
  psi_bilinear_matrix = 0.d0
  do k=1,N_det
    i = psi_bilinear_matrix_rows(k)
    j = psi_bilinear_matrix_columns(k)
    do istate=1,N_states
      psi_bilinear_matrix(i,j,istate) = psi_bilinear_matrix_values(k,istate)
    enddo
  enddo
END_PROVIDER

subroutine create_wf_of_psi_bilinear_matrix(truncate)
  use bitmasks
  implicit none
  BEGIN_DOC
! Generate a wave function containing all possible products 
! of alpha and beta determinants
  END_DOC
  logical, intent(in)            :: truncate
  integer                        :: i,j,k
  integer(bit_kind)              :: tmp_det(N_int,2)
  integer                        :: idx
  integer, external              :: get_index_in_psi_det_sorted_bit
  double precision               :: norm(N_states)

  call generate_all_alpha_beta_det_products
  norm = 0.d0
  do j=1,N_det_beta_unique
    do k=1,N_int
      tmp_det(k,2) = psi_det_beta_unique(k,j)
    enddo
    do i=1,N_det_alpha_unique
      do k=1,N_int
        tmp_det(k,1) = psi_det_alpha_unique(k,i)
      enddo
      idx = get_index_in_psi_det_sorted_bit(tmp_det,N_int)
      if (idx > 0) then
        do k=1,N_states
          psi_coef_sorted_bit(idx,k) = psi_bilinear_matrix(i,j,k) 
          norm(k) += psi_bilinear_matrix(i,j,k)
        enddo
      endif
    enddo
  enddo
  do k=1,N_states
    norm(k) = 1.d0/dsqrt(norm(k))
    do i=1,N_det
      psi_coef_sorted_bit(i,k) = psi_coef_sorted_bit(i,k)*norm(k)
    enddo
  enddo
  psi_det  = psi_det_sorted_bit
  psi_coef = psi_coef_sorted_bit
  TOUCH psi_det psi_coef
  psi_det  = psi_det_sorted
  psi_coef = psi_coef_sorted
  norm(1) = 0.d0
  do i=1,N_det
    norm(1) += psi_average_norm_contrib_sorted(i)
    if (truncate) then
      if (norm(1) >= 0.999999d0) then
        exit
      endif
    endif
  enddo
  N_det = min(i,N_det)
  SOFT_TOUCH psi_det psi_coef N_det

end

subroutine generate_all_alpha_beta_det_products
  implicit none
  BEGIN_DOC
!  Create a wave function from all possible alpha x beta determinants
  END_DOC
  integer                        :: i,j,k,l
  integer                        :: idx, iproc
  integer, external              :: get_index_in_psi_det_sorted_bit
  integer(bit_kind), allocatable :: tmp_det(:,:,:)
  logical, external              :: is_in_wavefunction
  integer, external              :: omp_get_thread_num

  !$OMP PARALLEL DEFAULT(NONE) SHARED(psi_coef_sorted_bit,N_det_beta_unique,&
      !$OMP N_det_alpha_unique, N_int, psi_det_alpha_unique, psi_det_beta_unique,&
      !$OMP N_det)                                                &
      !$OMP PRIVATE(i,j,k,l,tmp_det,idx,iproc)
  !$ iproc = omp_get_thread_num()
  allocate (tmp_det(N_int,2,N_det_alpha_unique))
  !$OMP DO
  do j=1,N_det_beta_unique
    l = 1
    do i=1,N_det_alpha_unique
      do k=1,N_int
        tmp_det(k,1,l) = psi_det_alpha_unique(k,i)
        tmp_det(k,2,l) = psi_det_beta_unique (k,j)
      enddo
      if (.not.is_in_wavefunction(tmp_det(1,1,l),N_int)) then
        l = l+1
      endif
    enddo
    call fill_H_apply_buffer_no_selection(l-1, tmp_det, N_int, iproc)
  enddo
  !$OMP END DO NOWAIT
  deallocate(tmp_det)
  !$OMP END PARALLEL
  call copy_H_apply_buffer_to_wf
  SOFT_TOUCH psi_det psi_coef N_det
end



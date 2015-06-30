subroutine save_mos
  implicit none
  double precision, allocatable  :: buffer(:,:)
  integer                        :: i,j
  
  call system('$QP_ROOT/scripts/save_current_mos.sh '//trim(ezfio_filename))
  
  call ezfio_set_mo_basis_mo_tot_num(mo_tot_num)
  call ezfio_set_mo_basis_mo_label(mo_label)
  call ezfio_set_mo_basis_ao_md5(ao_md5)
  allocate ( buffer(ao_num,mo_tot_num) )
  buffer = 0.d0
  do j = 1, mo_tot_num
    do i = 1, ao_num
      buffer(i,j) = mo_coef(i,j)
    enddo
  enddo
  call ezfio_set_mo_basis_mo_coef(buffer)
  call ezfio_set_mo_basis_mo_occ(mo_occ)
  deallocate (buffer)
  
end

subroutine mo_as_eigvectors_of_mo_matrix(matrix,n,m,label)
  implicit none
  integer,intent(in)             :: n,m
  character*(64), intent(in)     :: label
  double precision, intent(in)   :: matrix(n,m)
  
  double precision, allocatable  :: mo_coef_new(:,:), R(:,:),eigvalues(:)
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: mo_coef_new, R
  
  call write_time(output_mo_basis)
  if (m /= mo_tot_num) then
    print *, irp_here, ': Error : m/= mo_tot_num'
    stop 1
  endif
  allocate(R(n,m),mo_coef_new(ao_num_align,m),eigvalues(m))
  mo_coef_new = mo_coef
  
  call lapack_diag(eigvalues,R,matrix,size(matrix,1),size(matrix,2))
  integer :: i
  write (output_mo_basis,'(A)'), 'MOs are now **'//trim(label)//'**'
  write (output_mo_basis,'(A)'), ''
  write (output_mo_basis,'(A)'), 'Eigenvalues'
  write (output_mo_basis,'(A)'), '-----------'
  write (output_mo_basis,'(A)'), ''
  write (output_mo_basis,'(A)'), '======== ================'
  do i = 1, m
   write (output_mo_basis,'(I8,X,F16.10)'), i,eigvalues(i)
  enddo
  write (output_mo_basis,'(A)'), '======== ================'
  write (output_mo_basis,'(A)'), ''
  
  call dgemm('N','N',ao_num,m,m,1.d0,mo_coef_new,size(mo_coef_new,1),R,size(R,1),0.d0,mo_coef,size(mo_coef,1))
  deallocate(mo_coef_new,R,eigvalues)
  call write_time(output_mo_basis)
  
  mo_label = label
  SOFT_TOUCH mo_coef mo_label
end
subroutine mo_as_eigvectors_of_mo_matrix_sort_by_observable(matrix,observable,n,m,label)
  implicit none
  integer,intent(in)             :: n,m
  character*(64), intent(in)     :: label
  double precision, intent(in)   :: matrix(n,m),observable(n,n)
  
  double precision, allocatable  :: mo_coef_new(:,:), R(:,:),eigvalues(:),value(:)
  integer,allocatable :: iorder(:)
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: mo_coef_new, R
  
  call write_time(output_mo_basis)
  if (m /= mo_tot_num) then
    print *, irp_here, ': Error : m/= mo_tot_num'
    stop 1
  endif
  allocate(R(n,m),mo_coef_new(ao_num_align,m),eigvalues(m),value(m),iorder(m))
  mo_coef_new = mo_coef
  
  call lapack_diag(eigvalues,R,matrix,size(matrix,1),size(matrix,2))

  do i = 1, mo_tot_num
   value(i) = 0.d0
   iorder(i) = i
   do j = 1, mo_tot_num
    do k = 1, mo_tot_num
     value(i) += R(k,i) * R(j,i) * observable(k,j)
    enddo
   enddo
!  print*,'value(i) = ',i,value(i)
  enddo
  integer :: i,j,k,index
  double precision :: R_tmp(m,m),obs(m)
  print*,'sort ....'
  call dsort(value,iorder,n)
  do i = 1, mo_tot_num
   index = iorder(i)
!  print*,'iorder(i) = ',iorder(i)
   obs(i) = eigvalues(iorder(i))
   do j = 1, mo_tot_num
    R_tmp(j,i) = R(j,index)
   enddo
  enddo
  do i = 1, mo_tot_num
   do j = 1, mo_tot_num
    R(j,i) = R_tmp(j,i)
   enddo
  enddo

  do i = 1, mo_tot_num
   value(i) = 0.d0
   do j = 1, mo_tot_num
    do k = 1, mo_tot_num
     value(i) += R(k,i) * R(j,i) * observable(k,j)
    enddo
   enddo
   print*,'i = ',i
   print*,'value(i) = ',value(i)
   print*,'obs(i)   = ',obs(i)
   print*,''
  enddo

  write (output_mo_basis,'(A)'), 'MOs are now **'//trim(label)//'**'
  write (output_mo_basis,'(A)'), ''
  write (output_mo_basis,'(A)'), 'Eigenvalues'
  write (output_mo_basis,'(A)'), '-----------'
  write (output_mo_basis,'(A)'), ''
  write (output_mo_basis,'(A)'), '======== ================'
  do i = 1, m
   write (output_mo_basis,'(I8,X,F16.10)'), i,eigvalues(i)
  enddo
  write (output_mo_basis,'(A)'), '======== ================'
  write (output_mo_basis,'(A)'), ''
  
  call dgemm('N','N',ao_num,m,m,1.d0,mo_coef_new,size(mo_coef_new,1),R,size(R,1),0.d0,mo_coef,size(mo_coef,1))
  deallocate(mo_coef_new,R,eigvalues)
  call write_time(output_mo_basis)
  
  mo_label = label
  SOFT_TOUCH mo_coef mo_label
end


subroutine mo_sort_by_observable(observable,label)
  implicit none
  character*(64), intent(in)     :: label
  double precision, intent(in)   :: observable(mo_tot_num)
  
  double precision, allocatable  :: mo_coef_new(:,:),value(:)
  integer,allocatable :: iorder(:)
  
  allocate(mo_coef_new(ao_num_align,mo_tot_num),value(mo_tot_num),iorder(mo_tot_num))
  print*,'allocate !'
  mo_coef_new = mo_coef
  

  do i = 1, mo_tot_num
   iorder(i) = i
   value(i)  = observable(i)
  enddo
  integer :: i,j,k,index
  print*,'sort ....'
  call dsort(value,iorder,mo_tot_num)
  do i = 1, mo_tot_num
   index = iorder(i)
   do j = 1, mo_tot_num
    mo_coef(j,i) = mo_coef_new(j,index)
   enddo
  enddo

  write (output_mo_basis,'(A)'), 'MOs are now **'//trim(label)//'**'
  write (output_mo_basis,'(A)'), ''

  
  deallocate(mo_coef_new,value)
! call write_time(output_mo_basis)
  
  mo_label = label
  SOFT_TOUCH mo_coef mo_label
end



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

subroutine save_mos_truncated(n)
  implicit none
  double precision, allocatable  :: buffer(:,:)
  integer                        :: i,j,n
  
  call system('$QP_ROOT/scripts/save_current_mos.sh '//trim(ezfio_filename))
  
  call ezfio_set_mo_basis_mo_tot_num(n)
  call ezfio_set_mo_basis_mo_label(mo_label)
  call ezfio_set_mo_basis_ao_md5(ao_md5)
  allocate ( buffer(ao_num,n) )
  buffer = 0.d0
  do j = 1, n
    do i = 1, ao_num
      buffer(i,j) = mo_coef(i,j)
    enddo
  enddo
  call ezfio_set_mo_basis_mo_coef(buffer)
  call ezfio_set_mo_basis_mo_occ(mo_occ)
  deallocate (buffer)
  
end

subroutine mo_as_eigvectors_of_mo_matrix(matrix,n,m,label,sign,output)
  implicit none
  integer,intent(in)             :: n,m, sign
  character*(64), intent(in)     :: label
  double precision, intent(in)   :: matrix(n,m)
  logical, intent(in)            :: output
  
  integer :: i,j
  double precision, allocatable  :: mo_coef_new(:,:), R(:,:),eigvalues(:), A(:,:)
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: mo_coef_new, R
  
  call write_time(output_mo_basis)
  if (m /= mo_tot_num) then
    print *, irp_here, ': Error : m/= mo_tot_num'
    stop 1
  endif
  allocate(A(n,m),R(n,m),mo_coef_new(ao_num_align,m),eigvalues(m))
  if (sign == -1) then
    do j=1,m
      do i=1,n
        A(i,j) = -matrix(i,j)
      enddo
    enddo
  else
    do j=1,m
      do i=1,n
        A(i,j) = matrix(i,j)
      enddo
    enddo
  endif
  mo_coef_new = mo_coef
  
  call lapack_diag(eigvalues,R,A,n,m)
  if (output) then
    write (output_mo_basis,'(A)')  'MOs are now **'//trim(label)//'**'
    write (output_mo_basis,'(A)') ''
    write (output_mo_basis,'(A)')  'Eigenvalues'
    write (output_mo_basis,'(A)') '-----------'
    write (output_mo_basis,'(A)')  ''
    write (output_mo_basis,'(A)') '======== ================'
  endif
  if (sign == -1) then
    do i=1,m
      eigvalues(i) = -eigvalues(i)
    enddo
  endif
  if (output) then
    do i=1,m
      write (output_mo_basis,'(I8,1X,F16.10)')  i,eigvalues(i)
    enddo
    write (output_mo_basis,'(A)') '======== ================'
    write (output_mo_basis,'(A)')  ''
  endif
  
  call dgemm('N','N',ao_num,m,m,1.d0,mo_coef_new,size(mo_coef_new,1),R,size(R,1),0.d0,mo_coef,size(mo_coef,1))
  deallocate(A,mo_coef_new,R,eigvalues)
  call write_time(output_mo_basis)
  
  mo_label = label
end

subroutine mo_as_svd_vectors_of_mo_matrix(matrix,lda,m,n,label)
  implicit none
  integer,intent(in)             :: lda,m,n
  character*(64), intent(in)     :: label
  double precision, intent(in)   :: matrix(lda,n)
  
  integer :: i,j
  double precision, allocatable  :: mo_coef_new(:,:), U(:,:),D(:), A(:,:), Vt(:,:), work(:)
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: mo_coef_new, U, Vt, A
  
  call write_time(output_mo_basis)
  if (m /= mo_tot_num) then
    print *, irp_here, ': Error : m/= mo_tot_num'
    stop 1
  endif

  allocate(A(lda,n),U(lda,n),mo_coef_new(ao_num_align,m),D(m),Vt(lda,n))

  do j=1,n
    do i=1,m
      A(i,j) = matrix(i,j)
    enddo
  enddo
  mo_coef_new = mo_coef
  
  call svd(A,lda,U,lda,D,Vt,lda,m,n)

  write (output_mo_basis,'(A)') 'MOs are now **'//trim(label)//'**'
  write (output_mo_basis,'(A)')  ''
  write (output_mo_basis,'(A)') 'Eigenvalues'
  write (output_mo_basis,'(A)')  '-----------'
  write (output_mo_basis,'(A)') ''
  write (output_mo_basis,'(A)')  '======== ================'

  do i=1,m
    write (output_mo_basis,'(I8,1X,F16.10)')  i,D(i)
  enddo
  write (output_mo_basis,'(A)')  '======== ================'
  write (output_mo_basis,'(A)')  ''
  
  call dgemm('N','N',ao_num,m,m,1.d0,mo_coef_new,size(mo_coef_new,1),U,size(U,1),0.d0,mo_coef,size(mo_coef,1))
  deallocate(A,mo_coef_new,U,Vt,D)
  call write_time(output_mo_basis)
  
  mo_label = label
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

  write (output_mo_basis,'(A)')  'MOs are now **'//trim(label)//'**'
  write (output_mo_basis,'(A)')  ''
  write (output_mo_basis,'(A)')  'Eigenvalues'
  write (output_mo_basis,'(A)')  '-----------'
  write (output_mo_basis,'(A)')  ''
  write (output_mo_basis,'(A)')  '======== ================'
  do i = 1, m
   write (output_mo_basis,'(I8,1X,F16.10)')  i,eigvalues(i)
  enddo
  write (output_mo_basis,'(A)')  '======== ================'
  write (output_mo_basis,'(A)')  ''
  
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

  write (output_mo_basis,'(A)')  'MOs are now **'//trim(label)//'**'
  write (output_mo_basis,'(A)')  ''

  
  deallocate(mo_coef_new,value)
! call write_time(output_mo_basis)
  
  mo_label = label
  SOFT_TOUCH mo_coef mo_label
end


subroutine give_all_mos_at_r(r,mos_array)
 implicit none
 double precision, intent(in) :: r(3)
 double precision, intent(out) :: mos_array(mo_tot_num)
 call give_specific_mos_at_r(r,mos_array, mo_coef)
end

subroutine give_specific_mos_at_r(r,mos_array, mo_coef_specific)
 implicit none
 double precision, intent(in) :: r(3)
 double precision, intent(in)  :: mo_coef_specific(ao_num_align, mo_tot_num)
 double precision, intent(out) :: mos_array(mo_tot_num)
 double precision :: aos_array(ao_num),accu
 integer :: i,j
 call give_all_aos_at_r(r,aos_array)
 do i = 1, mo_tot_num
  accu = 0.d0
  do j = 1, ao_num
   accu += mo_coef_specific(j,i) * aos_array(j) 
  enddo
  mos_array(i) = accu
 enddo
end

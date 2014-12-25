subroutine save_mos
  implicit none
  double precision, allocatable  :: buffer(:,:)
  integer                        :: i,j
  
  call system('$QPACKAGE_ROOT/scripts/save_current_mos.sh '//trim(ezfio_filename))
  
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
  
  call write_time(output_mos)
  if (m /= mo_tot_num) then
    print *, irp_here, ': Error : m/= mo_tot_num'
    stop 1
  endif
  allocate(R(n,m),mo_coef_new(ao_num_align,m),eigvalues(m))
  mo_coef_new = mo_coef
  
  call lapack_diag(eigvalues,R,matrix,size(matrix,1),size(matrix,2))
  integer :: i
  write (output_mos,'(A)'), 'MOs are now **'//trim(label)//'**'
  write (output_mos,'(A)'), ''
  write (output_mos,'(A)'), 'Eigenvalues'
  write (output_mos,'(A)'), '-----------'
  write (output_mos,'(A)'), ''
  write (output_mos,'(A)'), '======== ================'
  do i = 1, m
   write (output_mos,'(I8,X,F16.10)'), i,eigvalues(i)
  enddo
  write (output_mos,'(A)'), '======== ================'
  write (output_mos,'(A)'), ''
  
  call dgemm('N','N',ao_num,m,m,1.d0,mo_coef_new,size(mo_coef_new,1),R,size(R,1),0.d0,mo_coef,size(mo_coef,1))
  deallocate(mo_coef_new,R,eigvalues)
  call write_time(output_mos)
  
  mo_label = label
  SOFT_TOUCH mo_coef mo_label
end

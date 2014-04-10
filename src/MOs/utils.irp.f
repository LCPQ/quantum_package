subroutine save_mos
  implicit none
  double precision, allocatable  :: buffer(:,:)
  integer                        :: i,j
  
  call system('save_current_mos.sh '//trim(ezfio_filename))
  
  call ezfio_set_mo_basis_mo_label(mo_label)
  allocate ( buffer(ao_num,mo_tot_num) )
  buffer = 0.d0
  do j = 1, mo_tot_num
    do i = 1, ao_num
      buffer(i,j) = mo_coef(i,j)
    enddo
  enddo
  call ezfio_set_mo_basis_mo_coef(buffer)
  deallocate (buffer)
  
end

subroutine mo_as_eigvectors_of_mo_matrix(matrix,n,m,label)
  implicit none
  integer,intent(in)             :: n,m
  character*(64), intent(in)     :: label
  double precision, intent(in)   :: matrix(n,m)
  
  double precision, allocatable  :: mo_coef_new(:,:), R(:,:),eigvalues(:)
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: mo_coef_new, R
  
  if (m /= mo_tot_num) then
    print *, irp_here, ': Error : m/= mo_tot_num'
  endif
  allocate(R(n,m))
  allocate(mo_coef_new(ao_num_align,m),eigvalues(m))
  mo_coef_new = mo_coef
  
  call lapack_diag(eigvalues,R,matrix,size(matrix,1),size(matrix,2))
  integer :: i
  do i = 1, m
   print*,'eigvalues(i) = ',eigvalues(i)
  enddo
  
  call dgemm('N','N',ao_num,m,m,1.d0,mo_coef_new,size(mo_coef_new,1),R,size(R,1),0.d0,mo_coef,size(mo_coef,1))
  deallocate(mo_coef_new,R,eigvalues)
  
  mo_label = label
  SOFT_TOUCH mo_coef
end

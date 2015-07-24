program mrcc
  implicit none
  if (.not.read_wf) then
    print *,  'read_wf has to be true.'
    stop 1
  endif
  call print_cas_coefs
  call run_mrcc
end

subroutine print_cas_coefs
  implicit none

  integer :: i,j
  print *,  'CAS'
  print *,  '==='
  do i=1,N_det_cas
    print *,  psi_cas_coef(i,:)
    call debug_det(psi_cas(1,1,i),N_int)
  enddo

  call write_double(6,ci_energy(1),"Initial CI energy")
end


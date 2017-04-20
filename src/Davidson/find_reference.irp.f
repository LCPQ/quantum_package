subroutine find_reference(thresh,n_ref,result)
  implicit none
  double precision, intent(in) :: thresh
  integer, intent(out) :: result(N_det),n_ref 
  integer :: i,j,istate
  double precision :: i_H_psi_array(1), E0, hii, norm
  double precision :: de
  integer(bit_kind), allocatable :: psi_ref_(:,:,:)
  double precision, allocatable :: psi_ref_coef_(:,:)

  allocate(psi_ref_coef_(N_det,1), psi_ref_(N_int,2,N_det))
  n_ref = 1
  result(1) = 1
  istate = 1
  psi_ref_coef_(1,1) = psi_coef(1,istate)
  psi_ref_(:,:,1) = psi_det(:,:,1)
  norm = psi_ref_coef_(1,1) * psi_ref_coef_(1,1)
  call u_0_H_u_0(E0,psi_ref_coef_,n_ref,psi_ref_,N_int,1,size(psi_ref_coef_,1))
  print *,  ''
  print *,  'Reference determinants'
  print *,  '======================'
  print *,  ''
  print *,  n_ref, ': E0 = ', E0 + nuclear_repulsion
  call debug_det(psi_ref_(1,1,n_ref),N_int)
  do i=2,N_det
    call i_h_psi(psi_det(1,1,i),psi_ref_(1,1,1),psi_ref_coef_(1,istate),N_int, &
      n_ref,size(psi_ref_coef_,1),1,i_H_psi_array)
    call i_H_j(psi_det(1,1,i),psi_det(1,1,i),N_int,hii)
    de = i_H_psi_array(istate)**2 / (E0 - hii)
    if (dabs(de) > thresh) then
      n_ref += 1
      result(n_ref) = i
      psi_ref_(:,:,n_ref) = psi_det(:,:,i)
      psi_ref_coef_(n_ref,1) = psi_coef(i,istate)
      call u_0_H_u_0(E0,psi_ref_coef_,n_ref,psi_ref_,N_int,1,size(psi_ref_coef_,1))
      print *,  n_ref, ': E0 = ', E0 + nuclear_repulsion
      call debug_det(psi_ref_(1,1,n_ref),N_int)
    endif
  enddo
end


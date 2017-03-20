subroutine multi_state(CI_electronic_energy_dressed_,CI_eigenvectors_dressed_,LDA)
  implicit none
  BEGIN_DOC
  ! Multi-state mixing
  END_DOC
  integer, intent(in)            :: LDA
  double precision, intent(inout) :: CI_electronic_energy_dressed_(N_states)
  double precision, intent(inout) :: CI_eigenvectors_dressed_(LDA,N_states)
  double precision, allocatable  :: h(:,:,:), s(:,:), Psi(:,:), H_Psi(:,:,:), H_jj(:)
  
  allocate( h(N_states,N_states,0:N_states), s(N_states,N_states) )
  allocate( Psi(LDA,N_states), H_Psi(LDA,N_states,0:N_states) )
  allocate (H_jj(LDA) )
  
!    e_0(i) = u_dot_v(v_0(1,i),u_0(1,i),n)/u_dot_u(u_0(1,i),n)

  integer                        :: i,j,k,istate
  double precision :: U(N_states,N_states), Vt(N_states,N_states), D(N_states)
  double precision, external :: diag_H_mat_elem
  do istate=1,N_states
    do i=1,N_det
      H_jj(i) = diag_H_mat_elem(psi_det(1,1,i),N_int)
    enddo

    do i=1,N_det_ref
      H_jj(idx_ref(i)) +=  delta_ii(istate,i)
    enddo
  
    do k=1,N_states
      do i=1,N_det
        Psi(i,k) = CI_eigenvectors_dressed_(i,k)
      enddo
    enddo
    call H_u_0_mrcc_nstates(H_Psi(1,1,istate),Psi,H_jj,N_det,psi_det,N_int,istate,N_states,LDA)

    do k=1,N_states
      do i=1,N_states
        double precision, external :: u_dot_v
        h(i,k,istate) = u_dot_v(Psi(1,i), H_Psi(1,k,istate), N_det)
      enddo
    enddo
  enddo

  do k=1,N_states
    do i=1,N_states
      s(i,k) = u_dot_v(Psi(1,i), Psi(1,k), N_det)
    enddo
  enddo

  print *,  s(:,:)
  print *,  ''

  h(:,:,0) = h(:,:,1)
  do istate=2,N_states
    U(:,:) = h(:,:,0)
    call dgemm('N','N',N_states,N_states,N_states,1.d0,&
      U, size(U,1), h(1,1,istate), size(h,1), 0.d0, &
      h(1,1,0), size(Vt,1))
  enddo

  call svd(h(1,1,0), size(h,1), U, size(U,1), D, Vt, size(Vt,1), N_states, N_states)
  do k=1,N_states
    D(k) = D(k)**(1./dble(N_states))
    if (D(k) > 0.d0) then
      D(k) = -D(k)
    endif
  enddo

  do j=1,N_states
    do i=1,N_states
      h(i,j,0) = 0.d0
      do k=1,N_states
        h(i,j,0) += U(i,k) * D(k) * Vt(k,j)
      enddo
    enddo
  enddo

  print *,  h(:,:,0)
  print *,''

  integer :: LWORK, INFO
  double precision, allocatable :: WORK(:)
  LWORK=3*N_states
  allocate (WORK(LWORK))
  call dsygv(1, 'V', 'U', N_states, h(1,1,0), size(h,1), s, size(s,1), D, WORK, LWORK, INFO)
  deallocate(WORK)

   do j=1,N_states
     do i=1,N_det
       CI_eigenvectors_dressed_(i,j) = 0.d0
       do k=1,N_states
         CI_eigenvectors_dressed_(i,j) += Psi(i,k) * h(k,j,0)
       enddo
     enddo
     CI_electronic_energy_dressed_(j) = D(j)
   enddo


  deallocate (h,s, H_jj)
  deallocate( Psi, H_Psi )
end

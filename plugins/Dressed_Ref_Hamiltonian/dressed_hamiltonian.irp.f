BEGIN_PROVIDER [double precision, dressing_ref_hamiltonian, (n_det_ref,n_det_ref,N_states)]
 implicit none
 integer :: i,j,k,l
 integer :: ii,jj,istate
 double precision :: hij,sec_order,H_ref(N_det_ref),hik,hkl
 integer          :: idx(0:N_det_ref)
 double precision :: accu_negative,accu_positive,phase
 integer :: degree_exc_ionic,degree_exc_neutral,exc(0:2,2,2)
 dressing_ref_hamiltonian = 0.d0
 accu_negative = 0.d0
 accu_positive = 0.d0
 integer :: h1,p1,h2,p2,s1,s2
 do istate = 1, N_states
   do i = 1, N_det_non_ref
    call filter_connected_i_H_psi0(psi_ref,psi_non_ref(1,1,i),N_int,N_det_ref,idx)
    H_ref = 0.d0
    do ii=1,idx(0)
      k = idx(ii)
      !DEC$ FORCEINLINE
      call i_H_j(psi_ref(1,1,k),psi_non_ref(1,1,i),N_int,hij)
      H_ref(k)  = hij
    enddo
    do ii= 1, idx(0)
     k = idx(ii)
     hik = H_ref(k) * lambda_mrcc(istate,i)
     do jj = 1, idx(0)
      l = idx(jj)
      dressing_ref_hamiltonian(k,l,istate) += hik * H_ref(l)
     enddo
    enddo
   enddo
 enddo
END_PROVIDER 

BEGIN_PROVIDER [double precision, hamiltonian_total_dressed, (n_det_ref,n_det_ref,N_states)]
 implicit none
 integer :: i,j,k
 do k = 1, N_states
  do i = 1, N_det_ref
   do j = 1, N_det_ref
    hamiltonian_total_dressed(j,i,k) = dressing_ref_hamiltonian(j,i,k) + ref_hamiltonian_matrix(j,i)
   enddo
  enddo
 enddo

END_PROVIDER 

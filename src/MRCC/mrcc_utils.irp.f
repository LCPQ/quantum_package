 use bitmasks
 BEGIN_PROVIDER [ integer(bit_kind), psi_cas, (N_int,2,N_det_generators) ]
&BEGIN_PROVIDER [ double precision, psi_cas_coefs,  (N_det_generators,n_states) ]
&BEGIN_PROVIDER [ integer(bit_kind), psi_sd,  (N_int,2,N_det) ]
&BEGIN_PROVIDER [ double precision, psi_sd_coefs, (N_det,n_states) ]
&BEGIN_PROVIDER [ integer, idx_cas, (N_det_generators) ]
&BEGIN_PROVIDER [ integer, idx_sd,  (N_det) ]
&BEGIN_PROVIDER [ integer, N_det_sd]
&BEGIN_PROVIDER [ integer, N_det_cas]
 implicit none
 BEGIN_DOC
 ! SD
 END_DOC
 integer                        :: i_cas,i_sd,j,k
 integer                        :: degree
 logical                        :: in_cas
 i_cas=0
 i_sd =0
 do k=1,N_det
   in_cas = .False.
   do j=1,n_det_generators
     call get_excitation_degree(psi_generators(1,1,j), psi_det(1,1,k), degree, N_int)
     if (degree == 0) then
       i_cas  += 1
       psi_cas(1:N_int,1:2,i_cas) = psi_det(1:N_int,1:2,k)
       psi_cas_coefs(i_cas,1:N_states) = psi_coef(k,1:N_states)
       in_cas = .True.
       idx_cas(i_cas) = k
       exit
     endif
   enddo
   if (.not.in_cas) then
     double precision :: hij
     i_sd += 1
     psi_sd(1:N_int,1:2,i_sd) = psi_det(1:N_int,1:2,k)
     psi_sd_coefs(i_sd,1:N_states) = psi_coef(k,1:N_states)
     idx_sd(i_sd) = k
   endif
 enddo
 N_det_sd = i_sd
 N_det_cas = i_cas
END_PROVIDER

BEGIN_PROVIDER [ double precision, lambda_mrcc, (psi_det_size,n_states) ]
 implicit none
 BEGIN_DOC
 ! cm/<Psi_0|H|D_m>
 END_DOC
 integer :: i,k
 double precision :: ihpsi(N_states)
 do i=1,N_det_sd
   call i_h_psi(psi_sd(1,1,i), psi_cas, psi_cas_coefs, N_int, N_det_cas, &
     size(psi_cas_coefs,1), n_states, ihpsi)
   double precision :: hij
   do k=1,N_states
     if (dabs(ihpsi(k)) < 1.d-6) then
       lambda_mrcc(i,k) = 0.d0
     else
       lambda_mrcc(i,k) = psi_sd_coefs(i,k)/ihpsi(k)
       lambda_mrcc(i,k) = min( lambda_mrcc (i,k),0.d0 )
     endif
   enddo
 enddo
END_PROVIDER

subroutine update_generators
  implicit none
  integer :: i,j,k
  n_det_generators = N_det_sd
  do k=1,N_det_sd
    do j=1,2
      do i=1,N_int
        psi_generators(i,j,k) = psi_sd(i,j,k)
      enddo
    enddo
  enddo
end

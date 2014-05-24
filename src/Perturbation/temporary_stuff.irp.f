use bitmasks
BEGIN_SHELL [ /bin/bash ]
./h_apply.py
END_SHELL

BEGIN_PROVIDER [ double precision, reference_energy, (N_states) ]
 implicit none
 BEGIN_DOC
 ! Reference energy
 END_DOC
 integer :: i

 call diagonalize(psi_det,psi_coef,reference_energy,size(psi_coef,1),N_det,N_states,N_int,output_CISD)
 SOFT_TOUCH psi_det psi_coef 
END_PROVIDER


subroutine diagonalize(psi_det_in,psi_coef_in,eigvalues_out,psi_coef_dim,Ndet,N_st,Nint,iunit)
  use bitmasks
  implicit none
  integer,intent(in) :: Nint,Ndet,N_st,psi_coef_dim, iunit
  integer(bit_kind), intent(in) :: psi_det_in(Nint,2,Ndet)
  double precision, intent(in) :: psi_coef_in(Ndet,N_st)
  double precision, intent(out) :: eigvalues_out(N_st)

  ASSERT (Nint == N_int)

  call davidson_diag(psi_det_in,psi_coef_in,eigvalues_out,psi_coef_dim,Ndet,N_st,Nint,iunit)
end


BEGIN_PROVIDER [ integer, psi_ref_size ]
 implicit none
 psi_ref_size = psi_det_size
END_PROVIDER
BEGIN_PROVIDER [ integer, N_det_ref]
 implicit none
  N_det_ref = N_det
END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), psi_ref, (N_int,2,psi_ref_size) ]
&BEGIN_PROVIDER [ double precision, psi_ref_coef, (psi_ref_size,N_states) ]
  implicit none
  BEGIN_DOC
  ! On what we apply <i|H|psi> for perturbation. If selection, it may be 0.9 of the norm.
  END_DOC
  integer                        :: i,k
  
  do i=1,N_det_ref
    do k=1,N_int
      psi_ref(k,1,i) = psi_det(k,1,i)
      psi_ref(k,2,i) = psi_det(k,2,i)
    enddo
  enddo
  do k=1,N_states
    do i=1,N_det_ref
      psi_ref_coef(i,k) = psi_coef(i,k)
    enddo
  enddo
END_PROVIDER



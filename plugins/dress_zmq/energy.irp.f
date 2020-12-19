BEGIN_PROVIDER [ logical, initialize_dress_E0_denominator ]
 implicit none
 BEGIN_DOC
 ! If true, initialize dress_E0_denominator
 END_DOC
 initialize_dress_E0_denominator = .True.
END_PROVIDER

BEGIN_PROVIDER [ double precision, dress_E0_denominator, (N_states) ]
 implicit none
 BEGIN_DOC
 ! E0 in the denominator of the dress
 END_DOC
 integer :: i
 if (initialize_dress_E0_denominator) then
  call u_0_H_u_0(dress_E0_denominator,psi_coef,N_det,psi_det,N_int,N_states,size(psi_coef,1))
  do i=N_det+1,N_states
    dress_E0_denominator(i) = 0.d0
  enddo
  call write_double(6,dress_E0_denominator(1)+nuclear_repulsion, 'dress Energy denominator')
 else
   dress_E0_denominator = -huge(1.d0)
 endif
END_PROVIDER


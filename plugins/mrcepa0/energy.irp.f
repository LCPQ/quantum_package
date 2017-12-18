BEGIN_PROVIDER [ logical, initialize_mrcc_E0_denominator ]
 implicit none
 BEGIN_DOC
 ! If true, initialize mrcc_E0_denominator
 END_DOC
 initialize_mrcc_E0_denominator = .True.
END_PROVIDER

BEGIN_PROVIDER [ double precision, mrcc_E0_denominator, (N_states) ]
 implicit none
 BEGIN_DOC
 ! E0 in the denominator of the mrcc
 END_DOC
 if (initialize_mrcc_E0_denominator) then
  mrcc_E0_denominator(1:N_states) = psi_energy(1:N_states)
! mrcc_E0_denominator(1:N_states) = HF_energy - nuclear_repulsion
! mrcc_E0_denominator(1:N_states) = barycentric_electronic_energy(1:N_states)
  call write_double(6,mrcc_E0_denominator(1)+nuclear_repulsion, 'mrcc Energy denominator')
 else
   mrcc_E0_denominator = -huge(1.d0)
 endif
END_PROVIDER


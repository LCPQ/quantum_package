BEGIN_PROVIDER [ logical, initialize_pt2_E0_denominator ]
 implicit none
 BEGIN_DOC
 ! If true, initialize pt2_E0_denominator
 END_DOC
 initialize_pt2_E0_denominator = .True.
END_PROVIDER

BEGIN_PROVIDER [ double precision, pt2_E0_denominator, (N_states) ]
 implicit none
 BEGIN_DOC
 ! E0 in the denominator of the PT2
 END_DOC
 if (initialize_pt2_E0_denominator) then
  pt2_E0_denominator(1:N_states) = psi_energy(1:N_states)
! call ezfio_get_full_ci_zmq_energy(pt2_E0_denominator(1))
! pt2_E0_denominator(1) -= nuclear_repulsion
! pt2_E0_denominator(1:N_states) = HF_energy - nuclear_repulsion
! pt2_E0_denominator(1:N_states) = barycentric_electronic_energy(1:N_states)
  call write_double(6,pt2_E0_denominator(1)+nuclear_repulsion, 'PT2 Energy denominator')
 else
   pt2_E0_denominator = -huge(1.d0)
 endif
END_PROVIDER


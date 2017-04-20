BEGIN_PROVIDER [ double precision, pt2_E0_denominator, (N_states) ]
 implicit none
 BEGIN_DOC
 ! E0 in the denominator of the PT2
 END_DOC
 pt2_E0_denominator(1:N_states) = CI_electronic_energy(1:N_states)
! pt2_E0_denominator(1:N_states) = HF_energy - nuclear_repulsion
! pt2_E0_denominator(1:N_states) = barycentric_electronic_energy(1:N_states)
 call write_double(6,pt2_E0_denominator(1)+nuclear_repulsion, 'PT2 Energy denominator')
END_PROVIDER


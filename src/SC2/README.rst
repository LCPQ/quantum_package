===============
CISD_SC2 Module
===============

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`cisd_sc2 <http://github.com/LCPQ/quantum_package/tree/master/src/SC2/SC2.irp.f#L1>`_
  CISD+SC2 method              :: take off all the disconnected terms of a CISD (selected or not)
  .br
  dets_in : bitmasks corresponding to determinants
  .br
  u_in : guess coefficients on the various states. Overwritten
  on exit
  .br
  dim_in : leftmost dimension of u_in
  .br
  sze : Number of determinants
  .br
  N_st : Number of eigenstates
  .br
  Initial guess vectors are not necessarily orthonormal

`repeat_excitation <http://github.com/LCPQ/quantum_package/tree/master/src/SC2/SC2.irp.f#L169>`_
  Undocumented

`cisd <http://github.com/LCPQ/quantum_package/tree/master/src/SC2/cisd_SC2.irp.f#L1>`_
  Undocumented

`ci_sc2_eigenvectors <http://github.com/LCPQ/quantum_package/tree/master/src/SC2/diagonalize_CI_SC2.irp.f#L19>`_
  Eigenvectors/values of the CI matrix

`ci_sc2_electronic_energy <http://github.com/LCPQ/quantum_package/tree/master/src/SC2/diagonalize_CI_SC2.irp.f#L18>`_
  Eigenvectors/values of the CI matrix

`ci_sc2_energy <http://github.com/LCPQ/quantum_package/tree/master/src/SC2/diagonalize_CI_SC2.irp.f#L1>`_
  N_states lowest eigenvalues of the CI matrix

`diagonalize_ci_sc2 <http://github.com/LCPQ/quantum_package/tree/master/src/SC2/diagonalize_CI_SC2.irp.f#L38>`_
  Replace the coefficients of the CI states by the coefficients of the
  eigenstates of the CI matrix

`pt2_epstein_nesbet_sc2_projected <http://github.com/LCPQ/quantum_package/tree/master/src/SC2/pert_sc2.irp.f#L2>`_
  compute the Epstein-Nesbet perturbative first order coefficient and second order energetic contribution
  .br
  for the various N_st states,
  .br
  but  with the correction in the denominator
  .br
  comming from the interaction of that determinant with all the others determinants
  .br
  that can be repeated by repeating all the double excitations
  .br
  : you repeat all the correlation energy already taken into account in CI_electronic_energy(1)
  .br
  that could be repeated to this determinant.
  .br
  In addition, for the perturbative energetic contribution you have the standard second order
  .br
  e_2_pert = <psi_i|H|det_pert>^2/(Delta_E)
  .br
  and also the purely projected contribution
  .br
  H_pert_diag = <HF|H|det_pert> c_pert

`repeat_all_e_corr <http://github.com/LCPQ/quantum_package/tree/master/src/SC2/pert_sc2.irp.f#L82>`_
  Undocumented



Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* `AOs <http://github.com/LCPQ/quantum_package/tree/master/src/AOs>`_
* `BiInts <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts>`_
* `Bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask>`_
* `CISD <http://github.com/LCPQ/quantum_package/tree/master/src/CISD>`_
* `Dets <http://github.com/LCPQ/quantum_package/tree/master/src/Dets>`_
* `Electrons <http://github.com/LCPQ/quantum_package/tree/master/src/Electrons>`_
* `Ezfio_files <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files>`_
* `Hartree_Fock <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock>`_
* `MonoInts <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts>`_
* `MOs <http://github.com/LCPQ/quantum_package/tree/master/src/MOs>`_
* `Nuclei <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei>`_
* `Output <http://github.com/LCPQ/quantum_package/tree/master/src/Output>`_
* `SingleRefMethod <http://github.com/LCPQ/quantum_package/tree/master/src/SingleRefMethod>`_
* `Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils>`_
* `Selectors_full <http://github.com/LCPQ/quantum_package/tree/master/src/Selectors_full>`_


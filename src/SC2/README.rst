===============
CISD_SC2 Module
===============

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`cisd_sc2 <http://github.com/LCPQ/quantum_package/tree/master/src/CISD_SC2/SC2.irp.f#L1>`_
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

`repeat_excitation <http://github.com/LCPQ/quantum_package/tree/master/src/CISD_SC2/SC2.irp.f#L169>`_
  Undocumented

`cisd <http://github.com/LCPQ/quantum_package/tree/master/src/CISD_SC2/cisd_SC2.irp.f#L1>`_
  Undocumented

`ci_sc2_eigenvectors <http://github.com/LCPQ/quantum_package/tree/master/src/CISD_SC2/diagonalize_CI_SC2.irp.f#L19>`_
  Eigenvectors/values of the CI matrix

`ci_sc2_electronic_energy <http://github.com/LCPQ/quantum_package/tree/master/src/CISD_SC2/diagonalize_CI_SC2.irp.f#L18>`_
  Eigenvectors/values of the CI matrix

`ci_sc2_energy <http://github.com/LCPQ/quantum_package/tree/master/src/CISD_SC2/diagonalize_CI_SC2.irp.f#L1>`_
  N_states lowest eigenvalues of the CI matrix

`diagonalize_ci_sc2 <http://github.com/LCPQ/quantum_package/tree/master/src/CISD_SC2/diagonalize_CI_SC2.irp.f#L38>`_
  Replace the coefficients of the CI states by the coefficients of the
  eigenstates of the CI matrix



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


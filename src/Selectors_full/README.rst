=====================
Selectors_full Module
=====================

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`coef_hf_selector <http://github.com/LCPQ/quantum_package/tree/master/src/Selectors_full/e_corr_selectors.irp.f#L27>`_
  energy of correlation per determinant respect to the Hartree Fock determinant
  .br
  for the all the double excitations in the selectors determinants
  .br
  E_corr_per_selectors(i) = <D_i|H|HF> * c(D_i)/c(HF) if |D_i> is a double excitation
  .br
  E_corr_per_selectors(i) = -1000.d0 if it is not a double excitation
  .br
  coef_hf_selector = coefficient of the Hartree Fock determinant in the selectors determinants

`double_index_selectors <http://github.com/LCPQ/quantum_package/tree/master/src/Selectors_full/e_corr_selectors.irp.f#L4>`_
  degree of excitation respect to Hartree Fock for the wave function
  .br
  for the all the selectors determinants
  .br
  double_index_selectors = list of the index of the double excitations
  .br
  n_double_selectors = number of double excitations in the selectors determinants

`e_corr_per_selectors <http://github.com/LCPQ/quantum_package/tree/master/src/Selectors_full/e_corr_selectors.irp.f#L28>`_
  energy of correlation per determinant respect to the Hartree Fock determinant
  .br
  for the all the double excitations in the selectors determinants
  .br
  E_corr_per_selectors(i) = <D_i|H|HF> * c(D_i)/c(HF) if |D_i> is a double excitation
  .br
  E_corr_per_selectors(i) = -1000.d0 if it is not a double excitation
  .br
  coef_hf_selector = coefficient of the Hartree Fock determinant in the selectors determinants

`exc_degree_per_selectors <http://github.com/LCPQ/quantum_package/tree/master/src/Selectors_full/e_corr_selectors.irp.f#L3>`_
  degree of excitation respect to Hartree Fock for the wave function
  .br
  for the all the selectors determinants
  .br
  double_index_selectors = list of the index of the double excitations
  .br
  n_double_selectors = number of double excitations in the selectors determinants

`n_double_selectors <http://github.com/LCPQ/quantum_package/tree/master/src/Selectors_full/e_corr_selectors.irp.f#L5>`_
  degree of excitation respect to Hartree Fock for the wave function
  .br
  for the all the selectors determinants
  .br
  double_index_selectors = list of the index of the double excitations
  .br
  n_double_selectors = number of double excitations in the selectors determinants

`n_det_selectors <http://github.com/LCPQ/quantum_package/tree/master/src/Selectors_full/selectors.irp.f#L26>`_
  For Single reference wave functions, the number of selectors is 1 : the
  Hartree-Fock determinant

`psi_selectors <http://github.com/LCPQ/quantum_package/tree/master/src/Selectors_full/selectors.irp.f#L48>`_
  Determinants on which we apply <i|H|psi> for perturbation.

`psi_selectors_coef <http://github.com/LCPQ/quantum_package/tree/master/src/Selectors_full/selectors.irp.f#L49>`_
  Determinants on which we apply <i|H|psi> for perturbation.

`psi_selectors_size <http://github.com/LCPQ/quantum_package/tree/master/src/Selectors_full/selectors.irp.f#L21>`_
  Undocumented

`threshold_selectors <http://github.com/LCPQ/quantum_package/tree/master/src/Selectors_full/selectors.irp.f#L3>`_
  Percentage of the norm of the state-averaged wave function to
  consider for the selectors



Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* `AOs <http://github.com/LCPQ/quantum_package/tree/master/src/AOs>`_
* `BiInts <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts>`_
* `Bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask>`_
* `Dets <http://github.com/LCPQ/quantum_package/tree/master/src/Dets>`_
* `Electrons <http://github.com/LCPQ/quantum_package/tree/master/src/Electrons>`_
* `Ezfio_files <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files>`_
* `Hartree_Fock <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock>`_
* `MonoInts <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts>`_
* `MOs <http://github.com/LCPQ/quantum_package/tree/master/src/MOs>`_
* `Nuclei <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei>`_
* `Output <http://github.com/LCPQ/quantum_package/tree/master/src/Output>`_
* `Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils>`_


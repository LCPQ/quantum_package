===========
Dets Module
===========

This module contains the determinants of the CI wave function.

H is applied on the list of generator determinants. Selected determinants
are added into the *H_apply buffer*. Then the new wave function is
constructred as the concatenation of the odl wave function and
some determinants of the H_apply buffer. Generator determinants are built
as a subset of the determinants of the wave function.


Assumptions
===========

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* The MOs are orthonormal
* All the determinants have the same number of electrons
* The determinants are orthonormal
* The number of generator determinants <= the number of determinants
* All the determinants in the H_apply buffer are supposed to be different from the 
  wave function determinants
* All the determinants in the H_apply buffer are supposed to be unique


Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* `AOs <http://github.com/LCPQ/quantum_package/tree/master/src/AOs>`_
* `BiInts <http://github.com/LCPQ/quantum_package/tree/master/src/BiInts>`_
* `Bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask>`_
* `Electrons <http://github.com/LCPQ/quantum_package/tree/master/src/Electrons>`_
* `Ezfio_files <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files>`_
* `Hartree_Fock <http://github.com/LCPQ/quantum_package/tree/master/src/Hartree_Fock>`_
* `MonoInts <http://github.com/LCPQ/quantum_package/tree/master/src/MonoInts>`_
* `MOs <http://github.com/LCPQ/quantum_package/tree/master/src/MOs>`_
* `Nuclei <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei>`_
* `Output <http://github.com/LCPQ/quantum_package/tree/master/src/Output>`_
* `Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils>`_

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`copy_h_apply_buffer_to_wf <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/H_apply.irp.f#L93>`_
None
`h_apply_buffer_coef <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/H_apply.irp.f#L82>`_
  Buffer of determinants/coefficients for H_apply. Uninitialized. Filled by H_apply subroutines.

`h_apply_buffer_det <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/H_apply.irp.f#L81>`_
  Buffer of determinants/coefficients for H_apply. Uninitialized. Filled by H_apply subroutines.

`h_apply_buffer_n_det <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/H_apply.irp.f#L83>`_
  Buffer of determinants/coefficients for H_apply. Uninitialized. Filled by H_apply subroutines.

`h_apply_buffer_size <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/H_apply.irp.f#L22>`_
  Size of the H_apply buffer.

`h_apply_threshold <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/H_apply.irp.f#L3>`_
  Theshold on | <Di|H|Dj> |

`resize_h_apply_buffer_det <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/H_apply.irp.f#L31>`_
None
`n_det <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants.irp.f#L11>`_
  Number of determinants in the wave function

`n_det_generators <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants.irp.f#L47>`_
  Number of generator determinants in the wave function

`n_states <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants.irp.f#L3>`_
  Number of states to consider

`psi_coef <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants.irp.f#L20>`_
  The wave function. Initialized with Hartree-Fock

`psi_det <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants.irp.f#L19>`_
  The wave function. Initialized with Hartree-Fock

`psi_generators <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants.irp.f#L55>`_
  Determinants on which H is applied

`double_exc_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants_bitmasks.irp.f#L40>`_
  double_exc_bitmask(:,1,i) is the bitmask for holes of excitation 1
  double_exc_bitmask(:,2,i) is the bitmask for particles of excitation 1
  double_exc_bitmask(:,3,i) is the bitmask for holes of excitation 2
  double_exc_bitmask(:,4,i) is the bitmask for particles of excitation 2
  for a given couple of hole/particle excitations i.

`n_double_exc_bitmasks <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants_bitmasks.irp.f#L31>`_
  Number of double excitation bitmasks

`n_single_exc_bitmasks <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants_bitmasks.irp.f#L8>`_
  Number of single excitation bitmasks

`single_exc_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/determinants_bitmasks.irp.f#L17>`_
  single_exc_bitmask(:,1,i) is the bitmask for holes
  single_exc_bitmask(:,2,i) is the bitmask for particles
  for a given couple of hole/particle excitations i.

`get_s2 <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/s2.irp.f#L1>`_
  Returns <S^2>

`a_operator <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L842>`_
  Needed for diag_H_mat_elem

`ac_operator <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L887>`_
  Needed for diag_H_mat_elem

`decode_exc <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L76>`_
  Decodes the exc arrays returned by get_excitation.
  h1,h2 : Holes
  p1,p2 : Particles
  s1,s2 : Spins (1:alpha, 2:beta)
  degree : Degree of excitation

`diag_h_mat_elem <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L779>`_
  Computes <i|H|i>

`filter_connected <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L602>`_
  Filters out the determinants that are not connected by H

`filter_connected_i_h_psi0 <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L687>`_
None
`get_double_excitation <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L140>`_
  Returns the two excitation operators between two doubly excited determinants and the phase

`get_excitation <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L30>`_
  Returns the excitation operators between two determinants and the phase

`get_excitation_degree <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L1>`_
  Returns the excitation degree between two determinants

`get_excitation_degree_vector <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L518>`_
  Applies get_excitation_degree to an array of determinants

`get_mono_excitation <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L273>`_
  Returns the excitation operator between two singly excited determinants and the phase

`get_occ_from_key <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L935>`_
  Returns a list of occupation numbers from a bitstring

`i_h_j <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L354>`_
  Returns <i|H|j> where i and j are determinants

`i_h_psim <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/slater_rules.irp.f#L490>`_
None
`h_matrix_all_dets <http://github.com/LCPQ/quantum_package/tree/master/src/Dets/utils.irp.f#L1>`_
  H matrix on the basis of the slater deter;inants defined by psi_det




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

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`fill_h_apply_buffer_cisd <http://github.com/LCPQ/quantum_package/tree/master/src/CISD/H_apply.irp.f#L/subroutine fill_H_apply_buffer_cisd(n_selected,det_buffer,Nint)/;">`_
  Fill the H_apply buffer with determiants for CISD

`h_apply_cisd <http://github.com/LCPQ/quantum_package/tree/master/src/CISD/H_apply.irp.f#L/subroutine H_apply_cisd/;">`_
  Calls H_apply on the HF determinant and selects all connected single and double
  excitations (of the same symmetry).

`cisd_sc2 <http://github.com/LCPQ/quantum_package/tree/master/src/CISD/SC2.irp.f#L/subroutine CISD_SC2(dets_in,u_in,energies,dim_in,sze,N_st,Nint)/;">`_
  CISD+SC2 method :: take off all the disconnected terms of a CISD (selected or not)
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

`davidson_diag_hjj <http://github.com/LCPQ/quantum_package/tree/master/src/CISD/SC2.irp.f#L/subroutine davidson_diag_hjj(dets_in,u_in,H_jj,energies,dim_in,sze,N_st,Nint)/;">`_
  Davidson diagonalization with specific diagonal elements of the H matrix
  .br
  H_jj : specific diagonal H matrix elements to diagonalize de Davidson
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

`repeat_excitation <http://github.com/LCPQ/quantum_package/tree/master/src/CISD/SC2.irp.f#L/subroutine repeat_excitation(key_in,key_1,key_2,i_ok,Nint)/;">`_
  Undocumented




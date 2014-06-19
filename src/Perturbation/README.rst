===================
Perturbation Module
===================


All subroutines in `*.irp.f` starting with ``pt2_`` in the current directory are
perturbation computed using the routine ``i_H_psi``. Other cases are not allowed.
The arguments of the ``pt2_`` are always:

  subroutine pt2_...(                                                &
      psi_ref,                                                       &
      psi_ref_coefs,                                                 &
      E_refs,                                                        &
      det_pert,                                                      &
      c_pert,                                                        &
      e_2_pert,                                                      &
      H_pert_diag,                                                   &
      Nint,                                                          &
      ndet,                                                          &
      n_st )


  integer, intent(in) :: Nint,ndet,n_st
  integer(bit_kind), intent(in)  :: psi_ref(Nint,2,ndet)
  double precision , intent(in)  :: psi_ref_coefs(ndet,n_st)
  double precision , intent(in)  :: E_refs(n_st)
  integer(bit_kind), intent(in)  :: det_pert(Nint,2)
  double precision , intent(out) :: c_pert(n_st),e_2_pert(n_st),H_pert_diag


psi_ref
  bitstring of the determinants present in the various n_st states
 
psi_ref_coefs
  coefficients of the determinants on the various n_st states
 
E_refs
  Energy of the various n_st states
 
det_pert
  Perturber determinant

c_pert
  Pertrubative coefficients for the various states
 
e_2_pert
  Perturbative energetic contribution for the various states

H_pert_diag
  Diagonal H matrix element of the perturber

Nint
  Should be equal to N_int

Ndet
  Number of determinants `i` in Psi on which we apply <det_pert|Hi>

N_st
  Number of states





Assumptions
===========

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* This is not allowed:

  subroutine &
    pt2_....




Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`pt2_moller_plesset <http://github.com/LCPQ/quantum_package/tree/master/src/Perturbation/Moller_plesset.irp.f#L/subroutine pt2_moller_plesset(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,n_st)/;">`_
  compute the standard Moller-Plesset perturbative first order coefficient and second order energetic contribution
  .br
  for the various n_st states.
  .br
  c_pert(i) = <psi(i)|H|det_pert>/(difference of orbital energies)
  .br
  e_2_pert(i) = <psi(i)|H|det_pert>^2/(difference of orbital energies)
  .br

`pt2_epstein_nesbet <http://github.com/LCPQ/quantum_package/tree/master/src/Perturbation/epstein_nesbet.irp.f#L/subroutine pt2_epstein_nesbet(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,N_st)/;">`_
  compute the standard Epstein-Nesbet perturbative first order coefficient and second order energetic contribution
  .br
  for the various N_st states.
  .br
  c_pert(i) = <psi(i)|H|det_pert>/( E(i) - <det_pert|H|det_pert> )
  .br
  e_2_pert(i) = <psi(i)|H|det_pert>^2/( E(i) - <det_pert|H|det_pert> )
  .br

`pt2_epstein_nesbet_2x2 <http://github.com/LCPQ/quantum_package/tree/master/src/Perturbation/epstein_nesbet.irp.f#L/subroutine pt2_epstein_nesbet_2x2(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,N_st)/;">`_
  compute the Epstein-Nesbet 2x2 diagonalization coefficient and energetic contribution
  .br
  for the various N_st states.
  .br
  e_2_pert(i) = 0.5 * (( <det_pert|H|det_pert> -  E(i) )  - sqrt( ( <det_pert|H|det_pert> -  E(i)) ^2 + 4 <psi(i)|H|det_pert>^2  )
  .br
  c_pert(i) = e_2_pert(i)/ <psi(i)|H|det_pert>
  .br

`pt2_epstein_nesbet_sc2_projected <http://github.com/LCPQ/quantum_package/tree/master/src/Perturbation/pert_sc2.irp.f#L/subroutine pt2_epstein_nesbet_SC2_projected(det_pert,c_pert,e_2_pert,H_pert_diag,Nint,ndet,N_st)/;">`_
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

`repeat_all_e_corr <http://github.com/LCPQ/quantum_package/tree/master/src/Perturbation/pert_sc2.irp.f#L/double precision function repeat_all_e_corr(key_in)/;">`_
  Undocumented

`fill_h_apply_buffer_selection <http://github.com/LCPQ/quantum_package/tree/master/src/Perturbation/selection.irp.f#L/subroutine fill_H_apply_buffer_selection(n_selected,det_buffer,e_2_pert_buffer,coef_pert_buffer, &>`_
  Fill the H_apply buffer with determiants for the selection

`remove_small_contributions <http://github.com/LCPQ/quantum_package/tree/master/src/Perturbation/selection.irp.f#L/subroutine remove_small_contributions/;">`_
  Remove determinants with small contributions. N_states is assumed to be
  provided.

`selection_criterion <http://github.com/LCPQ/quantum_package/tree/master/src/Perturbation/selection.irp.f#L/BEGIN_PROVIDER [ double precision, selection_criterion ]/;">`_
  Threshold to select determinants. Set by selection routines.

`selection_criterion_factor <http://github.com/LCPQ/quantum_package/tree/master/src/Perturbation/selection.irp.f#L/&BEGIN_PROVIDER [ double precision, selection_criterion_factor ]/;">`_
  Threshold to select determinants. Set by selection routines.

`selection_criterion_min <http://github.com/LCPQ/quantum_package/tree/master/src/Perturbation/selection.irp.f#L/&BEGIN_PROVIDER [ double precision, selection_criterion_min ]/;">`_
  Threshold to select determinants. Set by selection routines.



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


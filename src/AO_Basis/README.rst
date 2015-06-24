==========
AOs Module
==========

This module describes the atomic orbitals basis set.

An atomic orbital :math:`\chi` centered on nucleus A is represented as:

.. math::

   \chi_i({\bf r}) = (x-X_A)^a (y-Y_A)^b (z-Z_A)^c \sum_k c_{ki} e^{-\gamma_{ki} |{\bf r} - {\bf R}_A|^2}


The AO coefficients are normalized as:

.. math::

  {\tilde c}_{ki} = \frac{c_{ki}}{ \int \left( (x-X_A)^a (y-Y_A)^b (z-Z_A)^c  e^{-\gamma_{ki} |{\bf r} - {\bf R}_A|^2} \right)^2} dr

Warning: ``ao_coef`` contains the AO coefficients given in input. These do not
include the normalization constant of the AO. The ``ao_coef_normalized`` includes
this normalization factor.

The AOs are also sorted by increasing exponent to accelerate the calculation of
the two electron integrals.

Assumptions
===========

* The AO coefficients in the EZFIO files are not necessarily normalized and are normalized after reading


Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. by the `update_README.py` script.

.. image:: tree_dependency.png

* `Nuclei <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei>`_

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. by the `update_README.py` script.

`ao_coef <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L62>`_
  AO Coefficients, read from input. Those should not be used directly, as
  the MOs are expressed on the basis of **normalized** AOs.


`ao_coef_normalized <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L84>`_
  Coefficients including the AO normalization


`ao_coef_normalized_ordered <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L107>`_
  Sorted primitives to accelerate 4 index MO transformation


`ao_coef_normalized_ordered_transp <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L133>`_
  Transposed ao_coef_normalized_ordered


`ao_expo <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L41>`_
  AO Exponents read from input


`ao_expo_ordered <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L108>`_
  Sorted primitives to accelerate 4 index MO transformation


`ao_expo_ordered_transp <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L147>`_
  Transposed ao_expo_ordered


`ao_l <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L162>`_
  ao_l = l value of the AO: a+b+c in x^a y^b z^c


`ao_l_char <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L163>`_
  ao_l = l value of the AO: a+b+c in x^a y^b z^c


`ao_l_char_space <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L311>`_
  Undocumented


`ao_md5 <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L403>`_
  MD5 key characteristic of the AO basis


`ao_nucl <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L209>`_
  Index of the nuclei on which the ao is centered


`ao_num <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L1>`_
  Number of atomic orbitals


`ao_num_align <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L2>`_
  Number of atomic orbitals


`ao_overlap <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/ao_overlap.irp.f#L1>`_
  Overlap between atomic basis functions:
  :math:`\int \chi_i(r) \chi_j(r) dr)`


`ao_overlap_abs <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/ao_overlap.irp.f#L65>`_
  Overlap between absolute value of atomic basis functions:
  :math:`\int |\chi_i(r)| |\chi_j(r)| dr)`


`ao_overlap_x <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/ao_overlap.irp.f#L2>`_
  Overlap between atomic basis functions:
  :math:`\int \chi_i(r) \chi_j(r) dr)`


`ao_overlap_y <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/ao_overlap.irp.f#L3>`_
  Overlap between atomic basis functions:
  :math:`\int \chi_i(r) \chi_j(r) dr)`


`ao_overlap_z <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/ao_overlap.irp.f#L4>`_
  Overlap between atomic basis functions:
  :math:`\int \chi_i(r) \chi_j(r) dr)`


`ao_power <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L19>`_
  Powers of x,y and z read from input


`ao_prim_num <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L177>`_
  Number of primitives per atomic orbital


`ao_prim_num_max <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L199>`_
  Undocumented


`ao_prim_num_max_align <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L200>`_
  Undocumented


`l_to_charater <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L218>`_
  character corresponding to the "L" value of an AO orbital


`n_aos_max <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L231>`_
  Number of AOs per atom


`n_pt_max_i_x <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/dimensions_integrals.irp.f#L2>`_
  Undocumented


`n_pt_max_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/dimensions_integrals.irp.f#L1>`_
  Undocumented


`nucl_aos <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L244>`_
  List of AOs attached on each atom


`nucl_list_shell_aos <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L262>`_
  Index of the shell type Aos and of the corresponding Aos
  Per convention, for P,D,F and G AOs, we take the index
  of the AO with the the corresponding power in the "X" axis


`nucl_n_aos <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L230>`_
  Number of AOs per atom


`nucl_num_shell_aos <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L263>`_
  Index of the shell type Aos and of the corresponding Aos
  Per convention, for P,D,F and G AOs, we take the index
  of the AO with the the corresponding power in the "X" axis


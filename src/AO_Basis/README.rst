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

.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.

.. image:: tree_dependency.png

* `Nuclei <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei>`_

Needed Modules
==============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


.. image:: tree_dependency.png

* `Nuclei <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei>`_

Documentation
=============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


`ao_coef <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/ezfio_interface.irp.f#L24>`_
  AO Coefficients, read from input. Those should not be used directly, as the MOs are expressed on the basis of **normalized** AOs.


`ao_coef_normalized <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L22>`_
  Coefficients including the AO normalization


`ao_coef_normalized_ordered <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L45>`_
  Sorted primitives to accelerate 4 index MO transformation


`ao_coef_normalized_ordered_transp <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L71>`_
  Transposed ao_coef_normalized_ordered


`ao_expo <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/ezfio_interface.irp.f#L134>`_
  expo for each primitive of each ao_basis


`ao_expo_ordered <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L46>`_
  Sorted primitives to accelerate 4 index MO transformation


`ao_expo_ordered_transp <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L85>`_
  Transposed ao_expo_ordered


`ao_l <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L99>`_
  ao_l = l value of the AO: a+b+c in x^a y^b z^c


`ao_l_char <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L100>`_
  ao_l = l value of the AO: a+b+c in x^a y^b z^c


`ao_l_char_space <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L216>`_
  Undocumented


`ao_md5 <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/ezfio_interface.irp.f#L6>`_
  MD5 key characteristic of the AO basis


`ao_nucl <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/ezfio_interface.irp.f#L112>`_
  Index of the nuclei on which the ao is centered


`ao_num <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/ezfio_interface.irp.f#L68>`_
  number of ao


`ao_num_align <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L1>`_
  Number of atomic orbitals align


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


`ao_power <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/ezfio_interface.irp.f#L46>`_
  power for each dimension for each ao_basis


`ao_prim_num <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/ezfio_interface.irp.f#L90>`_
  Number of primitives per atomic orbital


`ao_prim_num_max <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L12>`_
  Undocumented


`ao_prim_num_max_align <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L112>`_
  Number of primitives per atomic orbital aligned


`l_to_charater <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L123>`_
  character corresponding to the "L" value of an AO orbital


`n_aos_max <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L136>`_
  Number of AOs per atom


`n_pt_max_i_x <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/dimensions_integrals.irp.f#L2>`_
  Undocumented


`n_pt_max_integrals <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/dimensions_integrals.irp.f#L1>`_
  Undocumented


`nucl_aos <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L149>`_
  List of AOs attached on each atom


`nucl_list_shell_aos <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L167>`_
  Index of the shell type Aos and of the corresponding Aos
  Per convention, for P,D,F and G AOs, we take the index
  of the AO with the the corresponding power in the "X" axis


`nucl_n_aos <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L135>`_
  Number of AOs per atom


`nucl_num_shell_aos <http://github.com/LCPQ/quantum_package/tree/master/src/AO_Basis/aos.irp.f#L168>`_
  Index of the shell type Aos and of the corresponding Aos
  Per convention, for P,D,F and G AOs, we take the index
  of the AO with the the corresponding power in the "X" axis


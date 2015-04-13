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

Assumptions
===========

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* The atomic orbitals are normalized:

  .. math::

   \int \left(\chi_i({\bf r}) \right)^2 dr = 1

* The AO coefficients in the EZFIO files are not necessarily normalized and are normalized after reading
* The AO coefficients and exponents are ordered in increasing order of exponents


Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* `Ezfio_files <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files>`_
* `Nuclei <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei>`_
* `Output <http://github.com/LCPQ/quantum_package/tree/master/src/Output>`_
* `Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils>`_

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`ao_overlap <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/ao_overlap.irp.f#L1>`_
  Overlap between atomic basis functions:
  :math:`\int \chi_i(r) \chi_j(r) dr)`

`ao_overlap_abs <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/ao_overlap.irp.f#L65>`_
  Overlap between absolute value of atomic basis functions:
  :math:`\int |\chi_i(r)| |\chi_j(r)| dr)`

`ao_overlap_x <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/ao_overlap.irp.f#L2>`_
  Overlap between atomic basis functions:
  :math:`\int \chi_i(r) \chi_j(r) dr)`

`ao_overlap_y <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/ao_overlap.irp.f#L3>`_
  Overlap between atomic basis functions:
  :math:`\int \chi_i(r) \chi_j(r) dr)`

`ao_overlap_z <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/ao_overlap.irp.f#L4>`_
  Overlap between atomic basis functions:
  :math:`\int \chi_i(r) \chi_j(r) dr)`

`ao_coef <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L21>`_
  Coefficients, exponents and powers of x,y and z

`ao_coef_transp <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L157>`_
  Transposed ao_coef and ao_expo

`ao_coef_unnormalized <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L116>`_
  Coefficients, exponents and powers of x,y and z as in the EZFIO file
  ao_coef(i,j) = coefficient of the jth primitive on the ith ao
  ao_l = l value of the AO: a+b+c in x^a y^b z^c

`ao_expo <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L20>`_
  Coefficients, exponents and powers of x,y and z

`ao_expo_transp <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L158>`_
  Transposed ao_coef and ao_expo

`ao_expo_unsorted <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L117>`_
  Coefficients, exponents and powers of x,y and z as in the EZFIO file
  ao_coef(i,j) = coefficient of the jth primitive on the ith ao
  ao_l = l value of the AO: a+b+c in x^a y^b z^c

`ao_l <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L118>`_
  Coefficients, exponents and powers of x,y and z as in the EZFIO file
  ao_coef(i,j) = coefficient of the jth primitive on the ith ao
  ao_l = l value of the AO: a+b+c in x^a y^b z^c

`ao_l_char <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L119>`_
  Coefficients, exponents and powers of x,y and z as in the EZFIO file
  ao_coef(i,j) = coefficient of the jth primitive on the ith ao
  ao_l = l value of the AO: a+b+c in x^a y^b z^c

`ao_l_char_space <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L309>`_
  Undocumented

`ao_md5 <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L400>`_
  MD5 key characteristic of the AO basis

`ao_nucl <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L207>`_
  Index of the nuclei on which the ao is centered

`ao_num <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L1>`_
  Number of atomic orbitals

`ao_num_align <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L2>`_
  Number of atomic orbitals

`ao_power <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L19>`_
  Coefficients, exponents and powers of x,y and z

`ao_prim_num <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L175>`_
  Number of primitives per atomic orbital

`ao_prim_num_max <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L197>`_
  Undocumented

`ao_prim_num_max_align <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L198>`_
  Undocumented

`l_to_charater <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L216>`_
  character corresponding to the "L" value of an AO orbital

`n_aos_max <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L229>`_
  Number of AOs per atom

`nucl_aos <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L242>`_
  List of AOs attached on each atom

`nucl_list_shell_aos <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L260>`_
  Index of the shell type Aos and of the corresponding Aos
  Per convention, for P,D,F and G AOs, we take the index
  of the AO with the the corresponding power in the "X" axis

`nucl_n_aos <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L228>`_
  Number of AOs per atom

`nucl_num_shell_aos <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L261>`_
  Index of the shell type Aos and of the corresponding Aos
  Per convention, for P,D,F and G AOs, we take the index
  of the AO with the the corresponding power in the "X" axis




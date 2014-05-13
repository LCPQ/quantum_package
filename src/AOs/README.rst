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

* The AO coefficients in the EZFIO files are not normalized
* The AO coefficients and exponents are ordered in increasing order of exponents


Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* `Ezfio_files <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files>`_
* `Nuclei <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei>`_
* `Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils>`_

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`ao_coef <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L/&BEGIN_PROVIDER [ double precision, ao_coef, (ao_num_align,ao_prim_num_max) ]/;"
>`_
  coefficient of the primitives on the AO basis
  ao_coef(i,j) = coefficient of the jth primitive on the ith ao

`ao_coef_transp <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L/BEGIN_PROVIDER [ double precision, ao_coef_transp, (ao_prim_num_max_align,ao_num) ]/;"
>`_
  Transposed ao_coef and ao_expo

`ao_expo <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L/&BEGIN_PROVIDER [ double precision, ao_expo, (ao_num_align,ao_prim_num_max) ]/;"
>`_
  coefficient of the primitives on the AO basis
  ao_coef(i,j) = coefficient of the jth primitive on the ith ao

`ao_expo_transp <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L/&BEGIN_PROVIDER [ double precision, ao_expo_transp, (ao_prim_num_max_align,ao_num) ]/;"
>`_
  Transposed ao_coef and ao_expo

`ao_nucl <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L/BEGIN_PROVIDER [ integer, ao_nucl, (ao_num)]/;"
>`_
  Index of the nuclei on which the ao is centered

`ao_num <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L/BEGIN_PROVIDER [ integer, ao_num ]/;"
>`_
  Number of atomic orbitals

`ao_num_align <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L/&BEGIN_PROVIDER [ integer, ao_num_align ]/;"
>`_
  Number of atomic orbitals

`ao_overlap <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L/BEGIN_PROVIDER [ double precision, ao_overlap, (ao_num_align,ao_num) ]/;"
>`_
  matrix of the overlap for tha AOs
  S(i,j) = overlap between the ith and the jth atomic basis function

`ao_power <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L/BEGIN_PROVIDER [ integer, ao_power, (ao_num_align,3) ]/;"
>`_
  coefficient of the primitives on the AO basis
  ao_coef(i,j) = coefficient of the jth primitive on the ith ao

`ao_prim_num <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L/BEGIN_PROVIDER [ integer, ao_prim_num, (ao_num_align) ]/;"
>`_
  Number of primitives per atomic orbital

`ao_prim_num_max <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L/BEGIN_PROVIDER [ integer, ao_prim_num_max ]/;"
>`_
None
`ao_prim_num_max_align <http://github.com/LCPQ/quantum_package/tree/master/src/AOs/aos.irp.f#L/&BEGIN_PROVIDER [ integer, ao_prim_num_max_align ]/;"
>`_
None



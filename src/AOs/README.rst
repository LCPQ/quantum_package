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
.. ASSUMPTIONS.rst file.

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


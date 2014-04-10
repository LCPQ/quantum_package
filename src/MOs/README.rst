==========
MOs Module
==========

Molecular orbitals are expressed as 

.. math::

  \phi_k({\bf r}) = \sum_i C_{ik} \chi_k({\bf r})

The current set of molecular orbitals has a label ``mo_label``.
When the orbitals are modified, the label should also be updated to keep
everything consistent.

When saving the MOs, the ``mo_basis`` directory of the EZFIO file is copied
in the ``save`` directory, named by the current ``mo_label``.

Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* `AOs <http://github.com/LCPQ/quantum_package/tree/master/src/AOs>`_
* `Ezfio_files <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files>`_
* `Nuclei <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei>`_
* `Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils>`_


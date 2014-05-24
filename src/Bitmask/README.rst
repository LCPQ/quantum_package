==============
Bitmask Module
==============

The central part of this module is the ``bitmasks_module.f90`` file. It contains
the constants that will be used to define on which kind of integer the bitmasks
will be defined.

In the program, when an integer ``X`` is used to represent a bit string (like a determinant
for example), it should be defined as, for example:

.. code-block:: fortran

  use bitmasks
  integer(bit_kind)  :: X


The ``bitmasks_routines.irp.f`` contains helper routines to manipulate bitmassk, like
transforming a bit string to a list of integers for example.

Assumptions
===========

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

``bit_kind_shift``, ``bit_kind_size`` and ``bit_kind`` are coherent:

.. code_block:: fortran

  2**bit_kind_shift = bit_kind_size
  bit_kind = bit_kind_size / 8




Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* `AOs <http://github.com/LCPQ/quantum_package/tree/master/src/AOs>`_
* `Electrons <http://github.com/LCPQ/quantum_package/tree/master/src/Electrons>`_
* `Ezfio_files <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files>`_
* `MOs <http://github.com/LCPQ/quantum_package/tree/master/src/MOs>`_
* `Nuclei <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei>`_
* `Output <http://github.com/LCPQ/quantum_package/tree/master/src/Output>`_
* `Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils>`_

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`full_ijkl_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L12>`_
  Bitmask to include all possible MOs

`generators_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L91>`_
  Bitmasks for generator determinants. (N_int, alpha/beta, hole/particle, generator)

`hf_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L32>`_
  Hartree Fock bit mask

`n_generators_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L58>`_
  Number of bitmasks for generators

`n_int <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L3>`_
  Number of 64-bit integers needed to represent determinants as binary strings

`n_reference_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L112>`_
  Number of bitmasks for reference

`ref_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L50>`_
  Reference bit mask, used in Slater rules, chosen as Hartree-Fock bitmask

`reference_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L145>`_
  Bitmasks for reference determinants. (N_int, alpha/beta, hole/particle, reference)

`bitstring_to_hexa <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks_routines.irp.f#L95>`_
  Transform a bit string to a string in hexadecimal format for printing

`bitstring_to_list <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks_routines.irp.f#L1>`_
  Gives the inidices(+1) of the bits set to 1 in the bit string

`bitstring_to_str <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks_routines.irp.f#L62>`_
  Transform a bit string to a string for printing

`debug_det <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks_routines.irp.f#L117>`_
  Undocumented

`list_to_bitstring <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks_routines.irp.f#L29>`_
  return the physical string "string(N_int,2)" from the array of occupations "list(N_int*bit_kind_size,2)
  list
  <== ipos ==>
  |
  v
  string :|------------------------|-------------------------|------------------------|
  <==== bit_kind_size ====> <==== bit_kind_size ====> <==== bit_kind_size ====>
  {        iint            } {         iint         } {         iint         }




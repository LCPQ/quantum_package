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
.. NEEDED_MODULES_CHILDREN file by the `update_README.py` script.

``bit_kind_shift``, ``bit_kind_size`` and ``bit_kind`` are coherent:

.. code_block:: fortran

  2**bit_kind_shift = bit_kind_size
  bit_kind = bit_kind_size / 8




Needed Modules
==============

.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.

.. image:: tree_dependency.png

* `MO_Basis <http://github.com/LCPQ/quantum_package/tree/master/src/MO_Basis>`_

Needed Modules
==============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


.. image:: tree_dependency.png

* `MO_Basis <http://github.com/LCPQ/quantum_package/tree/master/src/MO_Basis>`_

Documentation
=============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


`bitstring_to_hexa <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks_routines.irp.f#L98>`_
  Transform a bit string to a string in hexadecimal format for printing


`bitstring_to_list <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks_routines.irp.f#L1>`_
  Gives the inidices(+1) of the bits set to 1 in the bit string


`bitstring_to_str <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks_routines.irp.f#L65>`_
  Transform a bit string to a string for printing


`cas_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L277>`_
  Bitmasks for CAS reference determinants. (N_int, alpha/beta, CAS reference)


`closed_shell_ref_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L518>`_
  Undocumented


`core_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L452>`_
  Core orbitals bitmask


`debug_det <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks_routines.irp.f#L120>`_
  Subroutine to print the content of a determinant in '+-' notation and
  hexadecimal representation.


`debug_spindet <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks_routines.irp.f#L155>`_
  Subroutine to print the content of a determinant in '+-' notation and
  hexadecimal representation.


`full_ijkl_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L12>`_
  Bitmask to include all possible MOs


`full_ijkl_bitmask_4 <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L29>`_
  Undocumented


`generators_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L190>`_
  Bitmasks for generator determinants.
  (N_int, alpha/beta, hole/particle, generator).
  .br
  3rd index is :
  .br
  * 1 : hole     for single exc
  .br
  * 2 : particle for single exc
  .br
  * 3 : hole     for 1st exc of double
  .br
  * 4 : particle for 1st exc of double
  .br
  * 5 : hole     for 2nd exc of double
  .br
  * 6 : particle for 2nd exc of double
  .br


`generators_bitmask_restart <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L131>`_
  Bitmasks for generator determinants.
  (N_int, alpha/beta, hole/particle, generator).
  .br
  3rd index is :
  .br
  * 1 : hole     for single exc
  .br
  * 2 : particle for single exc
  .br
  * 3 : hole     for 1st exc of double
  .br
  * 4 : particle for 1st exc of double
  .br
  * 5 : hole     for 2nd exc of double
  .br
  * 6 : particle for 2nd exc of double
  .br


`hf_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L41>`_
  Hartree Fock bit mask


`i_bitmask_gen <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L469>`_
  Current bitmask for the generators


`inact_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L321>`_
  inact_bitmask : Bitmask of the inactive orbitals which are supposed to be doubly excited
  in post CAS methods
  n_inact_orb   : Number of inactive orbitals
  virt_bitmask  : Bitmaks of vritual orbitals which are supposed to be recieve electrons
  in post CAS methods
  n_virt_orb    : Number of virtual orbitals


`inact_virt_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L425>`_
  Reunion of the inactive and virtual bitmasks


`index_holes_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/modify_bitmasks.irp.f#L260>`_
  Index of the holes in the generators_bitmasks


`index_particl_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/modify_bitmasks.irp.f#L271>`_
  Index of the holes in the generators_bitmasks


`initialize_bitmask_to_restart_ones <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/modify_bitmasks.irp.f#L3>`_
  Initialization of the generators_bitmask to the restart bitmask


`is_a_1h <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmask_cas_routines.irp.f#L476>`_
  Undocumented


`is_a_1h1p <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmask_cas_routines.irp.f#L454>`_
  Undocumented


`is_a_1h2p <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmask_cas_routines.irp.f#L465>`_
  Undocumented


`is_a_1p <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmask_cas_routines.irp.f#L487>`_
  Undocumented


`is_a_2p <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmask_cas_routines.irp.f#L498>`_
  Undocumented


`is_a_two_holes_two_particles <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmask_cas_routines.irp.f#L206>`_
  Undocumented


`is_the_hole_in_det <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/find_hole.irp.f#L1>`_
  Undocumented


`is_the_particl_in_det <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/find_hole.irp.f#L29>`_
  Undocumented


`list_act <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L502>`_
  list of active orbitals


`list_core <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L437>`_
  List of the core orbitals that are never excited in post CAS method


`list_inact <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L370>`_
  list_inact : List of the inactive orbitals which are supposed to be doubly excited
  in post CAS methods
  list_virt  : List of vritual orbitals which are supposed to be recieve electrons
  in post CAS methods


`list_to_bitstring <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks_routines.irp.f#L29>`_
  Returns the physical string "string(N_int,2)" from the array of
  occupations "list(N_int*bit_kind_size,2)


`list_virt <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L371>`_
  list_inact : List of the inactive orbitals which are supposed to be doubly excited
  in post CAS methods
  list_virt  : List of vritual orbitals which are supposed to be recieve electrons
  in post CAS methods


`modify_bitmasks_for_hole <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/modify_bitmasks.irp.f#L25>`_
  modify the generators_bitmask in order that one can only excite
  the electrons occupying i_hole


`modify_bitmasks_for_hole_in_out <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/modify_bitmasks.irp.f#L60>`_
  modify the generators_bitmask in order that one can only excite
  the electrons occupying i_hole


`modify_bitmasks_for_particl <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/modify_bitmasks.irp.f#L83>`_
  modify the generators_bitmask in order that one can only excite
  the electrons to the orbital i_part


`n_act_orb <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L490>`_
  number of active orbitals


`n_cas_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L247>`_
  Number of bitmasks for CAS


`n_core_orb <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L453>`_
  Core orbitals bitmask


`n_generators_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L67>`_
  Number of bitmasks for generators


`n_generators_bitmask_restart <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L98>`_
  Number of bitmasks for generators


`n_inact_orb <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L323>`_
  inact_bitmask : Bitmask of the inactive orbitals which are supposed to be doubly excited
  in post CAS methods
  n_inact_orb   : Number of inactive orbitals
  virt_bitmask  : Bitmaks of vritual orbitals which are supposed to be recieve electrons
  in post CAS methods
  n_virt_orb    : Number of virtual orbitals


`n_int <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L3>`_
  Number of 64-bit integers needed to represent determinants as binary strings


`n_virt_orb <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L324>`_
  inact_bitmask : Bitmask of the inactive orbitals which are supposed to be doubly excited
  in post CAS methods
  n_inact_orb   : Number of inactive orbitals
  virt_bitmask  : Bitmaks of vritual orbitals which are supposed to be recieve electrons
  in post CAS methods
  n_virt_orb    : Number of virtual orbitals


`number_of_holes <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmask_cas_routines.irp.f#L1>`_
  Undocumented


`number_of_holes_verbose <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmask_cas_routines.irp.f#L400>`_
  Undocumented


`number_of_particles <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmask_cas_routines.irp.f#L103>`_
  Undocumented


`number_of_particles_verbose <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmask_cas_routines.irp.f#L428>`_
  Undocumented


`print_det <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks_routines.irp.f#L138>`_
  Subroutine to print the content of a determinant using the '+-' notation


`print_generators_bitmasks_holes <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/modify_bitmasks.irp.f#L169>`_
  Undocumented


`print_generators_bitmasks_holes_for_one_generator <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/modify_bitmasks.irp.f#L213>`_
  Undocumented


`print_generators_bitmasks_particles <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/modify_bitmasks.irp.f#L191>`_
  Undocumented


`print_generators_bitmasks_particles_for_one_generator <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/modify_bitmasks.irp.f#L236>`_
  Undocumented


`print_spindet <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks_routines.irp.f#L171>`_
  Subroutine to print the content of a determinant using the '+-' notation


`ref_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L59>`_
  Reference bit mask, used in Slater rules, chosen as Hartree-Fock bitmask


`reunion_of_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L412>`_
  Reunion of the inactive, active and virtual bitmasks


`reunion_of_cas_inact_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L528>`_
  Reunion of the inactive, active and virtual bitmasks


`reunion_of_core_inact_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L397>`_
  Reunion of the inactive, active and virtual bitmasks


`set_bitmask_hole_as_input <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/modify_bitmasks.irp.f#L144>`_
  set the generators_bitmask for the holes
  as the input_bimask


`set_bitmask_particl_as_input <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/modify_bitmasks.irp.f#L119>`_
  set the generators_bitmask for the particles
  as the input_bimask


`unpaired_alpha_electrons <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L478>`_
  Bitmask reprenting the unpaired alpha electrons in the HF_bitmask


`virt_bitmask <http://github.com/LCPQ/quantum_package/tree/master/src/Bitmask/bitmasks.irp.f#L322>`_
  inact_bitmask : Bitmask of the inactive orbitals which are supposed to be doubly excited
  in post CAS methods
  n_inact_orb   : Number of inactive orbitals
  virt_bitmask  : Bitmaks of vritual orbitals which are supposed to be recieve electrons
  in post CAS methods
  n_virt_orb    : Number of virtual orbitals


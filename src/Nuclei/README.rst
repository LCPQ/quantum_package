=============
Nuclei Module
=============

This module contains data relative to the nuclei (coordinates, charge,
nuclear repulsion energy, etc).
The coordinates are expressed in atomic units.

Needed Modules
==============

.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.

.. image:: tree_dependency.png

* `Ezfio_files <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files>`_
* `Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils>`_

Needed Modules
==============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


.. image:: tree_dependency.png

* `Ezfio_files <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files>`_
* `Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils>`_

Documentation
=============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


`center_of_mass <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L246>`_
  Center of mass of the molecule


`disk_access_nuclear_repulsion <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/ezfio_interface.irp.f#L6>`_
  Read/Write Nuclear Repulsion from/to disk [ Write | Read | None ]


`element_mass <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L206>`_
  Array of the name of element, sorted by nuclear charge (integer)


`element_name <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L205>`_
  Array of the name of element, sorted by nuclear charge (integer)


`inertia_tensor <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/inertia.irp.f#L1>`_
  Inertia tensor


`inertia_tensor_eigenvalues <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/inertia.irp.f#L22>`_
  Eigenvectors/eigenvalues of the inertia_tensor. Used to find normal orientation.


`inertia_tensor_eigenvectors <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/inertia.irp.f#L21>`_
  Eigenvectors/eigenvalues of the inertia_tensor. Used to find normal orientation.


`nucl_charge <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/ezfio_interface.irp.f#L74>`_
  Nuclear charges


`nucl_coord <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L1>`_
  Nuclear coordinates in the format (:, {x,y,z})


`nucl_coord_transp <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L69>`_
  Transposed array of nucl_coord


`nucl_dist <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L88>`_
  nucl_dist     : Nucleus-nucleus distances
  nucl_dist_2   : Nucleus-nucleus distances squared
  nucl_dist_vec : Nucleus-nucleus distances vectors


`nucl_dist_2 <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L84>`_
  nucl_dist     : Nucleus-nucleus distances
  nucl_dist_2   : Nucleus-nucleus distances squared
  nucl_dist_vec : Nucleus-nucleus distances vectors


`nucl_dist_vec_x <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L85>`_
  nucl_dist     : Nucleus-nucleus distances
  nucl_dist_2   : Nucleus-nucleus distances squared
  nucl_dist_vec : Nucleus-nucleus distances vectors


`nucl_dist_vec_y <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L86>`_
  nucl_dist     : Nucleus-nucleus distances
  nucl_dist_2   : Nucleus-nucleus distances squared
  nucl_dist_vec : Nucleus-nucleus distances vectors


`nucl_dist_vec_z <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L87>`_
  nucl_dist     : Nucleus-nucleus distances
  nucl_dist_2   : Nucleus-nucleus distances squared
  nucl_dist_vec : Nucleus-nucleus distances vectors


`nucl_label <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/ezfio_interface.irp.f#L109>`_
  Nuclear labels


`nucl_num <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/ezfio_interface.irp.f#L40>`_
  Number of nuclei


`nuclear_repulsion <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L142>`_
  Nuclear repulsion energy


`positive_charge_barycentre <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L126>`_
  Centroid of the positive charges


`slater_bragg_radii <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/atomic_radii.irp.f#L1>`_
  atomic radii in Angstrom defined in table I of JCP 41, 3199 (1964) Slater
  execpt for the Hydrogen atom where we took the value of Becke (1988, JCP)


`slater_bragg_radii_per_atom <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/atomic_radii.irp.f#L65>`_
  Undocumented


`slater_bragg_radii_per_atom_ua <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/atomic_radii.irp.f#L73>`_
  Undocumented


`slater_bragg_radii_ua <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/atomic_radii.irp.f#L57>`_
  Undocumented


`slater_bragg_type_inter_distance <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/atomic_radii.irp.f#L81>`_
  Undocumented


`slater_bragg_type_inter_distance_ua <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/atomic_radii.irp.f#L95>`_
  Undocumented


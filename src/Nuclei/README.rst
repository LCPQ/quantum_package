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


`disk_access_nuclear_repulsion <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/ezfio_interface.irp.f#L6>`_
  Read/Write Nuclear Repulsion from/to disk [ Write | Read | None ]


`element_name <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L191>`_
  Array of the name of element, sorted by nuclear charge (integer)


`nucl_charge <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/ezfio_interface.irp.f#L44>`_
  Nuclear charges


`nucl_coord <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L12>`_
  Nuclear coordinates in the format (:, {x,y,z})


`nucl_coord_transp <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L67>`_
  Transposed array of nucl_coord


`nucl_dist <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L86>`_
  nucl_dist     : Nucleus-nucleus distances
  nucl_dist_2   : Nucleus-nucleus distances squared
  nucl_dist_vec : Nucleus-nucleus distances vectors


`nucl_dist_2 <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L82>`_
  nucl_dist     : Nucleus-nucleus distances
  nucl_dist_2   : Nucleus-nucleus distances squared
  nucl_dist_vec : Nucleus-nucleus distances vectors


`nucl_dist_vec_x <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L83>`_
  nucl_dist     : Nucleus-nucleus distances
  nucl_dist_2   : Nucleus-nucleus distances squared
  nucl_dist_vec : Nucleus-nucleus distances vectors


`nucl_dist_vec_y <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L84>`_
  nucl_dist     : Nucleus-nucleus distances
  nucl_dist_2   : Nucleus-nucleus distances squared
  nucl_dist_vec : Nucleus-nucleus distances vectors


`nucl_dist_vec_z <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L85>`_
  nucl_dist     : Nucleus-nucleus distances
  nucl_dist_2   : Nucleus-nucleus distances squared
  nucl_dist_vec : Nucleus-nucleus distances vectors


`nucl_label <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/ezfio_interface.irp.f#L64>`_
  Nuclear labels


`nucl_num <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/ezfio_interface.irp.f#L25>`_
  Number of nuclei


`nucl_num_aligned <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L1>`_
  Number of nuclei algined


`nuclear_repulsion <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L144>`_
  Nuclear repulsion energy


`positive_charge_barycentre <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L128>`_
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


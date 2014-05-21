=============
Nuclei Module
=============

This module contains data relative to the nuclei (coordinates, charge,
nuclear repulsion energy, etc).
The coordinates are expressed in atomic units.

Needed Modules
==============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

* `Ezfio_files <http://github.com/LCPQ/quantum_package/tree/master/src/Ezfio_files>`_
* `Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils>`_
* `Output <http://github.com/LCPQ/quantum_package/tree/master/src/Output>`_

Documentation
=============

.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

`nucl_charge <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L/BEGIN_PROVIDER [ double precision, nucl_charge, (nucl_num) ]/;">`_
  Nuclear charges

`nucl_coord <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L/BEGIN_PROVIDER [ double precision, nucl_coord,  (nucl_num_aligned,3) ]/;">`_
  Nuclear coordinates in the format (:, {x,y,z})

`nucl_coord_transp <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L/BEGIN_PROVIDER [ double precision, nucl_coord_transp, (3,nucl_num) ]/;">`_
  Transposed array of nucl_coord

`nucl_dist <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L/&BEGIN_PROVIDER [ double precision, nucl_dist, (nucl_num_aligned,nucl_num) ]/;">`_
  nucl_dist     : Nucleus-nucleus distances
  nucl_dist_2   : Nucleus-nucleus distances squared
  nucl_dist_vec : Nucleus-nucleus distances vectors

`nucl_dist_2 <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L/BEGIN_PROVIDER [ double precision, nucl_dist_2, (nucl_num_aligned,nucl_num) ]/;">`_
  nucl_dist     : Nucleus-nucleus distances
  nucl_dist_2   : Nucleus-nucleus distances squared
  nucl_dist_vec : Nucleus-nucleus distances vectors

`nucl_dist_vec_x <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L/&BEGIN_PROVIDER [ double precision, nucl_dist_vec_x, (nucl_num_aligned,nucl_num) ]/;">`_
  nucl_dist     : Nucleus-nucleus distances
  nucl_dist_2   : Nucleus-nucleus distances squared
  nucl_dist_vec : Nucleus-nucleus distances vectors

`nucl_dist_vec_y <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L/&BEGIN_PROVIDER [ double precision, nucl_dist_vec_y, (nucl_num_aligned,nucl_num) ]/;">`_
  nucl_dist     : Nucleus-nucleus distances
  nucl_dist_2   : Nucleus-nucleus distances squared
  nucl_dist_vec : Nucleus-nucleus distances vectors

`nucl_dist_vec_z <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L/&BEGIN_PROVIDER [ double precision, nucl_dist_vec_z, (nucl_num_aligned,nucl_num) ]/;">`_
  nucl_dist     : Nucleus-nucleus distances
  nucl_dist_2   : Nucleus-nucleus distances squared
  nucl_dist_vec : Nucleus-nucleus distances vectors

`nucl_label <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L/BEGIN_PROVIDER [ character*(32), nucl_label, (nucl_num) ]/;">`_
  Nuclear labels

`nucl_num <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L/BEGIN_PROVIDER [ integer, nucl_num ]/;">`_
  Number of nuclei

`nucl_num_aligned <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L/&BEGIN_PROVIDER [ integer, nucl_num_aligned ]/;">`_
  Number of nuclei

`nuclear_repulsion <http://github.com/LCPQ/quantum_package/tree/master/src/Nuclei/nuclei.irp.f#L/BEGIN_PROVIDER [ double precision, nuclear_repulsion ]/;">`_
  Nuclear repulsion energy




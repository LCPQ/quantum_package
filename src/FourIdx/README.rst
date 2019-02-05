=======
FourIdx 
=======

Four-index transformation.

Needed Modules
==============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


.. image:: tree_dependency.png

* `ZMQ <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ>`_

Documentation
=============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


`four_idx_collector <http://github.com/LCPQ/quantum_package/tree/master/src/FourIdx/four_index_zmq.irp.f#L212>`_
  Undocumented


`four_idx_pull_results <http://github.com/LCPQ/quantum_package/tree/master/src/FourIdx/four_index_zmq.irp.f#L243>`_
  Undocumented


`four_idx_push_results <http://github.com/LCPQ/quantum_package/tree/master/src/FourIdx/four_index_zmq.irp.f#L303>`_
  Undocumented


`four_index_transform <http://github.com/LCPQ/quantum_package/tree/master/src/FourIdx/four_index.irp.f#L1>`_
  Performs a four-index transformation of map_a(N^4) into map_c(M^4) using b(NxM)
  C_{abcd} = \sum_{ijkl} A_{ijkl}.B_{ia}.B_{jb}.B_{kc}.B_{ld}
  Loops run over *_start->*_end


`four_index_transform_block <http://github.com/LCPQ/quantum_package/tree/master/src/FourIdx/four_index_block.irp.f#L1>`_
  Performs a four-index transformation of map_a(N^4) into map_c(M^4) using b(NxM)
  C_{abcd} = \sum_{ijkl} A_{ijkl}.B_{ia}.B_{jb}.B_{kc}.B_{ld}
  Loops run over *_start->*_end


`four_index_transform_slave <http://github.com/LCPQ/quantum_package/tree/master/src/FourIdx/four_index_zmq.irp.f#L141>`_
  Undocumented


`four_index_transform_slave_inproc <http://github.com/LCPQ/quantum_package/tree/master/src/FourIdx/four_index_zmq.irp.f#L130>`_
  Computes a buffer of integrals. i is the ID of the current thread.


`four_index_transform_slave_tcp <http://github.com/LCPQ/quantum_package/tree/master/src/FourIdx/four_index_zmq.irp.f#L120>`_
  Computes a buffer of integrals. i is the ID of the current thread.


`four_index_transform_slave_work <http://github.com/LCPQ/quantum_package/tree/master/src/FourIdx/four_index_slave.irp.f#L1>`_
  Performs a four-index transformation of map_a(N^4) into map_c(M^4) using b(NxM)
  C_{abcd} = \sum_{ijkl} A_{ijkl}.B_{ia}.B_{jb}.B_{kc}.B_{ld}
  Loops run over *_start->*_end


`four_index_transform_zmq <http://github.com/LCPQ/quantum_package/tree/master/src/FourIdx/four_index_zmq.irp.f#L1>`_
  Performs a four-index transformation of map_a(N^4) into map_c(M^4) using b(NxM)
  C_{abcd} = \sum_{ijkl} A_{ijkl}.B_{ia}.B_{jb}.B_{kc}.B_{ld}
  Loops run over *_start->*_end


`nthreads_four_idx <http://github.com/LCPQ/quantum_package/tree/master/src/FourIdx/four_index_zmq.irp.f#L196>`_
  Number of threads for 4-index transformation


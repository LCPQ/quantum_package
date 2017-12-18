===
ZMQ
===

Socket address : defined as an environment variable : QP_RUN_ADDRESS


Needed Modules
==============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


.. image:: tree_dependency.png

* `Utils <http://github.com/LCPQ/quantum_package/tree/master/src/Utils>`_

Documentation
=============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


`add_task_to_taskserver <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L704>`_
  Get a task from the task server


`add_task_to_taskserver_recv <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L761>`_
  Get a task from the task server


`add_task_to_taskserver_send <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L736>`_
  Get a task from the task server


`connect_to_taskserver <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L621>`_
  Connect to the task server and obtain the worker ID


`disconnect_from_taskserver <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L664>`_
  Disconnect from the task server


`end_parallel_job <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L583>`_
  End a new parallel job with name 'name'. The slave tasks execute subroutine 'slave'


`end_zmq_pair_socket <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L424>`_
  Terminate socket on which the results are sent.


`end_zmq_pull_socket <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L444>`_
  Terminate socket on which the results are sent.


`end_zmq_push_socket <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L470>`_
  Terminate socket on which the results are sent.


`end_zmq_sub_socket <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L404>`_
  Terminate socket on which the results are sent.


`end_zmq_to_qp_run_socket <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L890>`_
  Terminate the socket from the application to qp_run


`get_task_from_taskserver <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L836>`_
  Get a task from the task server


`new_parallel_job <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L506>`_
  Start a new parallel job with name 'name'. The slave tasks execute subroutine 'slave'


`new_zmq_pair_socket <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L161>`_
  Socket on which the collector and the main communicate


`new_zmq_pull_socket <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L221>`_
  Socket on which the results are sent. If thread is 1, use inproc


`new_zmq_push_socket <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L300>`_
  Socket on which the results are sent. If thread is 1, use inproc


`new_zmq_sub_socket <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L363>`_
  Socket to read the state published by the Task server


`new_zmq_to_qp_run_socket <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L123>`_
  Socket on which the qp_run process replies


`qp_run_address <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L16>`_
  Address of the qp_run socket
  Example : tcp://130.120.229.139:12345


`reset_zmq_addresses <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L64>`_
  Socket which pulls the results (2)


`switch_qp_run_to_master <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L81>`_
  Address of the master qp_run socket
  Example : tcp://130.120.229.139:12345


`task_done_to_taskserver <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L807>`_
  Get a task from the task server


`wait_for_next_state <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L955>`_
  Undocumented


`wait_for_state <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L979>`_
  Wait for the ZMQ state to be ready


`wait_for_states <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L1007>`_
  Wait for the ZMQ state to be ready


`zmq_abort <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L780>`_
  Aborts a running parallel computation


`zmq_context <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L4>`_
  Context for the ZeroMQ library


`zmq_delete_task <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L913>`_
  When a task is done, it has to be removed from the list of tasks on the qp_run
  queue. This guarantees that the results have been received in the pull.


`zmq_lock <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L5>`_
  Context for the ZeroMQ library


`zmq_port <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L110>`_
  Return the value of the ZMQ port from the corresponding integer


`zmq_port_start <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L17>`_
  Address of the qp_run socket
  Example : tcp://130.120.229.139:12345


`zmq_set_running <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L553>`_
  Set the job to Running in QP-run


`zmq_socket_pair_inproc_address <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L42>`_
  Socket which pulls the results (2)


`zmq_socket_pull_inproc_address <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L44>`_
  Socket which pulls the results (2)


`zmq_socket_pull_tcp_address <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L41>`_
  Socket which pulls the results (2)


`zmq_socket_push_inproc_address <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L45>`_
  Socket which pulls the results (2)


`zmq_socket_push_tcp_address <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L43>`_
  Socket which pulls the results (2)


`zmq_socket_sub_tcp_address <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L46>`_
  Socket which pulls the results (2)


`zmq_state <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ/utils.irp.f#L498>`_
  Threads executing work through the ZeroMQ interface


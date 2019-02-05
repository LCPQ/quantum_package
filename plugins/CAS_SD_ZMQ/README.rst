==========
CAS_SD_ZMQ
==========

Selected CAS+SD  module with Zero-MQ parallelization.

Needed Modules
==============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


.. image:: tree_dependency.png

* `Generators_CAS <http://github.com/LCPQ/quantum_package/tree/master/plugins/Generators_CAS>`_
* `Perturbation <http://github.com/LCPQ/quantum_package/tree/master/plugins/Perturbation>`_
* `Selectors_CASSD <http://github.com/LCPQ/quantum_package/tree/master/plugins/Selectors_CASSD>`_
* `ZMQ <http://github.com/LCPQ/quantum_package/tree/master/src/ZMQ>`_

Documentation
=============
.. Do not edit this section It was auto-generated
.. by the `update_README.py` script.


`add_task_to_taskserver <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L739>`_
  Get a task from the task server


`add_to_selection_buffer <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection_buffer.irp.f#L31>`_
  Undocumented


`assert <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L15>`_
  Undocumented


`cassd_zmq <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/cassd_zmq.irp.f#L1>`_
  Undocumented


`connect_to_taskserver <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L646>`_
  Connect to the task server and obtain the worker ID


`create_selection_buffer <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection_buffer.irp.f#L2>`_
  Undocumented


`delete_selection_buffer <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection_buffer.irp.f#L18>`_
  Undocumented


`disconnect_from_taskserver <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L694>`_
  Disconnect from the task server


`do_ddci <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/ezfio_interface.irp.f#L6>`_
  If true, remove purely inactive double excitations


`end_parallel_job <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L597>`_
  End a new parallel job with name 'name'. The slave tasks execute subroutine 'slave'


`end_zmq_pair_socket <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L434>`_
  Terminate socket on which the results are sent.


`end_zmq_pull_socket <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L454>`_
  Terminate socket on which the results are sent.


`end_zmq_push_socket <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L480>`_
  Terminate socket on which the results are sent.


`end_zmq_sub_socket <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L414>`_
  Terminate socket on which the results are sent.


`end_zmq_to_qp_run_socket <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L1007>`_
  Terminate the socket from the application to qp_run


`fci_zmq <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/target_pt2_ratio_cassd.irp.f#L1>`_
  Undocumented


`fill_buffer_double <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L611>`_
  Undocumented


`fill_buffer_single <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L157>`_
  Undocumented


`get_d0 <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L1044>`_
  Undocumented


`get_d1 <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L875>`_
  Undocumented


`get_d2 <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L757>`_
  Undocumented


`get_m0 <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L377>`_
  Undocumented


`get_m1 <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L315>`_
  Undocumented


`get_m2 <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L256>`_
  Undocumented


`get_mask_phase <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L26>`_
  Undocumented


`get_phase_bi <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L77>`_
  Undocumented


`get_task_from_taskserver <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L871>`_
  Get a task from the task server


`get_tasks_from_taskserver <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L933>`_
  Get multiple tasks from the task server


`new_parallel_job <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L516>`_
  Start a new parallel job with name 'name'. The slave tasks execute subroutine 'slave'


`new_zmq_pair_socket <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L169>`_
  Socket on which the collector and the main communicate


`new_zmq_pull_socket <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L229>`_
  Socket on which the results are sent. If thread is 1, use inproc


`new_zmq_push_socket <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L310>`_
  Socket on which the results are sent. If thread is 1, use inproc


`new_zmq_sub_socket <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L373>`_
  Socket to read the state published by the Task server


`new_zmq_to_qp_run_socket <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L131>`_
  Socket on which the qp_run process replies


`past_d1 <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L1104>`_
  Undocumented


`past_d2 <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L1120>`_
  Undocumented


`prog_selection_slave <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection_cassd_slave.irp.f#L1>`_
  Helper program to compute the PT2 in distributed mode.


`provide_everything <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection_cassd_slave.irp.f#L15>`_
  Undocumented


`psi_phasemask <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L4>`_
  Undocumented


`pt2_e0_denominator <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/energy.irp.f#L1>`_
  E0 in the denominator of the PT2


`pull_selection_results <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/run_selection_slave.irp.f#L139>`_
  Undocumented


`push_selection_results <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/run_selection_slave.irp.f#L100>`_
  Undocumented


`qp_run_address <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L16>`_
  Address of the qp_run socket
  Example : tcp://130.120.229.139:12345


`reset_zmq_addresses <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L68>`_
  Socket which pulls the results (2)


`run_selection_slave <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/run_selection_slave.irp.f#L2>`_
  Undocumented


`run_wf <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection_cassd_slave.irp.f#L20>`_
  Undocumented


`select_connected <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L48>`_
  Undocumented


`select_doubles <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L457>`_
  Undocumented


`select_singles <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L100>`_
  Select determinants connected to i_det by H


`selection_collector <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L1291>`_
  Undocumented


`selection_slave_inproc <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L1284>`_
  Undocumented


`sort_selection_buffer <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection_buffer.irp.f#L51>`_
  Undocumented


`splash_p <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L205>`_
  Undocumented


`splash_pq <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L683>`_
  Undocumented


`spot_hasbeen <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L403>`_
  Undocumented


`spot_isinwf <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L1146>`_
  Undocumented


`switch_qp_run_to_master <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L85>`_
  Address of the master qp_run socket
  Example : tcp://130.120.229.139:12345


`task_done_to_taskserver <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L799>`_
  Get a task from the task server


`tasks_done_to_taskserver <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L830>`_
  Get a task from the task server


`wait_for_next_state <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L1111>`_
  Undocumented


`wait_for_state <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L1135>`_
  Wait for the ZMQ state to be ready


`wait_for_states <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L1163>`_
  Wait for the ZMQ state to be ready


`zmq_abort <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L771>`_
  Aborts a running parallel computation


`zmq_context <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L4>`_
  Context for the ZeroMQ library


`zmq_delete_task <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L1030>`_
  When a task is done, it has to be removed from the list of tasks on the qp_run
  queue. This guarantees that the results have been received in the pull.


`zmq_delete_tasks <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L1067>`_
  When a task is done, it has to be removed from the list of tasks on the qp_run
  queue. This guarantees that the results have been received in the pull.


`zmq_get_dvector <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/put_get.irp.f#L39>`_
  Get psi_coef from the qp_run scheduler


`zmq_lock <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L5>`_
  Context for the ZeroMQ library


`zmq_port <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L118>`_
  Return the value of the ZMQ port from the corresponding integer


`zmq_port_start <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L17>`_
  Address of the qp_run socket
  Example : tcp://130.120.229.139:12345


`zmq_put_dvector <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/put_get.irp.f#L1>`_
  Put the X vector on the qp_run scheduler


`zmq_selection <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/selection.irp.f#L1190>`_
  Undocumented


`zmq_set_running <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L568>`_
  Set the job to Running in QP-run


`zmq_socket_pair_inproc_address <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L46>`_
  Socket which pulls the results (2)


`zmq_socket_pull_inproc_address <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L48>`_
  Socket which pulls the results (2)


`zmq_socket_pull_tcp_address <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L45>`_
  Socket which pulls the results (2)


`zmq_socket_push_inproc_address <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L49>`_
  Socket which pulls the results (2)


`zmq_socket_push_tcp_address <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L47>`_
  Socket which pulls the results (2)


`zmq_socket_sub_tcp_address <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L50>`_
  Socket which pulls the results (2)


`zmq_state <http://github.com/LCPQ/quantum_package/tree/master/plugins/CAS_SD_ZMQ/utils.irp.f#L508>`_
  Threads executing work through the ZeroMQ interface


active proctype fortran() {
  req_message msg;
  rep_message reply;
  byte state;
  byte count, wait;


  /* New parallel job */
  state=1;
  send_req( NEWJOB, state );
  assert (reply.m == OK);
  
  send_req( PUTPSI, state );
  assert (reply.m == PUTPSI_REPLY);
  
  /* Add tasks */
  count = 0;
  do
  :: (count == NTASKS) -> break;
  :: else ->
     count++;
     send_req( ADDTASK, count );
     assert (reply.m == OK);
  od

  wait = _nr_pr;
  /* Run collector */
  run collector(state);

  /* Run slaves */
  count = 0;
  do
  :: (count == NPROC) -> break;
  :: else ->  count++; run slave();
  od

  /* Wait for collector and slaves to finish */
  (_nr_pr == wait);
  
  send_req( ENDJOB, state );
  assert (reply.m == OK);
  state = reply.value;
  
  send_req( TERMINATE, 0);
  assert (reply.m == OK);

} 


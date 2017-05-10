proctype slave() {
  req_message msg;
  rep_message reply;
  byte task;
  byte state;

  send_req(CONNECT, 0);
  assert (reply.m == OK);
  state = reply.value;

  send_req(GETPSI, 0);
  assert (reply.m == PSI);

  task=255;
  do
  :: (task == 0) -> break;
  :: else ->
     send_req( GETTASK, 0);
     if
     :: (reply.m == NONE) -> task = 0;
     :: (reply.m == TASK) ->
        task = reply.value;
        /* Compute task */
        send_req( TASKDONE, task);
        assert (reply.m == OK);
        pull_socket ! task;
      fi
  od
}

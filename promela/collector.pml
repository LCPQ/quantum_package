proctype collector(byte state) {

  byte task;
  req_message msg;
  rep_message reply;
  bit loop = 1;
  xr pull_socket;

  do
  :: (loop == 0) -> break
  :: else ->
     pull_socket ? task;
     /* Handle result */
     send_req(DELTASK, task);
     assert (reply.m == OK);
     loop = reply.value;
  od;

}



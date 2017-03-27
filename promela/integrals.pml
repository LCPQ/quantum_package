#define NPROC 1
#define BUFSIZE 2
#define NTASKS 3

mtype = { NONE, OK, WRONG_STATE, TERMINATE, GETPSI, PUTPSI, NEWJOB, ENDJOB, SETRUNNING,
        SETWAITING, SETSTOPPED, CONNECT, DISCONNECT, ADDTASK, DELTASK, TASKDONE, GETTASK,
        PSI, TASK, PUTPSI_REPLY, WAITING, RUNNING, STOPPED
         }

typedef rep_message {
  mtype m = NONE;
  byte value = 0;
}

typedef req_message {
  mtype m = NONE;
  byte state = 0;
  byte value = 0;
  chan reply = [BUFSIZE] of { rep_message };
}

#define send_req( MESSAGE, VALUE ) msg.m=MESSAGE ; msg.value=VALUE ; msg.state=state; rep_socket ! msg; msg.reply ? reply

chan rep_socket   = [NPROC] of { req_message };
chan pull_socket  = [NPROC] of { byte };
chan pair_socket  = [NPROC] of { req_message };
chan task_queue   = [NTASKS+2] of { byte };
chan pub_socket   = [NTASKS+2] of { mtype };

bit socket_up = 0;
mtype global_state; /* Sent by pub */

active proctype qp_run() {

  bit psi = 0;
  bit address_tcp = 0;
  bit address_inproc = 0;
  bit running = 0;
  byte status = 0;
  byte state = 0;
  byte ntasks = 0;
  req_message msg;
  rep_message reply;
  byte nclients = 0;
  byte task;

  socket_up = 1;
  running = 1;
  do
//  :: ( (running == 0) && (nclients == 0) && (ntasks == 0) ) -> break
  :: ( running == 0 ) -> break
  :: else ->

        rep_socket ? msg;
        printf("req: "); printm(msg.m); printf("\t%d\n",msg.value); 

        if
        :: ( msg.m == TERMINATE ) ->
           assert (state != 0);
           assert (msg.state == state);
           running = 0;
           reply.m = OK;

        :: ( msg.m == PUTPSI ) -> 
           assert (state != 0);
           assert (msg.state == state);
           assert (psi == 0);
           psi = 1;
           reply.m = PUTPSI_REPLY;

        :: ( msg.m == GETPSI ) -> 
           assert (state != 0);
           assert (msg.state == state);
           assert (psi == 1);
           reply.m = PSI;

        :: ( msg.m == NEWJOB ) -> 
           assert (state == 0);
           state = msg.value;
           pair_socket ! WAITING;
           reply.m = OK;
           reply.value = state;

        :: ( msg.m == ENDJOB ) -> 
           assert (state != 0);
           assert (msg.state == state);
           state = 0;
           pair_socket ! WAITING;
           reply.m = OK;

        :: ( msg.m == ADDTASK ) -> 
           assert (state != 0);
           assert (msg.state == state);
           task_queue ! msg.value;
           ntasks++;
           reply.m = OK;

        :: ( msg.m == GETTASK ) -> 
           assert (nclients > 0);
           assert (state != 0);
           assert (msg.state == state);
           if
           :: ( task_queue ?[task] ) -> 
              pair_socket ! WAITING;
              reply.m = TASK;
              task_queue ? reply.value
           :: else -> 
              pair_socket ! RUNNING;
              reply.m = NONE;
              reply.value = 255;
           fi;

        :: ( msg.m == TASKDONE) -> 
           assert (state != 0);
           assert (msg.state == state);
           assert (nclients > 0);
           assert (ntasks > 0);
           reply.m = OK;

        :: ( msg.m == DELTASK ) -> 
           assert (state != 0);
           assert (msg.state == state);
           ntasks--;
           if
           :: (ntasks > 0) -> reply.value = 1;
           :: else -> reply.value = 0;
           fi;
           reply.m = OK;

        :: ( msg.m == CONNECT ) -> 
           assert ( state != 0 )
           nclients++;
           reply.m = OK;
           reply.value = state;

        :: ( msg.m == DISCONNECT ) -> 
           assert ( msg.state == state )
           nclients--;
           reply.m = OK;

        :: ( msg.m == STOPPED ) -> 
           pair_socket ! STOPPED;
           reply.m = OK;

        :: ( msg.m == WAITING ) -> 
           pair_socket ! WAITING;
           reply.m = OK;

        :: ( msg.m == RUNNING ) -> 
           assert ( state != 0 );
           pair_socket ! RUNNING;
           reply.m = OK;

        fi
        msg.reply ! reply
  od
  pair_socket ! STOPPED;
  socket_up = 0;
  
}


active proctype master() {

  req_message msg;
  rep_message reply;
  byte state = 0;
  byte count;

  run pub_thread();

  /* New parallel job */
  state=1;
  send_req( NEWJOB, state );
  assert (reply.m == OK);
  
  /* Add tasks */
  count = 0;
  do
  :: (count == NTASKS) -> break;
  :: else ->
     count++;
     send_req( ADDTASK, count );
     assert (reply.m == OK);
  od

  /* Run collector */
  run collector(state);

  /* Run slaves */
  count = 0;
  do
  :: (count == NPROC) -> break;
  :: else ->  count++; run slave();
  od

}

proctype slave() {

  req_message msg;
  rep_message reply;
  byte task;
  byte state;

  msg.m=CONNECT; 
  msg.state = 0;

  if 
  :: (!socket_up) -> goto exit;
  :: else -> skip;
  fi
  rep_socket ! msg;
  
  if 
  :: (!socket_up) -> goto exit;
  :: else -> skip;
  fi
  msg.reply ? reply;
  
  state = reply.value;
  

  task = 1;
  do
  :: (task == 255) -> break;
  :: else ->
     send_req( GETTASK, 0);
     if 
     :: (reply.m == NONE) ->
        task = 255;
     :: (reply.m == TASK) ->
        /* Compute task */
        task = reply.value;
        send_req( TASKDONE, task);
        assert (reply.m == OK);
        pull_socket ! task;
     fi
  od
  send_req( DISCONNECT, 0);
  assert (reply.m == OK);

exit: skip;
}

proctype collector(byte state) {
  byte task;
  req_message msg;
  rep_message reply;
  bit loop = 1;
  do
  :: (loop == 0) -> break
  :: else ->
     pull_socket ? task;
     /* Handle result */
     send_req(DELTASK, task);
     assert (reply.m == OK);
     loop = reply.value;
  od
  send_req( TERMINATE, 0);
  assert (reply.m == OK);
}

proctype pub_thread() {
  mtype state = WAITING;
  do 
  :: (state == STOPPED) -> break;
  :: (pair_socket ? [state]) ->
     pair_socket ? state;
     global_state = state;
  od
}

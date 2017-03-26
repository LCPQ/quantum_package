#define NPROC 3
#define BUFSIZE 2
#define NTASKS 5

#define STATE 1

mtype = { NONE, OK, WRONG_STATE, TERMINATE, GETPSI, PUTPSI, NEWJOB, ENDJOB, SETRUNNING,
        SETWAITING, SETSTOPPED, CONNECT, DISCONNECT, ADDTASK, DELTASK, TASKDONE, GETTASK,
        PSI, TASK
         }

typedef rep_message {
  mtype m = NONE;
  byte value = 0;
}

typedef req_message {
  mtype m = NONE;
  byte value = 0;
  chan reply = [BUFSIZE] of { rep_message };
}

#define send_req( MESSAGE, VALUE ) msg.m=MESSAGE ; msg.value=VALUE ; rep_socket ! msg; msg.reply ? reply

chan rep_socket   = [NPROC] of { req_message };
chan pull_socket  = [NPROC] of { byte };
chan pair_socket  = [NPROC] of { req_message };
chan task_queue   = [NTASKS+2] of { byte };


active proctype qp_run() {

  bit psi = 0;
  byte running = 0;
  bit  state = 0;
  bit terminate = 0;
  byte ntasks = 0;
  req_message msg;
  rep_message reply;
  byte nclients = 0;
  byte task;

  do
  :: ( (terminate == 1) && (nclients == 0) && (ntasks == 0) ) -> break
  :: else ->

        rep_socket ? msg;
        printf("req: "); printm(msg.m); printf("\t%d\n",msg.value); 

        if
        :: ( msg.m == TERMINATE ) ->
           assert (state != 0);
           terminate = 1;
           reply.m = OK;

        :: ( msg.m == PUTPSI ) -> 
           assert (state != 0);
           assert (psi == 0);
           psi = 1;
           reply.m = OK;

        :: ( msg.m == GETPSI ) -> 
           assert (state != 0);
           assert (psi == 1);
           reply.m = PSI;

        :: ( msg.m == NEWJOB ) -> 
           state = msg.value
           reply.m = OK;

        :: ( msg.m == ADDTASK ) -> 
           assert (state != 0);
           task_queue ! msg.value;
           ntasks++;
           reply.m = OK;

        :: ( msg.m == GETTASK ) -> 
           assert (nclients > 0);
           assert (state != 0);
           if
           :: ( task_queue ?[task] ) -> 
              reply.m = TASK;
              task_queue ? reply.value
           :: else -> 
              reply.m = NONE;
              reply.value = 255;
           fi;

        :: ( msg.m == TASKDONE) -> 
           assert (state != 0);
           assert (nclients > 0);
           assert (ntasks > 0);
           reply.m = OK;

        :: ( msg.m == DELTASK ) -> 
           assert (state != 0);
           ntasks--;
           if
           :: (ntasks > 0) -> reply.value = 1;
           :: else -> reply.value = 0;
           fi;
           reply.m = OK;

        :: ( msg.m == CONNECT ) -> 
           nclients++;
           reply.m = OK;

        :: ( msg.m == DISCONNECT ) -> 
           nclients--;
           reply.m = OK;

        fi
        msg.reply ! reply
  od
  
}


active proctype master() {

  req_message msg;
  rep_message reply;
  byte count;

  /* New parallel job */
  send_req( NEWJOB, STATE );
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
  run collector();

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

  send_req( CONNECT, 0 );
  assert (reply.m == OK);

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

}

proctype collector() {
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

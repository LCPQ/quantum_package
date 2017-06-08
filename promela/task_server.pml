/* State of the task server */
typedef state_t {
  chan queue = [NTASKS+2] of { byte };
  byte state = 0;
  bit address_tcp = 0;
  bit address_inproc = 0;
  bit psi = 0;
  bit running = 0;
  byte ntasks;
  byte nclients = 0;
}


active proctype task_server() {
  
  xr rep_socket;
  state_t state;
  req_message msg;
  rep_message reply;
  byte task;
  
  state.running = 1;
  do
  :: ( state.running + state.nclients == 0 ) -> break
  :: else ->
        rep_socket ? msg;
        printf("req: "); printm(msg.m); printf("\t%d\n",msg.value); 

        if
        :: ( msg.m == TERMINATE ) ->
           atomic {
            assert (state.state == 0);
            assert (msg.state == state.state);
            state.running = 0;
            reply.m = OK;
           }

        :: ( msg.m == CONNECT ) ->
           atomic {
            assert (state.state != 0);
            state.nclients++;
            reply.m = OK;
            reply.value = state.state;
           }

/*
        :: ( msg.m == DISCONNECT ) ->
           atomic {
            assert (state.state != 0);
            assert (msg.state == state.state);
            state.nclients--;
            reply.m = OK;
           }
*/

        :: ( msg.m == PUTPSI ) ->
          atomic {
            assert (state.state != 0);
            assert (msg.state == state.state);
            assert (state.psi == 0);
            state.psi = 1;
            reply.m = PUTPSI_REPLY;
           }

        :: ( msg.m == GETPSI ) ->
           atomic {
            assert (state.state != 0);
            assert (msg.state == state.state);
            assert (state.psi == 1);
            reply.m = PSI;
           }

        :: ( msg.m == NEWJOB ) ->
           atomic {
            assert (state.state == 0);
            state.state = msg.value;
            reply.m = OK;
            reply.value = state.state;
           }

        :: ( msg.m == ENDJOB ) ->
           atomic {
            assert (state.state != 0);
            assert (msg.state == state.state);
            state.state = 0;
            reply.m = OK;
           }

        :: ( msg.m == TASKDONE ) ->
           atomic {
            assert (state.state != 0);
            assert (state.ntasks > 0);
            assert (msg.state == state.state);
            reply.m = OK;
           }

        :: ( msg.m == GETTASK ) -> 
           assert (state.state != 0);
           assert (state.nclients > 0);
           assert (msg.state == state.state);
           if
           :: ( state.queue ?[task] ) -> 
              reply.m = TASK;
              state.queue ? reply.value
           :: else -> 
              atomic {
               reply.m = NONE;
               reply.value = 0;
               state.nclients--;
              }
            fi;

         :: ( msg.m == DELTASK ) -> 
           assert (state.state != 0);
           assert (msg.state == state.state);
           state.ntasks--;
           if
           :: (state.ntasks > 0) -> reply.value = 1;
           :: else -> reply.value = 0;
           fi;
           reply.m = OK;

        :: ( msg.m == ADDTASK ) -> 
           assert (state.state != 0);
           assert (msg.state == state.state);
           atomic {
            state.ntasks++;
            reply.m = OK;
           }
           state.queue ! msg.value;

        fi;
        msg.reply ! reply;
        printf("rep: "); printm(reply.m); printf("\t%d\n",reply.value); 

  od;
}


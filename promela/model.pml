#define NPROC 3
#define BUFSIZE 2
#define NTASKS 4

mtype = { NONE, OK, WRONG_STATE, TERMINATE, GETPSI, PUTPSI, NEWJOB, ENDJOB, SETRUNNING,
        SETWAITING, SETSTOPPED, CONNECT, DISCONNECT, ADDTASK, DELTASK, TASKDONE, GETTASK,
        PSI, TASK, PUTPSI_REPLY, WAITING, RUNNING, STOPPED
         }

#define send_req( MESSAGE, VALUE ) atomic { msg.m=MESSAGE ; msg.value=VALUE ; msg.state=state; } ; rep_socket ! msg; msg.reply ? reply

/* Request/Reply pattern */

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

/* Channels */

chan rep_socket   = [NPROC] of { req_message };
chan pull_socket  = [NPROC] of { byte };


#include "task_server.pml"
#include "fortran.pml"
#include "collector.pml"
#include "slave.pml"

#!/usr/bin/python

import zmq
import sys, os


def main():
  context = zmq.Context()
  socket = context.socket(zmq.REQ)
  socket.connect(os.environ["QP_RUN_ADDRESS"])

  def send(msg,expected):
    print "Send  : ", msg
    print " -> ", socket.send(msg)
    reply = socket.recv()
    print "Reply : ", reply
    print ""
    assert (reply == expected)
       

  send("new_job ao_integrals tcp://130.120.229.139:12345 inproc://ao_integrals",
       "ok")
  send("new_job ao_integrals tcp://130.120.229.139:12345 inproc://ao_integrals",
       "error A job is already running")

  send("connect","error Message not understood : connect")

  send("connect  tcp","connect_reply ao_integrals 1 tcp://130.120.229.139:12345")
  send("connect  inproc","connect_reply ao_integrals 2 inproc://ao_integrals")
  send("disconnect ao_integrals 3","error Queuing_system.ml:65:2 : disconnect ao_integrals 3")
  send("disconnect ao_integrals 2","disconnect_reply ao_integrals 1")
  send("connect  inproc","connect_reply ao_integrals 3 inproc://ao_integrals")

  for i in range(10):
     send("add_task ao_integrals %d %d"%(i,i+10), "ok")

  for i in range(10):
     send("get_task ao_integrals 3", "get_task_reply %d %d %d"%(i+1,i,i+10))
     send("task_done ao_integrals 3 %d"%(i+1), "ok")

  send("get_task ao_integrals 3", "terminate")

  send("terminate","ok")

if __name__ == '__main__':
  main()

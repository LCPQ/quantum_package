      program dummy_cispsi
        implicit none
        include 'f77_zmq.h'
        integer(ZMQ_PTR)        context
        integer(ZMQ_PTR)        responder
        character*(64)          address
        character*(20)          buffer
        integer                 rc
       
       
        address = 'tcp://*:5556'
       
        context   = f77_zmq_ctx_new()
        responder = f77_zmq_socket(context, ZMQ_PUB)
        rc        = f77_zmq_bind(responder,address)
       
        do
           print *, "send"
           rc = f77_zmq_send (responder, "log coucou", 10, 0)
           CALL SLEEP(1) 
        enddo


        rc = f77_zmq_close(responder)
        rc = f77_zmq_ctx_destroy(context)
      end


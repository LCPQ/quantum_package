      program server
        implicit none
        include 'f77_zmq.h'
        integer(ZMQ_PTR)        context
        integer(ZMQ_PTR)        responder
        character*(64)          address
        character*(20)          buffer
        integer                 rc
       
       
        address = 'tcp://*:5555'
       
        context   = f77_zmq_ctx_new()
        responder = f77_zmq_socket(context, ZMQ_REP)
        rc        = f77_zmq_bind(responder,address)
       
        do
          rc = f77_zmq_recv(responder, buffer, 20, 0)
          print *,  'Received :', buffer(1:rc)
          if (buffer(1:rc) /= 'end') then
            rc = f77_zmq_send (responder, "world", 5, 0)
          else
            rc = f77_zmq_send (responder, "end", 3, 0)
            exit
          endif
        enddo

        rc = f77_zmq_close(responder)
        rc = f77_zmq_ctx_destroy(context)
      end


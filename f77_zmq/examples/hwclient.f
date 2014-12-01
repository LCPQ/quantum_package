      program server
        implicit none
        include 'f77_zmq.h'
        integer(ZMQ_PTR)        context
        integer(ZMQ_PTR)        requester
        character*(64)          address
        character*(20)          buffer
        integer                 rc
        integer                 i
       
       
        address = 'tcp://localhost:5555'
       
        context   = f77_zmq_ctx_new()
        requester = f77_zmq_socket(context, ZMQ_REQ)
        rc        = f77_zmq_connect(requester,address)
       
        do i=1,10
          rc = f77_zmq_send(requester, "Hello!", 6, 0)
          rc = f77_zmq_recv(requester, buffer, 20, 0)
          print *,  i, 'Received :', buffer(1:rc)
        enddo
        rc = f77_zmq_send(requester, "end", 3, 0)
        rc = f77_zmq_recv(requester, buffer, 20, 0)

        rc = f77_zmq_close(requester)
        rc = f77_zmq_ctx_destroy(context)

      end


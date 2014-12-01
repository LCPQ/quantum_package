      program server
        implicit none
        include 'f77_zmq.h'
        integer(ZMQ_PTR)        context
        integer(ZMQ_PTR)        requester, term
        character*(64)          address, termination_addr
        character*(20)          buffer
        integer                 rc
        integer                 i
       
       
        address = 'tcp://localhost:5555'
        termination_addr = 'tcp://localhost:5556'

       
        context   = f77_zmq_ctx_new()
        requester = f77_zmq_socket(context, ZMQ_REQ)
        rc        = f77_zmq_connect(requester,address)
        term      = f77_zmq_socket(context, ZMQ_PUSH)
        rc        = f77_zmq_connect(term,termination_addr)
       
        do i=1,10
          rc = f77_zmq_send(term, '1', 1, 0)
          rc = f77_zmq_send(requester, 'Hello!', 6, 0)
          rc = f77_zmq_recv(requester, buffer, 20, 0)
          print *,  i, 'Received :', buffer(1:rc)
        enddo
        rc = f77_zmq_send(term, "0", 1, 0)

        rc = f77_zmq_close(term)
        rc = f77_zmq_close(requester)
        rc = f77_zmq_ctx_destroy(context)

      end


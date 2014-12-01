      program socketmonitor_client
        implicit none
        include 'f77_zmq.h'
        character*(32)  addr
        integer(ZMQ_PTR) ctx
        integer(ZMQ_PTR) req
        character*(20) buffer
        integer rc

        ! Wait a litle bit
        call sleep(1)

        ! Context
        addr = 'tcp://127.0.0.1:6666'
        ctx = f77_zmq_ctx_new ()

        ! REQ socket
        req = f77_zmq_socket (ctx, ZMQ_REQ)
        rc = f77_zmq_connect(req,addr)
        if (rc /= 0)  stop 'Failed  f77_zmq_connect(req,addr)'

        print *,  'sending hello'
        rc = f77_zmq_send(req, "Hello!", 6, 0)
        print *,  'waiting for reply'
        rc = f77_zmq_recv(req, buffer, 20, 0)
        print *,  'received ', buffer(1:rc)

        ! Close the REQ socket
        rc = f77_zmq_close(req)

        rc = f77_zmq_ctx_destroy(ctx)

      end

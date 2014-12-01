      program server
        implicit none
        include 'f77_zmq.h'
        integer(ZMQ_PTR)        context
        integer(ZMQ_PTR)        term
        integer(ZMQ_PTR)        responder
        integer(ZMQ_PTR)        request, reply
        integer(ZMQ_PTR)        msg_data
        character*(64)          address, termination_addr
        character*(20)          buffer
        integer                 rc
       
       
        address = 'tcp://*:5555'
        termination_addr = 'tcp://*:5556'
       
        context   = f77_zmq_ctx_new()
 
        term      = f77_zmq_socket(context, ZMQ_PULL)
        rc        = f77_zmq_bind(term,termination_addr)
        rc        = f77_zmq_recv(term, buffer, 20, 0)
       
        responder = f77_zmq_socket(context, ZMQ_REP)
        rc        = f77_zmq_bind(responder,address)
             
        request   = f77_zmq_msg_new()
        reply     = f77_zmq_msg_new()
       
        do 

          ! Example with f77_zmq_msg_copy_to/from_data 
          print *,  'msg_copy_from/to'
          rc = f77_zmq_msg_init(request)
          if (rc /= 0) stop 'f77_zmq_msg_init failed'

          rc = f77_zmq_msg_recv(request, responder, 0)

          rc = f77_zmq_msg_copy_from_data (request, buffer)

          print *,  'Received :', buffer(1:rc)

          rc = f77_zmq_msg_close(request)
          if (rc /= 0) stop 'f77_zmq_msg_close failed'

          rc = f77_zmq_msg_init_size(reply,5)
          if (rc /= 0) stop 'f77_zmq_msg_init_size failed'

          buffer = 'World'
          rc = f77_zmq_msg_copy_to_data (reply, buffer, 5)
          if (rc /= 0) stop 'f77_zmq_msg_copy_to_data failed'

          rc = f77_zmq_msg_send(reply, responder, 0)

          rc = f77_zmq_msg_close(reply)
          if (rc /= 0) stop 'f77_zmq_msg_close failed'

          rc = f77_zmq_recv(term, buffer, 1, 0)
          if (buffer(1:1) == '0') then
            exit
          endif


          ! Example with f77_zmq_msg_copy
          print *,  'msg_copy'

          rc = f77_zmq_msg_init(request)
          if (rc /= 0) stop 'f77_zmq_msg_init failed'

          rc = f77_zmq_msg_init(reply)
          if (rc /= 0) stop 'f77_zmq_msg_init failed'


          rc = f77_zmq_msg_recv(request, responder, 0)
          rc = f77_zmq_msg_copy(reply,request)
          if (rc /= 0) stop 'f77_zmq_msg_copy failed'

          rc = f77_zmq_msg_close(request)
          if (rc /= 0) stop 'f77_zmq_msg_close failed'

          rc = f77_zmq_msg_send(reply, responder, 0)

          rc = f77_zmq_msg_close(reply)
          if (rc /= 0) stop 'f77_zmq_msg_close failed'

          rc = f77_zmq_recv(term, buffer, 1, 0)
          if (buffer(1:1) == '0') then
            exit
          endif


          ! Example with f77_zmq_msg_init_data
          print *,  'msg_new/destroy_data'
          rc = f77_zmq_msg_init(request)
          if (rc /= 0) stop 'f77_zmq_msg_init failed'

          rc = f77_zmq_msg_recv(request, responder, 0)

          rc = f77_zmq_msg_copy_from_data (request, buffer)

          print *,  'Received :', buffer(1:rc)

          rc = f77_zmq_msg_close(request)
          if (rc /= 0) stop 'f77_zmq_msg_close failed'


          msg_data = f77_zmq_msg_data_new(10,'world',10)

          rc = f77_zmq_msg_init_data(reply,msg_data,6)
          if (rc /= 0) stop 'f77_zmq_msg_init_data failed'

          rc = f77_zmq_msg_send(reply, responder, 0)

          rc = f77_zmq_msg_close(reply)
          if (rc /= 0) stop 'f77_zmq_msg_close failed'

          rc = f77_zmq_recv(term, buffer, 1, 0)

!          rc = f77_zmq_msg_destroy_data (msg_data)
          if (buffer(1:1) == '0') then
            exit
          endif

        enddo
        rc = f77_zmq_msg_destroy(request)
        rc = f77_zmq_msg_destroy(reply)

        rc = f77_zmq_close(responder)
        rc = f77_zmq_close(term)
        rc = f77_zmq_ctx_destroy(context)

      end

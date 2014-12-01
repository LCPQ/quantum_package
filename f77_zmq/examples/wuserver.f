      program wuserver
        implicit none
        include 'f77_zmq.h'

        integer(ZMQ_PTR)      context
        integer(ZMQ_PTR)      publisher
        integer               rc
        integer               zipcode, temperature, relhumidity
        double precision      r
        character*(20)        update
        
        context = f77_zmq_ctx_new()
        publisher = f77_zmq_socket (context, ZMQ_PUB)
        rc = f77_zmq_bind (publisher, "tcp://*:5556")
        if (rc /= 0) stop '1st Bind failed'

        rc = f77_zmq_bind (publisher, "ipc://weather.ipc")
        if (rc /= 0) stop '2nd Bind failed'

        do
          ! Get values that will fool the boss
          call random_number(r)
          zipcode = int(r*100000)
          call random_number(r)
          temperature = int(r*215)-80
          call random_number(r)
          relhumidity = int(r*50)-10

          ! Send message to all subscribers
          write(update,'(I5.5,X,I4,X,I4)') 
     &      zipcode, temperature, relhumidity
          rc = f77_zmq_send (publisher, update, len(update), 0);
 
        enddo
        rc = f77_zmq_close(publisher)
        rc = f77_zmq_ctx_destroy(context)

      end

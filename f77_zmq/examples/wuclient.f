      program wuclient
        implicit none
        include 'f77_zmq.h'

        integer(ZMQ_PTR)      context
        integer(ZMQ_PTR)      subscriber
        integer               rc
        integer               zipcode, temperature, relhumidity
        double precision      r
        character*(20)        filter
        integer               update_nbr
        integer               iargc
        character*(40)        string
        integer*8             total_temp
        
        ! Socket to talk to server
        print *, 'Collecting updates from weather server…'
        context = f77_zmq_ctx_new()
        subscriber = f77_zmq_socket (context, ZMQ_SUB)
        rc = f77_zmq_connect (subscriber, "tcp://localhost:5556")
        if (rc /= 0) stop 'Connect failed'

        if (iargc() > 1) then
           call getarg(1,filter) 
        else
           filter = '10001 '
        endif

        rc = f77_zmq_setsockopt (subscriber, ZMQ_SUBSCRIBE,
     &                           filter, 6) 
        if (rc /= 0) stop 'set_sockopt failed'

        ! Process 100 updates
        total_temp = 0_8
        do update_nbr=1,100
          rc = f77_zmq_recv (subscriber, string, 40, 0);
          print '(A)',  string(1:rc)
          read(string(1:rc),*) zipcode, temperature, relhumidity
          total_temp = total_temp + temperature
        enddo
        print *, 'Average temperature for zipcode "'//trim(filter)//
     &    '" was ', int(total_temp/100.), "F"
        rc = f77_zmq_close(subscriber)
        rc = f77_zmq_ctx_destroy(context)

      end

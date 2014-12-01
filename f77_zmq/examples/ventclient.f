      program task_ventilator
!       Task worker
!       Connects PULL socket to tcp://localhost:5557
!       Collects workloads from ventilator via that socket
!       Connects PUSH socket to tcp://localhost:5558
!       Sends results to sink via that socket
        implicit none
        include 'f77_zmq.h'

        integer(ZMQ_PTR)      context
        integer(ZMQ_PTR)      sender, receiver, sink
        integer               rc
        integer               msecs
        character*(20)        string

        ! Socket to receive messages on
        context  = f77_zmq_ctx_new()
        receiver = f77_zmq_socket(context,ZMQ_PULL)
        rc = f77_zmq_connect (receiver, "tcp://localhost:5557")
        if (rc /= 0) stop '1st Connect failed'
       
        ! Socket to send messages to
        sender = f77_zmq_socket (context, ZMQ_PUSH);
        rc = f77_zmq_connect (sender, "tcp://localhost:5558");
        if (rc /= 0) stop '2nd Connect failed'
       
        ! Process tasks forever
        do
            rc = f77_zmq_recv (receiver, string, 20, 0)
            print '(A)', string(1:rc)
            read(string(1:rc),*) msecs
            call milli_sleep(msecs)
            rc = f77_zmq_send (sender, "", 0, 0) ! Send results to sink
        enddo
        rc = f77_zmq_close (receiver);
        rc = f77_zmq_close (sender);
        rc = f77_zmq_ctx_destroy (context);
      end

      subroutine milli_sleep(ms)
        implicit none
        integer                 ms
        integer                 t(8), ms1, ms2
        call date_and_time(values=t)
        ms1=(t(5)*3600+t(6)*60+t(7))*1000+t(8)
        do ! check time:
          call date_and_time(values=t)
          ms2=(t(5)*3600+t(6)*60+t(7))*1000+t(8)
          if(ms2<ms1) then
            ms1 = ms1 - 24*3600*1000
          endif
          if(ms2-ms1>=ms)exit
        enddo

      end

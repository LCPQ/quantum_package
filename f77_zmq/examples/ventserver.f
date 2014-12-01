      program task_ventilator
!       Task ventilator
!       Binds PUSH socket to tcp://localhost:5557
!       Sends batch of tasks to workers via that socket
        implicit none
        include 'f77_zmq.h'

        integer(ZMQ_PTR)      context
        integer(ZMQ_PTR)      sender, sink
        integer               rc
        integer               task_nbr
        integer               total_msec
        integer               workload
        double precision      r
        character*(10)        string

        context = f77_zmq_ctx_new()
        sender  = f77_zmq_socket(context,ZMQ_PUSH)
        rc = f77_zmq_bind (sender, "tcp://*:5557")
        if (rc /= 0) stop 'Bind failed'

        ! Socket to send start of batch message on
        sink = f77_zmq_socket (context, ZMQ_PUSH)
        rc = f77_zmq_connect (sink, "tcp://localhost:5558")
        if (rc /= 0) stop 'Connect failed'

        print *, 'Press Enter when the workers are ready:'
        read(*,*)
        print *, 'Sending tasks to workers…'

        ! The first message is "0" and signals start of batch
        rc = f77_zmq_send (sink, '0', 1, 0);
        if (rc /= 1) stop 'Send failed'

        ! Send 100 tasks
        total_msec = 0     ! Total expected cost in msecs
        do task_nbr=1,100
            ! Random workload from 1 to 100msecs
            call random_number(r)
            workload = int(100.*r) + 1
            total_msec = total_msec + workload
            write(string,'(I8)') workload
            print '(A)', string
            rc = f77_zmq_send(sender,string,len(trim(string)),0)
        enddo
        print *, 'Total expected cost: ',total_msec,' msec'
       
        rc = f77_zmq_close (sink)
        rc = f77_zmq_close (sender)
        rc = f77_zmq_ctx_destroy (context)
      end

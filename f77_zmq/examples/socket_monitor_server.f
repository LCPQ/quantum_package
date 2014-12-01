       integer function read_msg(s, event, ep)
         implicit none
         include 'f77_zmq.h'
         integer(ZMQ_PTR) s, event
         character*(*) ep

         integer(ZMQ_PTR) msg1, msg2
         integer rc

         integer(ZMQ_PTR) data
         integer*2 data_msg(4)

         msg1 = f77_zmq_msg_new()   ! Binary part
         rc = f77_zmq_msg_init(msg1)
         if (rc /= 0) stop 'Unable to initialize msg1'

         msg2 = f77_zmq_msg_new()   ! Address part
         rc = f77_zmq_msg_init(msg2)
         if (rc /= 0) stop 'Unable to initialize msg2'

         rc = f77_zmq_msg_recv (msg1, s, 0)
         if ( (rc == -1).and.(f77_zmq_errno() == ETERM) ) then
           read_msg = 1
           return
         endif
         if (rc == -1) stop 'Error in f77_zmq_msg_recv (msg1, s, 0)'

         rc = f77_zmq_msg_more(msg1)
         if (rc == 0) stop 'Error in rc = zmq_msg_more(msg1)'


         rc = f77_zmq_msg_recv (msg2, s, 0)
         if ( (rc == -1).and.(f77_zmq_errno() == ETERM) ) then
           read_msg = 1
           return
         endif
         if (rc == -1) stop 'Error in f77_zmq_msg_recv (msg2, s, 0)'

         rc = f77_zmq_msg_more(msg2)
         if (rc /= 0) stop 'Error in rc = zmq_msg_more(msg2)'

         ! Copy binary data to event struct
         rc = f77_zmq_msg_copy_from_data(msg1, data_msg)
         if (rc /= 6) then
           stop 'Failed in f77_zmq_msg_copy_from_data(msg1,data_int)'
         endif

         event = f77_zmq_event_new()
         rc = f77_zmq_event_set_event(event,data_msg(1))
         rc = f77_zmq_event_set_value(event,data_msg(2))

         ! Copy address part
         ep = ''
         rc = f77_zmq_msg_copy_from_data(msg2, ep)
         if (rc == 0) then
           stop 'Failed in f77_zmq_msg_copy_from_data(msg2,ep)'
         endif
         ep = ep(1:rc)
         read_msg = 0
         return

       end



      subroutine rep_socket_monitor(ctx)
        implicit none
        include 'f77_zmq.h'
        integer(ZMQ_PTR) ctx
        integer(ZMQ_PTR) event
        integer(ZMQ_PTR) s
        character*(64) addr
        integer rc
        integer read_msg
        integer ev, val

        event = f77_zmq_event_new()
        print *,  'Starting monitor'

        s = f77_zmq_socket(ctx,ZMQ_PAIR)
        
        rc = f77_zmq_connect(s, 'inproc://monitor.rep')
        if (rc /= 0) then
          stop 'Failed in f77_zmq_connect(s,inproc://monitor.rep)'
        endif

        do while (read_msg(s, event, addr) == 0)
          ev  = f77_zmq_event_event(event)
          val = f77_zmq_event_value(event)
          if (ev == ZMQ_EVENT_LISTENING) then
             print *, 'listening socket descriptor ', val
             print *, 'listening socket address ', trim(addr)
          else if (ev == ZMQ_EVENT_ACCEPTED) then
             print *, 'accepted socket descriptor', val
             print *, 'accepted socket address ', trim(addr)
          else if (ev == ZMQ_EVENT_CLOSE_FAILED) then
             print *, 'socket close failure error code ', val
             print *, 'socket address', trim(addr)
          else if (ev == ZMQ_EVENT_CLOSED) then
             print *, 'closed socket descriptor', val
             print *, 'closed socket address ', trim(addr)
          else if (ev ==ZMQ_EVENT_DISCONNECTED) then
             print *, 'disconnected socket descriptor', val
             print *, 'disconnected socket address ', trim(addr)
          endif

        enddo
        rc = f77_zmq_close(s)
        if (rc /= 0) stop 'Failed in f77_zmq_close(s)'
        
      end

      program socketmonitor
        implicit none
        include 'f77_zmq.h'
        character*(32)  addr
        integer(ZMQ_PTR) ctx
        integer(ZMQ_PTR) rep
        integer rc
        character*(20) buffer

        ! Context
        addr = 'tcp://127.0.0.1:6666'
        ctx = f77_zmq_ctx_new ()

        ! REP socket
        rep = f77_zmq_socket (ctx, ZMQ_REP)

        ! REP socket monitor, all events
        rc = f77_zmq_socket_monitor (rep, "inproc://monitor.rep",
     &      ZMQ_EVENT_ALL)
        
        !$OMP PARALLEL SECTIONS DEFAULT(SHARED)

        !$OMP SECTION

          call rep_socket_monitor(ctx)

        !$OMP SECTION

          rc = f77_zmq_bind (rep, addr)

          ! Allow some time for event detection
          call sleep(1)

          print *,  'waiting'
          rc = f77_zmq_recv(rep, buffer, 20, 0)
          print *,  'received ', buffer(1:rc)
          print *,  'sending reply'
          rc = f77_zmq_send(rep, "World", 5, 0)
          print *,  'done'

          ! Close the REP socket
          rc = f77_zmq_close(rep)

          rc = f77_zmq_ctx_destroy(ctx)

        !$OMP END PARALLEL SECTIONS

      end

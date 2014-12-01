# Creating a shared queue proxy

```
// Create frontend and backend sockets
void *frontend = zmq_socket (context, ZMQ_ROUTER);
assert (backend);
void *backend = zmq_socket (context, ZMQ_DEALER);
assert (frontend);
// Bind both sockets to TCP ports
assert (zmq_bind (frontend, "tcp://*:5555") == 0);
assert (zmq_bind (backend, "tcp://*:5556") == 0);
// Start the queue proxy, which runs until ETERM 
zmq_proxy (frontend, backend, NULL);
```


# Polling indefinitely for input events on both a 0mq socket and a standard socket.

```
typedef struct
{
    void //*socket//;
    int //fd//;
    short //events//;
    short //revents//;
} zmq_pollitem_t;


zmq_pollitem_t items [2];
/* First item refers to ØMQ socket 'socket' */
items[0].socket = socket;
items[0].events = ZMQ_POLLIN;
/* Second item refers to standard socket 'fd' */
items[1].socket = NULL;
items[1].fd = fd;
items[1].events = ZMQ_POLLIN;
/* Poll for events indefinitely */
int rc = zmq_poll (items, 2, -1);
assert (rc >= 0); /* Returned events will be stored in items[].revents */
```


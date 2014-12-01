# F77\_ZMQ

[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/scemama/f77_zmq?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

A Fortran 77 binding library for [ZeroMQ](http://zeromq.org)

## Usage

Copy the `f77_zmq.h` and `libf77zmq.so` or `libf77zmq.a` files into
your project.

In your Fortran source files, include the `f77_zmq.h` file. This will define all the `ZMQ_*` constants.
All the pointers (to sockets, messages, polling items, etc) are defined as `integer(ZMQ_PTR)`
in order to handle 32-bit or 64-bit pointers.

In your Makefile, compile as follows:

```
$(FC) -o $(TARGET) $(OBJ) -lzmq -lf77zmq 
```

or

```
$(FC) -o $(TARGET) $(OBJ) -lzmq libf77zmq.a
```

Be sure that `libzmq.so.4` is present in your `LD_LIBRARY_PATH` before executing the program.



## Installation instructions


Python >= 2.6 is required to create the `f77_zmq.h` file.

Set the `ZMQ_H` environment variable to the absolute path of the zmq.h file, and run make.
The default compiler is gcc.

For example:

```
$ export ZMQ_H=/usr/include/zmq.h
$ make
```

## Differences with the C API

In Fortran77 structs don't exist. They have been introduced with Fortran90.
To maintain F77 compatibility, the structs are created using C functions
and their pointers are passed to the Fortan. This implies the addition
of a few functions.

### Additional Message-related functions

* `integer(ZMQ_PTR) f77_zmq_msg_new()` : Allocates a `zmq_msg_t` and returns the pointer

* `integer f77_zmq_msg_destroy(msg)` : Deallocates the `zmq_msg_t`. Return value is `0`.

  + `integer(ZMQ_PTR) msg` : message 

* `integer(ZMQ_PTR) f77_zmq_msg_data_new(size, buffer, size_buffer)` : Allocates a data
  buffer for messages, and copies the buffer into the data segment. If `size_buffer` is `0`,
  the data segement is uninitialized. The return value is a pointer to the data segment.

  + `integer size` : Size of the data segment to allocate
  + `buffer` : Buffer to copy. Fortran array or string.
  + `integer size_buffer` : Number of bytes to copy from the buffer

* `integer f77_zmq_msg_destroy_data(data)` : Deallocates a data segment. Return value is `0`.

  + `integer(ZMQ_PTR) data` : pointer to the data segment to deallocate.

* `integer f77_zmq_msg_copy_from_data(msg, buffer)` : Copies the data segment of a message 
  into a buffer.

  + `integer(ZMQ_PTR) msg` : message
  + `buffer` : fortran array of string

* `integer f77_zmq_msg_copy_to_data(msg, buffer, size)` : Copies the data segment of a message 
  into a buffer.

  + `integer(ZMQ_PTR) msg` : message
  + `buffer` : fortran array of string
  + `integer size` : Number of bytes to copy


### Additional Polling-related functions

* `integer(ZMQ_PTR) f77_zmq_pollitem_new()` : Allocates a `zmq_pollitem_t` and returns the pointer

* `integer f77_zmq_pollitem_destroy(item)` : Deallocates the `zmq_pollitem_t`. Return value is `0`.

  + `integer(ZMQ_PTR) item` : poll item struct


### Additional Event-related functions

* `integer(ZMQ_PTR) f77_zmq_event_new()` : Allocates a `zmq_event_t` and returns the pointer

* `integer f77_zmq_event_destroy(event)` : Deallocates the `zmq_event_t`. Return value is `0`.

  + `integer(ZMQ_PTR) event` : event struct

* `int f77_zmq_event_event(event)` : Returns the event field of the `zmq_event_t`.

  + `integer(ZMQ_PTR) event` : `zmq_event_t` struct

* `int f77_zmq_event_set_event(event, bitfield) : Sets the event field of the `zmq_event_t`.
  Returns `0`.

  + `integer(ZMQ_PTR) event` : `zmq_event_t` struct
  + `integer bitfield` : id of the event as bitfield

* `int f77_zmq_event_value(event)` : Returns the value field of the `zmq_event_t`.

  + `integer(ZMQ_PTR) event` : `zmq_event_t` struct

* `int f77_zmq_event_set_value(event, value) : Sets the value field of the `zmq_event_t`.
  Returns `0`.

  + `integer(ZMQ_PTR) event` : `zmq_event_t` struct
  + `integer value` : value is either error code, fd or reconnect interval



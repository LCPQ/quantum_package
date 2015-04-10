# Installation

## Requirements

* curl
* m4
* GNU make
* Fortran compiler (ifort or gfortran are tested)
* Python >= 2.6
* Bash
* Patch (for opam)


## Standard installation

1) `./setup_environment.sh`

This command will download and install all the requirements.
Installing OCaml and the Core library may take somme time
(up to 20min on an old machine).

2) `source quantum_package.rc`

This file contains all the environement variables neeeded by the quantum package
both to compile and run. This should also be done before running calculations.

3) `cp ./src/Makefile.config.gfortran  ./src/Makefile.config`

Create the ``Makefile.config`` which contains all the flags needed by the compiler.
The is also an example for the Intel Compiler (`Makefile.config.ifort`).
Edit this file and tune the flags as you want.

4) `make build`

It will compile all the executables and tools. 

5) `make binary`

Optional. It will build a `tar.gz` file containing everything needed to run the quantum package on a
machine where you can't compile.


## Installing behind a firewall

1) Download `tsocks`:

    wget http://sourceforge.net/projects/tsocks/files/latest/download
    mv download tsocks.tar.gz

2) Tranfer `tsocks.tar.gz` on the remote host

3) Configure `tsocks` with the proper directory for the `tsocks.conf` file:

    tar -zxvf tsocks.tar.gz
    cd tsocks-*
    ./configure --with-conf=${PWD}/tsocks.conf

4) Create the `tsocks.conf` file with the following content:

    server = 127.0.0.1
    server_port = 10000

5) Create the tsocks library:

    make

6) Add the `libtsocks.so` to the `LD_PRELOAD` environment variable:

    export LD_PRELOAD="${PWD}/libtsocks.so.1.8"

7) Create a custom curl command to set the tsocks option: open a file named
   `curl`, which is accessible from your `PATH` environment variable before the
   real `curl` command, and fill this file with:

    #!/bin/bash
    /usr/bin/curl --socks5 127.0.0.1:10000 $@

8) Start a tsocks ssh tunnel:

    ssh -fN -D 10000 user@external-server.com


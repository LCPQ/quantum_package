# Installation

## Requirements

* curl
* wget
* m4
* GNU make
* Intel Fortran compiler
* Python
* Bash


By default, the Ocaml compiler will be installed in `$HOME/ocamlbrew`.
To install it somewhere else, set the `$OCAMLBREW_BASE` environment
variable to the required destination, for example:

    export OCAMLBREW_BASE=/usr/local/ocamlbrew

For more info about the Ocaml installation, check the ocamlbrew
website : https://github.com/hcarty/ocamlbrew



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



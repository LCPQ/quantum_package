Quantum package
===============

[![Build Status](https://travis-ci.org/LCPQ/quantum_package.svg?branch=master)](https://travis-ci.org/LCPQ/quantum_package)

[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/LCPQ/quantum_package?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


Set of quantum chemistry programs and libraries. 
(under GNU GENERAL PUBLIC LICENSE v2)

For more information, you can visit the [wiki of the project](http://github.com/LCPQ/quantum_package/wiki>), or bellow for the installation instruction.

# Installation

## Requirements
* Fortran compiler (`ifort` and `gfortran` are tested)
* Python >= 2.6
* GNU make
* Bash

## Standard installation

### 1) Configure

    $ ./configure <config_file> (--production | --development)

For example you can type `./configure config/gfortran.cfg --production`

This command have to purpose :

 - Download and install all the requirements.
   Installing OCaml and the Core library may take some time (up to 20min on an old machine).
 - And create the file which contains all the tree dependencies for the binaries.  
   It's not a Makefile, but a Ninja file (so don't type `make` is hopeless, type `ninja` instead)

####Compilation Flags (`<config_file>`)

`<config_file>` is the path to the file who contain all the flags useful for the compilation:  like the optimization flags, the Lapack libary, etc.  We have two default configure file in  ``$QP_ROOT/config`` : ``ifort.cfg`` and ``gfortran.cfg``.  You can edit these files to modify the compiling options. 

#### What utilization of the code will you do?

* If you only want the binaries (for production workflow) use the flag
  `--production` in when calling this script. It's quicker 
* Else if you are a developer and you want to be able to compile specific modules use: `--development`. It will create for you the `build.ninja` in each module

### 2) Set environment variable
 
    source quantum_package.rc
This file contains all the environment variables needed by the quantum package both to compile and run. This should also be done before running calculations.

### Optional) Add some new module

      Usage: qp_install_module.py list (--installed|--avalaible-local|--avalaible-remote)
       qp_install_module.py install <name>...
       qp_install_module.py create -n <name> [<children_module>...]
       qp_install_module.py download -n <name> [<path_folder>...]

 For exemple you can type :
`qp_install_module.py install Full_CI`

### 3) Compiling the fortran

    ninja 
Just type `ninja` if you are in `$QP_ROOT` (or `ninja -f $QP_ROOT/build.ninja`
elsewhere). The compilation will take approximately 3 min.

If you have set the `--developement` flag in a specific module you can go in
the corresponding module directory and run `ninja` to build only this module.
You can type `ninja all` in a module for compiling all the submodule


### 4) Compiling the OCaml

    cd ocaml ; make ; cd -

### 5) Testing if all is ok

    cd testing_no_regression ; ./unit_test.py

## Installing behind a firewall !

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

# Note on EZFIO.cfg

##Format specification :

```
Required:
    [<provider_name>]   The name of the provider in irp.f90 and in the EZFIO lib
    doc:<str>           The plain text documentation
    type:<str>          A Fancy_type supported by the ocaml.
                            type `ei_handler.py get_supported_type` for a list
    interface:<str>     The interface is list of string sepeared by ","  who can containt :
                          - ezfio (if you only whant the ezfiolib)
                          - provider (if you want the provider)
                          - ocaml (if you want the ocaml gestion)
Optional:
    default: <str>      The default value needed,
                            if 'ocaml' is in interface list.
                           ! No list is allowed for now !
    size: <str>         The size information.
                            (by default is one)
                            Example : 1, =sum(ao_num); (ao_num,3)
                            ATTENTION : The module and the value are separed by a "." not a "_".
                            For exemple (determinants.n_det)
    ezfio_name: <str>   The name for the EZFIO lib
                             (by default is <provider_name>)
    ezfio_dir: <str>    Will be the folder of EZFIO.
                              (by default is <module_lower>)
```

##Example of EZFIO.cfg:

```
[thresh_SCF]
doc: Threshold on the convergence of the Hartree Fock energy
type: Threshold
default: 1.e-10
interface: provider,ezfio,ocaml
size: 1

[energy]
type: Strictly_negative_float
doc: Calculated HF energy
interface: ezfio
```

#FAQ

### Error: ezfio_* is already defined.

#### Why ?

You have two or more ezfio configuration file for the same variable. Check in `$QP_ROOT/install/config/`

#### Fix

    - rm $QP_ROOT/install/EZFIO/config/*
    - ninja 

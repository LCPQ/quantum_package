![QP](https://raw.githubusercontent.com/LCPQ/quantum_package/master/data/qp.png)     
[![Build Status](https://travis-ci.org/LCPQ/quantum_package.svg?branch=master)](https://travis-ci.org/LCPQ/quantum_package)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/LCPQ/quantum_package?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Set of quantum chemistry programs and libraries. 
(under GNU GENERAL PUBLIC LICENSE v2)

For more information, you can visit the [wiki of the project](http://github.com/LCPQ/quantum_package/wiki>), or below for the installation instructions.



Demo
====

[![Full-CI energy of C2 in 2 minutes](https://i.vimeocdn.com/video/555047954_295x166.jpg)](https://vimeo.com/scemama/quantum_package_demo "Quantum Package Demo")

[![Frozen-core Full-CI energy of Ti](https://raw.githubusercontent.com/LCPQ/quantum_package/master/data/Titanium.png)](https://raw.githubusercontent.com/LCPQ/quantum_package/master/data/Titanium.png "Convergence of Ti in cc-pv{DTQ}Z")

# Installation


## Requirements
* Fortran compiler (`ifort` and `gfortran` are tested)
* Python >= 2.6
* GNU make
* Bash
* Blas/Lapack
* unzip
* g++ (For ninja)

## Standard installation

### 1) Configure

    $ ./configure <config_file> (--production | --development)

For example you can type `./configure config/gfortran.cfg --production`

This command has two purposes :

 - Download and install all the requirements.
   Installing OCaml and the Core library may take some time (up to 20min on an old machine).
 - Create the file which contains all the dependencies for the binaries.  
   It's not a Makefile, but a Ninja file (so don't type `make` is hopeless, type `ninja` instead)

####Compilation Flags (`<config_file>`)

`<config_file>` is the path to the file which contains all the compilation flags (optimization flags, Lapack libary, etc). There are two example configure files in  ``$QP_ROOT/config`` : ``ifort.cfg`` and ``gfortran.cfg``.  You can copy these files to create a new file adapted to your architecture. 

#### What utilization of the code will you do?

* If you only want the binaries (for production workflow) use the flag
  `--production`. It compiles faster. 
* Else if you are a developer and you want to be able to compile specific modules use: `--development`. It will create the `build.ninja` in each module.

### 2) Load environment variables
 
    source quantum_package.rc
    
This file contains all the environment variables needed by the quantum package both to compile and run. This should also be done before running calculations.

### Optional) Add some modules

```
Usage:
       qp_module.py create -n <name> [<children_modules>...]
       qp_module.py download -n <name> [<path_folder>...]
       qp_module.py install <name>...
       qp_module.py list (--installed | --available-local)
       qp_module.py uninstall <name>
```

 For exemple you can type :
`qp_module.py install Full_CI`

### 3) Compiling the Fortran

Just type `ninja` if you are in `$QP_ROOT` (or `ninja -f $QP_ROOT/build.ninja` elsewhere). The compilation will take approximately 3 min.

If you have set the `--developement` flag you can go in any module directory and run `ninja` to build only this particular module. You can type `ninja all` in a module to compile all the submodules.


### 4) Compiling the OCaml

    make -C ocaml

### 5) Testing if all is ok

    cd tests ; bats bats/qp.bats 



# Note on EZFIO.cfg

##Format specification :

```
Required:
    [<provider_name>]   The name of the provider in irp.f90 and in the EZFIO lib
    doc:<str>           The plain text documentation
    type:<str>          A type supported by the OCaml.
                            type `ei_handler.py get_supported_type` for a list
    interface:<str>     The interface is a list of strings sepeared by ","  which can contain :
                          - ezfio : to build the EZFIO API
                          - provider : to build the corresponding providers 
                          - ocaml : to build the corresponding bindings in OCaml
Optional:
    default: <str>      The default value,
                            needed if 'ocaml' is in interface list.
                           ! No list is allowed for now !
    size: <str>         The size information.
                            (by default is one)
                            Example : 1; =sum(ao_num); (ao_num,3)
                            WARNING : The module and the value are separed by a "." not a "_".
                            For example (determinants.n_det)
    ezfio_name: <str>   The name in the EZFIO API
                             (by default is <provider_name>)
    ezfio_dir: <str>    Will be the directory of EZFIO.
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

You have two or more ezfio configuration files for the same variable. Check files in `$QP_ROOT/install/EZFIO/config/`

#### Fix

    - rm $QP_ROOT/install/EZFIO/config/*
    - ninja 
    

### Error: Seg Fault (139)

```
Segmentation fault (core dumped)
Program exited with code 139.
```

#### Why ?

It's caused when we call the DGEMM routine of LAPACK. 

##### Fix

Set `ulimit -s unlimited`, before runing `qp_run`. It seem to fix the problem.


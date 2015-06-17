# `ei_handler.py`

This script in located in `$QP_ROOT/scripts/ezfio_interface/`.
It provide all the resource need to deal with the `EZFIO.cfg` files :
    - The creation of `$MODULE_LOWER_ezfio_config` in `$QP_ROOT/ezfio/config`
    - The `ezfio_interface.irp.f` who containt all the provider associate (in `$MODULE/`)
    - The `$MODULE_LOWER_ezfio_defaults` in `$QP_ROOT/data/`
    - The `Input_$MODULE_LOWER.ml` for the *qp_edit*

For more information you can type `ei_handler.py -h`

# `module_handler.py`

This script in located in `$QP_ROOT/scripts/module/`.
It provide all the resource related to the tree dependency of the modules.
If more useful as a librairy than a cli.

It have some usefull property:
    - The list of module
    - The dict of the descendant
    - The dict of the parent
    - The dict of the child
    - The dict of the root
    - The list reduced tree (For a list of module in input return only the root)

For tree syntax you can check http://en.wikipedia.org/wiki/Tree_%28data_structure%29#Terminologies_used_in_Trees

In the cli mode:
    - From a `NEEDED_CHILDREN_MODULE` file you can have all the descendant, and a png
    representation who correspond.


# `qp_install_module.py`
This script is located in `$QP_ROOT/scripts/module/`.

It is usefull when you need to install a new module. (From the soon to come repo or from scratch).

# `qp_create_ninja.py`

This script is located in `$QP_ROOT/scripts/compilation/`.
It will create the `build.ninja` file. It will use intersifly the `module_handler.py` module.

To read all the flag for the compilation the module `read_compilation_cfg.py` is used. 
You only need to know, that all flag are appending.

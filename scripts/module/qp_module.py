#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Usage:
       qp_module.py create -n <name> [<children_modules>...]
       qp_module.py download -n <name> [<path_folder>...]
       qp_module.py install <name>...
       qp_module.py list (--installed | --available-local)
       qp_module.py uninstall <name>
Options:
    list: List all the modules available
    create: Create a new module
"""

import sys
import os
import subprocess

try:
    from docopt import docopt
    from module_handler import ModuleHandler, get_dict_child
    from module_handler import get_l_module_descendant
    from qp_update_readme import D_KEY
    from qp_path import QP_SRC, QP_PLUGINS, QP_ROOT
except ImportError:
    print "Please check if you have sourced the ${QP_ROOT}/quantum_package.rc"
    print "(`source ${QP_ROOT}/quantum_package.rc`)"
    print sys.exit(1)


def save_new_module(path, l_child):

    # ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ #
    # N E E D E D _ C H I L D R E N _ M O D U L E S #
    # ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ #

    try:
        os.makedirs(path)
    except OSError:
        print "The module ({0}) already exists...".format(path)
        sys.exit(1)

    with open(os.path.join(path, "NEEDED_CHILDREN_MODULES"), "w") as f:
        f.write(" ".join(l_child))
        f.write("\n")

    # ~#~#~#~#~#~#~ #
    # R E A D _ M E #
    # ~#~#~#~#~#~#~ #

    module_name = os.path.basename(path)

    header = "{0}\n{1}\n{0}\n".format("=" * len(module_name), module_name)

    with open(os.path.join(path, "README.rst"), "w") as f:
        f.write(header + "\n")
        f.write(D_KEY["needed_module"])
        f.write(D_KEY["documentation"])


if __name__ == '__main__':
    arguments = docopt(__doc__)

    if arguments["list"]:

        if arguments["--installed"]:
            l_repository = [QP_SRC]
        elif arguments["--available-local"]:
            l_repository = [QP_PLUGINS]

        m_instance = ModuleHandler(l_repository)

        for module in sorted(m_instance.l_module):
            print "* {0}".format(module)

    elif arguments["create"]:
        m_instance = ModuleHandler([QP_SRC])

        l_children = arguments["<children_modules>"]

        name = arguments["<name>"][0]

        path = os.path.join(QP_PLUGINS, name)

        print "Created module:"
        print path, '\n'

        for children in l_children:
            if children not in m_instance.dict_descendant:
                print "This module ({0}) is not a valid module.".format(children)
                print "Run `list` for the list of available modules."
                print "Maybe you need to install some other module first."
                print "Aborting..."
                sys.exit(1)

        print "Needed module:"
        print l_children, '\n'

        print "This corresponds to using the following modules:"
        print l_children + m_instance.l_descendant_unique(l_children), '\n'

        print "Which is reduced to:"
        l_child_reduce = m_instance.l_reduce_tree(l_children)
        print l_child_reduce, '\n'

        print "Installation",
        save_new_module(path, l_child_reduce)

        print "    [ OK ]"
        print "Your module is created in the `plugins` directory."
        print "You need to create some `.irp.f` to be able to install it."
#        print "` {0} install {1} `".format(os.path.basename(__file__), name)
        print ""

    elif arguments["download"]:
        pass
#        d_local = get_dict_child([QP_SRC])
#        d_remote = get_dict_child(arguments["<path_folder>"])
#
#        d_child = d_local.copy()
#        d_child.update(d_remote)
#
#        name = arguments["<name>"]
#        l_module_descendant = get_l_module_descendant(d_child, [name])
#
#        for module in l_module_descendant:
#            if module not in d_local:
#                print "you need to install", module

    elif arguments["install"]:

        d_local = get_dict_child([QP_SRC])
        d_plugin = get_dict_child([QP_PLUGINS])

        d_child = d_local.copy()
        d_child.update(d_plugin)

        l_name = arguments["<name>"]

        for name in l_name:
            if name in d_local:
                print "{0} Is already installed".format(name)

        l_module_descendant = get_l_module_descendant(d_child, l_name)

        l_module_to_cp = [module for module in l_module_descendant if module not in d_local]

        if l_module_to_cp:

            print "You will need all these modules"
            print l_module_to_cp

            print "Installation...",

            for module_to_cp in l_module_to_cp:
                    src = os.path.join(QP_PLUGINS, module_to_cp)
                    des = os.path.join(QP_SRC, module_to_cp)
                    try:
                        os.symlink(src, des)
                    except OSError:
                        print "Your src directory is broken. Please remove %s" % des
                        raise
            try:
                subprocess.check_call(["qp_create_ninja.py", "update"])
            except:
                raise

            print "[ OK ]"
            print "You can now compile as usual"
            print "`cd {0} ; ninja` for exemple".format(QP_ROOT)
            print " or --in developement mode-- you can cd in a directory and compile here"

    elif arguments["uninstall"]:

        m_instance = ModuleHandler([QP_SRC])
        d_descendant = m_instance.dict_descendant

        d_local = get_dict_child([QP_SRC])
        l_name = arguments["<name>"]

        l_failed = [name for name in l_name if name not in d_local]

        if l_failed:
            print "Modules not installed:"
            for name in sorted(l_failed):
                print "* %s" % name
            sys.exit(1)

        l_name_to_remove = l_name + [module for module in m_instance.l_module for name in l_name if name in d_descendant[module]]

        print "You will remove all of:"
        print l_name_to_remove

        for module in set(l_name_to_remove):

            try:
                subprocess.check_call(["module_handler.py", "clean", module])
            except:
                raise

        for module in set(l_name_to_remove):

            try:
                os.unlink(os.path.join(QP_SRC, module))
            except OSError:
                print "%s is a core module which can't be removed" % module


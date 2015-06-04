#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Usage: qp_create_module.py list
       qp_create_module.py create -n <name> [<children_module>...]
"""

import sys
import os

try:
    from docopt import docopt
    from module_handler import ModuleHandler
    from update_README import Doc_key, Needed_key
except ImportError:
    print "source .quantum_package.rc"
    raise


def save_new_module(path, l_child):

    # ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ #
    # N E E D E D _ C H I L D R E N _ M O D U L E S #
    # ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ #

    try:
        os.makedirs(path)
    except OSError:
        print "The module ({0}) already exist...".format(path)
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
        f.write(Doc_key + "\n")
        f.write(Needed_key + "\n")


if __name__ == '__main__':
    arguments = docopt(__doc__)

    if arguments["list"]:
        for module in ModuleHandler.l_module:
            print module

    elif arguments["create"]:
        l_children = arguments["<children_module>"]

        qpackage_root = os.environ['QPACKAGE_ROOT']
        path = os.path.join(qpackage_root, "src", arguments["<name>"])

        print "You will create the module:"
        print path

        for children in l_children:
            if children not in ModuleHandler.dict_descendant:
                print "This module ({0}) is not a valide module.".format(children)
                print "Run `list` flag for the list of module avalaible"
                print "Aborting..."
                sys.exit(1)

        print "You ask for this submodule:"
        print l_children

        print "You can use all the routine in this module"
        print l_children + ModuleHandler.l_descendant_unique(l_children)

        print "This can be reduce to:"
        l_child_reduce = ModuleHandler.l_reduce_tree(l_children)
        print l_child_reduce
        save_new_module(path, l_child_reduce)

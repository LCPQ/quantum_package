#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Create the NEEDED_MODULE
    aka the genealogy (children module, subchildren module and so on),
of a NEEDED_CHILDREN_MODULES file

Usage:
    module_handler.py print_genealogy       [<NEEDED_CHILDREN_MODULES>]
    module_handler.py save_makefile_depend
    module_handler.py create_symlick        [<NEEDED_CHILDREN_MODULES>]
    module_handler.py create_png            [<NEEDED_CHILDREN_MODULES>]

Options:
    print_genealogy         Print the genealogy of the NEEDED_CHILDREN_MODULES
                                 aka (children, subchildren, etc)
    create_png              Create a png of the file
    NEEDED_CHILDREN_MODULES The path of NEEDED_CHILDREN_MODULES
                                by default try to open the file in the current path
"""

from docopt import docopt

import os
import sys
import os.path
from cache import cache


@cache
def get_dict_genealogy():
    """Loop over MODULE in  QPACKAGE_ROOT/src, open all the NEEDED_CHILDREN_MODULES
    and create a dict[MODULE] = [sub module needed, ...]
    """
    d_ref = dict()

    qpackage_root = os.environ['QPACKAGE_ROOT']
    dir_ = os.path.join(qpackage_root, 'src')

    for o in os.listdir(dir_):

        try:
            with open(os.path.join(dir_, o, "NEEDED_CHILDREN_MODULES"), "r") as f:
                l_children = f.read().split()
        except IOError:
            pass
        else:
            d_ref[o] = l_children

    return d_ref


def module_genealogy(path):
    """
    Take a name of a NEEDED_CHILDREN_MODULES
    and return a list of all the {sub, subsub, ...}children
    """
    try:
        with open(path, "r") as f:
            l_children = f.read().split()
    except IOError as e:
        print >> sys.stderr, e
        sys.exit(1)
    else:

        needed_module = get_it_and_children(l_children)

        return needed_module


def get_it_and_children(l_module):
    """
    From a list of module return the module and all of the genealogy
    """
    d_ref = get_dict_genealogy()

    l = []
    for module in l_module:
        if module not in l:
            l.append(module)
            try:
                l.extend(get_it_and_children(d_ref[module]))
            except KeyError:
                print >> sys.stderr, "`{0}` in not a good submodule name".format(
                    module)
                print >> sys.stderr, "Check the corresponding NEEDED_CHILDREN_MODULES"
                sys.exit(1)

    return list(set(l))


def get_all_children(l_module):
    """
    From a list of module return all the genealogy
    """

    it_and_all = get_it_and_children(l_module)
    return [children for children in it_and_all if children not in l_module]


def reduce_(l_module):
    """
    Take a l_module and try to find the lower combinaitions
    of module with the same genealogy
    """
    import itertools
    d_ref = get_dict_genealogy()

    target_genealogy = sorted(get_all_children(l_module))

    for i in xrange(len(d_ref)):
        for c in itertools.combinations(d_ref, i):

            guess_genealogy = sorted(get_it_and_children(d_ref, c))

            if target_genealogy == guess_genealogy:
                return c


def get_list_depend(l_module):
    """
    transform

    SRC = Utils.f90 test.f90
    OBJ = IRPF90_temp/map_module.o

    into

    ['Utils/map_module.f90','test.f90']
    ['IRPF90_tmp/Utils/map_module.o']
    """

    def get_list(sep):
        # Split for sep
        dump = [l.split(sep)[1] for l in data if l.startswith(sep)]

        # Delete the empy one
        l_unique = [k for k in map(str.strip, dump) if k]

        # Return the flat one (if multi in l_unique)
        l_flat = [j for i in l_unique for j in i.split()]
        return l_flat

    qpackage_root = os.environ['QPACKAGE_ROOT']
    dir_ = os.path.join(qpackage_root, 'src')

    l_src = []
    l_obj = []

    for module in l_module:
        path = os.path.join(dir_, module, "Makefile")

        with open(path, 'r') as f:
            data = f.readlines()

        l_src.extend("{0}/{1}".format(module, i) for i in get_list("SRC="))

        l_obj.extend(["IRPF90_temp/{0}/{1}".format(module, os.path.basename(i))
                      for i in get_list("OBJ=")])

    return l_src, l_obj


def save_makefile_depend(l_src, l_obj):
    header = "# This file was created by the module_handler.py script. Do not modify it by hand."

    try:
        with open("Makefile.depend", "r") as f:
            old_output = f.read()
    except IOError:
        old_output = None

    output = "\n".join([header,
                        "\n",
                        "SRC+= {0}".format(" ".join(l_src)),
                        "OBJ+= {0}".format(" ".join(l_obj)),
                        "\n"])

    if output != old_output:
        with open("Makefile.depend", "w+") as f:
            f.write(output)


def create_png_from_path(path):
    " Change a path like this into a module list"
    "path = /home/razoa/quantum_package/src/Molden/NEEDED_CHILDREN_MODULES"

    l_module = os.path.split(path)[0].split("/")[-1]

    import pydot
    try:
        create_png([l_module])
    except pydot.InvocationException:
        pass


def create_png(l_module):
    """Create the png of the dependancy tree for a l_module"""

    # Init
    import pydot
    all_ready_done = []

    def draw_module_edge(module, l_children):
        "Draw all the module recursifly"

        if module not in all_ready_done:
            for children in l_children:
                # Add Edge
                edge = pydot.Edge(module, children)
                graph.add_edge(edge)
                # Recurs
                draw_module_edge(children, d_ref[children])
            all_ready_done.append(module)

    # Init
    graph = pydot.Dot(graph_type='digraph')
    d_ref = get_dict_genealogy()

    # Create all the edge
    for module in l_module:
        node_a = pydot.Node(module, fontcolor="red")
        graph.add_node(node_a)
        draw_module_edge(module, d_ref[module])

    # Save
    path = '{0}.png'.format("tree_dependancy")
    # path = '{0}.png'.format("_".join(l_module))
    # print "png saved in {0}".format(path)

    graph.write_png(path)

if __name__ == '__main__':

    arguments = docopt(__doc__)

    if not arguments['<NEEDED_CHILDREN_MODULES>']:
        dir_ = os.getcwd()
        path = os.path.join(dir_, "NEEDED_CHILDREN_MODULES")
    else:
        path = os.path.abspath(arguments['<NEEDED_CHILDREN_MODULES>'])
        path = os.path.expanduser(path)
        path = os.path.expandvars(path)

    if arguments['print_genealogy']:
        l_all_needed_molule = module_genealogy(path)
        print " ".join(sorted(l_all_needed_molule))

        get_list_depend(l_all_needed_molule)

    if arguments['create_symlick']:
        src = os.getcwd()

        for link_name in module_genealogy(path) + ["include"]:

            source = os.path.join(
                "/home/razoa/quantum_package/src/",
                link_name)
            try:
                os.symlink(source, link_name)
            except OSError:
                pass

    if arguments['save_makefile_depend']:
        l_all_needed_molule = module_genealogy(path)
        l_src, l_obj = get_list_depend(l_all_needed_molule)
        save_makefile_depend(l_src, l_obj)

    if arguments["create_png"]:
        create_png_from_path(path)

#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Create the NEEDED_MODULE
    aka the genealogy (children module, subchildren module and so on),
of a NEEDED_CHILDREN_MODULES file

Usage:
    module_handler.py print_genealogy       [<NEEDED_CHILDREN_MODULES>]
    module_handler.py create_png            [<NEEDED_CHILDREN_MODULES>]
    module_handler.py head_module

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

from collections import namedtuple
Dependancy = namedtuple('Dependancy', ['src', 'obj'])
Module_info = namedtuple('Module_info', ['l_children', 'l_dependancy'])


def get_list_from_makefile(data, sep):
    # Split for sep
    dump = [l.split(sep)[1] for l in data if l.startswith(sep)]

    # Delete the empy one
    l_unique = [k for k in map(str.strip, dump) if k]

    # Return the flat one (if multi in l_unique)
    l_flat = [j for i in l_unique for j in i.split()]
    return l_flat


# Canot cache for namedtuple are not hashable
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
            continue

        try:
            with open(os.path.join(dir_, o, "Makefile"), "r") as f:
                data = f.readlines()
                l_depend = Dependancy(get_list_from_makefile(data, "SRC="),
                                      get_list_from_makefile(data, "OBJ="))
        except IOError:
            l_depend = []

        d_ref[o] = Module_info(l_children, l_depend)

    return d_ref


def him_and_all_children(d_ref, l_module):
    """
    From a list of module return the module and all of the genealogy
    """

    l = []
    for module in l_module:
        if module not in l:
            l.append(module)
            try:
                l.extend(him_and_all_children(d_ref, d_ref[module].l_children))
            except KeyError:
                print >> sys.stderr, "`{0}` in not a good submodule name".format(module)
                print >> sys.stderr, "Check the corresponding NEEDED_CHILDREN_MODULES"
                sys.exit(1)

    return list(set(l))


def module_genealogy(module_name):
    """
    Take a name of a NEEDED_CHILDREN_MODULES
    and return a list of all the {sub, subsub, ...}children
    """

    d_ref = get_dict_genealogy()
    return him_and_all_children(d_ref, d_ref[module_name].l_children)


def file_dependancy(module_name):

    d_ref = get_dict_genealogy()
    l_src, l_obj = d_ref[module_name].l_dependancy

    l_children_module = him_and_all_children(d_ref, d_ref[module_name].l_children)
    for module in l_children_module:
        l_src_dump, l_obj_dump = d_ref[module].l_dependancy
        l_src.extend("{0}/{1}".format(module, i) for i in l_src_dump)
        l_obj.extend("IRPF90_temp/{0}/{1}".format(module, os.path.basename(i)) for i in l_obj_dump)

    return Dependancy(l_src, l_obj)


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
                draw_module_edge(children, d_ref[children].l_children)
            all_ready_done.append(module)

    # Init
    graph = pydot.Dot(graph_type='digraph')
    d_ref = get_dict_genealogy()

    # Create all the edge
    for module in l_module:
        node_a = pydot.Node(module, fontcolor="red")
        graph.add_node(node_a)
        draw_module_edge(module, d_ref[module].l_children)

    # Save
    path = '{0}.png'.format("tree_dependancy")
    # path = '{0}.png'.format("_".join(l_module))
    # print "png saved in {0}".format(path)

    graph.write_png(path)

if __name__ == '__main__':

    arguments = docopt(__doc__)

    if not arguments['<NEEDED_CHILDREN_MODULES>']:
        dir_ = os.getcwd()
    else:
        path_file = os.path.abspath(arguments['<NEEDED_CHILDREN_MODULES>'])
        path_file = os.path.expanduser(path_file)
        path_file = os.path.expandvars(path_file)
        dir_ = os.path.dirname(path_file)

    path_file = os.path.basename(dir_)

    if arguments['print_genealogy']:
        l_all_needed_molule = module_genealogy(path_file)
        print " ".join(sorted(l_all_needed_molule))

    if arguments["create_png"]:
        create_png_from_path(path_file)

    if arguments["head_module"]:
            d_ref = get_dict_genealogy()
            l_all_module = d_ref.keys()

    from itertools import combinations

    print "len_ref", len(l_all_module)
    l_head = []
    # All the head module alredy discover

    l_head_and_genealogy = []

    # All the children of l_head
    l_check = [i for i in l_all_module if i not in l_head_and_genealogy]

    while l_check:
        len_max = -1
        head = None

        # Find the module in l_check who have the most direct children
        # not alredy find
        # We do not use the all genealogy for saving few second
        for i in l_check:
            l_module_and_children = [i] + d_ref[i].l_children

            l_missing_module = [l for l in l_module_and_children if l not in l_head_and_genealogy]

            if len(l_missing_module) > len_max:
                len_max = len(l_missing_module)
                head = i
        # Now add all the genealogy and remove duplicate
        l_head_and_genealogy = list(set(l_head_and_genealogy + him_and_all_children(d_ref, [head])))
        # Now only search in the missing module
        l_check = [i for i in l_all_module if i not in l_head_and_genealogy]

        l_head.append(head)

    print "l_head", l_head
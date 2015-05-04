#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Create the NEEDED_MODULE
    aka the genealogy (children module, subchildren module and so on),
of a NEEDED_CHILDREN_MODULES file

Usage:
    module_handler.py   print_genealogy [<NEEDED_CHILDREN_MODULES>]
    module_handler.py   create_png      [<NEEDED_CHILDREN_MODULES>]

Options:
    print_genealogy    Print the genealogy of the NEEDED_CHILDREN_MODULES
                            aka (children, subchildren, etc)
                        if NEEDED_CHILDREN_MODULES
                            try to open it in the current path
"""

from docopt import docopt

import os
import sys
import os.path
from functools import wraps


def cache(func):
    """
    A decorator for lazy evaluation off true function
    """
    saved = {}

    @wraps(func)
    def newfunc(*args):
        if args in saved:
            return saved[args]

        result = func(*args)
        saved[args] = result
        return result
    return newfunc


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
    except IOError:
        return []
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
                print >> sys.stderr, "`{0}` in not a good submodule name".format(module)
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


def create_png_from_path(path):
    " Change a path like this into a module list"
    "path = /home/razoa/quantum_package/src/Molden/NEEDED_CHILDREN_MODULES"

    l_module = os.path.split(path)[0].split("/")[-1]
    create_png([l_module])


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
    path = '{0}.png'.format("_".join(l_module))
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

    if arguments["create_png"]:
        create_png_from_path(path)

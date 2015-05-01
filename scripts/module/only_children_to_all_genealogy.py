#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import os.path

dir_ = '/home/razoa/quantum_package/src'


def get_dict_genealogy(all_children=False):

    d_ref = dict()

    for o in os.listdir(dir_):

        try:
            with open(os.path.join(dir_, o, "NEEDED_CHILDREN_MODULES"), "r") as f:
                l_children = f.read().split()
        except IOError:
            pass
        else:
            d_ref[o] = l_children

    if all_children:
        for module in d_ref:
            d_ref[module] = get_all_children(d_ref, d_ref[module], [])

    return d_ref


def module_children_to_all(d_ref,path):

    if not path:
        dir_ = os.getcwd()
        path = os.path.join(dir_, "NEEDED_CHILDREN_MODULES")

    try:
        with open(path, "r") as f:
            l_children = f.read().split()
    except IOError:
        return []
    else:
        needed_module = l_children
        for module in l_children:

            for children in get_all_children(d_ref, d_ref[module], []):
                if children not in needed_module:
                    needed_module.append(children)

        return needed_module


def get_all_children(d_ref, l_module, l=[]):
    """
    From a d_ref (who containt all the data --flatter or not-- create
        an flatten list who contain all the children
    """
    for module in l_module:
        if module not in l:
            l.append(module)
            get_all_children(d_ref, d_ref[module], l)

    return list(set(l))


def reduce_(d_ref, name):

    """
    Take a big list and try to find the lower parent
    available
    """
    import itertools

    a = sorted(get_all_children(d_ref[name]))

    for i in xrange(len(d_ref)):
        for c in itertools.combinations(d_ref, i):

                l = []
                b = sorted(get_all_children(c, l))

                if a == b:
                    return c

#for i in sorted(d_ref):
#    print i, reduce_(i)
#

if __name__ == '__main__':
    import sys

    try:
        path = sys.argv[1]
    except IndexError:
        path = None

    d_ref = get_dict_genealogy()

    l_all_needed_molule = module_children_to_all(d_ref, path)
    print " ".join(sorted(l_all_needed_molule))

#    print d_ref
#
#    d_ref = get_dict_genealogy(True)
#
#    print d_ref
#
#    module_hl_to_ll(d_ref)

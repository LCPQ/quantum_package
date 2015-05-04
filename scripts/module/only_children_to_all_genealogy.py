#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import os.path
from functools import wraps


def cache(func):
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
    if not path:
        dir_ = os.getcwd()
        path = os.path.join(dir_, "NEEDED_CHILDREN_MODULES")

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
            l.extend(get_it_and_children(d_ref[module]))

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


if __name__ == '__main__':
    import sys

    try:
        path = sys.argv[1]
    except IndexError:
        path = None

    l_all_needed_molule = module_genealogy(path)
    print " ".join(sorted(l_all_needed_molule))

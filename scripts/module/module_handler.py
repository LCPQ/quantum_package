#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Module utilitary

Usage:
    module_handler.py print_descendant      [<module_name>...]
    module_handler.py create_png            [<module_name>...]
    module_handler.py clean                 [ --all | <module_name>...]
    module_handler.py create_git_ignore     [<module_name>...]

Options:
    print_descendant         Print the genealogy of the NEEDED_CHILDREN_MODULES
                                 aka (children, subchildren, etc)
    create_png              Create a png of the file
    NEEDED_CHILDREN_MODULES The path of NEEDED_CHILDREN_MODULES
                                by default try to open the file in the current path
"""
import os
import sys
import os.path
import shutil

try:
    from docopt import docopt
    from qp_path import QP_SRC, QP_ROOT, QP_PLUGINS
except ImportError:
    print "source .quantum_package.rc"
    raise


def is_module(path_module_rel):
    return os.path.isfile(os.path.join(QP_SRC, path_module_rel,
                                       "NEEDED_CHILDREN_MODULES"))


def is_plugin(path_module_rel):
    return os.path.isfile(os.path.join(QP_PLUGINS, path_module_rel,
                                       "NEEDED_CHILDREN_MODULES"))


def is_exe(fpath):
    return os.path.isfile(fpath) and os.access(fpath, os.X_OK) and not fpath.endswith(".py")


def get_dict_child(l_root_abs=None):
    """Loop over MODULE in  QP_ROOT/src, open all the NEEDED_CHILDREN_MODULES
    and create a dict[MODULE] = [sub module needed, ...]
    """
    d_ref = dict()

    if not l_root_abs:
        l_root_abs = [QP_SRC]

    for root_abs in l_root_abs:
        for module_rel in os.listdir(root_abs):

            module_abs = os.path.join(root_abs, module_rel)
            try:
                path_file = os.path.join(module_abs, "NEEDED_CHILDREN_MODULES")

                with open(path_file, "r") as f:
                    l_children = f.read().split()
            except IOError:
                pass
            else:
                if module_rel not in d_ref:
                    d_ref[module_rel] = l_children
                else:
                    print "Module {0} alredy defined"
                    print "Abort"
                    sys.exit(1)

    return d_ref


def get_l_module_descendant(d_child, l_module):
    """
    From a list of module return the module and descendant
    """

    l = []
    for module in l_module:
        if module not in l:
            l.append(module)
            try:
                l.extend(get_l_module_descendant(d_child, d_child[module]))
            except KeyError:
                print >> sys.stderr, "Error: "
                print >> sys.stderr, "`{0}` is not a submodule".format(module)
                print >> sys.stderr, "Check the typo (spelling, case, '/', etc.) "
                sys.exit(1)

    return list(set(l))


class ModuleHandler():
    def __init__(self, l_root_abs=None):
        self.dict_child = get_dict_child(l_root_abs)

    @property
    def l_module(self):
        return self.dict_child.keys()

    @property
    def dict_parent(self):
        """
        Get a dic of the first parent
        """
        d_child = self.dict_child

        d = {}

        for module_name in d_child:
            d[module_name] = [i for i in d_child.keys()
                              if module_name in d_child[i]]

        return d

    @property
    def dict_descendant(self):
        """
        Get a dic of all the genealogy desc (children and all_children)
        """
        d = {}

        d_child = self.dict_child

        for module_name in d_child:
            try:
                d[module_name] = get_l_module_descendant(d_child,
                                                         d_child[module_name])
            except KeyError:
                print "Check NEEDED_CHILDREN_MODULES for {0}".format(
                    module_name)
                sys.exit(1)

        return d

    @property
    def dict_root(self):
        """
        Return a dict(module_name) = module_boss
        The top node in a tree.
        """
        d_asc = self.dict_parent
        d_desc = self.dict_descendant

        l_all_module = self.l_module

        dict_root = {}

        for module in l_all_module:
            dict_root[module] = [p for p in l_all_module
                                 if module in [p] + d_desc[p] and not d_asc[p]
                                 ][0]

        return dict_root

    def l_descendant_unique(self, l_module):
        d_desc = self.dict_descendant

        d = {}
        for module in l_module:
            for e in d_desc[module]:
                d[e] = 1

        return d.keys()

    def l_reduce_tree(self, l_module):
        """For a list of module in input return only the root"""
        l_d_u = self.l_descendant_unique(l_module)
        l_module_reduce = []
        for module in l_module:
            if module not in l_d_u:
                l_module_reduce.append(module)

        return l_module_reduce

    def create_png(self, l_module):
        """Create the png of the dependency tree for a l_module"""

        # Don't update if we are not in the main repository
        from is_master_repository import is_master_repository
        if not is_master_repository:
            return

        basename = "tree_dependency"
        path = '{0}.png'.format(basename)

        from graphviz import Digraph

        all_ready_done = []

        def draw_module_edge(module, l_children):
            "Draw all the module recursifly"

            if module not in all_ready_done:
                for children in l_children:
                    # Add Edge
                    graph.edge(module, children)
                    # Recurs
                    draw_module_edge(children, d_ref[children])
                all_ready_done.append(module)

        graph = Digraph(comment=l_module, format="png", filename=basename)
        d_ref = self.dict_child

        # Create all the edge
        for module in l_module:
            graph.node(module, fontcolor="red")
            draw_module_edge(module, d_ref[module])

        # Try to render the png
        # If not just touch it
        try:
            graph.render(cleanup=True)
        except:
            with open(path, 'a'):
                os.utime(path, None)
            return


if __name__ == '__main__':

    arguments = docopt(__doc__)

    if arguments['--all']:
        l_module = [f for f in os.listdir(QP_SRC)
                    if os.path.isdir(os.path.join(QP_SRC, f))]

    elif not arguments['<module_name>']:
        dir_ = os.getcwd()
        l_module = [os.path.basename(dir_)]
    else:
        l_module = arguments['<module_name>']

    for module in l_module:
        if not is_module(module):
            print "{0} is not a valide module. Abort".format(module)
            print "No NEEDED_CHILDREN_MODULES in it"
            sys.exit(1)

    m = ModuleHandler()

    if arguments['print_descendant']:

        for module in l_module:
            print " ".join(sorted(m.l_descendant_unique([module])))

    if arguments["create_png"]:
        try:
            m.create_png(l_module)
        except RuntimeError:
            pass
        except SyntaxError:
            print "Warning: The graphviz API dropped support for python 2.6."
            pass

    if arguments["clean"] or arguments["create_git_ignore"]:

        l_dir = ['IRPF90_temp', 'IRPF90_man']
        l_file = ["irpf90_entities", "tags", "irpf90.make", "Makefile",
                  "Makefile.depend", ".ninja_log", ".ninja_deps",
                  "ezfio_interface.irp.f"]

        for module in l_module:
            module_abs = os.path.realpath(os.path.join(QP_SRC, module))
            l_symlink = m.l_descendant_unique([module])
            l_exe = [f for f in os.listdir(module_abs)
                     if is_exe(os.path.join(module_abs, f))]

            if arguments["clean"]:
                for f in l_dir:
                    try:
                        shutil.rmtree(os.path.join(module_abs, f))
                    except:
                        pass

                for symlink in l_symlink:
                    try:
                        os.unlink(os.path.join(module_abs, symlink))
                    except:
                        pass

                for f in l_file:
                    try:
                        os.remove(os.path.join(module_abs, f))
                    except:
                        pass

                for f in l_exe:

                    try:
                        os.remove(os.path.join(module_abs, f))
                    except:
                        pass

            if arguments["create_git_ignore"]:

                # Don't update if we are not in the main repository
                from is_master_repository import is_master_repository
                if not is_master_repository:
                    print >>  sys.stderr, 'Not in the master repo'
                    sys.exit(0)

                path = os.path.join(module_abs, ".gitignore")

                with open(path, "w+") as f:
                    f.write("# Automatically created by {0} \n".format(__file__).replace(QP_ROOT,"$QP_ROOT"))
                    l_text = l_dir + l_file + l_symlink + l_exe
                    l_text.sort()
                    f.write("\n".join(l_text))

#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Usage: qp_create_ninja.py (--development | --production) CONFIG_FILE
"""

import os
import sys
import glob
from os.path import join
from collections import namedtuple
from collections import defaultdict

try:
    from module_handler import ModuleHandler
    from read_compilation_cfg import get_compilation_option
    from docopt import docopt
except ImportError:
    print "source .quantum_package.rc"
    sys.exit(1)

#  __
# /__ |  _  |_   _. |       _. ._ o  _. |_  |  _   _
# \_| | (_) |_) (_| |   \/ (_| |  | (_| |_) | (/_ _>
#

QP_ROOT = os.environ['QP_ROOT']
QP_ROOT_SRC = join(QP_ROOT, 'src')
QP_ROOT_EZFIO = join(QP_ROOT, 'install', 'EZFIO')

EZFIO_LIB = join(QP_ROOT, "lib", "libezfio.a")

#
# |\ |  _. ._ _   _   _|   _|_     ._  |  _
# | \| (_| | | | (/_ (_|    |_ |_| |_) | (/_
#                                  |
Path = namedtuple('Path', ['abs', 'rel'])
EZ_config_path = namedtuple('EZ_config', ['path_in_module', 'path_in_ezfio'])
EZ_handler = namedtuple('EZ_handler', ['ez_module', 'ez_cfg', 'ez_interface',
                                       'ez_config'])
Sym_link = namedtuple('Sym_link', ['source', 'destination'])
module_instance = ModuleHandler()


#  _
# |_ ._           _. ._ o  _. |_  |  _   _
# |_ | | \/   \/ (_| |  | (_| |_) | (/_ _>
def ninja_create_env_variable(pwd_config_file):
    """
    Return some ninja variable with the env variable expanded
    FC, FCFLAGS, IRPF90, IRPF90_FLAGS
    The env variable is usefull for the generation of EZFIO, and IRPF90
    """
    l_string = []
    for flag in ["FC", "FCFLAGS", "IRPF90", "IRPF90_FLAGS"]:
        str_ = "{0} = {1}".format(flag, get_compilation_option(pwd_config_file,
                                                               flag))
        l_string.append(str_)

    lib_lapack = get_compilation_option(pwd_config_file, "LAPACK_LIB")
    l_string.append("{0} = {1} {2}".format("LIB", lib_lapack, EZFIO_LIB))

    l_string.append("")

    return l_string


#  __
# /__  _  ._   _   _. |  _   _
# \_| (/_ | | (/_ (_| | (_) (_| \/
#                            _| /
def dict_module_genelogy_path(d_module_genelogy):
    """
    Just a dict with relative, and absolue path for the
    d_module_genelogy
    """
    d = dict()
    for module_rel, l_children_rel in d_module_genelogy.iteritems():
        module_abs = join(QP_ROOT_SRC, module_rel)

        p = Path(module_abs, module_rel)
        try:
            d[p] = Path(join(QP_ROOT_SRC, l_children_rel), l_children_rel)
        except:
            d[p] = [Path(join(QP_ROOT_SRC, children), children)
                    for children in l_children_rel]

    return d

#  _ __  _ ___  _         _
# |_  / |_  |  / \    _ _|_ _
# |_ /_ |  _|_ \_/ o (_  | (_|
#                           _|


def get_l_module_with_ezfio_cfg():
    """
    Return all the module who have a EZFIO.cfg
    """
    from os import listdir
    from os.path import isfile, join
    qp_src = QP_ROOT_SRC

    return [join(qp_src, m) for m in listdir(qp_src)
            if isfile(join(qp_src, m, "EZFIO.cfg"))]


def get_l_ezfio_config():
    """
    Return a namedtuple('EZ_config', ['path_in_module', 'path_in_ezfio'])
    """

    l = []

    cmd = "{0}/*/*.ezfio_config".format(QP_ROOT_SRC)
    for path_in_module in glob.glob(cmd):
        name_lower = os.path.split(path_in_module)[1].lower()
        path_in_ezfio = join(QP_ROOT_EZFIO, "config", name_lower)
        l.append(EZ_config_path(path_in_module, path_in_ezfio))

    return l


def ninja_ezfio_cfg_rule():
    """
    Return the ezfio_interface rule who will create
    the _ezfio_interface.irp.f the _ezfio_config from the EZFIO.cfg
    """

    l_string = ["rule build_ezfio_interface",
                "   command = ei_handler.py --path_module $sub_module", ""]

    return l_string


def ninja_ezfio_config_rule():
    """
    If a ezfio_config existe you just need to move it
    """
    l_string = ["rule build_ezfio_config", "   command = cp $in $out", ""]

    return l_string


def get_children_of_ezfio_cfg(l_module_with_ezfio_cfg):
    """
    From a module list of ezfio_cfg return all the stuff create by him
    """
    config_folder = join(QP_ROOT_EZFIO, "config")

    l_util = dict()

    for m in l_module_with_ezfio_cfg:

        name_module = os.path.split(m)[1]
        name_module_lower = name_module.lower()

        rel = name_module
        abs_ = m
        ez_module = Path(abs_, rel)

        rel = "EZFIO.cfg"
        abs_ = join(m, "EZFIO.cfg")
        ez_cfg = Path(abs_, rel)

        rel = "ezfio_interface.irp.f"
        abs_ = join(m, rel)
        ez_interface = Path(abs_, rel)

        rel = "{0}.ezfio_interface_config".format(name_module_lower)
        abs_ = join(config_folder, rel)
        ez_config = Path(abs_, rel)

        l_util[ez_module.rel] = EZ_handler(ez_module, ez_cfg, ez_interface,
                                           ez_config)

    return l_util


def ninja_ezfio_cfg_build(l_util):
    """
    Return the children created by EZFIO.cfg
    For us is only ez_interface.irp.f and ez_config
    """
    l_string = []

    for m in l_util.itervalues():

        str_ = "build {1} {2}: build_ezfio_interface {0}"
        l_string += [str_.format(m.ez_cfg.abs, m.ez_interface.abs,
                                 m.ez_config.abs)]

        l_string += ["   sub_module = {0}".format(m.ez_module.abs)]
        l_string += [""]

    return l_string


def ninja_ezfio_config_build(l_ezfio_config):
    """
    For the ezfio_config present in module move then
    """
    l_string = []

    for m in l_ezfio_config:
        file_source = m.path_in_module
        file_create = m.path_in_ezfio

        l_string += ["build {0}: build_ezfio_config {1}".format(file_create,
                                                                file_source)]
        l_string += [""]

    return l_string


def ninja_ezfio_rule():
    """
    Retun the rule for creation the ezfio
    Set some variable
    and run ninja
    """
    l_flag = ["export {0}='${0}'".format(flag)
              for flag in ["FC", "FCFLAGS", "IRPF90"]]

    install_lib_ezfio = join(QP_ROOT, 'install', 'EZFIO', "lib", "libezfio.a")
    l_cmd = ["cd {0}".format(QP_ROOT_EZFIO)] + l_flag
    l_cmd += ["ninja && ln -f {0} {1}".format(install_lib_ezfio, EZFIO_LIB)]

    l_string = ["rule build_ezfio",
                "   command = {0}".format(" ; ".join(l_cmd)),
                "   description = Create $out"
                ""]

    return l_string


def ninja_ezfio_build(l_ezfio_config, l_util):
    """
    Rule for create the ezfio
    we depend of the ezfio_config set by the user or created bu EZFIO.cfg
    """

    l_ezfio_config = [i.path_in_ezfio for i in l_ezfio_config]
    l_ezfio_from_cfg = [i.ez_config.abs for i in l_util.itervalues()]

    str_ = " ".join(l_ezfio_config + l_ezfio_from_cfg)

    ezfio_make_config = join(QP_ROOT_EZFIO, "make.config")
    l_string = ["build {0} {1}: build_ezfio {2}".format(EZFIO_LIB,
                                                        ezfio_make_config,
                                                        str_), ""]

    return l_string


#  __
# (_     ._ _  | o ._  |
# __) \/ | | | | | | | |<
#     /
def get_source_destination(path_module, l_needed_molule):
    """
    Return a list of Sym_link = namedtuple('Sym_link', ['source', 'destination'])
    for a module
    """
    return [Sym_link(m.abs, join(QP_ROOT_SRC, path_module.rel, m.rel))
            for m in l_needed_molule]


def ninja_symlink_rule():
    """
    Return the command to create for the symlink
    """
    return ["rule build_symlink", "   command =  ln -sf $in $out", ""]


def ninja_symlink_build(path_module, l_symlink):
    """
    Create the symlink
    and the l_symlink who are all the symlink list
    """

    if not l_symlink:
        return []

    l_string = ["build l_symlink_{0} : phony {1}".format(
        path_module.rel, " ".join([s.destination for s in l_symlink])), ""]

    for symlink in l_symlink:
        l_string += ["build {0}: build_symlink {1}".format(symlink.destination,
                                                           symlink.source), ""]

    return l_string


#           _  _   _
# o ._ ._ _|_ (_| / \   ._ _   _. |   _
# | |  |_) |    | \_/ o | | | (_| |< (/_
#      |
def get_l_file_for_module(path_module):
    '''
    return the list of irp.f in a module
    '''
    l_depend = []
    l_src = []
    l_obj = []

    l_template = []

    for f in os.listdir(path_module.abs):
        if f.lower().endswith(tuple([".template.f", ".include.f"])):
            l_template.append(join(path_module.abs, f))
        elif f.endswith(".irp.f"):
            l_depend.append(join(path_module.abs, f))
        elif f.lower().endswith(tuple([".f", ".f90", ".c", ".cpp", ".cxx"])):
            l_depend.append(join(path_module.abs, f))
            l_src.append(f)
            obj = '{0}.o'.format(os.path.splitext(f)[0])
            l_obj.append(obj)
        elif f == "EZFIO.cfg":
            l_depend.append(join(path_module.abs, "ezfio_interface.irp.f"))

    d = {
        "l_depend": l_depend,
        "l_src": l_src,
        "l_obj": l_obj,
        "l_template": l_template
    }

    return d


def get_file_dependency(d_info_module):
    """
    For a module return all the irp.f90 file who depend
    """
    d_irp = defaultdict(dict)

    for module, l_children in d_info_module.iteritems():

        for key, values in get_l_file_for_module(module).iteritems():
            if key in ["l_src"]:
                values = [join(module.abs, o) for o in values]
            if key in ["l_obj"]:
                values = [join(module.abs, "IRPF90_temp", o) for o in values]

            d_irp[module][key] = values

        for children in l_children:
            for key, values in get_l_file_for_module(children).iteritems():
                if key in ["l_src"]:
                    values = [join(module.abs, children.rel, o)
                              for o in values]
                if key in ["l_obj"]:
                    values = [join(module.abs, "IRPF90_temp", children.rel, o)
                              for o in values]

                d_irp[module][key].extend(values)

    return d_irp


def ninja_irpf90_make_rule():
    """
    The rule for creating the irpf90.make
    Export the flag and compile
    Only secontial make a possible
    """

    # ~#~#~#~#~ #
    # F l a g s #
    # ~#~#~#~#~ #
    l_flag = []
    for flag in ["FC", "FCFLAGS", "LIB", "SRC", "OBJ"]:
        str_ = "export {0}='${0}'".format(flag)
        l_flag.append(str_)

    # ~#~#~ #
    # c m d #
    # ~#~#~ #

    l_cmd = ["cd $module"] + l_flag + ["irpf90 $include_dir $IRPF90_FLAGS"]

    # ~#~#~#~#~#~ #
    # s t r i n g #
    # ~#~#~#~#~#~ #

    l_string = ["pool irp_pool", "   depth = 1", "", "rule build_irpf90.ninja",
                "   command = {0}".format(" ; ".join(l_cmd)),
                "   pool = irp_pool",
                "   description = Create the IRP_Tree for $module", ""]

    return l_string


def ninja_irpf90_make_build(path_module, l_needed_molule, d_irp):
    """
    Creatre the dependency for a irpf90.make
    We need all the symklink and all the irp.f
    """
    # ~#~#~#~#~#~ #
    # O u t p u t #
    # ~#~#~#~#~#~ #

    l_creation = [join(path_module.abs, i)
                  for i in ["irpf90.make", "irpf90_entities", "tags",
                            "IRPF90_temp/build.ninja"]]
    str_creation = " ".join(l_creation)

    # ~#~#~#~#~#~#~#~#~#~ #
    # D e p e n d a n c y #
    # ~#~#~#~#~#~#~#~#~#~ #

    l_depend = d_irp[path_module]["l_depend"]
    l_src = d_irp[path_module]["l_src"]
    l_obj = d_irp[path_module]["l_obj"]
    l_template = d_irp[path_module]["l_template"]

    if l_needed_molule:
        l_symlink = ["l_symlink_{0}".format(path_module.rel)]
    else:
        l_symlink = []

    str_depend = " ".join(l_depend + l_symlink + l_template)

    # ~#~#~#~#~#~#~#~#~#~#~ #
    # N i n j a _ b u i l d #
    # ~#~#~#~#~#~#~#~#~#~#~ #

    l_include_dir = ["-I {0}".format(m.rel) for m in l_needed_molule]

    l_string = [
        "build {0}: build_irpf90.ninja {1}".format(str_creation, str_depend),
        "   module = {0}".format(path_module.abs),
        "   SRC = {0}".format(" ".join(l_src)),
        "   OBJ = {0}".format(" ".join(l_obj)),
        "   include_dir = {0}".format(" ".join(l_include_dir)), ""
    ]

    return l_string


def ninja_readme_rule():
    """
    Rule for creation the readme.
    For not dealted the readme when ninja -t clean and the generator option
    """
    l_string = ["rule build_readme",
                "   command = cd $module ; update_README.py",
                "   generator = 1", ""]

    return l_string


def ninja_readme_build(path_module):
    """
    Rule for creation the readme
    """
    path_irp_man = join(path_module.abs, "irpf90.make")
    path_readme = join(path_module.abs, "README.rst")

    l_string = ["build {0}: build_readme {1}".format(path_readme,
                                                     path_irp_man),
                "   module = {0}".format(path_module.abs), ""]

    return l_string


#  _
# |_) o ._   _. ._
# |_) | | | (_| | \/
#                 /
def get_binaries(path_module):
    """
    Return the list of binaries (Path= namedtuple('Path', ['abs', 'rel']) for a module
    """
    import subprocess

    try:
        cmd = 'grep -l "program" {0}/*.irp.f'.format(path_module.abs)
        process = subprocess.Popen([cmd],
                                   shell=True,
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE)
        stdout, stderr = process.communicate()
    except OSError:
        return []
    else:
        if not stdout:
            return []
        elif "No such file or directory" not in stdout:
            l_bin = [i.replace(".irp.f", "", 1) for i in stdout.split()]
            return [Path(bin_, os.path.basename(bin_)) for bin_ in l_bin]
        else:
            return []


def get_dict_binaries(l_module, mode="production"):
    """
    Return a dict [module] = list_binaries
    If a the production mode is enable only header module
    who will produce all binaries

    Example : The module Full_CI can produce the binary SCF
    so you dont need to compile at all the module Hartree-Fock

    But you need to change the path acordingle
    Full_CI/Hartree-Fock/SCF
    """
    d_binaries = defaultdict(list)

    # Create d_binaries
    # Ake module => binaries generated
    for module in l_module:
        l_binaries = get_binaries(module)

        if l_binaries:
            d_binaries[module] += l_binaries

    if mode == "production":

        dict_root = module_instance.dict_root
        dict_root_module_path = dict_module_genelogy_path(dict_root)

        d_binaries_condensed = defaultdict(list)

        for module in d_binaries:

            root_module = dict_root_module_path[module]

            if module == root_module:
                d_binaries_condensed[root_module] += d_binaries[module]
            else:

                l_binaries = []
                for binaries in d_binaries[module]:
                    p_abs = join(QP_ROOT_SRC, root_module.rel, module.rel,
                                 binaries.rel)
                    p_rel = binaries.rel
                    p = Path(p_abs, p_rel)
                    l_binaries.append(p)

                d_binaries_condensed[root_module] += l_binaries

        d_binaries = d_binaries_condensed

    return d_binaries


def ninja_binaries_rule():
    """
    Rule for creating the binaries
    """

    # ~#~#~ #
    # c m d #
    # ~#~#~ #

    l_cmd = ["cd $module", "ninja -C IRPF90_temp $out"]

    # ~#~#~#~#~#~ #
    # s t r i n g #
    # ~#~#~#~#~#~ #

    l_string = ["rule build_binaries",
                "   command = {0}".format(" ; ".join(l_cmd)),
                "   description = Create all the binaries from $module", ""]

    return l_string


def ninja_binaries_build(path_module, l_children, d_binaries):
    """
    The binaries need the EZFIO_LIB, and the irpf90.make (aka build.ninja)
    """

    # ~#~#~ #
    # c m d #
    # ~#~#~ #

    ninja_module_path = join(path_module.abs, "IRPF90_temp/build.ninja")
    l_abs_bin = [binary.abs for binary in d_binaries[path_module]]

    # ~#~#~#~#~#~ #
    # s t r i n g #
    # ~#~#~#~#~#~ #

    l_string = ["build {0}: build_binaries {1} {2}".format(" ".join(l_abs_bin),
                                                           EZFIO_LIB,
                                                           ninja_module_path),
                "   module = {0}".format(path_module.abs), ""]

    return l_string


# ___
#  | ._ _   _     _|  _  ._   _  ._   _|  _  ._   _ o  _   _
#  | | (/_ (/_   (_| (/_ |_) (/_ | | (_| (/_ | | (_ | (/_ _>
#                        |
def ninja_dot_tree_rule():
    """
    Rule for creating the binaries
    """
    # ~#~#~ #
    # c m d #
    # ~#~#~ #

    l_cmd = ["cd $module", "module_handler.py create_png"]

    l_string = [
        "rule build_dot_tree", "   command = {0}".format(" ; ".join(l_cmd)),
        "   generator = 1",
        "   description = Generate Png representtion of the Tree Dependancies of $module",
        ""
    ]

    return l_string


def ninja_dot_tree_build(path_module):

    path_tree = join(path_module.abs, "tree_dependency.png")
    l_string = ["build {0}: build_dot_tree".format(path_tree),
                "   module = {0}".format(path_module.abs), ""]

    return l_string

#
# |\/|  _. o ._
# |  | (_| | | |
#
if __name__ == "__main__":

    arguments = docopt(__doc__)
    pwd_config_file = arguments["CONFIG_FILE"]

    #  _
    # |_ ._           _. ._ o  _. |_  |  _   _
    # |_ | | \/   \/ (_| |  | (_| |_) | (/_ _>
    #

    l_string = ninja_create_env_variable(pwd_config_file)

    #  _
    # |_)     |  _
    # | \ |_| | (/_
    #
    l_string += ninja_ezfio_cfg_rule()

    l_string += ninja_symlink_rule()

    l_string += ninja_irpf90_make_rule()
    l_string += ninja_readme_rule()

    l_string += ninja_binaries_rule()

    l_string += ninja_ezfio_config_rule()
    l_string += ninja_ezfio_rule()

    l_string += ninja_dot_tree_rule()

    #  _
    # |_)     o |  _|    _   _  ._   _  ._ _. |
    # |_) |_| | | (_|   (_| (/_ | | (/_ | (_| |
    #                    _|
    l_module_with_ezfio_cfg = get_l_module_with_ezfio_cfg()
    l_util = get_children_of_ezfio_cfg(l_module_with_ezfio_cfg)
    l_ezfio_config = get_l_ezfio_config()

    l_string += ninja_ezfio_cfg_build(l_util)
    l_string += ninja_ezfio_config_build(l_ezfio_config)
    l_string += ninja_ezfio_build(l_ezfio_config, l_util)

    #  _                  _
    # |_)     o |  _|   _|_ _  ._   ._ _   _   _|     |  _
    # |_) |_| | | (_|    | (_) |    | | | (_) (_| |_| | (/_
    #
    #

    # ~#~#~#~#~#~#~#~#~#~#~#~#~#~ #
    # G e n e a l o g y _ d i c t #
    # ~#~#~#~#~#~#~#~#~#~#~#~#~#~ #

    d_genealogy = module_instance.dict_descendant
    d_genealogy_path = dict_module_genelogy_path(d_genealogy)
    d_irp = get_file_dependency(d_genealogy_path)

    l_module = d_genealogy_path.keys()

    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #
    # M o d u l e _ t o _ i r p #
    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #

    if arguments["--production"]:

        d_binaries = get_dict_binaries(l_module, mode="production")

        l_module = d_binaries.keys()

    elif arguments["--development"]:

        d_binaries = get_dict_binaries(l_module, mode="development")

        l_module = d_binaries.keys()

    for module_to_compile in l_module:

        # ~#~#~#~#~#~#~#~ #
        #  S y m l i n k  #
        # ~#~#~#~#~#~#~#~ #
        l_children = d_genealogy_path[module_to_compile]
        l_symlink = get_source_destination(module_to_compile, l_children)

        l_string += ninja_symlink_build(module_to_compile, l_symlink)

        # ~#~#~#~#~#~#~#~ #
        #  i r p . f 9 0  #
        # ~#~#~#~#~#~#~#~ #
        l_string += ninja_irpf90_make_build(module_to_compile, l_children,
                                            d_irp)

        # ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ #
        # d o t _ t r e e  & r e a d  m e #
        # ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ #
        l_string += ninja_dot_tree_build(module_to_compile)
        l_string += ninja_readme_build(module_to_compile)

        l_string += ninja_binaries_build(module_to_compile, l_children,
                                         d_binaries)

    with open(join(QP_ROOT, "build.ninja"), "w+") as f:
        f.write("\n".join(l_string))

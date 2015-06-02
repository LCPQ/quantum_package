#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Usage: create_ninja_build.py (--development | --production) CONFIG_FILE
"""

import os
import sys
import glob
from os.path import join
from collections import namedtuple

try:
    from module_handler import file_dependency
    from module_handler import get_dict_module_boss, get_dict_genealogy_desc
    from read_compilation_cfg import get_compilation_option
    from docopt import docopt
except ImportError:
    print "source .quantum_package.rc"
    sys.exit(1)


#  __
# /__ |  _  |_   _. |       _. ._ o  _. |_  |  _   _
# \_| | (_) |_) (_| |   \/ (_| |  | (_| |_) | (/_ _>
#

QPACKAGE_ROOT = os.environ['QPACKAGE_ROOT']
QPACKAGE_ROOT_SRC = join(QPACKAGE_ROOT, 'src')
QPACKAGE_ROOT_EZFIO = join(QPACKAGE_ROOT, 'EZFIO')

EZFIO_LIB = join(QPACKAGE_ROOT_EZFIO, "lib", "libezfio.a")

#
# |\ |  _. ._ _   _   _|   _|_     ._  |  _
# | \| (_| | | | (/_ (_|    |_ |_| |_) | (/_
#                                  |
Path = namedtuple('Path', ['abs', 'rel'])
EZ_config_path = namedtuple('EZ_config', ['path_in_module', 'path_in_ezfio'])
EZ_handler = namedtuple('EZ_handler', ['ez_module', 'ez_cfg','ez_interface', 'ez_config'])
Sym_link = namedtuple('Sym_link', ['source', 'destination'])


#  _
# |_ ._           _. ._ o  _. |_  |  _   _
# |_ | | \/   \/ (_| |  | (_| |_) | (/_ _>
def ninja_create_env_variable(pwd_config_file):
    """
    Return some ninja varible with the env variable expanded
    FC, FCFLAGS, IRPF90, IRPF90_FLAGS
    The env variable is usefull for the generation of EZFIO, and IRPF90
    """
    l_string = []
    for flag in ["FC", "FCFLAGS", "IRPF90", "IRPF90_FLAGS"]:
        str_ = "{0} = {1}".format(flag, get_compilation_option(pwd_config_file, flag))
        l_string.append(str_)

    lib_lapack = get_compilation_option(pwd_config_file, "LAPACK_LIB")
    lib_ezfio = join(QPACKAGE_ROOT_EZFIO, "lib", "libezfio_irp.a")
    l_string.append("{0} = {1} {2}".format("LIB", lib_lapack, lib_ezfio))

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
        module_abs = join(QPACKAGE_ROOT_SRC, module_rel)
        try:
            d[Path(module_abs, module_rel)] = Path(join(QPACKAGE_ROOT_SRC, l_children_rel), l_children_rel)
        except:
            d[Path(module_abs, module_rel)] = [Path(join(QPACKAGE_ROOT_SRC, children), children) for children in l_children_rel]

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
    qp_src = QPACKAGE_ROOT_SRC

    return [join(qp_src, m) for m in listdir(qp_src) if isfile(join(qp_src, m, "EZFIO.cfg")) ]


def get_l_ezfio_config():
    """
    Return a namedtuple('EZ_config', ['path_in_module', 'path_in_ezfio'])
    """

    l = []

    cmd = "{0}/*/*.ezfio_config".format(QPACKAGE_ROOT_SRC)
    for path_in_module in glob.glob(cmd):
        name_lower = os.path.split(path_in_module)[1].lower()
        path_in_ezfio = join(QPACKAGE_ROOT_EZFIO, "config", name_lower)
        l.append(EZ_config_path(path_in_module, path_in_ezfio))

    return l


def ninja_ezfio_cfg_rule():
    """
    Return the ezfio_interface rule who will create
    the _ezfio_interface.irp.f the _ezfio_config from the EZFIO.cfg
    """

    l_string = ["rule build_ezfio_interface",
                "   command = ei_handler.py --path_module $sub_module",
                ""]

    return l_string


def ninja_ezfio_config_rule():
    """
    If a ezfio_config existe you just need to move it
    """
    l_string = ["rule build_ezfio_config",
                "   command = cp $in $out",
                ""]

    return l_string


def get_children_of_ezfio_cfg(l_module_with_ezfio_cfg):
    """
    From a module list of ezfio_cfg return all the stuff create by him
    """
    config_folder = join(QPACKAGE_ROOT_EZFIO, "config")

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

        l_util[ez_module.rel] = EZ_handler(ez_module,
                                           ez_cfg,
                                           ez_interface,
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
        l_string += [str_.format(m.ez_cfg.abs,
                                 m.ez_interface.abs,
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

        l_string += ["build {0}: build_ezfio_config {1}".format(file_create, file_source)]
        l_string += [""]

    return l_string


def ninja_ezfio_rule():
    """
    Retun the rule for creation the ezfio
    Set some variable
    and run ninja
    """
    l_flag = ["export {0}='${0}'".format(flag) for flag in ["FC", "FCFLAGS", "IRPF90"]]

    l_cmd = ["cd {0}".format(QPACKAGE_ROOT_EZFIO)] + l_flag + ["{0}/ninja/ninja".format(QPACKAGE_ROOT)]

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

    l_string = ["build {0}: build_ezfio {1}".format(EZFIO_LIB, str_),
                ""]

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
    l = [Sym_link(m.abs, join(QPACKAGE_ROOT_SRC, path_module.rel, m.rel)) for m in l_needed_molule]

    return l


def ninja_symlink_rule():
    """
    Return the command to create for the symlink
    """
    l_string = ["rule build_symlink",
                "   command =  ln -sf $in $out",
                ""]

    return l_string


def ninja_symlink_build(path_module, l_symlink):
    """
    Create the symlink
    and the l_symlink who are all the symlink list
    """

    if not l_symlink:
        return []

    l_string = ["build l_symlink_{0} : phony {1}".format(path_module.rel, " ".join([s.destination for s in l_symlink])),
                ""]

    for symlink in l_symlink:
        l_string += ["build {0}: build_symlink {1}".format(symlink.destination, symlink.source),
                     ""]

    return l_string


#           _  _   _
# o ._ ._ _|_ (_| / \   ._ _   _. |   _
# | |  |_) |    | \_/ o | | | (_| |< (/_
#      |
def get_l_irp_for_module(path_module_abs):
    '''
    return the list of irp.f in a module
    '''
    dump = []
    for file in os.listdir(path_module_abs):
        if file.endswith(".irp.f"):
            dump.append(join(path_module_abs, file))
        if file == "EZFIO.cfg":
            dump.append(join(path_module_abs, "ezfio_interface.irp.f"))
    return dump


def get_irp_dependency(d_info_module):
    """
    For a module return all the irp.f90 file who depend
    """
    d_irp = dict()
    for module, l_children in d_info_module.iteritems():

        dump = get_l_irp_for_module(module.abs)
        for children in l_children:
            dump.extend(get_l_irp_for_module(children.abs))

        d_irp[module] = dump

    return d_irp


def ninja_irpf90_make_rule():
    """
    The rule for creating the irpf90.make
    Export the flag and compile
    Only secontial make a possible
    """
    l_flag = []
    for flag in ["FC", "FCFLAGS", "LIB", "SRC", "OBJ"]:
        str_ = "export {0}='${0}'".format(flag)
        l_flag.append(str_)

    l_cmd = ["cd $module"] + l_flag + ["irpf90 $include_dir $IRPF90_FLAGS"]

    l_string = ["pool irp_pool",
                "   depth = 1",
                "",
                "rule build_irpf90.ninja",
                "   command = {0}".format(" ; ".join(l_cmd)),
                "   pool = irp_pool",
                "   generator = 1",
                "   description = Create the IRP_Tree for $module"
                ""]

    return l_string


def ninja_irpf90_make_build(path_module,
                            l_needed_molule,
                            d_irp):
    """
    Creatre the dependency for a irpf90.make
    We need all the symklink and all the irp.f
    """
    # ~#~#~#~#~#~ #
    # O u t p u t #
    # ~#~#~#~#~#~ #

    l_creation = [join(path_module.abs, i) for i in ["irpf90.make",
                                                     "irpf90_entities",
                                                     "tags",
                                                     "IRPF90_temp/build.ninja"]]
    str_creation = " ".join(l_creation)

    # ~#~#~#~#~#~#~#~#~#~ #
    # D e p e n d a n c y #
    # ~#~#~#~#~#~#~#~#~#~ #

    l_irp_need = d_irp[path_module]

    l_destination = ["l_symlink_{0}".format(path_module.rel)] if l_needed_molule else []
    str_depend = " ".join(l_irp_need + l_destination)

    # ~#~#~#~#~#~#~#~#~#~#~ #
    # N i n j a _ b u i l d #
    # ~#~#~#~#~#~#~#~#~#~#~ #

    l_src, l_obj = file_dependency(path_module.rel)
    l_include_dir = ["-I {0}".format(m.rel) for m in l_needed_molule]

    l_string = ["build {0}: build_irpf90.ninja {1}".format(str_creation, str_depend),
                "   module = {0}".format(path_module.abs),
                "   SRC = {0}".format(" ".join(l_src)),
                "   OBJ = {0}".format(" ".join(l_obj)),
                "   include_dir = {0}".format(" ".join(l_include_dir)),
                ""]

    return l_string


def ninja_readme_rule():
    """
    Rule for creation the readme.
    For not dealted the readme when ninja -t clean and the generator option
    """
    l_string = ["rule build_readme",
                "   command = cd $module ; update_README.py",
                "   generator = 1",
                ""]

    return l_string


def ninja_readme_build(path_module):
    """
    Rule for creation the readme
    """
    path_irp_man = join(path_module.abs, "irpf90.make")
    path_readme = join(path_module.abs, "README.rst")

    l_string = ["build {0}: build_readme {1}".format(path_readme,
                                                     path_irp_man)]

    l_string += ["   module = {0}".format(path_module.abs)]
    l_string += [""]

    return l_string


#  _
# |_) o ._   _. ._
# |_) | | | (_| | \/
#                 /
def get_program(path_module):
    """
    Return the list of binaries (Path= namedtuple('Path', ['abs', 'rel']) for a module
    """
    import subprocess

    try:
        cmd = 'grep -l "program" {0}/*.irp.f'.format(path_module.abs)
        process = subprocess.Popen([cmd],shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
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


def get_dict_binaries(mode="development"):
    """
    Return a dict [module] = list_binaries
    If a the production mode is enable only header module will produce binaries

    Example : The module Full_CI can produce the binary SCF
    so you dont need to use at all the module Hartree-Fock
    """

    from collections import defaultdict

    d_binaries = defaultdict(list)

    # Create d_binaries
    # Ake module => binaries generated
    for module in d_genealogy_path.keys():
        l_binaries = get_program(module)

        if l_binaries:
            d_binaries[module] += l_binaries

    if mode == "production":

        dict_boss = get_dict_module_boss()
        dict_boss_path = dict_module_genelogy_path(dict_boss)

        d_binaries_condensed = defaultdict(list)
        for module in d_binaries:
            d_binaries_condensed[dict_boss_path[module]] += d_binaries[module]

        d_binaries = d_binaries_condensed

    return d_binaries


def ninja_binaries_rule():
    """
    Rule for creating the binaries
    """

    l_cmd = ["cd $module/IRPF90_temp", "ninja"]

    l_string = ["rule build_binaries",
                "   command = {0}".format(" ; ".join(l_cmd)),
                "   description = Create all the binaries from $module"
                ""]

    return l_string


def ninja_binaries_build(path_module, l_children, d_binaries):
    """
    The binaries need the EZFIO_LIB, and the irpf90.make (aka build.ninja)
    """
    ninja_module_path = join(path_module.abs, "IRPF90_temp", "build.ninja")

    l_abs_bin = [binary.abs for binary in d_binaries[path_module]]

    l_string = ["build {0}: build_binaries {1} {2}".format(" ".join(l_abs_bin),
                                                           EZFIO_LIB,
                                                           ninja_module_path),
                "   module = {0}".format(path_module.abs),
                ""]

    return l_string


# ___
#  | ._ _   _     _|  _  ._   _  ._   _|  _  ._   _ o  _   _
#  | | (/_ (/_   (_| (/_ |_) (/_ | | (_| (/_ | | (_ | (/_ _>
#                        |
def ninja_dot_tree_rule():
    """
    Rule for creating the binaries
    """
    l_cmd = ["cd $module", "module_handler.py create_png"]

    l_string = ["rule build_dot_tree",
                "   command = {0}".format(" ; ".join(l_cmd)),
                "   description = Generate Dot Dependancies Tree of $module"
                ""]

    return l_string


def ninja_dot_tree_build(path_module):

    l_string = ["build {0}: build_dot_tree".format(join(path_module.abs, "tree_dependency.png")),
                "   module = {0}".format(path_module.abs),
                ""]

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

    d_genealogy = get_dict_genealogy_desc()
    d_genealogy_path = dict_module_genelogy_path(d_genealogy)
    d_irp = get_irp_dependency(d_genealogy_path)

    d_binaries_production = get_dict_binaries("production")
    d_binaries_development = get_dict_binaries("development")

    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #
    # M o d u l e _ t o _ i r p #
    # ~#~#~#~#~#~#~#~#~#~#~#~#~ #

    if arguments["--production"]:
        l_module_to_irp = d_binaries_production.keys()

    elif arguments["--development"]:
        l_module_to_irp = d_binaries_development.keys()

    for module_to_compile in l_module_to_irp:

        # ~#~#~#~#~#~#~#~ #
        #  S y m l i n k  #
        # ~#~#~#~#~#~#~#~ #
        l_children = d_genealogy_path[module_to_compile]
        l_symlink = get_source_destination(module_to_compile, l_children)

        l_string += ninja_symlink_build(module_to_compile, l_symlink)

        # ~#~#~#~#~#~#~#~ #
        #  i r p . f 9 0  #
        # ~#~#~#~#~#~#~#~ #
        l_string += ninja_irpf90_make_build(module_to_compile, l_children, d_irp)

        # ~#~#~#~#~#~#~#~ #
        # d o t _ t r e e #
        # ~#~#~#~#~#~#~#~ #
        l_string += ninja_dot_tree_build(module_to_compile)

    # ~#~#~#~#~#~#~ #
    #  b i n a r y  #
    # ~#~#~#~#~#~#~ #
    for module_to_compile in d_binaries_production.keys():

        l_string += ninja_binaries_build(module_to_compile, l_children, d_binaries_production)
        l_string += ninja_readme_build(module_to_compile)

    with open(join(QPACKAGE_ROOT, "build.ninja"), "w+") as f:
        f.write("\n".join(l_string))

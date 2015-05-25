#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os

from os.path import join


qpackage_root = os.environ['QPACKAGE_ROOT']
qpackage_root_src = join(qpackage_root, 'src')
qpackage_root_ocaml = join(qpackage_root, 'ocaml')
qpackage_root_ezfio = join(qpackage_root, 'EZFIO')

ezfio_lib = join(qpackage_root_ezfio, "lib", "libezfio.a")


from collections import namedtuple
Path = namedtuple('Path', ['abs', 'rel'])

EZ_config_path = namedtuple('EZ_config', ['path_in_module', 'path_in_ezfio'])
EZ_handler = namedtuple('EZ_handler', ['ez_module', 'ez_cfg', 'ez_interface', 'ez_config', 'ez_default', 'ez_ocaml'])

try:
    from module_handler import module_genealogy
    from cache import cache
except ImportError:
    print "source .quantum_package.rc"


#                  _
# |\/|  _. |   _ _|_ o |  _     _|  _  ._  ._   _|
# |  | (_| |< (/_ |  | | (/_ o (_| (/_ |_) | | (_|
#
def ninja_makefile_depend_rule():
    # Rule
    l_string = ["rule build_makefile.depend"]
    l_string += [
        "   command = module_handler.py save_makefile_depend $module/NEEDED_CHILDREN_MODULES"]
    l_string += [""]

    return l_string


def ninja_makefile_depend_build(l_all_needed_molule, path_module):
    l_makefile = [join(i.abs, "Makefile") for i in l_all_needed_molule]

    # Build
    path_mkdepend = join(path_module.abs, "Makefile.depend")
    str_l_makefile = " ".join(l_makefile)

    l_string = ["build {0}: build_makefile.depend {1}".format(path_mkdepend,
                                                              str_l_makefile)]

    l_string += ["   module = {0}".format(path_module.abs)]
    l_string += [""]

    return l_string

import glob


#  _ __  _ ___  _         _
# |_  / |_  |  / \    _ _|_ _
# |_ /_ |  _|_ \_/ o (_  | (_|
#                           _|


def get_l_module_with_ezfio_cfg():
    from os import listdir
    from os.path import isfile, join
    qp_src = qpackage_root_src

    return [join(qp_src, m) for m in listdir(qp_src) if isfile(join(qp_src, m, "EZFIO.cfg")) ]


def get_ezfio_config():
    # Path in module
    # Path in EZFIO/config/
    # namedtuple('EZ_config', ['path_in_module', 'path_in_ezfio'])

    l = []

    cmd = "{0}/*/*.ezfio_config".format(qpackage_root_src)
    for path_in_module in glob.glob(cmd):
        name_lower = os.path.split(path_in_module)[1].lower()
        path_in_ezfio = join(qpackage_root_ezfio, "config", name_lower)
        l.append(EZ_config_path(path_in_module, path_in_ezfio))

    return l


@cache
def get_l_irp_for_module(path_module_abs):
    dump = []
    for file in os.listdir(path_module_abs):
        if file.endswith(".irp.f"):
            dump.append(join(path_module_abs, file))
        if file == "EZFIO.cfg":
            dump.append(join(path_module_abs, "ezfio_interface.irp.f"))
    return dump


def ninja_ezfio_cfg_rule():
    # Rule
    l_string = ["rule build_ezfio_interface"]
    l_string += ["   command = ei_handler.py --path_module $sub_module"]
    l_string += [""]

    return l_string


def ninja_ezfio_config_rule():
    # Rule
    l_string = ["rule build_ezfio_config"]
    l_string += ["   command = cp $in $out"]
    l_string += [""]

    return l_string


def get_children_of_ezfio_cfg(l_module_with_ezfio_cfg):
    # Build

    import re
    p = re.compile(ur'interface:\s+input')

    config_folder = join(qpackage_root_ezfio, "config")
    default_folder = join(qpackage_root, "data", "ezfio_defaults")

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

        with open(ez_cfg.abs, 'r') as file_:
            if p.search(file_.read()):

                rel = "{0}.ezfio_interface_default".format(name_module_lower)
                abs_ = join(default_folder, rel)
                ez_default = Path(abs_, rel)

                rel = "Input_{0}.ml".format(name_module_lower)
                abs_ = join(qpackage_root_ocaml, rel)
                ez_ocaml = Path(abs_, rel)
            else:
                ez_default = None
                ez_ocaml = None

        l_util[ez_module.rel] = EZ_handler(ez_module,
                                           ez_cfg,
                                           ez_interface,
                                           ez_config,
                                           ez_default, ez_ocaml)

    return l_util


def ninja_ezfio_cfg_build(l_util):

    l_string = []

    for m in l_util.itervalues():

        try:
            str_ = "build {1} {2} {3} {4}: build_ezfio_interface {0}"
            l_string += [str_.format(m.ez_cfg.abs,
                                     m.ez_interface.abs,
                                     m.ez_config.abs,
                                     m.ez_default.abs,
                                     m.ez_ocaml.abs)]
        except AttributeError:
            str_ = "build {1} {2}: build_ezfio_interface {0}"
            l_string += [str_.format(m.ez_cfg.abs,
                                     m.ez_interface.abs,
                                     m.ez_config.abs)]
        finally:
            l_string += ["   sub_module = {0}".format(m.ez_module.abs)]
            l_string += [""]

    return l_string


def ninja_ezfio_config_build(l_ezfio_config):
    # Build
    l_string = []

    for m in l_ezfio_config:
        file_source = m.path_in_module
        file_create = m.path_in_ezfio

        l_string += ["build {0}: build_ezfio_config {1}".format(file_create, file_source)]
        l_string += [""]

    return l_string


def ninja_ezfio_rule():
    # Rule
    l_string = ["rule build_ezfio"]
    l_string += ["   command = cd {0}; make ; cd -".format(qpackage_root_ezfio)]
    l_string += ["   description = Make ezfio"]

    return l_string


def ninja_ezfio_build(l_ezfio_config, l_util):
    # Rule
    ezfio_ocam_lib = join(qpackage_root, "EZFIO", "Ocaml", "ezfio.ml")

    l = [i.path_in_ezfio for i in l_ezfio_config]

    str_ = " ".join(l + [i.ez_config.abs for i in l_util.itervalues()])

    l_string = ["build {0} {1}: build_ezfio {2}".format(ezfio_lib,
                                                        ezfio_ocam_lib,
                                                        str_)]
    l_string += [""]

    return l_string


#  __
# (_     ._ _  | o ._  |
# __) \/ | | | | | | | |<
#     /
def get_source_destination(l_all_needed_molule, path_module):

    l_source = [m.abs for m in l_all_needed_molule]
    l_destination = [join(qpackage_root_src, path_module.rel, m.rel)
                     for m in l_all_needed_molule]

    return l_source, l_destination


def ninja_symlink_rule():
    # Rule
    l_string = ["rule build_symlink"]
    l_string += ["   command =  ln -sf $in $out"]
    l_string += [""]

    return l_string


def ninja_symlink_build(l_source, l_destination, path_module):
    # Rule
    l_string = []
    for source, destination in zip(l_source, l_destination):
        l_string += ["build {0}: build_symlink {1}".format(destination,
                                                           source)]
        l_string += [""]

    out = "l_symlink_{0}".format(path_module.rel)
    dep = " ".join(l_destination)

    l_string += ["build {0} : phony {1}".format(out, dep)]
    l_string += [""]

    return l_string


#           _  _   _
# o ._ ._ _|_ (_| / \   ._ _   _. |   _
# | |  |_) |    | \_/ o | | | (_| |< (/_
#      |
def dict_needed_modules(l_module_to_compile):

    d = dict()
    for module_rel in l_module_to_compile:
        module_abs = join(qpackage_root_src, module_rel)

        path_neeeded_module = join(module_abs, "NEEDED_CHILDREN_MODULES")

        d[Path(module_abs, module_rel)] = [Path(join(qpackage_root_src, m_children), m_children) for m_children in  module_genealogy(path_neeeded_module)]

    return d


def get_irp_dependancy(d_info_module):
    l_all_module = [m for m in d_info_module.keys()]

    for l_m_children in d_info_module.values():
        for m_children in l_m_children:
            if m_children not in l_all_module:
                l_all_module.append(m_children)

    d_irp = dict()
    for module in l_all_module:
        d_irp[module.rel] = get_l_irp_for_module(module.abs)

    return d_irp


def ninja_irpf90_make_rule():
    # Rule
    l_string = ["pool irp_pool"]
    l_string += ["   depth = 1"]
    l_string += [""]

    l_string += ["rule build_irpf90.make"]
    l_string += ["   command = cd $module ; irpf90 $include_dir $irpf90_flag ; cd -"]
    l_string += ["   pool = irp_pool"]
    l_string += [""]

    return l_string


def ninja_create_l_irp_build(d_irp):

    l_string = []
    for m, l_irp in d_irp.iteritems():

        dep = " ".join(l_irp)
        l_string += ["build l_irp_{0}: phony {1}".format(m, dep)]
        l_string += [""]

    return l_string


def ninja_irpf90_make_build(l_all_needed_molule,
                            path_module):

    path_irpf90_make = join(path_module.abs, "irpf90.make")

    l_irp_need = ["l_irp_{0}".format(i.rel) for i in l_all_needed_molule]
    str_l_irp_need = " ".join(l_irp_need)

    path_makefiledepend = join(path_module.abs, "Makefile.depend")
    str_l_destination = "l_symlink_{0}".format(path_module.rel)

    str_depend = "{0} {1} {2}".format(str_l_irp_need,
                                      path_makefiledepend,
                                      str_l_destination)

    # Build
    l_string = ["build {0}: build_irpf90.make {1}".format(path_irpf90_make,
                                                          str_depend)]
    l_string += ["   module = {0}".format(path_module.abs)]

    # Option
    str_include_dir = " ".join(["-I {0}".format(m.rel)
                                for m in l_all_needed_molule])

    l_string += ["   include_dir = {0}".format(str_include_dir)]
    l_string += ["   irpf90_flag = {0}".format("--align=32 --openmp")]
    l_string += [""]

    return l_string


#  _
# / \  _  _. ._ _  |
# \_/ (_ (_| | | | |

def get_qp_file():
    qp_edit = join(qpackage_root_ocaml, "qp_edit.ml")
    l = glob.glob(join(qpackage_root_ocaml, "qp_*.ml"))

    if qp_edit not in l:
        l.append(qp_edit)
    return l


def get_ml_file(l_util, l_qp):
    """
    Get all the ml file
    Remove the binary
    """
    ezfio_ml = join(qpackage_root_ocaml, "ezfio.ml")

    l_ml_auto_generated = [i.ez_ocaml.abs for i in l_util.itervalues() if i.ez_ocaml] + [ezfio_ml]

    # .ml present
    l_ml_present = glob.glob("{0}/*.ml".format(qpackage_root_ocaml))
    l_ml = list(set(l_ml_present + l_ml_auto_generated))

    # Return all the .ml who are not ocaml_binary
    return [ml for ml in l_ml if ml not in l_qp]


def ninja_ocaml_rule():
    # Rule
    cmd = "   command = cd {0} ; make -j 1 $binary ; touch $binary; cd -"

    l_string = ["rule build_ocaml"]
    l_string += [cmd.format(qpackage_root_ocaml)]
    l_string += [""]

    l_string += ["rule cp_input.ml"]
    l_string += ["   command = cp $in $out"]
    l_string += [""]

    l_string += ["rule build_qp_edit.ml"]
    l_string += ["   command =  ei_handler.py ocaml_global"]
    l_string += [""]

    return l_string


def ninja_ml_build(l_util):

    source = join(qpackage_root_ezfio, "Ocaml", "ezfio.ml")
    dest = join(qpackage_root_ocaml, "ezfio.ml")

    l_string = ["build {0}: cp_input.ml {1}".format(dest, source)]
    l_string += [""]

    ocaml_ml = [join(qpackage_root_ocaml, i) for i in ["qp_edit.ml", "Input_auto_generated.ml"]]
    ocaml_ml_str = " ".join(ocaml_ml)

    qp_edit_template = join(qpackage_root, "scripts", "ezfio_interface", "qp_edit_template")
    l_depend = [i.ez_ocaml.abs for i in l_util.itervalues() if i.ez_ocaml] + [qp_edit_template]

    depend_str = " ".join(l_depend)
    l_string = ["build {0}: build_qp_edit.ml {1}".format(ocaml_ml_str, depend_str)]
    return l_string


def ninja_ocaml_build(l_bin_ml, l_ml):

    # Rule
    l_string = [""]
    str_depend = " ".join(l_ml + l_bin_ml)

    for bin_ in [i.replace(".ml", ".native") for i in l_bin_ml]:
        binary_name = os.path.split(bin_)[1]
        l_string += ["build {0}: build_ocaml {1}".format(bin_, str_depend)]
        l_string += ["   binary = {0}".format(binary_name)]
        l_string += [""]

    return l_string


#  _
# |_) o ._   _. ._
# |_) | | | (_| | \/
#                 /
def get_program(path_module):
    import subprocess

    try:
        fnull = open(os.devnull, 'w')
        cmd = 'grep -l "program" {0}/*.irp.f'.format(path_module.abs)
        p = subprocess.check_output([cmd], shell=True, stderr=fnull)
    except subprocess.CalledProcessError:
        return []
    else:
        return [os.path.basename(f).split(".")[0] for f in p.split()]


def ninja_binary_rule():
    # Rule
    l_string = ["rule build_binary"]
    l_string += ["   command = cd $module ; make -j 1 $binary ; touch $binary; cd -"]
    l_string += [""]

    return l_string


def ninja_binary_build(l_bin, path_module):

    # Build
    irpf90mk_path = join(path_module.abs, "irpf90.make")

    l_string = []

    l_abs_bin = [join(path_module.abs, binary) for binary in l_bin]

    for path, abs_path in zip(l_bin, l_abs_bin):
        l_string += ["build {0}: build_binary {1} {2}".format(abs_path,
                                                              ezfio_lib,
                                                              irpf90mk_path)]

        l_string += ["   module = {0}".format(path_module.abs)]
        l_string += ["   binary = {0}".format(path)]
        l_string += [""]

    ocaml_bin = [join(qpackage_root_ocaml, i) for i in ["qp_run.native", "qp_edit.native"]]
    str_l_abs_bin = " ".join(l_abs_bin + ocaml_bin)

    l_string += ["build build_all_binary_{0}: phony {1}".format(path_module.rel,
                                                                str_l_abs_bin)]
    l_string += [""]

    return l_string


def ninja_all_binary_build(l_module):
    l_build_name = ["build_all_binary_{0}".format(m) for m in l_module]
    str_l_build_name = " ".join(l_build_name)

    l_string = ["build build_all_binary: phony {0}".format(str_l_build_name)]
    l_string += [""]

    return l_string

if __name__ == "__main__":
    #  _
    # |_)     |  _
    # | \ |_| | (/_
    #
    l_string = ninja_makefile_depend_rule()
    l_string += ninja_ezfio_cfg_rule()

    l_string += ninja_symlink_rule()

    l_string += ninja_irpf90_make_rule()

    l_string += ninja_binary_rule()

    l_string += ninja_ezfio_config_rule()
    l_string += ninja_ezfio_rule()

    l_string += ninja_ocaml_rule()

    #  _
    # |_)     o |  _|    _   _  ._   _  ._ _. |
    # |_) |_| | | (_|   (_| (/_ | | (/_ | (_| |
    #                    _|
    l_module_with_ezfio_cfg = get_l_module_with_ezfio_cfg()
    l_util = get_children_of_ezfio_cfg(l_module_with_ezfio_cfg)
    l_ezfio_config = get_ezfio_config()

    l_string += ninja_ezfio_cfg_build(l_util)
    l_string += ninja_ezfio_config_build(l_ezfio_config)
    l_string += ninja_ezfio_build(l_ezfio_config, l_util)

    l_qp = get_qp_file()
    l_ml = get_ml_file(l_util, l_qp)

    l_string += ninja_ml_build(l_util)
    l_string += ninja_ocaml_build(l_qp, l_ml)

    #  _                  _
    # |_)     o |  _|   _|_ _  ._   ._ _   _   _|     |  _
    # |_) |_| | | (_|    | (_) |    | | | (_) (_| |_| | (/_
    #
    #
    with open(join(qpackage_root_src, "NEEDED_MODULES"), "r") as f:
        l_module_to_compile = f.read().split()

    d_info_module = dict_needed_modules(l_module_to_compile)

    d_irp = get_irp_dependancy(d_info_module)
    l_string += ninja_create_l_irp_build(d_irp)

    for module, l_children in d_info_module.iteritems():

        l_source, l_destination = get_source_destination(l_children, module)

        l_string += ninja_makefile_depend_build(l_children, module)

        # irpf90.make
        l_string += ninja_symlink_build(l_source, l_destination, module)

        l_string += ninja_irpf90_make_build(l_children, module)

        # ninja_binary
        l_binary = get_program(module)
        l_string += ninja_binary_build(l_binary, module)

    l_string += ninja_all_binary_build(l_module_to_compile)

    print "\n".join(l_string)

#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os

from os.path import join

from module_handler import module_genealogy

qpackage_root = os.environ['QPACKAGE_ROOT']
qpackage_root_src = join(qpackage_root, 'src')


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
    l_makefile = [join(qpackage_root_src, i, "Makefile")
                  for i in l_all_needed_molule]

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


def get_module_with_ezfio_cfg():
    from os import listdir
    from os.path import isfile, join
    qp_src = qpackage_root_src

    return [join(qp_src, m) for m in listdir(qp_src) if isfile(join(qp_src, m, "EZFIO.cfg")) ]


def get_ezfio_config():
    return glob.glob("{0}/*/*.ezfio_config".format(qpackage_root_src))


def get_l_ezfio_irp(l_all_needed_molule, path_module):

    l_module_abs = [join(qpackage_root_src, m) for m in l_all_needed_molule]

    l_irp = []

    for m in l_module_abs + [path_module.abs]:

        for file in os.listdir(m):
            if file.endswith(".irp.f"):
                l_irp.append(join(m, file))
            if file == "EZFIO.cfg":
                l_irp.append(join(m, "ezfio_interface.irp.f"))

    return l_irp


def ninja_ezfio_cfg_rule():
    # Rule
    l_string = ["rule build_ezfio_interface"]
    l_string += ["   command = ei_handler.py --path_module $sub_module --irpf90"]
    l_string += [""]

    return l_string


def ninja_ezfio_interface_config_rule():
    # Rule
    l_string = ["rule build_ezfio_interface_config"]
    l_string += ["   command = ei_handler.py --path_module $sub_module --ezfio_config"]
    l_string += [""]

    return l_string


def ninja_ezfio_config_rule():
    # Rule
    l_string = ["rule build_ezfio_config"]
    l_string += ["   command = cp $in $out"]
    l_string += [""]

    return l_string


def ninja_ezfio_cfg_build(l_module_with_ezfio_cfg):
    # Build
    l_string = []

    for m in l_module_with_ezfio_cfg:
        ez_interface = join(m, "ezfio_interface.irp.f")
        ez_cfg = join(m, "EZFIO.cfg")

        l_string += ["build {0}: build_ezfio_interface {1}".format(ez_interface,
                                                                   ez_cfg)]
        l_string += ["   sub_module = {0}".format(m)]
        l_string += [""]

    return l_string


def ninja_ezfio_config_build(l_module_with_ezfio_cfg,l_ezfio_config):
    # Build
    l_string = []
    l_file_create = []

    ezfio_folder = join(qpackage_root, "EZFIO/config")

    for m in l_module_with_ezfio_cfg:
        file_source = join(m, "EZFIO.cfg")
        name = "{0}.ezfio_interface_config".format(os.path.split(m)[1].lower())
        file_create = join(ezfio_folder, name)

        l_file_create.append(file_create)
        l_string += ["build {0}: build_ezfio_interface_config {1}".format(file_create, file_source)]
        l_string +=  ["   sub_module = {0}".format(m)]
        l_string += [""]

    for m in l_ezfio_config:
        file_source = m
        name = os.path.split(m)[1].lower()
        file_create = join(ezfio_folder, name)

        l_file_create.append(file_create)
        l_string += ["build {0}: build_ezfio_config {1}".format(file_create, file_source)]
        l_string += [""]

    return l_string, l_file_create


def ninja_ezfio_rule():
    # Rule
    l_string = ["rule build_ezfio"]
    ezfio_folder = join(qpackage_root, "EZFIO")
    l_string += ["   command = cd {0}; make ; cd -".format(ezfio_folder)]

    return l_string


def ninja_ezfio_build(l_file_create):
    # Rule
    ezfio_lib = join(qpackage_root, "EZFIO", "lib", "libezfio.a")
    str_ = " ".join(l_file_create)

    l_string = ["build {0}: build_ezfio {1}".format(ezfio_lib, str_)]
    l_string += [""]

    return l_string


#  __
# (_     ._ _  | o ._  |
# __) \/ | | | | | | | |<
#     /
def get_source_destination(l_all_needed_molule, path_module):
    l_all_needed_molule_include = l_all_needed_molule + ["include"]

    l_source = [join(qpackage_root_src, m)
                for m in l_all_needed_molule_include]
    l_destination = [join(qpackage_root_src, path_module.rel, m)
                     for m in l_all_needed_molule_include]

    return l_source, l_destination


def ninja_symlink_rule():
    # Rule
    l_string = ["rule build_symlink"]
    l_string += ["   command =  ln -s $module_source $module_destination"]
    l_string += [""]

    return l_string


def ninja_symlink_build(l_source, l_destination):
    # Rule
    l_string = []
    for source, destination in zip(l_source, l_destination):
        l_string += ["build {0}: build_symlink".format(destination)]
        l_string += ["   module_source = {0}".format(source)]
        l_string += ["   module_destination = {0}".format(destination)]
        l_string += [""]

    return l_string


#           _  _   _
# o ._ ._ _|_ (_| / \   ._ _   _. |   _
# | |  |_) |    | \_/ o | | | (_| |< (/_
#      |
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


def ninja_irpf90_make_build(path_module,
                            l_all_needed_molule_include,
                            l_irp,
                            l_destination):

    path_irpf90_make = join(path_module.abs, "irpf90.make")
    str_l_irp_need = " ".join(l_irp)
    str_l_destination = "  ".join(l_destination)
    path_makefiledepend = join(path_module.abs, "Makefile.depend")

    str_depend = "{0} {1} {2}".format(str_l_irp_need,
                                      path_makefiledepend,
                                      str_l_destination)

    # Build
    l_string = ["build {0}: build_irpf90.make {1}".format(path_irpf90_make,
                                                          str_depend)]
    l_string += ["   module = {0}".format(path_module.abs)]

    # Option
    str_include_dir = " ".join(["-I {0}".format(m)
                                for m in l_all_needed_molule_include])

    l_string += ["   include_dir = {0}".format(str_include_dir)]
    l_string += ["   irpf90_flag = {0}".format("--align=32 --openmp")]
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
    ezfio_lib = join(qpackage_root, "EZFIO", "lib", "libezfio.a")

    l_abs_bin = [join(path_module.abs, binary) for binary in l_bin]

    l_string = []
    for path, abs_path in zip(l_bin, l_abs_bin):
        l_string += ["build {0}: build_binary {1} {2}".format(abs_path,
                                                              ezfio_lib,
                                                              irpf90mk_path)]
        l_string += ["   module = {0}".format(path_module.abs)]
        l_string += ["   binary = {0}".format(path)]

        l_string += [""]

    str_l_abs_bin = " ".join(l_abs_bin)

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

    with open(join(qpackage_root_src, "NEEDED_MODULES"), "r") as f:
        l_module_to_compile = f.read().split()

    l_string = ninja_makefile_depend_rule()
    l_string += ninja_ezfio_cfg_rule()
    l_string += ninja_symlink_rule()
    l_string += ninja_irpf90_make_rule()
    l_string += ninja_binary_rule()
    l_string += ninja_ezfio_interface_config_rule()
    l_string += ninja_ezfio_config_rule()
    l_string += ninja_ezfio_rule()

    from collections import namedtuple

#    l_module_to_compile = ["AOs", "CAS_SD", "Hartree_Fock"]

    for module_to_consider in l_module_to_compile:

        Path = namedtuple('Path', ['abs', 'rel'])

        path_module = Path(join(qpackage_root_src, module_to_consider),
                           module_to_consider)

        path_neeeded_module = join(path_module.abs, "NEEDED_CHILDREN_MODULES")
        l_all_needed_molule = module_genealogy(path_neeeded_module)

        # Make.depend rule and build
        l_string += ninja_makefile_depend_build(l_all_needed_molule, path_module)

        # EZFIO.cfg rule and build
        l_irp = get_l_ezfio_irp(l_all_needed_molule, path_module)

        # Symlink rule and build
        l_source, l_destination = get_source_destination(l_all_needed_molule,
                                                         path_module)

        l_string += ninja_symlink_build(l_source, l_destination)

        # irpf90.make
        l_string += ninja_irpf90_make_build(path_module,
                                            l_all_needed_molule + ["include"],
                                            l_irp,
                                            l_destination)
        # ninja_binary
        l_binary = get_program(path_module)
        l_string += ninja_binary_build(l_binary, path_module)

    l_module = get_module_with_ezfio_cfg()

    l_string += ninja_ezfio_cfg_build(l_module)

    l_ezfio_config = get_ezfio_config()

    l_string_dump, l_file_create = ninja_ezfio_config_build(l_module_with_ezfio_cfg=l_module,
                                                            l_ezfio_config=l_ezfio_config)
    l_string += l_string_dump

    l_string += ninja_all_binary_build(l_module_to_compile)

    l_string += ninja_ezfio_build(l_file_create)

    print "\n".join(l_string)

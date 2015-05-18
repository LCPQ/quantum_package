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


#  _ __  _ ___  _         _
# |_  / |_  |  / \    _ _|_ _
# |_ /_ |  _|_ \_/ o (_  | (_|
#                           _|
def get_l_ezfio_irp(l_all_needed_molule, path_module):

    l_module_abs = [join(qpackage_root_src, m) for m in l_all_needed_molule]

    l_irp = []
    l_ezfio_module_abs = []

    for m in l_module_abs + [path_module.abs]:

        for file in os.listdir(m):
            if file.endswith(".irp.f"):
                l_irp.append(join(m, file))
            if file == "EZFIO.cfg":
                l_ezfio_module_abs.append(m)
                l_irp.append(join(m, "ezfio_interface.irp.f"))

    return l_irp, l_ezfio_module_abs


def ninja_ezfio_cfg_rule():
    # Rule
    l_string = ["rule build_ezfio_interface"]
    l_string += [
        "   command = cd $sub_module ; ei_handler.py ; cd -"]
    l_string += [""]

    return l_string


def ninja_ezfio_cfg_build(l_ezfio_module_abs):
    # Build
    l_string = []
    for m in l_ezfio_module_abs:
        ez_interface = join(m, "ezfio_interface.irp.f")
        ez_cfg = join(m, "EZFIO.cfg")

        l_string += ["build {0}: build_ezfio_interface {1}".format(ez_interface,
                                                                   ez_cfg)]
        l_string += ["   sub_module = {0}".format(m)]
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
    l_string = ["rule build_irpf90.make"]
    l_string += ["   command = cd $module ; irpf90 $include_dir $irpf90_flag ; cd -"]
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
        cmd = 'grep -l "program" {0}/*.irp.f'.format(path_module.abs)
        p = subprocess.check_output([cmd], shell=True)
    except subprocess.CalledProcessError:
        return []
    else:
        return [os.path.basename(f).split(".")[0] for f in p.split()]


def ninja_binary_rule():

    # Rule
    l_string = ["rule build_binary"]
    l_string += ["   command = cd $module ; make $binary ; touch $binary; cd -"]
    l_string += [""]

    return l_string


def ninja_binary_build(l_bin, path_module):

    # Build
    irpf90mk_path = join(path_module.abs, "irpf90.make")

    l_abs_bin = [join(path_module.abs, binary) for binary in l_bin]

    l_string = []
    for path, abs_path in zip(l_bin, l_abs_bin):
        l_string += ["build {0}: build_binary {1}".format(abs_path,
                                                          irpf90mk_path)]
        l_string += ["   module = {0}".format(path_module.abs)]
        l_string += ["   binary = {0}".format(path)]

        l_string += [""]

    str_l_abs_bin = " ".join(l_abs_bin)

    l_string += ["build build_all_binary_{0}: phony {1}".format(path_module.rel,
                                                                str_l_abs_bin)]

    return l_string

if __name__ == "__main__":

    with open(join(qpackage_root_src, "NEEDED_MODULES"), "r") as f:
        l_module_to_compile = f.read().split()

    l_string = ninja_makefile_depend_rule()
    l_string += ninja_ezfio_cfg_rule()
    l_string += ninja_symlink_rule()
    l_string += ninja_irpf90_make_rule()
    l_string += ninja_binary_rule()

    from collections import namedtuple

    for module_to_consider in ["Hartree_Fock", "AOs"]: #l_module_to_compile:

        Path = namedtuple('Path', ['abs', 'rel'])

        path_module = Path(join(qpackage_root_src, module_to_consider),
                           module_to_consider)

        path_neeeded_module = join(path_module.abs, "NEEDED_CHILDREN_MODULES")
        l_all_needed_molule = module_genealogy(path_neeeded_module)

        # Make.depend rule and build
        l_string += ninja_makefile_depend_build(l_all_needed_molule, path_module)

        # EZFIO.cfg rule and build
        l_irp, l_ezfio_module_abs = get_l_ezfio_irp(l_all_needed_molule, path_module)
        l_string += ninja_ezfio_cfg_build(l_ezfio_module_abs)

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

    print "\n".join(l_string)

#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Take a path in argv
Check if EZFIO.cfg exists.
EZFIO.cfg are in MODULE directories.
create : ezfio_interface.irp.f
         folder_ezfio_inteface_config

Example EZFIO.cfg:
```
[thresh_SCF]
doc: Threshold on the convergence of the Hartree Fock energy
type: Threshold
default: 1.e-10

[do_pt2_end]
type: logical
doc: If true, compute the PT2 at the end of the selection
default: true
```

"""

import sys
import os
import os.path

import ConfigParser

from collections import defaultdict
from collections import namedtuple

Type = namedtuple('Type', 'ocaml fortran')


def get_type_dict():
    """
    This function makes the correspondance between the type of value read in
    ezfio.cfg into the f90 and Ocam Type
    return fancy_type[fancy_type] = namedtuple('Type', 'ocaml fortran')
    For example fancy_type['Ndet'].fortran = interger
                                  .ocaml   = int
    """

    # ~#~#~#~ #
    # I n i t #
    # ~#~#~#~ #

    fancy_type = defaultdict(dict)

    # ~#~#~#~#~#~#~#~ #
    # R a w _ t y p e #
    # ~#~#~#~#~#~#~#~ #

    fancy_type['integer'] = Type("int", "integer")
    fancy_type['int'] = Type("int", "integer")

    fancy_type['float'] = Type("float", "double precision")
    fancy_type['double precision'] = Type("float", "double precision")

    fancy_type['logical'] = Type("bool", "logical")
    fancy_type['bool'] = Type("bool", "logical")

    fancy_type['character*32'] = Type("string", "character*32")
    fancy_type['character*60'] = Type("string", "character*60")
    fancy_type['character*256'] = Type("string", "character*256")

    # ~#~#~#~#~#~#~#~ #
    # q p _ t y p e s #
    # ~#~#~#~#~#~#~#~ #

    # Dict to change ocaml LowLevel type into FortranLowLevel type
    ocaml_to_fortran = {"int": "integer",
                        "float": "double precision",
                        "logical": "logical",
                        "string": "character*32"}

    # Read and parse qptype
    src = os.environ['QPACKAGE_ROOT'] + "/ocaml/qptypes_generator.ml"
    with open(src, "r") as f:
        l = [i for i in f.read().splitlines() if i.strip().startswith("*")]

    # Read the fancy_type, the ocaml. and convert the ocam to the fortran
    for i in l:
        str_fancy_type = i.split()[1].strip()
        str_ocaml_type = i.split()[3]
        str_fortran_type = ocaml_to_fortran[str_ocaml_type]

        fancy_type[str_fancy_type] = Type(str_ocaml_type, str_fortran_type)

    return dict(fancy_type)


type_dict = get_type_dict()


def get_dict_config_file(config_file_path, module_lower):
    """
    Input:
        config_file_path is the config file path
            (for example FULL_PATH/EZFIO.cfg)
        module_lower is the MODULE name lowered
            (Ex fullci)

    Return a dict d[provider_name] = {type,
                                      doc,
                                      ezfio_name,
                                      ezfio_dir,
                                      size,
                                      interface,
                                      default}

    Type       : Is a fancy_type named typle who containt fortran and ocaml type
    doc        : Is the doc
    ezfio_name : Will be the name of the file
    ezfio_dir  : Will be the folder who containt the ezfio_name
        * /ezfio_dir/ezfio_name
        * equal to MODULE_lower name for the moment.
    interface  : The provider is a imput or a output
    if is a output:
        default    : The default value

    """
    # ~#~#~#~ #
    # I n i t #
    # ~#~#~#~ #

    d = defaultdict(dict)
    l_info_required = ["doc", "interface"]
    l_info_optional = ["ezfio_name", "size"]

    # ~#~#~#~#~#~#~#~#~#~#~ #
    # L o a d _ C o n f i g #
    # ~#~#~#~#~#~#~#~#~#~#~ #

    config_file = ConfigParser.ConfigParser()
    config_file.readfp(open(config_file_path))

    # ~#~#~#~#~#~#~#~#~ #
    # F i l l _ d i c t #
    # ~#~#~#~#~#~#~#~#~ #

    def error(o, p, c):
        "o option ; p provider_name ;c config_file_path"
        print "You need a {0} for {1} in {2}".format(o, p, c)

    for section in config_file.sections():
        # pvd = provider
        pvd = section.lower()

        # Create the dictionary who containt the value per default
        d_default = {"ezfio_name": pvd, "size": 1}

        # Set the ezfio_dir
        d[pvd]["ezfio_dir"] = module_lower

        # Check if type if avalaible
        type_ = config_file.get(section, "type")
        if type_ not in type_dict:
            print "{0} not avalaible. Choose in:".format(type_)
            print ", ".join([i for i in type_dict])
            sys.exit(1)
        else:
            d[pvd]["type"] = type_dict[type_]

        # Fill the dict with REQUIRED information
        for option in l_info_required:
            try:
                d[pvd][option] = config_file.get(section, option)
            except ConfigParser.NoOptionError:
                error(option, pvd, config_file_path)
                sys.exit(1)

        # Fill the dict with OPTIONAL information
        for option in l_info_optional:
            try:
                d[pvd][option] = config_file.get(section, option).lower()
            except ConfigParser.NoOptionError:
                d[pvd][option] = d_default[option]

        # If interface is output we need a default value information
        if d[pvd]["interface"] == "output":
            try:
                d[pvd]["default"] = config_file.get(section, "default")
            except ConfigParser.NoOptionError:
                error("default", pvd, config_file_path)
                sys.exit(1)

    return dict(d)


def create_ezfio_provider(dict_ezfio_cfg):
    """
    From dict d[provider_name] = {type,
                                  doc,
                                  ezfio_name,
                                  ezfio_dir,
                                  interface,
                                  default}
    create the a list who containt all the code for the provider
    return [code, ...]
    """
    from ezfio_with_default import EZFIO_Provider

    dict_code_provider = dict()

    ez_p = EZFIO_Provider()
    for provider_name, dict_info in dict_ezfio_cfg.iteritems():
        if "default" in dict_info:
            ez_p.set_type(dict_info['type'].fortran)
            ez_p.set_name(provider_name)
            ez_p.set_doc(dict_info['doc'])
            ez_p.set_ezfio_dir(dict_info['ezfio_dir'])
            ez_p.set_ezfio_name(dict_info['ezfio_name'])
            ez_p.set_default(dict_info['default'])

            ez_p.set_output("output_%s" % dict_info['ezfio_dir'])
            dict_code_provider[provider_name] = str(ez_p)

    return dict_code_provider


def save_ezfio_provider(path_head, dict_code_provider):
    """
    Write in path_head/"ezfio_interface.irp.f" the value of dict_code_provider
    """

    path = "{0}/ezfio_interface.irp.f".format(path_head)

    print "Path = {0}".format(path)

    try:
        f = open(path, "r")
    except IOError:
        old_output = ""
    else:
        old_output = f.read()
        f.close()

    l_output = ["! DO NOT MODIFY BY HAND",
                "! Created by $QPACKAGE_ROOT/scripts/ezfio_interface.py",
                "! from file {0}/EZFIO.cfg".format(path_head),
                "\n"]

    l_output += [code for code in dict_code_provider.values()]

    output = "\n".join(l_output)

    if output != old_output:
        with open(path, "w") as f:
            f.write(output)


def create_ezfio_config(dict_ezfio_cfg, opt, module_lower):
    """
    From dict_ezfio_cfg[provider_name] = {type, default, ezfio_name,ezfio_dir,doc}
    Return the string ezfio_interface_config
    """

    def size_format_to_ezfio(size_raw):
        """If = is a formula do nothing
        Else but the parenthsesis because is the ezfio_ format
        """
        size_raw = str(size_raw)
        if size_raw.startswith('='):
            size_convert = size_raw
        else:
            size_convert = "({0})".format(size_raw)
        return size_convert

    def create_format_string(size):
        """
        Take a size and return the string format for bewing right align
        Example size = 10
        Return '{:<10}'.format
        """

        return "{{:<{0}}}".format(size).format

    # ~#~#~#~#~#~#~#~#~#~#~# #
    #  F o r m a t _ i n f o #
    # ~#~#~#~#~#~#~#~#~#~#~# #

    lenmax_name = max([len(i) for i in dict_ezfio_cfg])
    lenmax_type = max([len(i["type"].fortran)
                       for i in dict_ezfio_cfg.values()])

    str_name_format = create_format_string(lenmax_name + 2)
    str_type_format = create_format_string(lenmax_type + 2)

    # ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~# #
    #  C r e a t e _ t h e _ s t r i n g #
    # ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~# #

    result = [module_lower]

    for provider_name, provider_info in sorted(dict_ezfio_cfg.iteritems()):

        # Get the value from dict
        name_raw = provider_name.lower()
        fortran_type_raw = provider_info["type"].fortran
        size_raw = size_format_to_ezfio(provider_info["size"])

        # Get the string in to good format (left align and co)
        str_name = str_name_format(name_raw)
        str_fortran_type = str_type_format(fortran_type_raw)
        str_size = size_raw

        # Return the string
        s = "  {0} {1} {2}".format(str_name, str_fortran_type, str_size)

        # Append
        result.append(s)

    return "\n".join(result)


def save_ezfio_config(module_lower, str_ezfio_config):
    """
    Write the str_ezfio_config in
    $QPACKAGE_ROOT/EZFIO/{0}.ezfio_interface_config".format(module_lower)
    """

    ezfio_dir = "{0}/EZFIO".format(os.environ['QPACKAGE_ROOT'])
    path = "{0}/config/{1}.ezfio_interface_config".format(ezfio_dir,
                                                          module_lower)

    try:
        f = open(path, "r")
    except IOError:
        old_output = ""
    else:
        old_output = f.read()
        f.close()

    if str_ezfio_config != old_output:
        with open(path, "w") as f:
            f.write(str_ezfio_config)


def main():
    """
    Two condition:
        -Take the EZFIO.cfg path in arg
        or
        -Look if EZFIO.cfg is present in the pwd
    """

    try:
        config_file_path = sys.argv[1]
    except:
        config_file_path = "EZFIO.cfg"
        if "EZFIO.cfg" not in os.listdir(os.getcwd()):
            sys.exit(0)

    config_file_path = os.path.expanduser(config_file_path)
    config_file_path = os.path.expandvars(config_file_path)
    config_file_path = os.path.abspath(config_file_path)
#    print config_file_path

    path_dirname = os.path.dirname(config_file_path)
    module = [i for i in path_dirname.split("/") if i][-1]
    module_lower = module.lower()

#    print "Read {0}".format(config_file_path)
    dict_info_provider = get_dict_config_file(config_file_path, module_lower)

#    print "Generating the ezfio_interface.irp.f: \n"
    d_config = create_ezfio_provider(dict_info_provider)

#    print "Saving the ezfio_interface.irp.f"
    save_ezfio_provider(path_dirname, d_config)

#    print "Generating the ezfio_config"
    config_ezfio = create_ezfio_config(
        dict_info_provider,
        "config",
        module_lower)

#    print "Saving ezfio_config"
    save_ezfio_config(module_lower, config_ezfio)


if __name__ == "__main__":
    main()

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
def bool_convertor(b):
  return ( b.lower() in [ "true", ".true." ] )


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

    # Dict to change ocaml LowLevel type into FortranLowLevel type
    ocaml_to_fortran = {"int": "integer",
                        "float": "double precision",
                        "logical": "logical",
                        "string": "character*60"}

    fancy_type = defaultdict(dict)

    # ~#~#~#~#~#~#~#~ #
    # R a w _ t y p e #
    # ~#~#~#~#~#~#~#~ #

    fancy_type['integer'] = Type("int", "integer")
    fancy_type['int'    ] = Type("int", "integer")

    fancy_type['float'           ] = Type("float", "double precision")
    fancy_type['double precision'] = Type("float", "double precision")

    fancy_type['logical'] = Type("bool", "logical")
    fancy_type['bool'   ] = Type("bool", "logical")

    # ~#~#~#~#~#~#~#~ #
    # q p _ t y p e s #
    # ~#~#~#~#~#~#~#~ #

    src = os.environ['QPACKAGE_ROOT'] + "/ocaml/qptypes_generator.ml"

    with open(src, "r") as f:
        l = [i for i in f.read().splitlines() if i.strip().startswith("*")]

    for i in l:
        ocaml_fancy_type = i.split()[1].strip()
        ocaml_type = i.split()[3]
        fortran_type = ocaml_to_fortran[ocaml_type]

        fancy_type[ocaml_fancy_type] = Type(ocaml_type, fortran_type)

    return dict(fancy_type)


type_dict = get_type_dict()


def get_dict_config_file(config_file_path,folder):
    """
    Read a ezfio.cfg
    Return a dict d[provider_name] = {type, default, ezfio_name,ezfio_dir,doc}
    """
    # ~#~#~#~ #
    # I n i t #
    # ~#~#~#~ #

    d = defaultdict(dict)
    list_option_required = ["default", "doc"]
    list_option          = ["ezfio_name", "output"]

    # ~#~#~#~#~#~#~#~#~#~#~ #
    # L o a d _ C o n f i g #
    # ~#~#~#~#~#~#~#~#~#~#~ #

    config_file = ConfigParser.ConfigParser()
    config_file.readfp(open(config_file_path))

    # ~#~#~#~#~#~#~#~#~ #
    # F i l l _ d i c t #
    # ~#~#~#~#~#~#~#~#~ #

    provider_names = config_file.sections()
    for p in provider_names:
        provider_name = p.lower()
        default_d = {"ezfio_name": provider_name, "output": "false" }
        # Check if type if avalaible
        type_ = config_file.get(p, "type")
        if type_ not in type_dict:
            print "{0} not avalaible. Choose in:".format(type_)
            print ", ".join([i for i in type_dict])
            sys.exit(1)
        else:
            d[provider_name]["type"] = type_dict[type_]

        # Fill the dict with allother the information
        for k in list_option_required:
            try:
                d[provider_name][k] = config_file.get(p, k)
            except ConfigParser.NoOptionError:
                print "You need a {0} for {1} in {2}".format(k,
                                                             provider_name,
                                                             config_file_path)
        d[provider_name]["ezfio_dir"] = folder
        for k in list_option:
            try:
                d[provider_name][k] = config_file.get(p, k).lower()
            except ConfigParser.NoOptionError:
                d[provider_name][k] = default_d[k]

        # Convert string to bool
        d[provider_name]["output"] = bool_convertor(d[provider_name]["output"])

    return dict(d)


def create_ezfio_provider(dict_ezfio_cfg):
    """
    From dict d[provider_name] = {type, default, ezfio_name,ezfio_dir,doc}
    create the a list who containt all the code for the provider
    return [code, ...]
    """
    from ezfio_with_default import EZFIO_Provider

    dict_code_provider = dict()

    ez_p = EZFIO_Provider()
    for provider_name, dict_info in dict_ezfio_cfg.iteritems():
        if not dict_info["output"]:
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
    Write in "ezfio_interface.irp.f" the
    value of dict_code_provider
    """

    path = "{0}/ezfio_interface.irp.f".format(path_head)

    print "Path = {}".format(path)

    with open(path, "w") as f:
        f.write("!DO NOT MODIFY BY HAND \n")
        f.write("!Created by $QPACKAGE_ROOT/scripts/ezfio_interface.py \n")
        f.write("!from file {0}/EZFIO.cfg\n".format(path_head))
        f.write("\n")
        for provider_name, code in dict_code_provider.iteritems():
            f.write(code + "\n")


def create_ezfio_config(dict_ezfio_cfg, opt, folder):
    """
    From dict_ezfio_cfg[provider_name] = {type, default, ezfio_name,ezfio_dir,doc}
    Return the string ezfio_interface_config
    """

    result = [ folder ]
    lenmax = max( [ len(i) for i in dict_ezfio_cfg ]  )+2
    l = sorted(dict_ezfio_cfg.keys())
    for provider_name in l:
      provider_info = dict_ezfio_cfg[provider_name]
      s = "  {0} {1}".format( provider_name.lower().ljust(lenmax), provider_info["type"].fortran )
      result.append(s)
    return "\n".join(result)

def save_ezfio_config(folder, str_ezfio_config):
    """
    Write the str_ezfio_config in
    $QPACKAGE_ROOT/EZFIO/{0}.ezfio_interface_config".format(folder)
    """

    ezfio_dir = "{0}/EZFIO".format(os.environ['QPACKAGE_ROOT'])
    path = "{0}/config/{1}.ezfio_interface_config".format(ezfio_dir,
                                                          folder)

    print "Path = {}".format(path)

    with open(path, "w") as f:
        f.write(str_ezfio_config)

def main():
    """Take in argument a EZFIO.cfg"""

    try:
      path = sys.argv[1]
    except:
      path = "EZFIO.cfg"
      if "EZFIO.cfg" not in os.listdir(os.getcwd()):
        sys.exit(0)

    path = os.path.expanduser(path)
    path = os.path.expandvars(path)
    path = os.path.abspath(path)
    print path

    path_dirname = os.path.dirname(path)
    folder = [i for i in path_dirname.split("/") if i][-1]
    folder = folder.lower()

    print "Find a EZFIO.cfg in {}".format(path)
    dict_info_provider = get_dict_config_file(path,folder)

    print "Generating the ezfio_interface.irp.f: \n"
    d_config = create_ezfio_provider(dict_info_provider)
#    for provider, code in d_config.iteritems():
#        print code

    print "Saving the ezfio_interface.irp.f"
    save_ezfio_provider(path_dirname, d_config)

    print "Generating the ezfio_config"
    config_ezfio = create_ezfio_config(dict_info_provider, "config", folder)
#    print config_ezfio

    print "Saving ezfio_config"
    save_ezfio_config(folder, config_ezfio)


if __name__ == "__main__":
  main()


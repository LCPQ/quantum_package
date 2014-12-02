#!/usr/bin/env python
# -*- coding: utf-8 -*-

import ConfigParser
from collections import defaultdict


def create_dic_fancy_type():
    fancy_type = defaultdict(dict)

    ocaml_to_fortran = {"int": "integer",
                        "float": "double precision",
                        "logical": "logical",
                        "string": "character*60"}

    fancy_type['integer'] = {
        "ocaml": "int",
        "fortran": "integer"}

    fancy_type['double precision'] = {
        "ocaml": "float",
        "fortran": "double precision"}
    fancy_type['logical'] = {
        "ocaml": "logical",
        "fortran": "logical"}

    src = os.environ.get('QPACKAGE_ROOT') + "/ocaml/qptypes_generator.ml"

    with open(src, "r") as f:
        l = f.read().splitlines()

    l = [i for i in l if i.strip().startswith("*")]

    for i in l:
        ocaml_fancy_type = i.split()[1].strip()
        ocaml_type = i.split()[3]
        fortran_type = ocaml_to_fortran[ocaml_type]

        fancy_type[ocaml_fancy_type] = {"ocaml": ocaml_type,
                                        "fortran": fortran_type}

    return dict(fancy_type)

import sys


def dict_config_file(config_file, dic_type):
    d = defaultdict(dict)
    list_key = ["doc", "ezfio_name", "ezfio_dir", "default_value"]

    provider_names = config_file.sections()
    for provider_name in provider_names:

        if config_file.get(provider_name, "type") not in dic_type:
            print config_file.get(provider_name, "type"), "not avalaible. Choose in: "
            print ", ".join([i for i in dic_type])

            sys.exit(1)

        for k in list_key:
            d[provider_name][k] = config_file.get(provider_name, k)

            type_raw = config_file.get(provider_name, "type")
            d[provider_name]["ezfio_type"] = dic_type[type_raw]["fortran"]
            d[provider_name]["ocaml_type"] = dic_type[type_raw]["ocaml"]

    return dict(d)


def create_ezfio_string(dict_all, t):
    a = {"config": "ezfio_type",
         "default": "default_value"}

    info = a[t]
    d = defaultdict(list)

    for provider_name, dict_info in dict_all.items():
        l_to_join = [" ", dict_info['ezfio_name'], dict_info[info], "\n"]
        d[dict_info['ezfio_dir']].append(l_to_join)

    # Only for fun,sry reader...
    return [" ".join([k, "\n"] + [j for i in v for j in i])
            for k, v in d.items()]


def create_ezfio_provider(dict_all):
    from ezfio_with_default import EZFIO_Provider
    lp = []

    T = EZFIO_Provider()
    for provider_name, dict_info in dict_all.items():
        T.set_type(dict_info['ezfio_type'])
        T.set_name(provider_name)
        T.set_doc(dict_info['doc'])
        T.set_ezfio_dir(dict_info['ezfio_dir'])
        T.set_ezfio_name(dict_info['ezfio_name'])
        T.set_default(dict_info['default_value'])

        T.set_output("output_%s" % dict_info['ezfio_dir'])
        lp.append(str(T))

    return lp

import os


def save_ezfio_config(array):
    path = os.environ.get('QPACKAGE_ROOT') + "/EZFIO/config/"

    with open(path + "interface.ezfio_config","w") as f:
        for i in array:
            f.write(i + "\n")


def save_ezfio_provider(array):
    path = os.environ.get('QPACKAGE_ROOT') + "/src/Ezfio_files/"

    with open(path + "interface.irp.f","w") as f:
        f.write("!DO NOT MODIFY BY HAND \n")
        for i in array:
            f.write(i + "\n")


def get_dict_cfg(pickle=False):

    if pickle:
        import pickle
        path = os.environ.get('QPACKAGE_ROOT') + "/data/"
        dict_cfg = pickle.load(open(path + "dict_cfg.p", "rb"))
    else:
        dict_cfg = {}
        fancy_type = create_dic_fancy_type()

        src = os.environ.get('QPACKAGE_ROOT') + "/src/"

        dirs_name = [x[0] for x in os.walk(src, topdown=False)]
        for path_config_folder in dirs_name:

            try:
                config = ConfigParser.ConfigParser()
                config.readfp(open(path_config_folder + '/EZFIO.cfg'))
            except Exception as inst:
                pass
            else:
                dict_cfg.update(dict_config_file(config, fancy_type))

    return dict_cfg


def get_ezfio_cmd(dict_cfg):
    d = defaultdict(dict)
    for k, v in dict_cfg.items():
        d[k] = {"set": "ezfio_set_" + v["ezfio_dir"] + "_" + k,
                "get": "ezfio_get_" + v["ezfio_dir"] + "_" + k}

    return dict(d)


def save_dict_cfg(d):
    import pickle
    path = os.environ.get('QPACKAGE_ROOT') + "/data/"
    pickle.dump(d, open(path + "dict_cfg.p", "wb"))


if __name__ == "__main__":
    roger = get_dict_cfg()
    save_dict_cfg(roger)
    biloute = get_dict_cfg(True)

    print biloute

    config = create_ezfio_string(biloute, "config")
    save_ezfio_config(config)
#    for i in config:
#        print i
#
#    for i in create_ezfio_string(biloute, "default"):
#        print i
#
#    for i in get_ezfio_cmd(biloute).items():
#        print i

    irp = create_ezfio_provider(biloute)
    save_ezfio_provider(irp)
#    for i in irp:
#        print i
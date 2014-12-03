#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
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
        l = [i for i in f.read().splitlines() if i.strip().startswith("*")]

    for i in l:
        ocaml_fancy_type = i.split()[1].strip()
        ocaml_type = i.split()[3]
        fortran_type = ocaml_to_fortran[ocaml_type]

        fancy_type[ocaml_fancy_type] = {"ocaml": ocaml_type,
                                        "fortran": fortran_type}

    return dict(fancy_type)


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

import re


def find_doc_prog(path):
    regex_prog = re.compile(r'\bprogram\b', re.IGNORECASE)
    regex_begin = re.compile(r'\bBEGIN_DOC\b')
    regex_end = re.compile(r'\bEND_DOC\b')

    bible = open(path, "r")
    find = False
    read = False
    doc = ""

    for line in bible:
        if regex_prog.search(line):
            find = True
            continue

        if find:
            if regex_begin.search(line):
                read = True
                continue

        if find:
            if regex_end.search(line):
                read = False
                break

        if read:
            doc += line[line.index("!") + 1:]

    if doc:
        return doc
    else:
        raise


def dict_binary_file(irp_file, dcf):
    d = defaultdict(dict)

    try:
        doc = find_doc_prog(irp_file)
    except:
        pass
    else:
        path_bin = irp_file[:irp_file.index(".irp.f")]
        name = os.path.basename(path_bin)

        d[name]["doc"] = doc
        d[name]["providers"] = [i for i in dcf]
        d[name]["path"] = path_bin

    return dict(d)


def create_ezfio_string(dict_all, t):
    a = {"config": "ezfio_type",
         "default": "default_value"}

    info = a[t]
    d = defaultdict(list)

    for provider_name, dict_info in dict_all.items():
        l_to_join = [dict_info['ezfio_name'], dict_info[info], "\n"]
        d[dict_info['ezfio_dir']].append(l_to_join)

    # Only for fun,sry reader...
    return ["  ".join([k, "\n"] + [j for i in v for j in i])
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

    with open(path + "interface.ezfio_config", "w") as f:
        for i in array:
            f.write(i + "\n")


def save_ezfio_provider(array):
    path = os.environ.get('QPACKAGE_ROOT') + "/src/Ezfio_files/"

    with open(path + "interface.irp.f", "w") as f:
        f.write("!DO NOT MODIFY BY HAND \n")
        for i in array:
            f.write(i + "\n")


def get_dicts(pickle=False):

    if pickle:
        import pickle as pickle_lib
        path = os.environ.get('QPACKAGE_ROOT') + "/data/"
        dict_binary = pickle_lib.load(open(path + "dict_binary.p", "rb"))
        dict_cfg = pickle_lib.load(open(path + "dict_cfg.p", "rb"))

    else:

        dict_cfg = {}
        dict_binary = {}

        fancy_type = create_dic_fancy_type()

        src = os.environ.get('QPACKAGE_ROOT') + "/src/"
        for root, dirs, files in os.walk(src):

            dcf = {}

            if "EZFIO.cfg" in files:
                config = ConfigParser.ConfigParser()
                config.readfp(open(root + '/EZFIO.cfg'))
                dcf = dict_config_file(config, fancy_type)
                dict_cfg.update(dcf)

            list_irp = [root + "/" + i for i in files if i.endswith(".irp.f")]
            for i in list_irp:
                dict_binary.update(dict_binary_file(i, dcf))

    return [dict_binary, dict_cfg]


def get_dict_binary(pickle=False):
    return get_dicts(pickle)[0]


def get_dict_cfg(pickle=False):
    return get_dicts(pickle)[1]


def save_dicts(d):
    import pickle
    path = os.environ.get('QPACKAGE_ROOT') + "/data/"
    pickle.dump(d[0], open(path + "dict_binary.p", "wb"))
    pickle.dump(d[1], open(path + "dict_cfg.p", "wb"))


def get_ezfio_cmd(dict_cfg):
    d = defaultdict(dict)
    for k, v in dict_cfg.items():
        d[k] = {"set": "set_" + v["ezfio_dir"] + "_" + k,
                "get": "get_" + v["ezfio_dir"] + "_" + k,
                "has": "has_" + v["ezfio_dir"] + "_" + k}

    return dict(d)


if __name__ == "__main__":
    roger = get_dicts()
    print roger[0]
    print roger[1]

    save_dicts(roger)
    biloute = get_dict_cfg(True)

    print biloute

    config = create_ezfio_string(biloute, "config")
    save_ezfio_config(config)
    for i in config:
        print i

    for i in create_ezfio_string(biloute, "default"):
        print i

    for i in get_ezfio_cmd(biloute).items():
        print i

    irp = create_ezfio_provider(biloute)
    save_ezfio_provider(irp)
#    for i in irp:
#        print i

#!/usr/bin/env python
# -*- coding: utf-8 -*-

import ConfigParser
from collections import defaultdict


def create_ezfio_config(config_file, save, root):
    d = defaultdict(list)

    ls = config_file.sections()
    for i in ls:
        l_to_join = [" ",
                     config_file.get(i, 'ezfio_name'),
                     config_file.get(i, 'ezfio_type'),
                     "\n"]

        d[config_file.get(i, 'ezfio_dir')].append(l_to_join)

    s = ""
    for k, v in d.items():
        s += k + "\n"
        for i in v:
            s += " ".join(i)

    if (save):
        for k, v in d.items():
            f = open(root + k + ".ezfio_config", "w")
            f.write(k + "\n")
            for i in v:
                f.write(" ".join(i))
            f.close()

    return s


def create_ezfio_default(config_file):

    d = defaultdict(list)

    ls = config_file.sections()
    for i in ls:
        l_to_join = [" ",
                     config_file.get(i, 'ezfio_name'),
                     config_file.get(i, 'ezfio_default_value'),
                     "\n"]

        d[config_file.get(i, 'ezfio_dir')].append(l_to_join)

    f = ""
    for k, v in d.items():
        f += k + "\n"
        for i in v:
            f += " ".join(i)

    return f


def create_ezfio_provider(config_file):
    from ezfio_with_default import EZFIO_Provider
    lp = []

    T = EZFIO_Provider()

    ls = config_file.sections()
    for i in ls:
        T.set_type(config_file.get(i, 'ezfio_type'))
        T.set_name(i)
        T.set_doc(config_file.get(i, 'doc'))
        T.set_ezfio_dir(config_file.get(i, 'ezfio_dir'))
        T.set_ezfio_name(config_file.get(i, 'ezfio_name'))
        T.set_default(config_file.get(i, 'ezfio_default_value'))

        T.set_output("output_%s" % config_file.get(i, 'ezfio_dir'))
        lp.append(T.get_string())

    return lp

import sys
import os

if __name__ == "__main__":
    path_config_folder = os.path.expanduser(sys.argv[1])

    config = ConfigParser.ConfigParser()

    try:
        config.readfp(open(path_config_folder + '/DEPENDENCY'))
    except:
        sys.exit("No DEPENDENCY file in %s" % (path_config_folder))

    create_ezfio_config(config, True, path_config_folder)

    for i in create_ezfio_provider(config):
        print i

    path = "{root}/options.irp.f".format(
        root=path_config_folder)
    with open(path, "w") as f:
        for i in create_ezfio_provider(config):
            f.write(i)

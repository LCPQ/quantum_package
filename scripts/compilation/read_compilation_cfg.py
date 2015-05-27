#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import ConfigParser


def get_l_option_section(config):
    return [o for o in ['OPENMP', 'PROFILE', 'DEBUG'] if config.getboolean("OPTION", o)]


def get_compilation_option(pwd_cfg, flag_name):

    config = ConfigParser.ConfigParser()
    config.read(pwd_cfg)

    l_option_section = get_l_option_section(config)

    l = []
    for section in ["COMMON"] + l_option_section:
        try:
            l.extend(config.get(section, flag_name).split())
        except ConfigParser.NoOptionError:
            pass

    return " ".join(l)

if __name__ == '__main__':

    qpackage_root = os.environ['QPACKAGE_ROOT']
    pwd_cfg = os.path.join(qpackage_root, "scripts/compilation/compilation_ifort.cfg")

    print get_compilation_option(pwd_cfg, "FC")

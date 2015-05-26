#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import ConfigParser
import sys
from cache import cache

qpackage_root = os.environ['QPACKAGE_ROOT']

Config = ConfigParser.ConfigParser()
pwd = os.path.join(qpackage_root, "scripts/compilation/compilation.cfg")
Config.read(pwd)


@cache
def get_l_option_section():
    return [o for o in ['OPENMP', 'PROFILE', 'DEBUG'] if Config.getboolean("OPTION", o)]


@cache
def get_compilation_option(name):

    l_option_section = get_l_option_section()

    l = []
    for section in ["COMMON"] + l_option_section:
        try:
            l.extend(Config.get(section, name).split())
        except ConfigParser.NoOptionError:
            pass

    return " ".join(l)

if __name__ == '__main__':

    name = sys.argv[1]

    print get_compilation_option(name)

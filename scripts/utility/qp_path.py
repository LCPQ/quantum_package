#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import os
import sys

try:
    QP_ROOT = os.environ['QP_ROOT']
except:
    print "source quantum_package.rc"
    sys.exit(1)
else:
    QP_SRC = os.path.join(QP_ROOT, "src")
    QP_PLUGINS = os.path.join(QP_ROOT, "plugins")
    QP_EZFIO = os.path.join(QP_ROOT, "install", "EZFIO")
    QP_OCAML = os.path.join(QP_ROOT, "ocaml")

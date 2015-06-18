#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os

try:
    qp_root = os.environ['QP_ROOT']
except:
    print "source quantum_package.rc"
    sys.exit(1)
else:
    qp_root_src = os.path.join(qp_root, "src")

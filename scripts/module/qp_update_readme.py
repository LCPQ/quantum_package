#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Updates the README.rst of a module
Usage:
       qp_update_readme.py [<module_path>...] [--root_module=<module_path>]

Options:
    path_readme: All the absolute path you want to update.
                 By default is the cwd
    --root_module: Is the path of the root module who containt the tags file.
                   By default is the cwd
"""

import sys

try:
    from docopt import docopt
except:
    print "Please check if you have sourced the .quantum_package.rc"
    print "(`source .quantum_package.rc`)"
    print sys.exit(1)

import os
from collections import namedtuple
from collections import defaultdict


def header_format(str_):
    return "{0}\n{1}\n".format(str_, "=" * len(str_))


D_KEY = {"needed_module": header_format("Needed Modules"),
         "documentation": header_format("Documentation")}


URL = "http://github.com/LCPQ/quantum_package/tree/master/src"

HEADER = "\n".join([".. Do not edit this section It was auto-generated",
                    ".. by the `update_README.py` script."])

# d[Path] ={humain, needed_module, documentation}
d_readme = defaultdict(dict)


def fetch_splitted_data(l_module_readme):
    """Read the README.rst file and split it in strings:
    * The documentation
    * The needed modules
    The result is given as a list of strings
    """

    sentinel = "@@$%&@@"

    for path_readme in l_module_readme:
        with open(os.path.join(path_readme, "README.rst"), 'r') as f:
            data = f.read()

        # Place sentinels

        for v in D_KEY.values():
            data = data.replace(v, sentinel + v)

        # Now Split data using the sentinels
        d_readme[path_readme] = {"human": data.split(sentinel)[0]}


def update_needed(d_readme):
    """Read the NEEDED_CHILDREN_MODULES file, and replace the data with it.
    Create the links to the GitHub pages."""

    header_image = ".. image:: tree_dependency.png"

    for path in d_readme:

        with open(os.path.join(path, 'NEEDED_CHILDREN_MODULES'), 'r') as f:
            modules = f.read()

        if modules.strip():
            l_module = ['* `{0} <{1}>`_'.format(name, os.path.join(URL, name))
                        for name in modules.split()]

            l_module_section = [D_KEY["needed_module"],
                                HEADER, '',
                                header_image, '',
                                '\n'.join(l_module), '', '']
        else:
            l_module_section = ""

        d_readme[path]["needed_module"] = "\n".join(l_module_section)


def extract_doc(root_module, provider):
    """Extracts the documentation contained in IRPF90_man file"""

    path = os.path.join(root_module, "IRPF90_man/%s.l" % (provider))
    with open(path, 'r') as f:
        l_line = f.readlines()

    result = []
    inside = False
    for line in l_line:
        if not inside:
            inside = line.startswith(".SH Description")
        else:
            if line.startswith(".SH"):
                break
            result.append("  {0}".format(line.strip()))

    if not result:
        result = ["  Undocumented"]

    return "\n".join(result) + "\n"


def update_documentation(root_module, d_readme):
    """Reads the BEGIN_DOC ... END_DOC blocks and builds the documentation"""

    IRP_info = namedtuple('IRP_info', ["module", "file", "provider", "line"])

    # If the file does not exist, don't do anything
    path = os.path.join(root_module, "tags")

    with open(path, 'r') as f:
        dump = f.readlines()

    d_info = defaultdict(list)

    for i in dump:
        # i =
        # output_cpu_time_0  Ezfio_files/output.irp.f    2
        provider, irp_file_raw, ligne = i.split()

        for path in d_readme:

            if root_module == path and "/" not in irp_file_raw:
                d_info[path].append(IRP_info(os.path.basename(path),
                                             irp_file_raw,
                                             provider,
                                             ligne))

            elif "/" in irp_file_raw and os.path.dirname(irp_file_raw) in path:

                module, irp_file = os.path.split(irp_file_raw)
                d_info[path].append(IRP_info(module, irp_file, provider, ligne))

    for path in d_readme:

        l_doc = []

        for irp in d_info[path]:
            url = os.path.join(URL, os.path.basename(path), irp.file)
            doc = extract_doc(root_module, irp.provider)

            l_doc += ["`{0} <{1}#L{2}>`_".format(irp.provider, url, irp.line),
                      doc,
                      ""]

        l_doc_section = [D_KEY["documentation"],
                         HEADER, '',
                         "\n".join(l_doc)]

        d_readme[path]["documentation"] = "\n".join(l_doc_section)

if __name__ == '__main__':
    arguments = docopt(__doc__)

    if arguments["--root_module"]:
        root_module = os.path.realpath(arguments["--root_module"])
    else:
        root_module = os.getcwd()

    if not arguments["<module_path>"]:
        l_module_readme = [os.path.join(os.getcwd())]
    else:
        l_module_readme = arguments["<module_path>"]

    try:
        fetch_splitted_data(l_module_readme)
    except IOError:
        print l_module_readme, "is not a module and/or",
        print "have not a `README.rst` file inside"
        print "Abort..."
        sys.exit(1)

    update_needed(d_readme)
    update_documentation(root_module, d_readme)

    for path, d in d_readme.iteritems():

        with open(os.path.join(path, "README.rst"), 'w') as f:
            for k in ["human",
                      "needed_module",
                      "documentation"]:

                f.write(d[k])

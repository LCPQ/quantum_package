#!/usr/bin/env python2
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
    from module_handler import is_module, is_plugin
except:
    print "Please check if you have sourced the .quantum_package.rc"
    print "(`source .quantum_package.rc`)"
    print sys.exit(1)

import os
from collections import namedtuple
from collections import defaultdict


def header_format(str_):

    warning = "\n".join([".. Do not edit this section It was auto-generated",
                        ".. by the `update_README.py` script."])

    return "{0}\n{1}\n{2}\n".format(str_, "=" * len(str_), warning)

D_KEY = {"needed_module": header_format("Needed Modules"),
         "documentation": header_format("Documentation")}


def get_url(path_module_rel):
    if is_plugin(path_module_rel):
        url = "http://github.com/LCPQ/quantum_package/tree/master/plugins"
    elif is_module(path_module_rel):
        url = "http://github.com/LCPQ/quantum_package/tree/master/src"
    else:
        print "{0} Is not a valid module nor plugin".format(path_module_rel)
        sys.exit(1)

    return os.path.join(url, path_module_rel)


def fetch_splitted_data(d_readme, l_module_readme):
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
            l_module = ['* `{0} <{1}>`_'.format(name, get_url(name))
                        for name in modules.split()]

            l_module_section = [D_KEY["needed_module"], '',
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


def update_documentation(d_readmen, root_module):
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
            url = os.path.join(get_url(os.path.basename(path)), irp.file)
            doc = extract_doc(root_module, irp.provider)

            if ".irp.f_shell_" in irp.file:
                l_doc += ["{0}".format(irp.provider),
                          doc,
                          ""]
            else:
                l_doc += ["`{0} <{1}#L{2}>`_".format(irp.provider, url, irp.line),
                          doc,
                          ""]

        l_doc_section = [D_KEY["documentation"], '',
                         "\n".join(l_doc)]

        d_readme[path]["documentation"] = "\n".join(l_doc_section)


if __name__ == '__main__':

    # Update documentation only if the remote repository is
    # the main repository
    from is_master_repository import is_master_repository
    if not is_master_repository:
        sys.exit(0)

    arguments = docopt(__doc__)

    if arguments["--root_module"]:
        root_module = os.path.realpath(arguments["--root_module"])
    else:
        root_module = os.getcwd()

    if not arguments["<module_path>"]:
        l_module_readme = [os.path.join(os.getcwd())]
    else:
        l_module_readme = arguments["<module_path>"]

    # d[Path] ={humain, needed_module, documentation}
    d_readme = defaultdict(dict)

    try:
        fetch_splitted_data(d_readme, l_module_readme)
    except IOError:
        print l_module_readme, "is not a module and/or",
        print "has not a `README.rst` file inside"
        print "Aborting..."
        sys.exit(1)

    update_needed(d_readme)
    update_documentation(d_readme, root_module)

    for path, d in d_readme.iteritems():

        with open(os.path.join(path, "README.rst"), 'w') as f:
            for k in ["human",
                      "needed_module",
                      "documentation"]:

                f.write(d[k])

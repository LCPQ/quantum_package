#!/usr/bin/env python
"""Updates the README.rst file as the include directive is disabled on GitHub."""
__date__ = "Thu Apr  3 23:06:18 CEST 2014"
__author__ = "Anthony Scemama<scemama@irsamc.ups-tlse.fr>  & TApplencourt "

README = "README.rst"
Assum_key = "Assumptions\n===========\n"
Needed_key = "Needed Modules\n==============\n"
Doc_key = "Documentation\n=============\n"
Sentinel = "@@$%&@@"
URL = "http://github.com/LCPQ/quantum_package/tree/master/src/"

import os
import subprocess
from collections import namedtuple
import sys

"""
NEED to call in a module
First arg can be the root parent
"""
try:
    ROOT_module = os.path.realpath(sys.argv[1])
except:
    ROOT_module = os.getcwd()

if ROOT_module != os.getcwd():
    change = True
else:
    change = False

MODULE_NAME = os.path.basename(os.getcwd())


header = """
.. Do not edit this section. It was auto-generated from the
.. by the `update_README.py` script.

"""


def fetch_splitted_data():
    """Read the README.rst file and split it in strings:
    * The documentation
    * The needed modules
    The result is given as a list of strings
    """

    with open(README, 'r') as f:
        data = f.read()

    # Place sentinels
    data = data.replace(Doc_key, Sentinel + Doc_key)
    data = data.replace(Needed_key, Sentinel + Needed_key)

    # Now Split data using the sentinels
    result = data.split(Sentinel)

    return result


def update_needed(data):
    """Read the NEEDED_CHILDREN_MODULES file, and replace the data with it.
    Create the links to the GitHub pages."""

    with open('NEEDED_CHILDREN_MODULES', 'r') as f:
        modules = f.read()

    header_image = ".. image:: tree_dependency.pdf\n\n"

    if modules.strip():
        modules = ['* `{0} <{1}>`_'.format(name, os.path.join(URL, name))
                   for name in modules.split()]
        modules = "\n".join(modules)
        modules = Needed_key + header + header_image + modules + '\n\n'

    has_modules = False
    for i in range(len(data)):
        if data[i].startswith(Needed_key):
            has_modules = True
            data[i] = modules

    if not has_modules:
        data.append(modules)

    return data


def extract_doc(item):
    """Extracts the documentation contained in IRPF90_man file"""

    path = os.path.join(ROOT_module, "IRPF90_man/%s.l" % (item))
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

    return "\n".join(result) + '\n'


def update_documentation(data):
    """Reads the BEGIN_DOC ... END_DOC blocks and builds the documentation"""

    IRP_info = namedtuple('IRP_info', ["name", "file", "line"])

    # If the file does not exist, don't do anything

    path = os.path.join(ROOT_module, "tags")

    with open(path, 'r') as f:
        dump = f.readlines()

    l_info = []
    for i in dump:
        name, f, ligne = i.split()

        if not change and "/" not in i:
            l_info.append(IRP_info(name, f, ligne))
        elif change and MODULE_NAME in i:
            l_info.append(IRP_info(name, f.split("/")[-1], ligne))

    l_line = []

    for irp in l_info:
        url = os.path.join(URL, MODULE_NAME, irp.file)
        doc = extract_doc(irp.name)

        l_line += ["`{0} <{1}#L{2}>`_".format(irp.name, url, irp.line), doc,
                   ""]

    documentation = Doc_key + header + "\n".join(l_line)

    has_doc = False
    for i in range(len(data)):
        if data[i].startswith(Doc_key):
            has_doc = True
            data[i] = documentation

    if not has_doc:
        data.append(documentation)

    return data


def git_add():
    """Executes:
    git add README.rst
    throw an error if git is not precent"""

    try:
        # pipe output to /dev/null for silence
        null = open("/dev/null", "w")
        subprocess.Popen("git add README.rst", stdout=null, stderr=null)
        null.close()

    except OSError:
        raise


def main():
    data = fetch_splitted_data()

    data = update_documentation(data)
    data = update_needed(data)
    output = ''.join(data)

    with open(README, 'w') as f:
        f.write(output)

    try:
        git_add()
    except OSError:
        pass


if __name__ == '__main__':
    main()

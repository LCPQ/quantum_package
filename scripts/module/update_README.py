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

header = """
.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES_CHILDREN file by the `update_README.py` script.

"""


def fetch_splitted_data():
    """Read the README.rst file and split it in strings:
    * The description
    * The assumptions
    * The documentation
    * The needed modules
    The result is given as a list of strings
    """

    file = open(README, 'r')
    data = file.read()
    file.close()

    # Place sentinels
    data = data.replace(Assum_key, Sentinel + Assum_key)
    data = data.replace(Doc_key, Sentinel + Doc_key)
    data = data.replace(Needed_key, Sentinel + Needed_key)

    # Now Split data using the sentinels
    result = data.split(Sentinel)

    return result


def update_assumptions(data):
    """Read the ASSUMPTIONS.rst file, and replace the data with it."""

    try:
        file = open('ASSUMPTIONS.rst', 'r')
    except IOError:
        file = open('ASSUMPTIONS.rst', 'w')
        assumptions = ""
    else:
        assumptions = file.read()
    file.close()

    if assumptions.strip() != "":
        assumptions = Assum_key + header + assumptions + '\n\n'

    has_assumptions = False
    for i in range(len(data)):
        if data[i].startswith(Assum_key):
            has_assumptions = True
            data[i] = assumptions

    if not has_assumptions:
        data.insert(1, assumptions)

    return data


def update_needed(data):
    """Read the NEEDED_CHILDREN_MODULES file, and replace the data with it.
    Create the links to the GitHub pages."""

    file = open('NEEDED_CHILDREN_MODULES', 'r')
    modules = file.read()
    file.close()

    header_image = ".. image:: tree_dependancy.png\n\n"

    if modules.strip() != "":
        modules = ['* `%s <%s%s>`_' % (x, URL, x) for x in modules.split()]
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


def update_documentation(data):
    """Reads the BEGIN_DOC ... END_DOC blocks and builds the documentation"""

    # If the file does not exist, don't do anything
    try:
        file = open('tags', 'r')
    except:
        return
    tags = file.readlines()
    file.close()

    def extract_doc(item):
        """Extracts the documentation contained in IRPF90_man file"""
        file = open("IRPF90_man/%s.l" % (item), 'r')
        lines = file.readlines()
        file.close()
        result = []
        inside = False
        for line in lines:
            if not inside:
                inside = line.startswith(".SH Description")
            else:
                if line.startswith(".SH"):
                    break
                result.append("  " + line.strip())

        if result == []:
            result = ["  Undocumented"]
        return "\n".join(result) + '\n'

    items = []
    dirname = os.path.basename(os.getcwd())
    command = "git ls-tree --full-tree --name-only HEAD:src/%s"
    command = command % (dirname)
    try:
        if dirname != 'src':
            p = subprocess.Popen(command.split(), stdout=subprocess.PIPE)
            tracked_files = p.stdout.read()
        else:
            tracked_files = ""
        tracked_files = tracked_files.splitlines()
    except:
        tracked_files = []
    for filename in tracked_files:
        if filename.endswith('.irp.f'):
                    # Search for providers, subroutines and functions in each file using
                    # the tags file
            search = "\t" + filename + "\t"
            tmp = filter(lambda line: search in line, tags)

            # Search for the documentation in the IRPF90_man directory
            for item in tmp:
                item, _, line = item.strip().split('\t')
                doc = extract_doc(item)
                items.append((item, filename, doc, line))

    dirname = os.path.basename(os.getcwd())
    # Write the documentation in the README
    template = "`%(item)s <%(url)s%(dirname)s/%(filename)s#L%(line)s>`_\n%(doc)s\n"

    documentation = Doc_key + header
    url = URL
    for item, filename, doc, line in items:
        documentation += template % locals()
    documentation += '\n\n'

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

    import subprocess

    try:
        # pipe output to /dev/null for silence
        null = open("/dev/null", "w")
        subprocess.Popen("git add README.rst", stdout=null, stderr=null)
        null.close()

    except OSError:
        raise


def main():
    data = fetch_splitted_data()
    data = update_assumptions(data)
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

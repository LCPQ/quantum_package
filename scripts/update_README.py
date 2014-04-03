#!/usr/bin/env python

"""Updates the README.rst file as the include directive is disabled on GitHub."""
__date__     = "Thu Apr  3 23:06:18 CEST 2014"
__author__ = "Anthony Scemama <scemama@irsamc.ups-tlse.fr>"


README="README.rst"
Assum_key="Assumptions\n===========\n"
Needed_key="Needed Modules\n==============\n"
Sentinel="@@$%&@@"
URL="http://github.com/LCPQ/quantum_package/tree/master/src/"


def fetch_splitted_data():
    """Read the README.rst file and split it in 3 strings:
    * The description
    * The assumptions
    * The needed modules
    The result is given as a list of strings
    """

    file = open(README,'r')
    data = file.read()
    file.close()

    # Place sentinels
    data = data.replace(Assum_key,Sentinel+Assum_key)
    data = data.replace(Needed_key,Sentinel+Needed_key)
    
    # Now Split data using the sentinels
    result = data.split(Sentinel)

    return result


def update_assumptions(data):
    """Read the ASSUMPTIONS.rst file, and replace the data with it."""

    try:
        file = open('ASSUMPTIONS.rst','r')
    except IOError:
        file = open('ASSUMPTIONS.rst','w')
        assumptions = ""
    else:
        assumptions = file.read()
    file.close()

    header = """
.. Do not edit this section. It was auto-generated from the
.. ASSUMPTIONS.rst file.

"""
    if assumptions.strip() != "":
        assumptions = Assum_key + header + assumptions + '\n\n'

    has_assumptions = False
    for i in range(len(data)):
        if data[i].startswith(Assum_key):
            has_assumptions = True
            data[i] = assumptions

    if not has_assumptions:
        data.insert(1,assumptions)

    return data


def update_needed(data):
    """Read the NEEDED_MODULES file, and replace the data with it.
    Create the links to the GitHub pages."""

    file = open('NEEDED_MODULES','r')
    modules = file.read()
    file.close()

    header = """
.. Do not edit this section. It was auto-generated from the
.. NEEDED_MODULES file.

"""
    if modules.strip() != "":
        modules = [ '* `%s <%s%s>`_'%(x,URL,x) for x in  modules.split() ]
        modules = "\n".join(modules)
        modules = Needed_key + header + modules + '\n\n'

    has_modules = False
    for i in range(len(data)):
        if data[i].startswith(Needed_key):
            has_modules = True
            data[i] = modules

    if not has_modules:
        data.append(modules)

    return data

import subprocess

def git_add():
    """Executes:
    git add README.rst
    if git is present on the machine."""
    command = "git add "+README

    try:
      subprocess.call(command.split())
    except OSError:
      pass


def main():
    data = fetch_splitted_data()
    data = update_assumptions(data)
    data = update_needed(data)
    output = ''.join(data)
    
    file = open(README,'w')
    file.write(output)
    file.close()

    git_add()


if __name__ == '__main__':
    main()


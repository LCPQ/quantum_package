#!/usr/bin/env python
# -*- coding: utf-8 -*-

import subprocess
import os
import sys
import pprint

from os.path import join

#  __                           _
# /__ |   _  |_   _. |   o ._ _|_ _
# \_| |_ (_) |_) (_| |   | | | | (_)
#

QP_ROOT = os.getcwd()
QP_ROOT_BIN = join(QP_ROOT, "bin")
QP_ROOT_INSTALL = join(QP_ROOT, "install")

d_dependancy = {
    "ocaml": ["m4", "curl", "zlib", "patch", "gcc"],
    "m4": [],
    "curl": [],
    "zlib": ["gcc"],
    "patch": [],
    "ezfio": ["irpf90"],
    "irpf90": ["python"],
    "docopt": ["python"],
    "resultsFile": ["python"],
    "emsl": ["python"],
    "gcc": [],
    "python": [],
    "ninja": ["gcc", "python"]
}

from collections import namedtuple

Info = namedtuple("Info", ["url", "description", "default_path"])

path_github = {"head": "http://github.com/", "tail": "archive/master.tar.gz"}

ocaml = Info(
    url='http://raw.github.com/ocaml/opam/master/shell/opam_installer.sh',
    description=' ocaml',
    default_path=join(QP_ROOT_BIN, "opam"))

m4 = Info(
    url="http://ftp.gnu.org/gnu/m4/m4-latest.tar.gz",
    description=" m4",
    default_path=join(QP_ROOT_BIN, "m4"))

curl = Info(
    url="http://qmcchem.ups-tlse.fr/files/scemama/curl-7.30.0.ermine.tar.bz2",
    description=" curl",
    default_path=join(QP_ROOT_BIN, "curl"))

zlib = Info(
    url='http://zlib.net/zlib-1.2.8.tar.gz',
    description=' zlib',
    default_path=join(QP_ROOT_INSTALL, "zlib"))

path = Info(
    url='ftp://ftp.gnu.org/gnu/patch/patch-2.7.5.tar.gz',
    description=' path',
    default_path=join(QP_ROOT_BIN, "patch"))

irpf90 = Info(
    url='{head}/scemama/irpf90/archive/v1.6.5.tar.gz'.format(**path_github),
    description=' irpf90',
    default_path=join(QP_ROOT_BIN, "irpf90"))

docopt = Info(
    url='{head}/docopt/docopt/{tail}'.format(**path_github),
    description=' docop',
    default_path=join(QP_ROOT_INSTALL, "docopt"))

resultsFile = Info(
    url='{head}/LCPQ/resultsFile/{tail}'.format(**path_github),
    description=' resultsFile',
    default_path=join(QP_ROOT_INSTALL, "resultsFile"))

ninja = Info(
    url='{head}/martine/ninja/{tail}'.format(**path_github),
    description=' nina',
    default_path=join(QP_ROOT_BIN, "ninja"))

emsl = Info(
    url='{head}/LCPQ/EMSL_Basis_Set_Exchange_Local/{tail}'.format(**
                                                                  path_github),
    description=' emsl',
    default_path=join(QP_ROOT_INSTALL, "emsl"))

ezfio = Info(
    url='{head}/LCPQ/EZFIO/{tail}'.format(**path_github),
    description=' EZFIO',
    default_path=join(QP_ROOT_INSTALL, "EZFIO"))

d_info = dict()

for m in ["ocaml", "m4", "curl", "zlib", "path", "irpf90", "docopt",
          "resultsFile", "ninja", "emsl", "ezfio"]:
    exec ("d_info['{0}']={0}".format(m))

l_need = []


#  _
# |_    ._   _ _|_ o  _  ._
# | |_| | | (_  |_ | (_) | |
#
def check_output(*popenargs, **kwargs):
    """Run command with arguments and return its output as a byte string.

    Backported from Python 2.7 as it's implemented as pure python on stdlib.

    >>> check_output(['/usr/bin/python', '--version'])
    Python 2.6.2
    """
    process = subprocess.Popen(stdout=subprocess.PIPE, *popenargs, **kwargs)
    output, unused_err = process.communicate()
    retcode = process.poll()
    if retcode:
        cmd = kwargs.get("args")
        if cmd is None:
            cmd = popenargs[0]
        error = subprocess.CalledProcessError(retcode, cmd)
        error.output = output
        raise error
    return output


def check_python():
    req_version = (2, 6)
    cur_version = sys.version_info

    # Check python
    if cur_version >= req_version:
        l_installed["python"] = ""
    else:
        print "To old version (need >2.5). Abort"
        sys.exit(1)


def check_avabiliy(binary):

    if binary == "zlib":
        binary_name = "zlib-flate"
    else:
        binary_name = binary

    if binary == "python":
        check_python()

    try:
        return check_output(["which", binary_name])
    except subprocess.CalledProcessError:
        default_path = d_info[binary].default_path
        if os.path.exists(default_path):
            return default_path
        else:
            return 0


def splitext(path):
    for ext in ['.tar.gz', '.tar.bz2']:
        if path.endswith(ext):
            return path[:-len(ext)], path[-len(ext):]
    return os.path.splitext(path)


l_installed = dict()

print """
  _
 /  |_   _   _ |  o ._   _
 \_ | | (/_ (_ |< | | | (_|
                         _|
"""

# Check all the other
for i in d_dependancy.keys():
    print "Checking if you have {0} avalaible...".format(i),

    r = check_avabiliy(i)
    if r:
        print "OK"
        l_installed[i] = r.strip()
    else:
        print "We will try to compile if from source in a few moment"
        l_need.append(i)

# Expend the need_stuff for all the genealogy

d_need_genealogy = dict()

for need in l_need:
    d_need_genealogy[need] = None
    for childen in d_dependancy[need]:
        if childen not in l_installed:
            d_need_genealogy[childen] = None

l_need_genealogy = d_need_genealogy.keys()

print """
  __
 (_      ._ _  ._ _   _. ._
 __) |_| | | | | | | (_| | \/
                           /
"""

print "You have already installed :"
pprint.pprint(l_installed)

print """
 ___
  |  ._   _ _|_  _. | |  _. _|_ o  _  ._
 _|_ | | _>  |_ (_| | | (_|  |_ | (_) | |

"""

if l_need_genealogy:
    print "You need to install"
    pprint.pprint(l_need_genealogy)
else:
    print "Nothing to do"
    sys.exit()

if "ninja" in l_need_genealogy:

    print """
# ~#~#~#~#~#~#~#~#~#~#~#~#~ #
# I n s t a l l _ n i n j a #
# ~#~#~#~#~#~#~#~#~#~#~#~#~ #
"""
    url = d_info["ninja"].url
    extension = splitext(url)[1]
    path_archive = "Downloads/{0}{1}".format("ninja", extension)

    l_cmd = ["cd install;",
             "wget {0} -O {1} -o /dev/null ;".format(url, path_archive),
             "./scripts/install_ninja.sh 2>/dev/null;", "cd -"]

    subprocess.check_output(" ".join(l_cmd), shell=True)
    l_need_genealogy.remove("ninja")

print """
# ~#~#~#~#~#~#~#~#~#~#~#~#~#~ #
# C r e a t i n g _ n i n j a #
# ~#~#~#~#~#~#~#~#~#~#~#~#~#~ #
"""


def create_rule():

    l_rules = [
        "rule download", "  command = wget ${url} -O ${out} -o /dev/null",
        "  description = Downloading ${descr}", "", "rule install",
        "  command = ./scripts/install_${target}.sh > _build/${target}.log 2>&1",
        "  description = Installing ${descr}", ""
    ]

    return l_rules

l_string = create_rule()

l_build = []

for need in l_need_genealogy:

    url = d_info[need].url
    extension = splitext(url)[1]

    archive_path = "Downloads/{0}{1}".format(need, extension)

    descr = d_info[need].description

    # Build to dowload
    l_build += ["build {0}: download".format(archive_path),
                "   url = {0}".format(url),
                "   descr = {0}".format(descr), ""]

    # Build to install
    l_dependancy = [d_info[i].default_path for i in d_dependancy[need] if i in l_need_genealogy]

    l_build += ["build {0}: install {1} {2}".format(d_info[need].default_path,
                                                    archive_path,
                                                    " ".join(l_dependancy)),
                "   target = {0}".format(need),
                "   descr = {0}".format(descr), ""]

l_string += l_build

path = join(QP_ROOT_INSTALL, "build.ninja")
with open(path, "w+") as f:
    f.write("\n".join(l_string))

print "Done"
print "You can check at {0}".format(path)

print """
# ~#~#~#~#~#~#~#~#~ #
# R u n _ n i n j a #
# ~#~#~#~#~#~#~#~#~ #
"""

subprocess.check_call("./bin/ninja -C install", shell=True)


print """
# ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ #
# C r e a t e   q u a n t u m _ p a c k a g e . r c
# ~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~ #
"""

python_path = [join(QP_ROOT, "scripts"), join(QP_ROOT, "install")]

l_python = [join(QP_ROOT, "scripts")]
for dir_ in python_path:
    for folder in os.listdir(dir_):
        path = join(dir_, folder)
        if os.path.isdir(path):
            l_python.append(path)


def find_path(bin_):
    try:
        locate = l_installed[bin_]
    except KeyError:
        locate = d_info[bin_].default_path
    return locate


l_rc = [
    'export QP_ROOT={0}'.format(QP_ROOT),
    'export QP_EZFIO={0}'.format(find_path('ezfio')),
    'export IRPF90={0}'.format(find_path("irpf90")),
    'export NINJA={0}'.format(find_path("ninja")),
    'export QP_PYTHON={0}'.format(":".join(l_python)),
    "",
    'export PYTHONPATH="${PYTHONPATH}":"${QP_PYTHON}"',
    'export PATH="${PATH}":"${QP_PYTHON}":"${QP_ROOT}"/bin',
    'export LD_LIBRARY_PATH="${QP_ROOT}"/lib:"${LD_LIBRARY_PATH}"',
    'export LIBRARY_PATH="${QP_ROOT}"/lib:"${LIBRARY_PATH}"', ""
    'source . ${HOME}/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true',
    ""
]

path = join(QP_ROOT, "quantum_package.rc")
with open(path, "w+") as f:
    f.write("\n".join(l_rc))

print "Done"
print "You can check at {0}".format(path)
print "Don't forget to source it"

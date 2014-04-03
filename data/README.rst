====
Data
====


This directory contains all the data files needed for the Quantum Package.
These include:

* Input data for the test suites 
* Atomic basis sets

*Important*: EZFIO files are large and should not be tracked by git.

To avoid tracking large binary files with git, only the MD5 digests of the files
present in the directory are tracked. Input EZFIO files should be archived using the
``archive_ezfio.sh`` script. The name of the archive will be the md5 digest.

The corresponding files are loaded by running::

  $ make pull

And the new files are uploaded with::

  $ make push

All previously downloaded files are stored in the ``cache`` directory.



#!/bin/bash
#
# Creates the .gitignore file in the Modules to ignore the symlinks
#
# Tue Jan 13 14:18:05 CET 2015
#

cat << EOF > .gitignore
#
# Do not modify this file. Add your ignored files to the gitignore
# (without the dot at the beginning) file.
#
IRPF90_temp
IRPF90_man
irpf90.make
tags
Makefile.depend
irpf90_entities
.gitignore
EOF

if [[ -f gitignore ]]
then
  cat gitignore >> .gitignore
fi

find . -type l | sed "s@./@@" >> .gitignore


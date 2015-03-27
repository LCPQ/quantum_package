#!/usr/bin/env python

import sys


class EZFIO_ocaml(object):

    def __init__(self):
        pass

    def create_read(self, **param):
        '''
        Take an imput a list of keyword argument
        ezfio_dir  = str
        ezfio_name = str
        type       = Named_tuple(fancy_type, ocaml_type, fortrant_type)

        Return the read template
        '''

        l_name_need = "ezfio_dir ezfio_name type".split()

        for key in l_name_need:
            if key not in param:
                print "You need to provide {0} at keyword argument".format(key)
                sys.exit(1)

        # ~#~#~#~#~#~#~#~#~#~#~#~#~#~# #
        # C r e a t e _ t e m pl a t e #
        # ~#~#~#~#~#~#~#~#~#~#~#~#~#~# #

        l_template = ['(* Read snippet for {ezfio_name} *)',
                      'let read_{ezfio_name} () =',
                      '  if not (Ezfio.has_{ezfio_dir}_{ezfio_name} ()) then',
                      '     get_default "{ezfio_name}"',
                      '     |> {Ocaml_type}.of_string',
                      '     |> Ezfio.set_{ezfio_dir}_{ezfio_name}',
                      '  ;',
                      '  Ezfio.get_{ezfio_dir}_{ezfio_name} ()']

        if param["type"].fancy:
                l_template += ["    |> {fancy_type}.of_{Ocaml_type}"]

        l_template += [";;;"]

        template = "\n  ".join(l_template)

        # ~#~#~#~#~#~ #
        # R e n d e r #
        # ~#~#~#~#~#~ #

        param["Ocaml_type"] = param["type"].ocaml.capitalize()
        param["fancy_type"] = param["type"].fancy
        template_rendered = template.format(**param)

        # ~#~#~#~#~#~ #
        # R e t u r n #
        # ~#~#~#~#~#~ #
        return template_rendered

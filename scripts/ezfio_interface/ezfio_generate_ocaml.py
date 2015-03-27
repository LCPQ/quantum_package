#!/usr/bin/env python

import sys


class EZFIO_ocaml(object):

    def __init__(self, **kwargs):

        for k, v in kwargs.iteritems():
            if k == "type":
                self.type = kwargs["type"]
            else:
                exec "self.{0} = '{1}'".format(k, v)

    @property
    def Ocaml_type(self):
        return self.type.ocaml.capitalize()

    @property
    def fancy_type(self):
        return self.type.fancy

    def create_read(self):
        '''
        Take an imput a list of keyword argument
        ezfio_dir  = str
        ezfio_name = str
        type       = Named_tuple(fancy_type, ocaml_type, fortrant_type)

        Return the read template
        '''

        for i in ["ezfio_dir", "ezfio_name", "type"]:
            try:
                "exec self.{0}".format(i)
            except NameError:
                msg = "You need to provide a '{0}' for creating read function"
                raise KeyError(msg.format(i))

        # ~#~#~#~#~#~#~#~#~#~#~#~#~#~# #
        # C r e a t e _ t e m pl a t e #
        # ~#~#~#~#~#~#~#~#~#~#~#~#~#~# #

        l_template = ['(* Read snippet for {self.ezfio_name} *)',
                      'let read_{self.ezfio_name} () =',
                      '  if not (Ezfio.has_{self.ezfio_dir}_{self.ezfio_name} ()) then',
                      '     get_default "{self.ezfio_name}"',
                      '     |> {self.Ocaml_type}.of_string',
                      '     |> Ezfio.set_{self.ezfio_dir}_{self.ezfio_name}',
                      '  ;',
                      '  Ezfio.get_{self.ezfio_dir}_{self.ezfio_name} ()']

        if self.fancy_type:
                l_template += ["    |> {self.fancy_type}.of_{self.Ocaml_type}"]

        l_template += [";;;"]

        template = "\n  ".join(l_template)

        # ~#~#~#~#~#~ #
        # R e n d e r #
        # ~#~#~#~#~#~ #

        template_rendered = template.format(**locals())

        # ~#~#~#~#~#~ #
        # R e t u r n #
        # ~#~#~#~#~#~ #
        return template_rendered

    def create_write(self):
        pass

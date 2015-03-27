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
    def ocaml_type(self):
        return self.type.ocaml

    @property
    def fancy_type(self):
        return self.type.fancy

    def create_read(self):
        '''
        You need to instantiate the EZFIO_ocaml with this keyword argument
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

        l_template = [
            '(* Read snippet for {self.ezfio_name} *)',
            'let read_{self.ezfio_name} () =',
            '  if not (Ezfio.has_{self.ezfio_dir}_{self.ezfio_name} ()) then',
            '     get_default "{self.ezfio_name}"',
            '     |> {self.Ocaml_type}.of_string',
            '     |> Ezfio.set_{self.ezfio_dir}_{self.ezfio_name}',
            '  ;',
            '  Ezfio.get_{self.ezfio_dir}_{self.ezfio_name} ()']

        if self.fancy_type:
            l_template += ["    |> {self.fancy_type}.of_{self.ocaml_type}"]

        l_template += [";;"]

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
        '''
        You need to instantiate the EZFIO_ocaml with this keyword argument
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

        l_template = ['(* Write snippet for {self.ezfio_name} *)']

        if self.fancy_type:
            l_template += ['let write_{self.ezfio_name} var = ',
                           '  {self.fancy_type}.to_{self.ocaml_type} var',
                           '  |> Ezfio.set_{self.ezfio_dir}_{self.ezfio_name}']
        else:
            l_template += ['let write_{self.ezfio_name} =',
                           '   Ezfio.set_{self.ezfio_dir}_{self.ezfio_name}']

        l_template += [';;']

        template = "\n  ".join(l_template)

        # ~#~#~#~#~#~ #
        # R e n d e r #
        # ~#~#~#~#~#~ #

        template_rendered = template.format(**locals())

        # ~#~#~#~#~#~ #
        # R e t u r n #
        # ~#~#~#~#~#~ #
        return template_rendered

    @staticmethod
    def create_type(l_provider, l_type):

        l_template = ["(* Generate type *)",
                      "type t = ",
                      "  {"]

        for p, t in zip(l_provider, l_type):

            if t.fancy:
                l_template += ["    {0:<30} : {1}.t;".format(p, t.fancy)]
            else:
                l_template += ["    {0:<30} : {1};".format(p, t.ocaml)]
        l_template += ["  } with sexp",
                       ";;"]

        # ~#~#~#~#~#~ #
        # R e t u r n #
        # ~#~#~#~#~#~ #
        return "\n   ".join(l_template)

    @staticmethod
    def create_read_global(l_provider):

        l_template = ["(* Read all *)",
                      "let read() = ",
                      "  Some",
                      "  {"]
        l_template += ["    {0:<30} = read_{0} ();".format(p) for p in l_provider]

        l_template += ["  }",
                       ";;"]

        # ~#~#~#~#~#~ #
        # R e t u r n #
        # ~#~#~#~#~#~ #
        return "\n   ".join(l_template)

    @staticmethod
    def create_write_global(l_provider):

        l_template =  ["(* Write all *)",
                       "let write{ "]
        l_template += ["           {0};".format(p) for p in l_provider]
        l_template += ["         } ="]
        l_template += ["  write_{0:<30} {0};".format(p) for p in l_provider]
        l_template += [";;"]

        # ~#~#~#~#~#~ #
        # R e t u r n #
        # ~#~#~#~#~#~ #
        return "\n   ".join(l_template)

    @staticmethod
    def create_to_string(l_provider, l_type):

        l_template = ['(* to_string*)',
                      'let to_string b =',
                      '  Printf.sprintf "']

        l_template += ["{0} = %s".format(p) for p in l_provider] 
        l_template += ['"']

        for p, t in zip(l_provider, l_type):

            if t.fancy:
                str_ = t.fancy
            else:
                str_ = t.ocaml.capitalize()

            l_template += ["    ({0}.to_string b.{1})".format(str_, p)]

        l_template += [";;"]

        # ~#~#~#~#~#~ #
        # R e t u r n #
        # ~#~#~#~#~#~ #
        return "\n   ".join(l_template)

    @staticmethod
    def create_to_rst(l_provider, l_type, l_doc):

        l_template = ['(* to_rst*)',
                      'let to_rst b =',
                      '  Printf.sprintf "']

        for p, d in zip(l_provider, l_doc):

            l_template += ["{0} ::".format(d),
                           "",
                           "  {0} = %s".format(p),
                           ""]
        l_template += ['"']

        for p, t in zip(l_provider, l_type):

            if t.fancy:
                str_ = t.fancy
            else:
                str_ = t.ocaml.capitalize()

            l_template += ["    ({0}.to_string b.{1})".format(str_, p)]

        l_template += ["|> Rst_string.of_string",
                       ";;"]

        # ~#~#~#~#~#~ #
        # R e t u r n #
        # ~#~#~#~#~#~ #
        return "\n   ".join(l_template)

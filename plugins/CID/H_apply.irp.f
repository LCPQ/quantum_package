! Generates subroutine H_apply_cid
! ----------------------------------

BEGIN_SHELL [ /usr/bin/env python2 ]
from generate_h_apply import H_apply
H = H_apply("cid",do_double_exc=True,do_mono_exc=False)
print H
END_SHELL


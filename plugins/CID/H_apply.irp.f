! Generates subroutine H_apply_cisd
! ----------------------------------

BEGIN_SHELL [ /usr/bin/env python ]
from generate_h_apply import H_apply
H = H_apply("cisd",do_double_exc=True,do_mono_exc=False)
print H
END_SHELL


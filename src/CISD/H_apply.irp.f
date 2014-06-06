! Generates subroutine H_apply_cisd
! ----------------------------------

BEGIN_SHELL [ /usr/bin/env python ]
from generate_h_apply import H_apply
H = H_apply("cisd")
print H
END_SHELL


BEGIN_PROVIDER [logical, do_double_excitations]
 implicit none
 BEGIN_DOC
 ! if True then the double excitations are performed in the calculation
 ! always true in the CISD
 END_DOC
 do_double_excitations = .True.

END_PROVIDER

BEGIN_PROVIDER [logical, do_mono_excitations]
 implicit none
 BEGIN_DOC
 ! if True then the mono excitations are performed in the calculation
 ! always true in the CISD
 END_DOC
 do_mono_excitations = .True.

END_PROVIDER

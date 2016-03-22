use libint_module
BEGIN_PROVIDER [ logical, libint_initialized ]
 implicit none
 BEGIN_DOC
 ! if true, libint is initialized
 END_DOC
 call init_libint(trim(ezfio_filename)//char((0)))
END_PROVIDER

BEGIN_PROVIDER [ integer, shell_num ]
 implicit none
 BEGIN_DOC
 ! Number of shells
 END_DOC
 PROVIDE libint_initialized
 shell_num = get_nb_shell()

END_PROVIDER


BEGIN_PROVIDER [ integer, shell_idx, (2,shell_num) ]
 implicit none
 BEGIN_DOC
 ! Contains the 1st and last AO index in each shell
 END_DOC
 PROVIDE libint_initialized
 call map_shell_to_basis_function_interval(2*shell_num,shell_idx)
END_PROVIDER



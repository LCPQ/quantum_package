use libint_module
BEGIN_PROVIDER [ logical, has_libint ]
  implicit none
  BEGIN_DOC
  ! If true, use libint
  END_DOC
  character                      :: y
  call getenv('QP_LIBINT', y)
  if (y=='1') then
    has_libint = .True.
    call init_libint(trim(ezfio_filename)//char((0)))
  else
    PROVIDE ezfio_filename
    has_libint = .False.
  endif
    
END_PROVIDER

BEGIN_PROVIDER [ integer, shell_num ]
  implicit none
  BEGIN_DOC
  ! Number of shells
  END_DOC
  if (has_libint) then
    shell_num = get_nb_shell()
  else
    stop 'shell_num not implemented without libint'
  endif
  
END_PROVIDER


BEGIN_PROVIDER [ integer, shell_idx, (2,shell_num) ]
  implicit none
  BEGIN_DOC
  ! Contains the 1st and last AO index in each shell
  END_DOC
  if (has_libint) then
    call map_shell_to_basis_function_interval(2*shell_num,shell_idx)
  else
    stop 'shell_idx not implemented without libint'
  endif
END_PROVIDER



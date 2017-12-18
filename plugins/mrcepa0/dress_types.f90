module dress_types
  type dress_buffer
    double precision, pointer :: buf(:,:,:), buf0(:,:), coef(:)
    double precision :: N
    integer, pointer :: det_to_buf(:), buf_to_det(:)
    integer :: free_under, pos, N_slot
    logical :: full
  endtype
end module


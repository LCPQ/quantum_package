module libint_module
  use iso_c_binding

  implicit none
  interface
    subroutine init_libint(str) bind(c,name='init_libint')
          import :: c_char
          character(len=1,kind=C_char), dimension(*), intent(in) :: str
    end subroutine

    integer(c_int) function get_nb_shell() bind(c,name='nb_shell')
          import :: c_int
    end function

    subroutine finalize_libint() bind(c,name='finalize_libint')
    end subroutine

    subroutine map_shell_to_basis_function_interval(sze, out_val) bind(c,name='map_shell_to_basis_function_interval')
                import :: c_ptr
                import :: c_int

                integer(c_int), INTENT(IN), value :: sze
                integer(c_int), INTENT(OUT) :: out_val(sze)
    end subroutine

    real(c_double) function ao_bielec_integral_libint(i,j,k,l) bind(c,name='ao_bielec_integral')
                import :: c_int
                import :: c_double

                integer(c_int), value :: i
                integer(c_int), value :: j
                integer(c_int), value :: k
                integer(c_int), value :: l
    end function

    subroutine compute_ao_bielec_integrals_shell(i,j,k,l,sze,values) bind(c,name='compute_ao_bielec_integrals_shell')
                import :: c_ptr
                import :: c_int
                import :: c_double

                integer(c_int), value :: i
                integer(c_int), value :: j
                integer(c_int), value :: k
                integer(c_int), value :: l
                integer(c_int), INTENT(IN), value :: sze
                real(c_double), INTENT(OUT) :: values(sze)
     end subroutine


  end interface
end module libint_module

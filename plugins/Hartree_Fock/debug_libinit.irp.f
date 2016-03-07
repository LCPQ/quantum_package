 program debug_libint
   use libint_module

   implicit none
   double precision :: ao_bielec_integral

   integer, allocatable :: s2bf(:)
   double precision, allocatable :: buffer_int(:)

   call init_libint(trim(ezfio_filename)//char(0))

   integer :: nb_shell_f
   nb_shell_f = get_nb_shell()

   allocate(s2bf(2*nb_shell_f))
   call map_shell_to_basis_function_interval(2*nb_shell_f,s2bf)

   integer :: s1, s2,s3,s4
   integer :: bf1,bf2,bf3,bf4
   integer :: bf1_begin,bf2_begin,bf3_begin,bf4_begin
   integer :: bf1_end,bf2_end,bf3_end,bf4_end
   integer :: n1,n2,n3,n4
   integer :: f1,f2,f3,f4,f1234

   ! =================== !
   ! Loop over the shell !
   ! =================== !

   do s1 = 1, nb_shell_f

      print*, s1, "/", nb_shell_f

      bf1_begin = s2bf(2*s1-1)
      bf1_end = s2bf(2*s1)
      n1 = 1 + bf1_end - bf1_begin
   
      do s2 = 1, nb_shell_f
   
         bf2_begin = s2bf(2*s2-1)
         bf2_end = s2bf(2*s2)
         n2 = 1 + bf2_end - bf2_begin
   
         do s3 = 1, nb_shell_f
     
            bf3_begin = s2bf(2*s3-1)
            bf3_end = s2bf(2*s3)
            n3 = 1 + bf3_end - bf3_begin
   
            do s4 = 1, nb_shell_f
   
               bf4_begin = s2bf(2*s4-1)
               bf4_end = s2bf(2*s4)     
               n4 = 1 + bf4_end - bf4_begin
   
               ! ========================== !
               ! Compute the shell integral !
               ! ========================== !
               integer :: sze
               sze = n1*n2*n3*n4
               allocate(buffer_int(sze))
               call compute_ao_bielec_integrals_shell(s1,s2,s3,s4,sze,buffer_int)
   
               ! ============================ !
               ! Loop over the basis function !
               ! ============================ !
   
               do bf1 = bf1_begin, bf1_end
                  do bf2 = bf2_begin, bf2_end
                     do bf3 = bf3_begin, bf3_end
                        do bf4 = bf4_begin, bf4_end
   
                           f1 = bf1 - bf1_begin
                           f2 = bf2 - bf2_begin
                           f3 = bf3 - bf3_begin
                           f4 = bf4 - bf4_begin
                           
                           !Get the integral from the buffer
                           f1234 = f1*n2*n3*n4+f2*n3*n4+f3*n4+f4 + 1;
                           
                           !Compute the norm
                           double precision:: coef1, coef2, coef3, coef4, norm

                           coef1 = ao_coef_normalization_libint_factor(bf1)
                           coef2 = ao_coef_normalization_libint_factor(bf2) 
                           coef3 = ao_coef_normalization_libint_factor(bf3) 
                           coef4 = ao_coef_normalization_libint_factor(bf4)

                           norm = coef1*coef2*coef3*coef4
   
                           double precision:: libint, ref
   
                           !Value of itegral bf1,bf2,bf3, bf4
                           libint = buffer_int(f1234) * norm

                           !Verify with the manu's one
!                           ref = ao_bielec_integral(bf1,bf2,bf3,bf4)
!   
!                           if ( (ABS(ABS(ref) - ABS(libint)).GE.1e-6) )  THEN
!
!                              print*, bf1,bf2,bf3,bf4
!                              print*,"r", ref
!                              print*,"l", libint
!                              print*,"r/l", ref/libint
!                              print*,"l/r", libint/ref
!                              print*,"n", norm
!
!                              call exit(1)
!                           end if

                         enddo
                     enddo
                  enddo
               enddo
           
               !Deallocate the buffer_intergral for the shell
               deallocate(buffer_int)

            enddo
         enddo
      enddo
   enddo
   
   call finalize_libint()
 end debug_libint

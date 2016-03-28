 program debug_libint

   implicit none
   double precision :: ao_bielec_integral

   double precision, allocatable :: buffer_int(:)

   integer :: s1, s2,s3,s4
   integer :: bf1,bf2,bf3,bf4
   integer :: bf1_begin,bf2_begin,bf3_begin,bf4_begin
   integer :: bf1_end,bf2_end,bf3_end,bf4_end
   integer :: n1,n2,n3,n4
   integer :: f1,f2,f3,f4,f1234

   PROVIDE has_libint
   ! =================== !
   ! Loop over the shell !
   ! =================== !

   do s1 = 1, shell_num

      print*, s1, "/", shell_num

      bf1_begin = shell_idx(1,s1)
      bf1_end = shell_idx(2,s1)
      n1 = 1 + bf1_end - bf1_begin
   
      do s2 = 1, shell_num
   
         bf2_begin = shell_idx(1,s2)
         bf2_end = shell_idx(2,s2)
         n2 = 1 + bf2_end - bf2_begin
   
         do s3 = 1, shell_num
     
            bf3_begin = shell_idx(1,s3)
            bf3_end = shell_idx(2,s3)
            n3 = 1 + bf3_end - bf3_begin
   
            do s4 = 1, shell_num
   
               bf4_begin = shell_idx(1,s4)
               bf4_end = shell_idx(2,s4)     
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
                           ref = ao_bielec_integral(bf1,bf2,bf3,bf4)
   

                              print*, bf1,bf2,bf3,bf4
                              print*,"r", ref
                              print*,"l", libint
                              print*,"r/l", ref/libint
                              print*,"l/r", libint/ref
                              print*,"n", norm

                           if ( (ABS(ABS(ref) - ABS(libint)) >= 1.e-6) )  THEN
                              call exit(1)
                           end if

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
 end 

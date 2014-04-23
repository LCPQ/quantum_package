 BEGIN_PROVIDER [ double precision, Fock_matrix_mo, (mo_tot_num,mo_tot_num) ]
&BEGIN_PROVIDER [ double precision, Fock_matrix_diag_mo, (mo_tot_num)]
   implicit none
   BEGIN_DOC
   ! Fock matrix on the MO basis.
   ! For open shells, the ROHF Fock Matrix is
   !
   !  |   F-K    |  F + K/2  |    F     |
   !  |---------------------------------|
   !  | F + K/2  |     F     |  F - K/2 |
   !  |---------------------------------|
   !  |    F     |  F - K/2  |  F + K   |
   !
   ! F = 1/2 (Fa + Fb)
   !
   ! K = Fb - Fa
   !
   END_DOC
   integer                        :: i,j,n
   double precision               :: get_mo_bielec_integral
   if (elec_alpha_num == elec_beta_num) then
     Fock_matrix_mo = Fock_matrix_alpha_mo
   else
     
     do j=1,elec_beta_num
       ! F-K
       do i=1,elec_beta_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_alpha_mo(i,j)+Fock_matrix_beta_mo(i,j))&
             - (Fock_matrix_beta_mo(i,j) - Fock_matrix_alpha_mo(i,j))
       enddo
       ! F+K/2
       do i=elec_beta_num+1,elec_alpha_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_alpha_mo(i,j)+Fock_matrix_beta_mo(i,j))&
             + 0.5d0*(Fock_matrix_beta_mo(i,j) - Fock_matrix_alpha_mo(i,j))
       enddo
       ! F
       do i=elec_alpha_num+1, mo_tot_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_alpha_mo(i,j)+Fock_matrix_beta_mo(i,j))
       enddo
     enddo

     do j=elec_beta_num+1,elec_alpha_num
       ! F+K/2
       do i=1,elec_beta_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_alpha_mo(i,j)+Fock_matrix_beta_mo(i,j))&
             + 0.5d0*(Fock_matrix_beta_mo(i,j) - Fock_matrix_alpha_mo(i,j))
       enddo
       ! F
       do i=elec_beta_num+1,elec_alpha_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_alpha_mo(i,j)+Fock_matrix_beta_mo(i,j))
       enddo
       ! F-K/2
       do i=elec_alpha_num+1, mo_tot_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_alpha_mo(i,j)+Fock_matrix_beta_mo(i,j))&
             - 0.5d0*(Fock_matrix_beta_mo(i,j) - Fock_matrix_alpha_mo(i,j))
       enddo
     enddo

     do j=elec_alpha_num+1, mo_tot_num
       ! F
       do i=1,elec_beta_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_alpha_mo(i,j)+Fock_matrix_beta_mo(i,j))
       enddo
       ! F-K/2
       do i=elec_beta_num+1,elec_alpha_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_alpha_mo(i,j)+Fock_matrix_beta_mo(i,j))&
             - 0.5d0*(Fock_matrix_beta_mo(i,j) - Fock_matrix_alpha_mo(i,j))
       enddo
       ! F+K
       do i=elec_alpha_num+1,mo_tot_num
         Fock_matrix_mo(i,j) = 0.5d0*(Fock_matrix_alpha_mo(i,j)+Fock_matrix_beta_mo(i,j)) &
             + (Fock_matrix_beta_mo(i,j) - Fock_matrix_alpha_mo(i,j))
       enddo
     enddo
     
   endif
   do i = 1, mo_tot_num
     Fock_matrix_diag_mo(i) = Fock_matrix_mo(i,i)
   enddo
END_PROVIDER
 
 
BEGIN_PROVIDER [ double precision, Fock_matrix_alpha_mo, (mo_tot_num,mo_tot_num) ]
   implicit none
   BEGIN_DOC
   ! Fock matrix on the MO basis
   END_DOC
   integer                        :: i,j,n
   double precision               :: get_mo_bielec_integral
   do j=1,mo_tot_num
     do i=1,mo_tot_num
       Fock_matrix_alpha_mo(i,j) = mo_mono_elec_integral(i,j)
       do n=1,elec_beta_num
         Fock_matrix_alpha_mo(i,j) += 2.d0*get_mo_bielec_integral(i,n,j,n,mo_integrals_map) -&
             get_mo_bielec_integral(i,n,n,j,mo_integrals_map)
       enddo
       do n=elec_beta_num+1,elec_alpha_num
         Fock_matrix_alpha_mo(i,j) += get_mo_bielec_integral(i,n,j,n,mo_integrals_map) -&
             get_mo_bielec_integral(i,n,n,j,mo_integrals_map)
       enddo
     enddo
   enddo
END_PROVIDER
 
 
BEGIN_PROVIDER [ double precision, Fock_matrix_beta_mo, (mo_tot_num,mo_tot_num) ]
   implicit none
   BEGIN_DOC
   ! Fock matrix on the MO basis
   END_DOC
   integer                        :: i,j,n
   double precision               :: get_mo_bielec_integral
   do j=1,mo_tot_num
     do i=1,mo_tot_num
       Fock_matrix_beta_mo(i,j) = mo_mono_elec_integral(i,j)
       do n=1,elec_beta_num
         Fock_matrix_beta_mo(i,j) += 2.d0*get_mo_bielec_integral(i,n,j,n,mo_integrals_map) -&
             get_mo_bielec_integral(i,n,n,j,mo_integrals_map)
       enddo
       do n=elec_beta_num+1,elec_alpha_num
         Fock_matrix_beta_mo(i,j) += get_mo_bielec_integral(i,n,j,n,mo_integrals_map)
       enddo
     enddo
   enddo
END_PROVIDER
 
 

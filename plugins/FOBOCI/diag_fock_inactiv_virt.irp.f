subroutine diag_inactive_virt_and_update_mos
 implicit none
 integer :: i,j,i_inact,j_inact,i_virt,j_virt
 double precision :: tmp(mo_tot_num_align,mo_tot_num)
 character*(64) :: label
 print*,'Diagonalizing the occ and virt Fock operator'
 tmp = 0.d0
 do i = 1, mo_tot_num
  tmp(i,i) = Fock_matrix_mo(i,i)
 enddo
 
 do i = 1, n_inact_orb
  i_inact = list_inact(i)
  do j = i+1, n_inact_orb 
   j_inact = list_inact(j)
   tmp(i_inact,j_inact) = Fock_matrix_mo(i_inact,j_inact)
   tmp(j_inact,i_inact) = Fock_matrix_mo(j_inact,i_inact)
  enddo
 enddo

 do i = 1, n_virt_orb
  i_virt = list_virt(i)
  do j = i+1, n_virt_orb 
   j_virt = list_virt(j)
   tmp(i_virt,j_virt) = Fock_matrix_mo(i_virt,j_virt) 
   tmp(j_virt,i_virt) = Fock_matrix_mo(j_virt,i_virt) 
  enddo
 enddo


 label = "Canonical"
 call mo_as_eigvectors_of_mo_matrix(tmp,size(tmp,1),size(tmp,2),label,1,.false.)
 soft_touch mo_coef


end

subroutine diag_inactive_virt_new_and_update_mos
 implicit none
 integer :: i,j,i_inact,j_inact,i_virt,j_virt,k,k_act
 double precision :: tmp(mo_tot_num_align,mo_tot_num),accu,get_mo_bielec_integral
 character*(64) :: label
 tmp = 0.d0
 do i = 1, mo_tot_num
  tmp(i,i) = Fock_matrix_mo(i,i)
 enddo
 
 do i = 1, n_inact_orb
  i_inact = list_inact(i)
  do j = i+1, n_inact_orb 
   j_inact = list_inact(j)
   accu =0.d0
   do k = 1, n_act_orb
    k_act = list_act(k)
    accu += get_mo_bielec_integral(i_inact,k_act,j_inact,k_act,mo_integrals_map)
    accu -= get_mo_bielec_integral(i_inact,k_act,k_act,j_inact,mo_integrals_map)
   enddo
   tmp(i_inact,j_inact) = Fock_matrix_mo(i_inact,j_inact) + accu
   tmp(j_inact,i_inact) = Fock_matrix_mo(j_inact,i_inact) + accu
  enddo
 enddo

 do i = 1, n_virt_orb
  i_virt = list_virt(i)
  do j = i+1, n_virt_orb 
   j_virt = list_virt(j)
   accu =0.d0
   do k = 1, n_act_orb
    k_act = list_act(k)
    accu += get_mo_bielec_integral(i_virt,k_act,j_virt,k_act,mo_integrals_map)
   enddo
   tmp(i_virt,j_virt) = Fock_matrix_mo(i_virt,j_virt) - accu
   tmp(j_virt,i_virt) = Fock_matrix_mo(j_virt,i_virt) - accu
  enddo
 enddo


 label = "Canonical"
 call mo_as_eigvectors_of_mo_matrix(tmp,size(tmp,1),size(tmp,2),label,1,.false.)
 soft_touch mo_coef


end

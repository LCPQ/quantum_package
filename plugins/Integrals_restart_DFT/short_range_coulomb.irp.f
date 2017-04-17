BEGIN_PROVIDER [double precision, density_matrix_read, (mo_tot_num, mo_tot_num)]
 implicit none
 integer :: i,j,k,l
 logical                        :: exists
 call ezfio_has_determinants_density_matrix_mo_disk(exists)
 if(exists)then
  print*, 'reading the density matrix from input'
  call ezfio_get_determinants_density_matrix_mo_disk(exists)
  print*, 'reading done'
 else 
  print*, 'no density matrix found in EZFIO file ...'
  print*, 'stopping ..'
  stop
 endif

END_PROVIDER


BEGIN_PROVIDER [double precision, effective_short_range_operator, (mo_tot_num,mo_tot_num)]
 implicit none
 integer :: i,j,k,l,m,n
 double precision :: get_mo_bielec_integral,get_mo_bielec_integral_erf
 double precision :: integral, integral_erf
 effective_short_range_operator = 0.d0
 do i = 1, mo_tot_num
  do j = 1, mo_tot_num
   if(dabs(one_body_dm_mo(i,j)).le.1.d-10)cycle
   do k = 1, mo_tot_num
    do l = 1, mo_tot_num
     integral = get_mo_bielec_integral(i,k,j,l,mo_integrals_map)
!    integral_erf = get_mo_bielec_integral_erf(i,k,j,l,mo_integrals_erf_map)
     effective_short_range_operator(l,k) += one_body_dm_mo(i,j) * integral
    enddo
   enddo
  enddo
 enddo
END_PROVIDER


BEGIN_PROVIDER [double precision, effective_one_e_potential, (mo_tot_num_align, mo_tot_num,N_states)]
 implicit none
 integer :: i,j,i_state
 effective_one_e_potential = 0.d0
 do i_state = 1, N_states
  do i = 1, mo_tot_num
   do j = 1, mo_tot_num
    effective_one_e_potential(i,j,i_state) = effective_short_range_operator(i,j) + mo_nucl_elec_integral(i,j) + mo_kinetic_integral(i,j) & 
                                   + 0.5d0 * (lda_ex_potential_alpha_ao(i,j,i_state) + lda_ex_potential_beta_ao(i,j,i_state))
   enddo
  enddo
 enddo

END_PROVIDER 

subroutine save_one_e_effective_potential  
 implicit none
 double precision, allocatable :: tmp(:,:)
 allocate(tmp(size(effective_one_e_potential,1),size(effective_one_e_potential,2)))
 integer :: i,j
 do i = 1, mo_tot_num
  do j = 1, mo_tot_num
   tmp(i,j) = effective_one_e_potential(i,j,1)
  enddo
 enddo
 call write_one_e_integrals('mo_one_integral', tmp,      &
      size(tmp,1), size(tmp,2))
 call ezfio_set_integrals_monoelec_disk_access_only_mo_one_integrals("Read")
 deallocate(tmp)

end

subroutine save_erf_bi_elec_integrals
 implicit none
 integer :: i,j,k,l
 PROVIDE mo_bielec_integrals_erf_in_map
 call ezfio_set_work_empty(.False.)
 call map_save_to_disk(trim(ezfio_filename)//'/work/mo_ints',mo_integrals_erf_map)
 call ezfio_set_integrals_bielec_disk_access_mo_integrals("Read")
end

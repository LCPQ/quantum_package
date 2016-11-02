program test_sc2
 implicit none
 read_wf = .True.
 touch read_wf
 call routine


end

subroutine routine
  implicit none
  double precision, allocatable :: energies(:),diag_H_elements(:)
  double precision, allocatable :: H_matrix(:,:)
  allocate(energies(N_states),diag_H_elements(N_det))
  call diagonalize_CI
  call test_hcc
  call test_mulliken
  call SC2_1h1p(psi_det,psi_coef,energies, &
        diag_H_elements,size(psi_coef,1),N_det,N_states_diag,N_int,threshold_convergence_SC2)
! allocate(H_matrix(N_det,N_det))
! call SC2_1h1p_full(psi_det,psi_coef,energies, &
!       H_matrix,size(psi_coef,1),N_det,N_states_diag,N_int,threshold_convergence_SC2)
! deallocate(H_matrix)
  integer :: i,j
  double precision :: accu,coef_hf
! coef_hf = 1.d0/psi_coef(1,1)
! do i = 1, N_det
!  psi_coef(i,1) *= coef_hf
! enddo
  touch psi_coef
  call pouet
end

subroutine pouet
  implicit none
  double precision :: accu,coef_hf
  provide one_body_dm_mo_alpha one_body_dm_mo_beta
! call density_matrix_1h1p(psi_det,psi_coef,one_body_dm_mo_alpha,one_body_dm_mo_beta,accu,size(psi_coef,1),N_det,N_states_diag,N_int)
! touch one_body_dm_mo_alpha one_body_dm_mo_beta 
  call test_hcc
  call test_mulliken
  call save_wavefunction

end

subroutine test_hcc
 implicit none
 double precision :: accu
 integer :: i,j
 print*,'Z               AU           GAUSS              MHZ             cm^-1'
 do i = 1, nucl_num
  write(*,'(I2,X,F3.1,X,4(F16.6,X))')i,nucl_charge(i),spin_density_at_nucleous(i),iso_hcc_gauss(i),iso_hcc_mhz(i),iso_hcc_cm_1(i)
 enddo

end

subroutine test_mulliken
 double precision :: accu
 integer :: i
 integer :: j
 accu= 0.d0
 do i = 1, nucl_num
  print*,i,nucl_charge(i),mulliken_spin_densities(i)
  accu += mulliken_spin_densities(i)
 enddo
 print*,'Sum of Mulliken SD = ',accu
!print*,'AO SPIN POPULATIONS'
 accu = 0.d0
!do i = 1, ao_num
! accu += spin_gross_orbital_product(i)
! write(*,'(X,I3,X,A4,X,I2,X,A4,X,F10.7)')i,trim(element_name(int(nucl_charge(ao_nucl(i))))),ao_nucl(i),trim(l_to_charater(ao_l(i))),spin_gross_orbital_product(i)
!enddo
!print*,'sum = ',accu
!accu = 0.d0
!print*,'Angular momentum analysis'
!do i = 0,  ao_l_max
! accu += spin_population_angular_momentum(i)
! print*,' ',trim(l_to_charater(i)),spin_population_angular_momentum(i)
!print*,'sum = ',accu
!enddo

end


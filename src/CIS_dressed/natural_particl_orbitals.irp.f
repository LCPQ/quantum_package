 BEGIN_PROVIDER [double precision, particle_natural_orb_CIS_properties, (6,n_state_cis)]
&BEGIN_PROVIDER [double precision, CIS_states_properties, (6,n_state_cis)]
 implicit none
 BEGIN_DOC
! properties of the natural orbital of the particle of the various n_state_cis eigenvectors of the CIS matrix
!
! You first build the density matrix of the one eigenvector and you take off the Hartree Fock density matrix
!
! particl(i,j)(state = k) ==   dm(i,j)(Hartree Fock)  - dm(i,j)(state = k) 
!
! you diagonalize particl(i,j) and the first eigenvector is the natural orbital corresponding to the particl 
!
! that is specific to the excitation in the CIS state
!
! particle_natural_orb_CIS_properties(i,1) = <phi_i|x|phi_i>
!
! particle_natural_orb_CIS_properties(i,2) = <phi_i|y|phi_i>
!
! particle_natural_orb_CIS_properties(i,3) = <phi_i|z|phi_i>
!
! particle_natural_orb_CIS_properties(i,5) = <phi_i|x^2|phi_i>
!
! particle_natural_orb_CIS_properties(i,6) = <phi_i|y^2|phi_i>
!
! particle_natural_orb_CIS_properties(i,7) = <phi_i|z^2|phi_i>
!
! CIS_states_properties(i,1:6) = the same but for the hole state i
 END_DOC

 integer :: i,j,k,l
 double precision :: dm_alpha(mo_tot_num,mo_tot_num)
 double precision :: dm_beta(mo_tot_num,mo_tot_num)
 double precision :: dm(mo_tot_num,mo_tot_num)
 double precision :: eigvalues(mo_tot_num)
 double precision :: eigvectors(mo_tot_num,mo_tot_num)
 double precision :: accu_n_elec,c_k


 do i = 1, n_state_cis
  print*,' state cis = ',i
  call get_dm_from_psi(psi_CIS,coefs_CIS(1,i),size_psi_CIS,size_psi_CIS,N_int,dm_alpha,dm_beta)

  dm = dm_alpha + dm_beta
  call get_properties_from_density_matrix(dm,CIS_states_properties(1,i))
  dm = -dm
  do k = 1, elec_alpha_num
   dm(k,k) += 1.d0
  enddo
  do k = 1, elec_beta_num
   dm(k,k) += 1.d0
  enddo
  call lapack_diag(eigvalues,eigvectors,dm,mo_tot_num,mo_tot_num)
  accu_n_elec = 0.d0
  do k = 1, mo_tot_num
   accu_n_elec += eigvalues(k)
  enddo
  do k = 1, mo_tot_num
   do l = 1, mo_tot_num
    c_k = eigvectors(k,j) * eigvectors(l,j)
    particle_natural_orb_CIS_properties(1,i) += c_k * mo_dipole_x(k,l)
    particle_natural_orb_CIS_properties(2,i) += c_k * mo_dipole_y(k,l)
    particle_natural_orb_CIS_properties(3,i) += c_k * mo_dipole_z(k,l)
    particle_natural_orb_CIS_properties(4,i) += c_k * mo_spread_x(k,l)
    particle_natural_orb_CIS_properties(5,i) += c_k * mo_spread_y(k,l)
    particle_natural_orb_CIS_properties(6,i) += c_k * mo_spread_z(k,l)
   enddo
  enddo
 enddo

END_PROVIDER
subroutine get_properties_from_density_matrix(dm,properties)
  implicit none
  double precision, intent(in)  :: dm(mo_tot_num,mo_tot_num)
  double precision, intent(out) :: properties(6)
  integer :: k,l
  double precision :: c_k
  do k = 1, 6
   properties(k) = 0.d0
  enddo
  do k = 1, mo_tot_num
   do l = 1, mo_tot_num
    c_k = dm(k,l)
    properties(1) += c_k * mo_dipole_x(k,l)
    properties(2) += c_k * mo_dipole_y(k,l)
    properties(3) += c_k * mo_dipole_z(k,l)
    properties(4) += c_k * mo_spread_x(k,l)
    properties(5) += c_k * mo_spread_y(k,l)
    properties(6) += c_k * mo_spread_z(k,l)
   enddo
  enddo

end

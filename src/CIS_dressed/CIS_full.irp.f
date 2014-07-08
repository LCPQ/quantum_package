program CIS_full
 implicit none
 integer :: i
 provide eigenvalues_CIS 
 call save_cis
end
subroutine save_cis
 do i = 1, n_state_cis
  print*,'eigenvalues_CIS(i) = ',eigenvalues_CIS(i)
 enddo
 call save_wavefunction_general(size_psi_CIS,n_state_cis,psi_CIS,coefs_CIS)



end

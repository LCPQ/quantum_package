 BEGIN_PROVIDER [double precision, H_OVB_dressing, (min_number_ionic:max_number_ionic, min_number_ionic:max_number_ionic, n_states)]
 BEGIN_DOC 
 ! Hamiltonian matrix expressed in the basis of all the 
 END_DOC
 implicit none
 integer :: i,j,istate,k,l
 double precision :: accu,hij
 do i = min_number_ionic,max_number_ionic
  do j = min_number_ionic,max_number_ionic
   accu = 0.d0
   do istate = 1, N_states                                                  
    do k = 1,  ionic_index(i,0)                                             
     do l = 1,  ionic_index(j,0)
      hij = dressing_ref_hamiltonian(ionic_index(i,k),ionic_index(j,l),istate)
!     accu += psi_ref_coef(ionic_index(i,k),istate) * normalization_factor_ionic(i,istate) * & 
!             psi_ref_coef(ionic_index(j,l),istate) * normalization_factor_ionic(j,istate) * hij
      accu += psi_ref_coef_dressed(ionic_index(i,k),istate) * normalization_factor_ionic_dressed(i,istate) * & 
              psi_ref_coef_dressed(ionic_index(j,l),istate) * normalization_factor_ionic_dressed(j,istate) * hij
     enddo
    enddo
    H_OVB_dressing(i,j,istate) = accu
   enddo
  enddo
 enddo
 END_PROVIDER



 BEGIN_PROVIDER [double precision, H_OVB_total_dressed, (min_number_ionic:max_number_ionic, min_number_ionic:max_number_ionic, n_states)]
 BEGIN_DOC 
 ! Hamiltonian matrix expressed in the basis of all the 
 END_DOC
 implicit none
 integer :: i,j,istate
 double precision :: accu,hij
 do i = min_number_ionic,max_number_ionic
  do j = min_number_ionic,max_number_ionic
   do istate = 1, N_states                                                  
    H_OVB_total_dressed(i,j,istate) = H_OVB_dressing(i,j,istate) + H_OVB_naked(i,j,istate)
   enddo
  enddo
 enddo
 END_PROVIDER

 BEGIN_PROVIDER [double precision, normalization_factor_ionic_dressed, (min_number_ionic:max_number_ionic, N_states) ]
 implicit none
 integer :: i,j,istate,k
 double precision :: accu
 do j = min_number_ionic, max_number_ionic
  do istate = 1, N_states
   accu = 0.d0
   do k = 1, ionic_index(j,0)
    accu += psi_ref_coef_dressed(ionic_index(j,k),istate) **2
   enddo
   normalization_factor_ionic_dressed(j,istate) = 1.d0/dsqrt(accu)
  enddo
 enddo

 END_PROVIDER 

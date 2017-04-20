 BEGIN_PROVIDER [double precision, ao_potential_alpha_xc, (ao_num_align, ao_num)] 
&BEGIN_PROVIDER [double precision, ao_potential_beta_xc, (ao_num_align, ao_num)] 
 implicit none
 integer :: i,j,k,l
 ao_potential_alpha_xc = 0.d0
 ao_potential_beta_xc = 0.d0
!if(exchange_functional == "LDA")then
  do i = 1, ao_num
   do j = 1, ao_num
    ao_potential_alpha_xc(i,j) = (1.d0 - HF_exchange) * lda_ex_potential_alpha_ao(i,j,1)
    ao_potential_beta_xc(i,j) =  (1.d0 - HF_exchange) * lda_ex_potential_beta_ao(i,j,1)
   enddo
  enddo
!endif
END_PROVIDER 

BEGIN_PROVIDER [double precision, e_exchange_dft]
 implicit none
!if(exchange_functional == "LDA")then
  e_exchange_dft = lda_exchange(1)
!endif
 
END_PROVIDER 

BEGIN_PROVIDER [double precision, e_correlation_dft]
 implicit none
!if(correlation_functional == "LDA")then
  e_correlation_dft = 0.d0
!endif
 
END_PROVIDER 

BEGIN_PROVIDER [double precision, HF_density_matrix_ao_alpha, (ao_num,ao_num) ]
   implicit none
   BEGIN_DOC
   ! S^{-1}.P_alpha.S^{-1}
   END_DOC
   
   call dgemm('N','T',ao_num,ao_num,elec_alpha_num,1.d0, &
        mo_coef, size(mo_coef,1), &
        mo_coef, size(mo_coef,1), 0.d0, &
        HF_density_matrix_ao_alpha, size(HF_density_matrix_ao_alpha,1))

END_PROVIDER

BEGIN_PROVIDER [ double precision, HF_density_matrix_ao_beta,  (ao_num,ao_num) ]
   implicit none
   BEGIN_DOC
   ! S^{-1}.P_beta.S^{-1}
   END_DOC
   
   call dgemm('N','T',ao_num,ao_num,elec_beta_num,1.d0, &
        mo_coef, size(mo_coef,1), &
        mo_coef, size(mo_coef,1), 0.d0, &
        HF_density_matrix_ao_beta, size(HF_density_matrix_ao_beta,1))

END_PROVIDER
 
BEGIN_PROVIDER [ double precision, HF_density_matrix_ao, (ao_num,ao_num) ]
   implicit none
   BEGIN_DOC
   ! S^{-1}.P.S^{-1}  where P = C.C^t
   END_DOC
   ASSERT (size(HF_density_matrix_ao,1) == size(HF_density_matrix_ao_alpha,1))
   if (elec_alpha_num== elec_beta_num) then
     HF_density_matrix_ao = HF_density_matrix_ao_alpha + HF_density_matrix_ao_alpha
   else
     ASSERT (size(HF_density_matrix_ao,1) == size(HF_density_matrix_ao_beta ,1))
     HF_density_matrix_ao = HF_density_matrix_ao_alpha + HF_density_matrix_ao_beta
   endif
   
END_PROVIDER
 

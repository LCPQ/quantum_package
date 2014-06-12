 BEGIN_PROVIDER [ double precision, HF_density_matrix_ao_alpha, (ao_num_align,ao_num) ]
&BEGIN_PROVIDER [ double precision, HF_density_matrix_ao_beta,  (ao_num_align,ao_num) ]
   implicit none
   BEGIN_DOC
   ! Alpha and Beta density matrix in the AO basis
   END_DOC
   integer                        :: i,j,k,l1,l2
   integer, allocatable           :: occ(:,:)
   
   allocate ( occ(elec_alpha_num,2) )
   call bitstring_to_list( HF_bitmask(1,1), occ(1,1), j, N_int)
   ASSERT ( j==elec_alpha_num )
   
   call bitstring_to_list( HF_bitmask(1,2), occ(1,2), j, N_int)
   ASSERT ( j==elec_beta_num )
   
   do j=1,ao_num
     !DIR$ VECTOR ALIGNED
     do i=1,ao_num_align
       HF_density_matrix_ao_alpha(i,j) = 0.d0
       HF_density_matrix_ao_beta (i,j) = 0.d0
     enddo
     do k=1,elec_beta_num
       l1 = occ(k,1)
       l2 = occ(k,2)
       !DIR$ VECTOR ALIGNED
       do i=1,ao_num
         HF_density_matrix_ao_alpha(i,j) = HF_density_matrix_ao_alpha(i,j) +&
             mo_coef(i,l1) * mo_coef(j,l1)
         HF_density_matrix_ao_beta (i,j) = HF_density_matrix_ao_beta (i,j) +&
             mo_coef(i,l2) * mo_coef(j,l2)
       enddo
     enddo
     do k=elec_beta_num+1,elec_alpha_num
       l1 = occ(k,1)
       !DIR$ VECTOR ALIGNED
       do i=1,ao_num
         HF_density_matrix_ao_alpha(i,j) = HF_density_matrix_ao_alpha(i,j) +&
             mo_coef(i,l1) * mo_coef(j,l1)
       enddo
     enddo
   enddo
   deallocate(occ)
END_PROVIDER
 
BEGIN_PROVIDER [ double precision, HF_density_matrix_ao, (ao_num_align,ao_num) ]
   implicit none
   BEGIN_DOC
   ! Density matrix in the AO basis
   END_DOC
   integer                        :: i,j,k,l1,l2
   integer, allocatable           :: occ(:,:)
   
   allocate ( occ(elec_alpha_num,2) )
   call bitstring_to_list( HF_bitmask(1,1), occ(1,1), j, N_int)
   ASSERT ( j==elec_alpha_num )
   
   call bitstring_to_list( HF_bitmask(1,2), occ(1,2), j, N_int)
   ASSERT ( j==elec_beta_num )
   
   do j=1,ao_num
     !DIR$ VECTOR ALIGNED
     do i=1,ao_num_align
       HF_density_matrix_ao(i,j) = 0.d0
     enddo
     do k=1,elec_beta_num
       l1 = occ(k,1)
       l2 = occ(k,2)
       !DIR$ VECTOR ALIGNED
       do i=1,ao_num
         HF_density_matrix_ao(i,j) = HF_density_matrix_ao(i,j) +           &
             mo_coef(i,l1) * mo_coef(j,l1) +                         &
             mo_coef(i,l2) * mo_coef(j,l2)
       enddo
     enddo
     do k=elec_beta_num+1,elec_alpha_num
       l1 = occ(k,1)
       !DIR$ VECTOR ALIGNED
       do i=1,ao_num
         HF_density_matrix_ao(i,j) = HF_density_matrix_ao(i,j) +           &
             mo_coef(i,l1) * mo_coef(j,l1)
       enddo
     enddo
   enddo
   deallocate(occ)
END_PROVIDER
 

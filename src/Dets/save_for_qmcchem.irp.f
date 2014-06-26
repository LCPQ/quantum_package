subroutine save_dets_qmcchem
 use bitmasks
 implicit none
 character :: c(mo_tot_num)
 integer :: i,k

 integer, allocatable :: occ(:,:,:), occ_tmp(:,:)
 !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: occ, occ_tmp

 call ezfio_set_determinants_det_num(N_det)
 call ezfio_set_determinants_det_coef(psi_coef_sorted(1,1))

 allocate (occ(elec_alpha_num,N_det,2))
 ! OMP PARALLEL DEFAULT(NONE) &
 ! OMP PRIVATE(occ_tmp,i,k)&
 ! OMP SHARED(N_det,psi_det_sorted,elec_alpha_num, &
 ! OMP   occ,elec_beta_num,N_int)
 allocate (occ_tmp(N_int*bit_kind_size,2))
 occ_tmp = 0
 ! OMP DO 
 do i=1,N_det
  call bitstring_to_list(psi_det_sorted(1,1,i), occ_tmp(1,1), elec_alpha_num, N_int )
  call bitstring_to_list(psi_det_sorted(1,2,i), occ_tmp(1,2), elec_beta_num, N_int )
  do k=1,elec_alpha_num
    occ(k,i,1) = occ_tmp(k,1)
    occ(k,i,2) = occ_tmp(k,2)
  enddo
 enddo
 ! OMP END DO
 deallocate(occ_tmp)
 ! OMP END PARALLEL
 call ezfio_set_determinants_det_occ(occ)
 call write_int(output_dets,N_det,'Determinants saved for QMC')
 deallocate(occ)
end



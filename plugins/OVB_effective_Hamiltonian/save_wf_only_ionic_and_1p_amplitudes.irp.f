program save_wf
 implicit none
 read_wf = .True.
 touch read_wf 
 call routine
end

subroutine routine
 implicit none
 use bitmasks
 integer :: i,j,k,l
 integer(bit_kind), allocatable :: psi_save_final(:,:,:)
 double precision, allocatable :: psi_coef_save_final(:,:)
 integer :: index_ref_determinants_save(psi_det_size)
 integer :: n_det_ref_determinants_save
 integer :: index_non_ref_determinants_save(psi_det_size)
 integer :: n_det_non_ref_determinants_save
 
 integer :: n_det_save_final
 integer :: number_of_particles
 n_det_ref_determinants_save = 0
 integer :: ionic_level
 ionic_level = 1
 do i = 1, ionic_index(ionic_level,0)  ! number of determinants in the ref wf that are neutrals
  n_det_ref_determinants_save +=1
  index_ref_determinants_save(n_det_ref_determinants_save) = ionic_index(ionic_level,i)
 enddo
 ! save all the 1p determinants in order to have the single excitations 
 ! on the top of the neutral structures
 n_det_non_ref_determinants_save = 0
 do i = 1, N_det_non_ref 
  if(number_of_particles(psi_non_ref(1,1,i))==1)then
   n_det_non_ref_determinants_save +=1 
   index_non_ref_determinants_save(n_det_non_ref_determinants_save) = i
  endif
 enddo
 print*,'n_det_ref_determinants_save     = ',n_det_ref_determinants_save
 print*,'n_det_non_ref_determinants_save = ',n_det_non_ref_determinants_save
 n_det_save_final = n_det_ref_determinants_save + n_det_non_ref_determinants_save
 allocate (psi_save_final(N_int,2,n_det_save_final))
 allocate (psi_coef_save_final(n_det_save_final,1))
 integer :: n_det_tmp
 n_det_tmp = 0
 do i = 1, n_det_ref_determinants_save ! set the CAS determinants to psi_save_final
  n_det_tmp +=1
  do j = 1, N_int
   psi_save_final(j,1,n_det_tmp) = psi_ref(j,1,index_ref_determinants_save(i))
   psi_save_final(j,2,n_det_tmp) = psi_ref(j,2,index_ref_determinants_save(i))
  enddo
  psi_coef_save_final(n_det_tmp,1) = psi_ref_coef(index_ref_determinants_save(i),1)
 enddo
 pause
 do i = 1, n_det_non_ref_determinants_save ! set the non ref determinants to psi_save_final
  n_det_tmp +=1
  do j = 1, N_int
   psi_save_final(j,1,n_det_tmp) = psi_non_ref(j,1,index_non_ref_determinants_save(i))
   psi_save_final(j,2,n_det_tmp) = psi_non_ref(j,2,index_non_ref_determinants_save(i))
  enddo
  accu = 0.d0
  double precision :: t_ik,hij
  do j = 1, n_det_ref_determinants_save
   call i_H_j(psi_non_ref(1,1,index_non_ref_determinants_save(i)),psi_ref(1,1,index_ref_determinants_save(j)),N_int,hij)
   t_ik = hij * lambda_mrcc(1,index_non_ref_determinants_save(i)) 
   accu += psi_ref_coef(index_ref_determinants_save(j),1) * t_ik
  enddo
  psi_coef_save_final(n_det_tmp,1) = accu
 enddo
 double precision :: accu
 accu = 0.d0
 do i = 1, n_det_save_final 
  accu += psi_coef_save_final(i,1) * psi_coef_save_final(i,1)
 enddo
 accu = 1.d0/dsqrt(accu)
 do i = 1, n_det_save_final 
  psi_coef_save_final(i,1) = accu * psi_coef_save_final(i,1)
 enddo
 
 do i = 1, n_det_save_final
  print*,''
  print*,'Det'
  call debug_det(psi_save_final(1,1,i),N_int)  
  print*,'coef = ',psi_coef_save_final(i,1)
 enddo

 call save_wavefunction_general(n_det_save_final,1,psi_save_final,n_det_save_final,psi_coef_save_final)
 deallocate (psi_save_final)
 deallocate (psi_coef_save_final)


end

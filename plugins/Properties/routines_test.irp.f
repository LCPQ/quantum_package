

subroutine test_average_value(array,value)
 implicit none
 double precision, intent(in) :: array(mo_tot_num,mo_tot_num)
 double precision, intent(in) :: value
 double precision :: tmp,hij
 integer :: i,j
 tmp = 0.d0
 do i = 1, N_det
  do j = 1, N_det
   call i_O1_j(array,psi_det(1,1,i),psi_det(1,1,j),N_int,hij)
   tmp+= hij * psi_coef(i,1) * psi_coef(j,1)
  enddo
 enddo
 if(dabs(tmp-value).ge.1.d-8)then
  print*,'subroutine test_average_value'
  print*,'PB WITH AVERAGE VALUE !!!!!!!!!'
  print*,'<psi|O|psi>        = ',tmp
  print*,'value              = ',value
  stop
 endif
end

subroutine test_average_value_alpha_beta(array,value)
 implicit none
 double precision, intent(in) :: array(mo_tot_num,mo_tot_num)
 double precision, intent(in) :: value
 double precision :: tmp,hij
 integer :: i,j
 tmp = 0.d0
 do i = 1, N_det
  do j = 1, N_det
   call i_O1_j_alpha_beta(array,psi_det(1,1,i),psi_det(1,1,j),N_int,hij)
   tmp+= hij * psi_coef(i,1) * psi_coef(j,1)
  enddo
 enddo
 print*,'tmp   = ',tmp
 print*,'value = ',value
 tmp = 0.d0
 do i = 1, N_det
   call i_O1_psi_alpha_beta(array,psi_det(1,1,i),psi_det,psi_coef,N_int,N_det,psi_det_size,N_states,hij)
   tmp+= hij * psi_coef(i,1) 
 enddo
 print*,'tmp   = ',tmp
 print*,'value = ',value
 if(dabs(tmp-value).ge.1.d-8)then
  print*,'subroutine test_average_value'
  print*,'PB WITH AVERAGE VALUE !!!!!!!!!'
  print*,'<psi|O|psi>        = ',tmp
  print*,'value              = ',value
  stop
 endif
end

subroutine test_dm(tmp_array)
 use bitmasks
 implicit none
 double precision,intent(out) :: tmp_array(mo_tot_num,mo_tot_num)
 double precision :: phase
 integer :: i,j,ispin,k
 integer :: degree
 integer :: exc(0:2,2,2),h1,p1,h2,p2,s1,s2
 integer :: occ_det(N_int*bit_kind_size,2)
 integer :: tmp
 double precision :: accu

 tmp_array = 0.d0

 do i = 1, N_det
  call bitstring_to_list(psi_det(1,1,i), occ_det(1,1), tmp, N_int)
  call bitstring_to_list(psi_det(1,2,i), occ_det(1,2), tmp, N_int)
  do ispin = 1, 2
   do k = 1, elec_num_tab(ispin)
    tmp_array(occ_det(k,ispin),occ_det(k,ispin)) += psi_coef(i,1) * psi_coef(i,1)
   enddo
  enddo
  do j = i+1, N_det
   call get_excitation_degree(psi_det(1,1,i),psi_det(1,1,j),degree,N_int)
   if (degree.ne.1)cycle
   call get_excitation(psi_det(1,1,i),psi_det(1,1,j),exc,degree,phase,N_int)
   call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
   tmp_array(h1,p1) += phase * psi_coef(i,1) * psi_coef(j,1)
   tmp_array(p1,h1)  = tmp_array(h1,p1)
  enddo
 enddo


end

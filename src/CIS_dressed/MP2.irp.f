 BEGIN_PROVIDER [double precision, MP2_corr_energy]
 &BEGIN_PROVIDER [double precision, p_imp,(elec_alpha_num+1:n_act_cis)]
 &BEGIN_PROVIDER [double precision, h_imp,(n_core_cis+1:elec_alpha_num)]
 &BEGIN_PROVIDER [double precision, hp_imp,(n_core_cis+1:elec_alpha_num,elec_alpha_num+1:n_act_cis)]

 BEGIN_DOC
 !Calculation of the MP2 correlation energy (MP2_corr_energy)
 !and calculation of the contribution of the disconnected Triples on the 
 !Singles, via the impossible (p_imp, h_imp, hp_imp)
 END_DOC

 implicit none
 integer :: i,j,k,l !variables for going over the occupied (i,j) and virutal (k,l)
 double precision :: direct,exchg,hij !calculating direct, exchange and total contribution
 double precision :: get_mo_bielec_integral
 double precision :: e_i,e_k,e_j,e_l !epsilons of i,j,k,l
 double precision :: delta_e_ik,delta_e_ikj
 double precision :: delta_e !delta epsilons



 MP2_corr_energy=0.d0

 do k =elec_beta_num + 1, n_act_cis
  p_imp(k)=0.d0
 enddo


 do i=n_core_cis+1,elec_alpha_num
  h_imp(i)=0.d0
 
  e_i=diagonal_Fock_matrix_mo(i)
 
  do k=elec_alpha_num+1,n_act_cis
   hp_imp(i,k)=0.d0
  
   e_k=diagonal_Fock_matrix_mo(k)
   delta_e_ik=e_i-e_k

   !same spin contribution for MP2_corr_energy 
   do j=i+1,elec_alpha_num
    e_j=diagonal_Fock_matrix_mo(j)
    delta_e_ikj=delta_e_ik+e_j

    !same spin contribution for MP2_corr_energy and a part of the contribution to p_imp and h_imp
    do l=k+1,n_act_cis
     e_l=diagonal_Fock_matrix_mo(l)
     delta_e=delta_e_ikj-e_l

     direct=get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
     exchg =get_mo_bielec_integral(i,j,l,k,mo_integrals_map)

     hij=(direct-exchg)*(direct-exchg)/delta_e

     MP2_corr_energy=MP2_corr_energy+2*hij
     p_imp(k)=p_imp(k)+hij
     h_imp(i)=h_imp(i)+hij

    enddo

    !remaining same spin contribution for p_imp    
    do l=elec_alpha_num+1,k-1
     e_l=diagonal_Fock_matrix_mo(l)
     delta_e=delta_e_ikj-e_l 

     direct=get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
     exchg =get_mo_bielec_integral(i,j,l,k,mo_integrals_map)

     hij=(direct-exchg)*(direct-exchg)/delta_e

     p_imp(k)=p_imp(k)+hij 

    enddo
   enddo

   !remaining same spin contribution for h_imp
   do j=n_core_cis+1,i-1
    e_j=diagonal_Fock_matrix_mo(j)
    delta_e_ikj=delta_e_ik+e_j

    do l=k+1,n_act_cis
     e_l=diagonal_Fock_matrix_mo(l)
     delta_e=delta_e_ikj-e_l

     direct=get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
     exchg =get_mo_bielec_integral(i,j,l,k,mo_integrals_map)

     hij=(direct-exchg)*(direct-exchg)/delta_e

     h_imp(i)=h_imp(i)+hij

    enddo
   enddo

   !same spin contribution for hp_imp
   do j=n_core_cis+1,elec_alpha_num
    e_j=diagonal_Fock_matrix_mo(j)
    delta_e_ikj=delta_e_ik+e_j
 
    do l=elec_alpha_num+1,n_act_cis
     e_l=diagonal_Fock_matrix_mo(l)
     delta_e=delta_e_ikj-e_l

     direct=get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
     exchg =get_mo_bielec_integral(i,j,l,k,mo_integrals_map)

     hij=(direct-exchg)*(direct-exchg)/delta_e

     hp_imp(i,k)=hp_imp(i,k)+hij

    enddo
   enddo

   !different spin contribution
   do j=n_core_cis+1,elec_beta_num
    e_j=diagonal_Fock_matrix_mo(j)
    delta_e_ikj=delta_e_ik+e_j

    do l=elec_beta_num+1,n_act_cis
     e_l=diagonal_Fock_matrix_mo(l)
     delta_e=delta_e_ikj-e_l 

     direct=get_mo_bielec_integral(i,j,k,l,mo_integrals_map)

     hij=direct*direct/delta_e

     MP2_corr_energy=MP2_corr_energy+hij
     p_imp(k)=p_imp(k)+hij
     h_imp(i)=h_imp(i)+hij
     hp_imp(i,k)=hp_imp(i,k)+hij

    enddo
   enddo
  enddo
 enddo

 print*,'MP2 correlation energy=',MP2_corr_energy

 END_PROVIDER




 BEGIN_PROVIDER [integer, size_psi_CIS]

 BEGIN_DOC
 !Definition of the size of the CIS vector
 END_DOC

 implicit none
 size_psi_CIS=(elec_alpha_num-n_core_cis)*(n_act_cis-elec_alpha_num)*2+1 !!!Is this still correct for open shell systems???
 print*,'size_psi_CIS = ',size_psi_CIS

 END_PROVIDER


 BEGIN_PROVIDER [double precision, diag_elements_sorted, (size_psi_CIS)]
 &BEGIN_PROVIDER [integer , diag_elements_sorted_index, (size_psi_CIS)]
 BEGIN_DOC
 !Array of the energy of the CIS determinants sorted by energy and
 !Index in the CIS matrix
 END_DOC
 
 implicit none
 integer :: iorder(size_psi_CIS),i
 
 print*,'diag_elements_sorted'

 do i = 1,size_psi_CIS
  diag_elements_sorted(i) = diag_elements(i) 
  iorder(i) = i
 enddo
 
 call dsort(diag_elements_sorted,iorder,size_psi_CIS)
 
 do i = 1,size_psi_CIS
  diag_elements_sorted(i) = diag_elements(iorder(i))
  diag_elements_sorted_index(i) = iorder(i)
 enddo
 
 END_PROVIDER




 
 
 BEGIN_PROVIDER [double precision, eigenvalues_dressed_CIS,(n_state_CIS)]

 BEGIN_DOC
 !The first states of the dressed CIS matrix
 END_DOC
 
 implicit none
 integer :: i,j,k
 double precision :: s2,e_corr,tmp
 double precision,allocatable :: coefs_tmp(:)
 double precision,allocatable  :: eigvalues(:),eigvectors(:,:)
 double precision,allocatable  :: delta_H_trip(:,:)
 double precision,allocatable  :: delta_H_matrix_doub(:,:)
 double precision,allocatable  :: delta_H_matrix_dpdiscon(:,:)
 double precision,allocatable  :: delta_H_matrix(:,:)
 double precision              :: eigenvalues_dressed_CIS_D(n_state_CIS)
 double precision              :: eigenvalues_dressed_CIS_DT(n_state_CIS)
 double precision,allocatable  :: eigvald(:),eigvecd(:,:)
 allocate(eigvalues(size_psi_CIS),eigvectors(size_psi_CIS,size_psi_CIS),delta_H_trip(size_psi_CIS,size_psi_CIS),delta_H_matrix_doub(size_psi_CIS,size_psi_CIS),delta_H_matrix(size_psi_CIS,size_psi_CIS))
 allocate (eigvald(size_psi_CIS),eigvecd(size_psi_CIS,size_psi_CIS),coefs_tmp(size_psi_CIS),delta_H_matrix_dpdiscon(size_psi_CIS,size_psi_CIS) )

 print*,'eigenvalues_dressed_CIS'

 provide coefs_CIS
  do i = 1,n_state_CIS

   write(66,*),'i=',i

  call dress_by_doubles(eigenvalues_CIS(i),coefs_CIS(1,i),delta_H_matrix,size_psi_CIS) !dressing of the Doubles
   do j = 1,size_psi_CIS
    do k = 1,size_psi_CIS    
     delta_H_matrix(j,k)=delta_H_matrix(j,k)+H_CIS(j,k)

     delta_H_matrix_doub(j,k)=delta_H_matrix(j,k) 
     delta_H_matrix(j,k)=delta_H_matrix(j,k)+delta_H_trip(j,k)
     delta_H_matrix_dpdiscon(j,k)=delta_H_matrix(j,k)  
   enddo
    delta_H_matrix(j,j)=delta_H_matrix(j,j)+dress_T_discon_array_CIS(j) !dressing by the disconnected Triples  
  enddo




  call lapack_diag(eigvalues,eigvectors,delta_H_matrix_doub,size_psi_CIS,size_psi_CIS)
  double precision :: overlap
  double precision :: max_overlap
  integer :: i_overlap
  max_overlap = 0.d0
  do k = 1, size_psi_CIS
   overlap = 0.d0
   do j = 1,size_psi_CIS
    overlap += eigvectors(j,k)*coefs_CIS(j,i)
   enddo
   if(dabs(overlap).gt.max_overlap)then
    max_overlap = dabs(overlap)
    i_overlap = k
   endif
   ! <CIS(i)|state(k)>
  enddo
! print*,'i_overlap = ',i_overlap
  eigenvalues_dressed_CIS_D(i) = eigvalues(i_overlap)

  do j = 1,size_psi_CIS
   coefs_tmp(j) = eigvectors(j,i_overlap)

  enddo
  call get_s2_u0(psi_CIS,coefs_tmp,size_psi_CIS,size_psi_CIS,s2)
! print*,'s2(D)=',s2
! print*,''
! print*,'diagonal elements sorted(i)                =',diag_elements_sorted(i)
! print*,'eigenvalues of the CIS(i)                  =',eigenvalues_CIS(i)
! print*,'eigenvalues dressed by Doubles (D)         =',eigenvalues_dressed_CIS_D(i)

! write(12,*),''
! write(12,*),'i=',i
! write(12,*),'eigenvalues of the CIS(i)  =',eigenvalues_CIS(i)
! write(12,*),'diagonal elements sorted(i)=',diag_elements_sorted(i)
! write(12,*),''
! write(12,*),'s2(D)=',s2
! write(12,*),'eigenvalues dressed by Doubles (D)               =',eigenvalues_dressed_CIS_D(i)

  write(66,*),'CIS-HF   ',eigenvalues_CIS(i)-eigenvalues_CIS(1)
  write(66,*),'Doub-HF  ',eigenvalues_dressed_CIS_D(i)-eigenvalues_dressed_CIS_D(1)

    call lapack_diag(eigvalues,eigvectors,delta_H_matrix_dpdiscon,size_psi_CIS,size_psi_CIS)
  max_overlap = 0.d0
  do k = 1, size_psi_CIS
   overlap = 0.d0
   do j = 1,size_psi_CIS
    overlap += eigvectors(j,k)*coefs_CIS(j,i)
   enddo
   if(dabs(overlap).gt.max_overlap)then
    max_overlap = dabs(overlap)
    i_overlap = k
   endif
   ! <CIS(i)|state(k)>
  enddo
! print*,'i_overlap = ',i_overlap


    eigenvalues_dressed_CIS_DT(i) = eigvalues(i_overlap)
  
    do j = 1,size_psi_CIS
     coefs_tmp(j) = eigvectors(j,i_overlap)
    enddo
    
    call get_s2_u0(psi_CIS,coefs_tmp,size_psi_CIS,size_psi_CIS,s2)
  
!   write(12,*),'s2(D+cT)=',s2
!   write(12,*),'eigenvalues dressed by D and connected Triples=',eigenvalues_dressed_CIS_DT(i)
! print*,'s2(DcT)=',s2
! print*,'eigenvalues dressed D and cT=',eigenvalues_dressed_CIS_DT(i)
 
   write(66,*),'D+cT-HF ',eigenvalues_dressed_CIS_DT(i)-eigenvalues_dressed_CIS_DT(1)



  call lapack_diag(eigvalues,eigvectors,delta_H_matrix,size_psi_CIS,size_psi_CIS)
  do k = 1, size_psi_CIS
   overlap = 0.d0
   do j = 1,size_psi_CIS
    overlap += eigvectors(j,k)*coefs_CIS(j,i)
   enddo
   if(dabs(overlap).gt.max_overlap)then
    max_overlap = dabs(overlap)
    i_overlap = k
   endif
   ! <CIS(i)|state(k)>
  enddo
! print*,'i_overlap = ',i_overlap


  eigenvalues_dressed_CIS(i) = eigvalues(i_overlap)


  do j = 1,size_psi_CIS
   coefs_tmp(j) = eigvectors(j,i_overlap)
  enddo
   call get_s2_u0(psi_CIS,coefs_tmp,size_psi_CIS,size_psi_CIS,s2)

! print*,'s2(Tot)=',s2
! print*,'eigenvalues dressed =',eigenvalues_dressed_CIS(i)

! write(12,*),''
! write(12,*),'s2(tot)=',s2
! write(12,*),'eigenvalues of dressed CIS(i)=',eigenvalues_dressed_CIS(i)

  write(66,*)'Eigval-HF ',eigenvalues_dressed_CIS(i)-eigenvalues_dressed_CIS(1)
 enddo

 deallocate(eigvalues,eigvectors,delta_H_trip,delta_H_matrix_doub,delta_H_matrix_dpdiscon)
 deallocate (eigvald,eigvecd,coefs_tmp)
!call system ( "mkdir results_$(date +%d%m%y_%I%M)" )
!call system ( "cp ../newcode/MP2.irp.f results_$(date +%d%m%y_%I%M)" )
!call system ( "mv fort.10 Ecorr.out" )
!call system ( "mv fort.11 CIS.out" )
!call system ( "mv fort.12 Dressing.out" )
!call system ( "mv fort.66 delta_e_states" )
!call system ( "cp Ecorr.out results_$(date +%d%m%y_%I%M)" )
!call system ( "cp CIS.out results_$(date +%d%m%y_%I%M)" )
!call system ( "cp Dressing.out results_$(date +%d%m%y_%I%M)" )
!call system ( "cp delta_e_states results_$(date +%d%m%y_%I%M)"  )
!call system ( "rm delta_e_states" )  
!call system ( "rm Ecorr.out" )
!call system ( "rm CIS.out" )
!call system ( "rm Dressing.out" )

 END_PROVIDER


 BEGIN_PROVIDER [double precision, dress_T_discon,(n_core_cis+1:elec_alpha_num,elec_alpha_num+1:n_act_cis)]
 &BEGIN_PROVIDER [double precision, dress_T_discon_array_CIS,(size_psi_CIS)]

 BEGIN_DOC
 !Calculation of the dressing by the disconnected Triples, via the impossible
 END_DOC

 implicit none
 integer :: i,k !variables for going over the occupied (i) and virutal (k)
 integer :: key !key for CIS-matrix

 print*,'dress_T_discon'
 print*,'mp2_dressing = ',mp2_dressing

 if(mp2_dressing)then
  dress_T_discon_array_CIS(1)=MP2_corr_energy
  
  do i=n_core_cis+1,elec_alpha_num
   do k=elec_alpha_num+1,n_act_cis
    key=psi_CIS_adress(i,k)
    
    dress_T_discon(i,k)=MP2_corr_energy-p_imp(k)-h_imp(i)+hp_imp(i,k)

    dress_T_discon_array_CIS(key) = dress_T_discon(i,k)
    dress_T_discon_array_CIS(key+1) = dress_T_discon(i,k)

   enddo
  enddo

 else !EN Dressing
  print*,'coucou !'
  dress_T_discon_array_CIS(1)=EN2_corr_energy
  print*,'coucou !'
  
! do i=n_core_cis+1,elec_alpha_num
!  print*,'i',i,n_core_cis
!  do k=elec_alpha_num+1,n_act_cis
!   print*,'k',k,n_act_cis
!   key=psi_CIS_adress(i,k)
!   
!   dress_T_discon(i,k)=EN2_corr_energy-p_imp_EN(k)-h_imp_EN(i)+hp_imp_EN(i,k)

!   dress_T_discon_array_CIS(key) = dress_T_discon(i,k)
!   dress_T_discon_array_CIS(key+1) = dress_T_discon(i,k)

!  enddo
! enddo
 end if
 print*,'end'

 END_PROVIDER


 subroutine dress_by_doubles(ref_energy,coefs,delta_H_matrix,ndet)

 use bitmasks
 implicit none
 integer, intent(in) :: ndet
 double precision, intent(in) :: ref_energy
 double precision, intent(in) ::coefs(size_psi_CIS)
 double precision, intent(out) :: delta_H_matrix(ndet,ndet)

 integer :: i,j,k,l !variables for going over the occupied (i,j) and virutal (k,l)
 integer :: key !key for CIS-matrix
 integer :: a,b,i_ok !control variables
 integer(bit_kind) :: key_out(N_int,2) !Doubles
 integer :: ispin1,ispin2 !spin variables
 integer :: degree,i_count_connected
 double precision :: hmD,hij,hij_array(ndet),hij_index(ndet)
 double precision :: delta_hij
 double precision :: delta_e , e_double!delta epsilons
 double precision ::  accu_H_mD,diag_H_mat_elem
 double precision :: epsilon_D,s2

 print*,'dress_by_doubles'

 double precision :: accu,accu_2
 accu = 0.d0
 accu_2 = 0.d0
 do i =1,ndet
  accu += coefs(i) * coefs(i)
 enddo
 if (dabs(accu-1.d0) >  1.d-10)then
  print*,'coefficients not normalized !!'
  print*,accu
 !stop
 endif

 accu = 0.d0
 if(standard_doubles)then
  delta_H_matrix = 0.d0
 
  do i=n_core_cis+1,elec_alpha_num 
    
   do k=elec_alpha_num+1,n_act_cis
 
    !same spin contribution
    do j=i+1,elec_alpha_num 
 
     do l=k+1,n_act_cis
      !Alpha
      ispin1=1
      ispin2=1
 
      call diexcitation(i,j,k,l,ispin1,ispin2,ref_bitmask,key_out,i_ok,N_int)
      
      e_double =diag_H_mat_elem(key_out,N_int)
                
      delta_e = 1.d0 / (ref_energy - e_double)
 
      i_count_connected = 0
 
      do a=2, ndet
       call get_excitation_degree(key_out,psi_CIS(1,1,a),degree,N_int)
 
       if (degree /= 1 .and.degree /= 2) cycle

       call i_H_j(key_out,psi_CIS(1,1,a),N_int,hij)

       if (dabs(hij).le.1.d-10) cycle
 
       i_count_connected = i_count_connected +1 

       hij_array(i_count_connected) = hij
       hij_index(i_count_connected) = a

      enddo
 
      do a=1,i_count_connected
       delta_hij = hij_array(a)*hij_array(a) * delta_e

       delta_H_matrix(hij_index(a),hij_index(a))=delta_H_matrix(hij_index(a),hij_index(a))+delta_hij

       do b=a+1,i_count_connected
          delta_hij = hij_array(a)*hij_array(b) * delta_e
 
          delta_H_matrix(hij_index(a),hij_index(b))=delta_H_matrix(hij_index(a),hij_index(b))+delta_hij
          delta_H_matrix(hij_index(b),hij_index(a))=delta_H_matrix(hij_index(b),hij_index(a))+delta_hij

       enddo
      enddo
      
      !Beta
      ispin1=2
      ispin2=2
 
      call diexcitation(i,j,k,l,ispin1,ispin2,ref_bitmask,key_out,i_ok,N_int)
      
      e_double =diag_H_mat_elem(key_out,N_int)
      delta_e = 1.d0 / (ref_energy - e_double)

      i_count_connected = 0
 
      do a=2, ndet 
       call get_excitation_degree(key_out,psi_CIS(1,1,a),degree,N_int)
 
       if (degree /= 1 .and.degree /= 2) cycle
       call i_H_j(key_out,psi_CIS(1,1,a),N_int,hij)
       if (dabs(hij).le.1.d-10) cycle
 
       i_count_connected = i_count_connected +1 
 
       hij_array(i_count_connected) = hij
       hij_index(i_count_connected) = a

      enddo
 
      do a=1,i_count_connected
       delta_hij = hij_array(a)*hij_array(a)*delta_e

       delta_H_matrix(hij_index(a),hij_index(a))=delta_H_matrix(hij_index(a),hij_index(a))+delta_hij

       do b=a+1,i_count_connected
          delta_hij = hij_array(a)*hij_array(b)*delta_e
 
          delta_H_matrix(hij_index(a),hij_index(b))=delta_H_matrix(hij_index(a),hij_index(b))+delta_hij
          delta_H_matrix(hij_index(b),hij_index(a))=delta_H_matrix(hij_index(b),hij_index(a))+delta_hij

       enddo
      enddo
 
     enddo
    enddo
 
    !different spin contribution
    do j=n_core_cis+1,elec_beta_num
 
     do l=elec_beta_num+1,n_act_cis
       ispin1=2
       ispin2=1

       hij_array=0.d0

 
      call diexcitation(i,j,k,l,ispin1,ispin2,ref_bitmask,key_out,i_ok,N_int)
      
      e_double =diag_H_mat_elem(key_out,N_int)
      delta_e = 1.d0 / (ref_energy - e_double)
      
      i_count_connected = 0
 
      do a=2, ndet 
       call get_excitation_degree(key_out,psi_CIS(1,1,a),degree,N_int)

       if (degree /= 1 .and.degree /= 2) cycle
       
       call i_H_j(key_out,psi_CIS(1,1,a),N_int,hij)
       
       if (dabs(hij).le.1.d-10) cycle
 
       i_count_connected = i_count_connected +1 
 
       hij_array(i_count_connected) = hij
       hij_index(i_count_connected) = a

      enddo
 
      do a=1,i_count_connected
       delta_hij = hij_array(a)*hij_array(a)*delta_e

       delta_H_matrix(hij_index(a),hij_index(a))=delta_H_matrix(hij_index(a),hij_index(a))+delta_hij

       do b=a+1,i_count_connected
          delta_hij = hij_array(a)*hij_array(b)*delta_e
 
          delta_H_matrix(hij_index(a),hij_index(b))=delta_H_matrix(hij_index(a),hij_index(b))+delta_hij
          delta_H_matrix(hij_index(b),hij_index(a))=delta_H_matrix(hij_index(b),hij_index(a))+delta_hij

       enddo
      enddo
 
     enddo
    enddo
   enddo
  enddo

 else !2x2 Matrix diagonalisation


 delta_H_matrix = 0.d0
 
  do i=n_core_cis+1,elec_alpha_num 
    
   do k=elec_alpha_num+1,n_act_cis
 
    !same spin contribution
    do j=i+1,elec_alpha_num 
 
     do l=k+1,n_act_cis
      !Alpha
      ispin1=1
      ispin2=1
 
      call diexcitation(i,j,k,l,ispin1,ispin2,ref_bitmask,key_out,i_ok,N_int)
      
      e_double =diag_H_mat_elem(key_out,N_int)

      i_count_connected = 0
      accu_H_md=0.d0
 
      do a=2, ndet 
       call get_excitation_degree(key_out,psi_CIS(1,1,a),degree,N_int)
 
       if (degree /= 1 .and.degree /= 2) cycle

       call i_H_j(key_out,psi_CIS(1,1,a),N_int,hij)

       if (dabs(hij).le.1.d-10) cycle
 
       i_count_connected = i_count_connected +1 

       hij_array(i_count_connected) = hij
       hij_index(i_count_connected) = a
       
       accu_H_mD=accu_H_mD+(coefs(a))*hij

      enddo

      hmD=accu_H_mD

!    if (dabs(hmD).le.1.d-7) cycle

      epsilon_D=0.5*((e_double-ref_energy)-sqrt((ref_energy -e_double)*(ref_energy -e_double)+4*hmD*hmD))
      if (dabs(hmD*hmD).le.1.d-10.or.dabs(epsilon_D).le.1.d-10) cycle
      accu += epsilon_D
      accu_2 += hmD*hmD

      delta_e = epsilon_D/(hmD*hmD)
      do a=1,i_count_connected
       delta_hij = hij_array(a)*hij_array(a) * delta_e

       delta_H_matrix(hij_index(a),hij_index(a))=delta_H_matrix(hij_index(a),hij_index(a))+delta_hij
 
       do b=a+1,i_count_connected
          delta_hij = hij_array(a)*hij_array(b) * delta_e
 
          delta_H_matrix(hij_index(a),hij_index(b))=delta_H_matrix(hij_index(a),hij_index(b))+delta_hij
          delta_H_matrix(hij_index(b),hij_index(a))=delta_H_matrix(hij_index(b),hij_index(a))+delta_hij
 
       enddo
      enddo

      !Beta 
      ispin1=2
      ispin2=2
   
      call diexcitation(i,j,k,l,ispin1,ispin2,ref_bitmask,key_out,i_ok,N_int)
      
      e_double =diag_H_mat_elem(key_out,N_int)

      i_count_connected = 0
      accu_H_md=0.d0

      do a=2, ndet 
       call get_excitation_degree(key_out,psi_CIS(1,1,a),degree,N_int)
 
       if (degree /= 1 .and.degree /= 2) cycle

       call i_H_j(key_out,psi_CIS(1,1,a),N_int,hij)

       if (dabs(hij).le.1.d-10) cycle
 
       i_count_connected = i_count_connected +1 

       hij_array(i_count_connected) = hij
       hij_index(i_count_connected) = a
       
        accu_H_mD=accu_H_mD+(coefs(a))*hij

      enddo

      hmD=accu_H_mD

!     if (dabs(hmD).le.1.d-7) cycle

      epsilon_D=0.5*((e_double-ref_energy)-sqrt((ref_energy -e_double)*(ref_energy -e_double)+4*hmD*hmD))
      if (dabs(hmD*hmD).le.1.d-10.or.dabs(epsilon_D).le.1.d-10) cycle
      accu += epsilon_D
      accu_2 += hmD*hmD

      delta_e = epsilon_D/(hmD*hmD)
 
      do a=1,i_count_connected
       delta_hij = hij_array(a)*hij_array(a) * delta_e

       delta_H_matrix(hij_index(a),hij_index(a))=delta_H_matrix(hij_index(a),hij_index(a))+delta_hij
    
       do b=a+1,i_count_connected
          delta_hij = hij_array(a)*hij_array(b) * delta_e
 
          delta_H_matrix(hij_index(a),hij_index(b))=delta_H_matrix(hij_index(a),hij_index(b))+delta_hij
          delta_H_matrix(hij_index(b),hij_index(a))=delta_H_matrix(hij_index(b),hij_index(a))+delta_hij
 
       enddo
      enddo
 
     enddo
    enddo
 
    !different spin contribution
    do j=n_core_cis+1,elec_beta_num
 
     do l=elec_beta_num+1,n_act_cis
      ispin1=2
      ispin2=1
 
      call diexcitation(i,j,k,l,ispin1,ispin2,ref_bitmask,key_out,i_ok,N_int)
      
      e_double =diag_H_mat_elem(key_out,N_int)

      i_count_connected = 0
      accu_H_md=0.d0

      do a=2, ndet 
       call get_excitation_degree(key_out,psi_CIS(1,1,a),degree,N_int)
 
       if (degree /= 1 .and.degree /= 2) cycle

       call i_H_j(key_out,psi_CIS(1,1,a),N_int,hij)

       if (dabs(hij).le.1.d-10) cycle
 
       i_count_connected = i_count_connected +1 

       hij_array(i_count_connected) = hij
       hij_index(i_count_connected) = a
       
       accu_H_mD=accu_H_mD+(coefs(a))*hij

      enddo

      hmD=accu_H_mD


      epsilon_D=0.5*((e_double-ref_energy)-sqrt((ref_energy -e_double)*(ref_energy -e_double)+4*hmD*hmD))
      if (dabs(hmD*hmD).le.1.d-10.or.dabs(epsilon_D).le.1.d-10) cycle
      accu += epsilon_D
      accu_2 += hmD*hmD

      delta_e = epsilon_D/(hmD*hmD)
!     e_double =1.d0/(ref_energy - H_jj_total_diff(key_out,ref_bitmask,HF_energy,N_int))
!     if(dabs(e_double - delta_e)/(dabs(e_double)).gt.1.d-3)then
!      print*,delta_e,e_double
!     endif

      do a=1,i_count_connected
       delta_hij = hij_array(a)*hij_array(a) * delta_e

       delta_H_matrix(hij_index(a),hij_index(a))=delta_H_matrix(hij_index(a),hij_index(a))+delta_hij

       do b=a+1,i_count_connected
          delta_hij = hij_array(a)*hij_array(b) * delta_e
 
          delta_H_matrix(hij_index(a),hij_index(b))=delta_H_matrix(hij_index(a),hij_index(b))+delta_hij
          delta_H_matrix(hij_index(b),hij_index(a))=delta_H_matrix(hij_index(b),hij_index(a))+delta_hij
 
       enddo
      enddo
 
     enddo
    enddo
   enddo
  enddo


 end if

 end

 subroutine dress_T_con(ref_energy,delta_H_trip,ndet) 

 BEGIN_DOC
 !Generating all the Triples and, in the loops, the connected Singles
 END_DOC

 implicit none
 double precision :: e_i,e_r,e_j,e_s,e_k,e_t !epsilons of occupied and virtuals
 double precision :: delta_e_ir,delta_e_irj,delta_e_irjs,delta_e_irjsk
 double precision :: delta_e,energy !delta epsilons
 double precision :: phi_tmp,phi_doub
 double precision :: direct,exchg,get_mo_bielec_integral
 double precision :: phi_aaa(n_core_cis+1:elec_alpha_num,elec_alpha_num+1:n_act_cis),phi_aab_alpha(n_core_cis+1:elec_alpha_num,elec_alpha_num+1:n_act_cis),phi_aab_beta(n_core_cis+1:elec_alpha_num,elec_alpha_num+1:n_act_cis),phi_aab(n_core_cis+1:elec_alpha_num,elec_alpha_num+1:n_act_cis)
 integer :: i,j,k,r,s,t !holes and particles of the Triples
 integer :: occ,vir,occ_tmp,vir_tmp !hole and particle of the Singles
 integer :: a,b,c,d !variables of the conected Singels
 integer :: m,key_ir,key_js,key_is,key_jr,key_kt,key_ks,key_kr,key_it,key_jt,key_tmp

 integer, intent(in) :: ndet
 double precision, intent(in) :: ref_energy
 double precision, intent(out) :: delta_H_trip(ndet,ndet)

 delta_H_trip=0.d0
!do i=5,6
 do i=n_core_cis+1,elec_alpha_num
  e_i=diagonal_Fock_matrix_mo(i)
 !r=7
  do r=elec_alpha_num+1,n_act_cis
   e_r=diagonal_Fock_matrix_mo(r)
   delta_e_ir=e_i-e_r
   key_ir=psi_CIS_adress(i,r)
   do j=i+1,elec_alpha_num
    e_j=diagonal_Fock_matrix_mo(j)
    delta_e_irj=delta_e_ir+e_j
    do s=r+1,n_act_cis
     e_s=diagonal_Fock_matrix_mo(s)
     delta_e_irjs=delta_e_irj-e_s
     !alpha-alpha-alpha
     do k=j+1,elec_alpha_num
      e_k=diagonal_Fock_matrix_mo(k)
      delta_e_irjsk=delta_e_irjs+e_k
      do t=s+1,n_act_cis
       e_t=diagonal_Fock_matrix_mo(t)
       delta_e=delta_e_irjsk-e_t
       energy=1.d0/(ref_energy-delta_e)
       occ=i
       a=j
       b=k
       vir=r
       c=s
       d=t
       direct=get_mo_bielec_integral(a,b,c,d,mo_integrals_map)
       exchg =get_mo_bielec_integral(a,b,d,c,mo_integrals_map)
       phi_tmp=direct-exchg
       do occ=i,k
        if (occ==i) then
         a=j
         b=k
        else if (occ==j) then
         a=i
         b=k
       !else if (occ==k) then
       ! a=i
       ! b=j
       !else
       ! cycle
        end if
        do vir=r,t
         if (vir==r) then
          c=s
          d=t
         else if (vir==s) then
          c=r
          d=t
       ! else if (vir==t) then
       !  c=r
       !  d=t
       ! else
       !  cycle
         end if
          key_tmp=psi_CIS_adress(occ,vir)
          direct=get_mo_bielec_integral(a,b,c,d,mo_integrals_map)
          exchg =get_mo_bielec_integral(a,b,d,c,mo_integrals_map)
         if (occ==j .and. vir==s .or. occ==k .and. vir==t ) then
          phi_aaa(occ,vir)=direct-exchg 
         else if (occ==i .and. vir==r) then
          phi_aaa(occ,vir)=0.d0
        !else if (occ/=i .or. occ/=j .or. occ/=k .or. vir/=r .or. vir/=s .or. vir /=t) then
        ! phi_aaa(occ,vir)=0.d0
         else
          phi_aaa(occ,vir)=-direct+exchg
         endif
          phi_doub=phi_tmp*phi_aaa(occ,vir)*energy
          delta_H_trip(key_ir,key_tmp)=delta_H_trip(key_ir,key_tmp)+phi_doub
          delta_H_trip(key_ir+1,key_tmp+1)=delta_H_trip(key_ir+1,key_tmp+1)+phi_doub

  !     if (phi_doub.ge.1.d-6) then
  !       print*,'alpha alpha'
  !       print*,'occ,vir,key_tmp',occ,vir,key_tmp
  !     endif

  !     if (key_ir==key_tmp) then
  !      print*,'ir=tmp'
  !      print*,'i,r,j,s,k,t',i,r,j,s,k,t
  !      print*,'occ,vir',occ,vir
  !     !stop
  !     end if

        enddo
       enddo
      enddo
     enddo
     !alpha-alpha-beta
     do k=n_core_cis+1,elec_alpha_num
      e_k=diagonal_Fock_matrix_mo(k)
      delta_e_irjsk=delta_e_irjs+e_k
      do t=elec_alpha_num+1,n_act_cis
       e_t=diagonal_Fock_matrix_mo(t)
       delta_e=delta_e_irjsk-e_t
       energy=1.d0/(ref_energy-delta_e)
       occ=i
       a=j
       b=k
       vir=r
       c=s
       d=t
       direct=get_mo_bielec_integral(a,b,c,d,mo_integrals_map)
       phi_tmp=direct-exchg
       do occ=i,k
        if (occ==i) then
         a=j
         b=k
        else if (occ==j) then
         a=i
         b=k
        else if (occ==k) then
         a=i
         b=j 
        else
         cycle 
        end if
        do vir=r,t
         if (vir==r) then
          c=s
          d=t
         else if (vir==s) then
          c=r
          d=t
         else if (vir==t) then
          c=r
          d=s
         else 
          cycle
         end if
          key_tmp=psi_CIS_adress(occ,vir)
          direct=get_mo_bielec_integral(a,b,c,d,mo_integrals_map)
         if (occ==j .and. vir==s .or. occ==k .and. vir==t ) then
          phi_aab(occ,vir)=direct
         else if (occ==i .and. vir==r .or. occ==i .and. vir==t .or. occ==j .and. vir==t .or. occ==k .and. vir==r .or. occ==k .and. vir==s) then
          phi_aab(occ,vir)=0.d0
        !else if (occ/=i .or. occ/=j .or. occ/=k .or. vir/=r .or. vir/=s .or. vir /=t) then
        ! phi_aab(occ,vir)=0.d0

         else
          phi_aab(occ,vir)=-direct
         endif
         
          phi_doub=phi_tmp*phi_aab(occ,vir)*energy
          delta_H_trip(key_ir,key_tmp+1)=delta_H_trip(key_ir,key_tmp+1)+phi_doub
          delta_H_trip(key_ir+1,key_tmp)=delta_H_trip(key_ir,key_tmp+1)+phi_doub
  !     if (phi_doub.ge.1.d-6) then
  !     if (key_ir==key_tmp) then
  !      print*,'ir=tmp beta '
  !      print*,'i,r,j,s,k,t',i,r,j,s,k,t
  !      print*,'occ,vir',occ,vir
  !     !stop
  !     end if
  !     end if

  !     if (phi_doub.ge.1.d-6) then
  !      if (vir==r .or. vir==s) then
  !       print*,'alpha'
  !        print*,'occ,vir,key_tmp',occ,vir,key_tmp

  !      else if (vir==t) then
  !       print*,'beta'
  !        print*,'occ,vir,key_tmp',occ,vir,key_tmp

  !      endif
  !        print*,'vir=?',vir
  !     endif

        enddo
       enddo
      enddo
     enddo
    enddo
   enddo
  enddo
 enddo

!delta_H_trip=0.d0



!!generating the Singles included in the Triples
!! do m=1,n_state_CIS
!do i=n_core_cis+1,elec_alpha_num
! e_i=diagonal_Fock_matrix_mo(i)



!!do r=elec_alpha_num+1,n_state_CIS
! do r=elec_alpha_num+1,n_act_cis
!  e_r=diagonal_Fock_matrix_mo(r)
!  delta_e_ir=e_i-e_r
!key_ir=psi_CIS_adress(i,r)

!  !alpha-alpha-x (=beta-beta-x)
!  do j=i+1,elec_alpha_num
!   e_j=diagonal_Fock_matrix_mo(j)
!   delta_e_irj=delta_e_ir+e_j
!key_jr=psi_CIS_adress(j,r)
!   do s=r+1,n_act_cis
!    e_s=diagonal_Fock_matrix_mo(s)
!    delta_e_irjs=delta_e_irj-e_s

! key_is=psi_CIS_adress(i,s)
! key_js=psi_CIS_adress(j,s)


!    !alpha-alpha-alpha (=beta-beta-beta)
!    do k=j+1,elec_alpha_num
!     e_k=diagonal_Fock_matrix_mo(k)
!     delta_e_irjsk=delta_e_irjs+e_k
!   key_kr=psi_CIS_adress(k,r)
!   key_ks=psi_CIS_adress(k,s)

!     do t=s+1,n_act_cis
!      e_t=diagonal_Fock_matrix_mo(t)
!      delta_e=delta_e_irjsk-e_t

!     key_it=psi_CIS_adress(i,t)
!     key_jt=psi_CIS_adress(j,t)
!     key_kt=psi_CIS_adress(k,t)



!      !generating the connected singles
!      do occ=i,k
!       if (occ==i) then
!        a=j
!        b=k
!       else if (occ==j) then
!        a=i
!        b=k
!       else if (occ==k) then
!        a=i
!        b=j
!       else 
!        cycle       
!       end if

!       do vir=r,t
!        if (vir==r) then
!         c=s
!         d=t
!        else if (vir==s) then
!         c=r
!         d=t
!        else if (vir==t) then
!         c=r
!         d=s
!        else 
!         cycle
!        end if

!        if (occ==i .and. vir==r .or. occ==j .and. vir==s .or. occ==k .and. vir==t ) then 
!        direct=get_mo_bielec_integral(a,b,c,d,mo_integrals_map)
!        exchg =get_mo_bielec_integral(a,b,d,c,mo_integrals_map)

!        phi_aaa(occ,vir)=direct-exchg
!        else
!        direct=get_mo_bielec_integral(a,b,c,d,mo_integrals_map)
!        exchg =get_mo_bielec_integral(a,b,d,c,mo_integrals_map)

!        phi_aaa(occ,vir)=-direct+exchg         
!        endif

!    !   phi_aaa(occ,vir)=phi_aaa(occ,vir)+phi_tmp

!       enddo
!      enddo

!     phi_tmp=(phi_aaa(i,r)*phi_aaa(j,s))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_ir,key_js)=delta_H_trip(key_ir,key_js)+phi_tmp
!     delta_H_trip(key_ir+1,key_js+1)=delta_H_trip(key_ir+1,key_js+1)+phi_tmp

!     phi_tmp=(phi_aaa(i,r)*phi_aaa(k,t))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_ir,key_kt)=delta_H_trip(key_ir,key_kt)+phi_tmp
!     delta_H_trip(key_ir+1,key_kt+1)=delta_H_trip(key_ir+1,key_kt+1)+phi_tmp

!     phi_tmp=(phi_aaa(i,r)*phi_aaa(j,t))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_ir,key_jt)=delta_H_trip(key_ir,key_jt)+phi_tmp
!     delta_H_trip(key_ir+1,key_jt+1)=delta_H_trip(key_ir+1,key_jt+1)+phi_tmp

!     phi_tmp=(phi_aaa(i,r)*phi_aaa(k,s))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_ir,key_ks)=delta_H_trip(key_ir,key_ks)+phi_tmp
!     delta_H_trip(key_ir+1,key_ks+1)=delta_H_trip(key_ir+1,key_ks+1)+phi_tmp


!     phi_tmp=(phi_aaa(i,s)*phi_aaa(j,r))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_is,key_jr)=delta_H_trip(key_is,key_jr)+phi_tmp
!     delta_H_trip(key_is+1,key_jr+1)=delta_H_trip(key_is+1,key_jr+1)+phi_tmp

!     phi_tmp=(phi_aaa(i,s)*phi_aaa(k,t))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_is,key_kt)=delta_H_trip(key_is,key_kt)+phi_tmp
!     delta_H_trip(key_is+1,key_kt+1)=delta_H_trip(key_is+1,key_kt+1)+phi_tmp

!     phi_tmp=(phi_aaa(i,s)*phi_aaa(j,t))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_is,key_jt)=delta_H_trip(key_is,key_jt)+phi_tmp
!     delta_H_trip(key_is+1,key_jt+1)=delta_H_trip(key_is+1,key_jt+1)+phi_tmp

!     phi_tmp=(phi_aaa(i,s)*phi_aaa(k,r))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_is,key_kr)=delta_H_trip(key_is,key_kr)+phi_tmp
!     delta_H_trip(key_is+1,key_kr+1)=delta_H_trip(key_is+1,key_kr+1)+phi_tmp

!     phi_tmp=(phi_aaa(i,t)*phi_aaa(j,s))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_it,key_js)=delta_H_trip(key_it,key_js)+phi_tmp
!     delta_H_trip(key_it+1,key_js+1)=delta_H_trip(key_it+1,key_js+1)+phi_tmp

!     phi_tmp=(phi_aaa(i,t)*phi_aaa(j,r))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_it,key_jr)=delta_H_trip(key_it,key_jr)+phi_tmp
!     delta_H_trip(key_it+1,key_jr+1)=delta_H_trip(key_it+1,key_jr+1)+phi_tmp

!     phi_tmp=(phi_aaa(i,t)*phi_aaa(k,s))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_it,key_ks)=delta_H_trip(key_it,key_ks)+phi_tmp
!     delta_H_trip(key_it+1,key_ks+1)=delta_H_trip(key_it+1,key_ks+1)+phi_tmp

!     phi_tmp=(phi_aaa(i,t)*phi_aaa(k,r))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_it,key_kr)=delta_H_trip(key_it,key_kr)+phi_tmp
!     delta_H_trip(key_it+1,key_kr+1)=delta_H_trip(key_it+1,key_kr+1)+phi_tmp


!     phi_tmp=(phi_aaa(j,r)*phi_aaa(k,t))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_jr,key_kt)=delta_H_trip(key_jr,key_kt)+phi_tmp
!     delta_H_trip(key_jr+1,key_kt+1)=delta_H_trip(key_jr+1,key_kt+1)+phi_tmp

!     phi_tmp=(phi_aaa(j,r)*phi_aaa(k,s))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_jr,key_ks)=delta_H_trip(key_jr,key_ks)+phi_tmp
!     delta_H_trip(key_jr+1,key_ks+1)=delta_H_trip(key_jr+1,key_ks+1)+phi_tmp

!     phi_tmp=(phi_aaa(j,s)*phi_aaa(k,t))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_js,key_kt)=delta_H_trip(key_js,key_kt)+phi_tmp
!     delta_H_trip(key_js+1,key_kt+1)=delta_H_trip(key_js+1,key_kt+1)+phi_tmp

!     phi_tmp=(phi_aaa(j,s)*phi_aaa(k,r))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_js,key_kr)=delta_H_trip(key_js,key_kr)+phi_tmp
!     delta_H_trip(key_js+1,key_kr+1)=delta_H_trip(key_js+1,key_kr+1)+phi_tmp

!     phi_tmp=(phi_aaa(j,t)*phi_aaa(k,s))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_jt,key_ks)=delta_H_trip(key_jt,key_ks)+phi_tmp
!     delta_H_trip(key_jt+1,key_ks+1)=delta_H_trip(key_jt+1,key_ks+1)+phi_tmp

!     phi_tmp=(phi_aaa(j,t)*phi_aaa(k,r))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_jt,key_kr)=delta_H_trip(key_jt,key_kr)+phi_tmp
!     delta_H_trip(key_jt+1,key_kr+1)=delta_H_trip(key_jt+1,key_kr+1)+phi_tmp

!     enddo
!    enddo

!    !alpha-alpha-beta (=beta-beta-alpha)
!    do k=n_core_cis+1,elec_beta_num
!     e_k=diagonal_Fock_matrix_mo(k)
!     delta_e_irjsk=delta_e_irjs+e_k

!  key_kr=psi_CIS_adress(k,r)
!  key_ks=psi_CIS_adress(k,s)



!     do t=elec_beta_num+1,n_act_cis
!      e_t=diagonal_Fock_matrix_mo(t)
!      delta_e=delta_e_irjsk-e_t

!   key_it=psi_CIS_adress(i,t)
!   key_jt=psi_CIS_adress(j,t)
!   key_kt=psi_CIS_adress(k,t)


!      !generating the connected singles alpha (beta)
!      do occ=i,j
!       if (occ==i) then
!        a=j
!        b=k
!       else if (occ==j) then
!        a=i
!        b=k
!       end if

!       do vir = r,s
!        if (vir==r) then
!         c=s
!         d=t
!        else if (vir==s) then
!         c=r
!         d=t
!        end if

!        direct=get_mo_bielec_integral(a,b,c,d,mo_integrals_map)

!        if (occ==i .and. vir==r .or. occ==j .and. vir==s) then
!         phi_aab_alpha(occ,vir)=direct
!        else if (occ==i .and. vir==s .or. occ==j .and. vir==r) then
!         phi_aab_alpha(occ,vir)=-1.d0*direct

!        end if

!!      phi_aab_alpha(occ,vir)=phi_aab_alpha(occ,vir)+phi_tmp

!       enddo
!      enddo

!     !alpha single alpha single connected
!      phi_tmp=(phi_aab_alpha(i,r)*phi_aab_alpha(j,s))/(ref_energy-delta_e-eigenvalues_CIS(1))
!      delta_H_trip(key_ir,key_js)=delta_H_trip(key_ir,key_js)+phi_tmp
!      delta_H_trip(key_ir+1,key_js+1)=delta_H_trip(key_ir+1,key_js+1)+phi_tmp


!   ! phi_tmp=(phi_aab_alpha(i,r)*phi_aab_alpha(i,s))/(ref_energy-delta_e-eigenvalues_CIS(1))
!   ! delta_H_trip(key_ir,key_is)=delta_H_trip(key_ir,key_is)+phi_tmp
!   ! delta_H_trip(key_ir+1,key_is+1)=delta_H_trip(key_ir+1,key_is+1)+phi_tmp


!     phi_tmp=(phi_aab_alpha(i,s)*phi_aab_alpha(j,r))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_is,key_jr)=delta_H_trip(key_is,key_jr)+phi_tmp
!     delta_H_trip(key_is+1,key_jr+1)=delta_H_trip(key_is+1,key_jr+1)+phi_tmp





!      !generating the conncected singles beta (alpha)
!   !  do occ=i,k
!   !    if (occ==i) then
!   !    a=j
!   !    b=k
!   !   else if (occ==j) then
!   !    a=i
!   !    b=k
!   !   else if (occ==k) then
!   !    
!   !    a=i
!   !    b=j
!   !   end if
!   !  do vir=r,t
!   ! if (vir==r) then
!   !     c=s
!   !     d=t
!   !    else if (vir==s) then
!   !     c=r
!   !     d=t
!   !    else if (vir==k) then 
!   !     
!   !     c=r
!   !     d=t
!   !    end if

!    occ=k
!     a=i
!     b=j
!    vir=t
!     c=r
!     d=s
!      direct=get_mo_bielec_integral(a,b,c,d,mo_integrals_map)
!      exchg =get_mo_bielec_integral(a,b,d,c,mo_integrals_map)

!      phi_aab_beta(occ,vir)=direct-exchg
!   !   enddo
!   ! enddo
!!     phi_aab_beta(occ,vir)=phi_aab_beta(occ,vir)+phi_tmp
!  




!     !alpha single beta single connected
!     phi_tmp=(phi_aab_alpha(i,r)*phi_aab_beta(k,t))/(ref_energy-delta_e-eigenvalues_CIS(1))
!      delta_H_trip(key_ir,key_kt+1)=delta_H_trip(key_ir,key_kt+1)+phi_tmp
!      delta_H_trip(key_ir+1,key_kt)=delta_H_trip(key_ir+1,key_kt)+phi_tmp

!     phi_tmp=(phi_aab_alpha(i,s)*phi_aab_beta(k,t))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_is,key_kt+1)=delta_H_trip(key_is,key_kt+1)+phi_tmp
!     delta_H_trip(key_is+1,key_kt)=delta_H_trip(key_is+1,key_kt)+phi_tmp


!     phi_tmp=(phi_aab_alpha(j,r)*phi_aab_beta(k,t))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_jr,key_kt+1)=delta_H_trip(key_jr,key_kt+1)+phi_tmp
!     delta_H_trip(key_jr+1,key_kt)=delta_H_trip(key_jr+1,key_kt)+phi_tmp


!     phi_tmp=(phi_aab_alpha(j,s)*phi_aab_beta(k,t))/(ref_energy-delta_e-eigenvalues_CIS(1))
!     delta_H_trip(key_js+1,key_kt)=delta_H_trip(key_js+1,key_kt)+phi_tmp
!     delta_H_trip(key_js,key_kt+1)=delta_H_trip(key_js,key_kt+1)+phi_tmp


!     enddo 
!    enddo
!   enddo
!  enddo
! enddo
!enddo

 END

subroutine diexcitation(i,j,k,l,ispin1,ispin2,key_in,key_out,i_ok,Nint)
  implicit none
  use bitmasks
  ! realize the double excitation i-->k (ispin1) +  j-->l (ispin2)  on key_in
  ! returns key_out and i_ok (i_ok = 0 means not possible, i_ok = 1 means the excitation was possible)
  integer, intent(in) :: ispin1,ispin2,i,j,k,l,Nint
  integer(bit_kind), intent(in) :: key_in(Nint,2)
  integer, intent(out):: i_ok
  integer(bit_kind), intent(out):: key_out(Nint,2)
  integer :: k_hole,j_hole,k_particl,j_particl,i_nint,Nelec_alpha,Nelec_beta
  integer :: i_test_hole,i_test_particl
  key_out = key_in

  k_hole = ishft(i-1,-bit_kind_shift)+1
  j_hole = i-ishft(k_hole-1,bit_kind_shift)-1
  i_test_hole = ibset(0,j_hole)
  if(iand(key_in(k_hole,ispin1),i_test_hole).ne.i_test_hole)then
   i_ok = 0
   return
  endif
  key_out(k_hole,ispin1) = ibclr(key_out(k_hole,ispin1),j_hole)
  k_particl = ishft(k-1,-bit_kind_shift)+1
  j_particl = k-ishft(k_particl-1,bit_kind_shift)-1
  i_test_particl= ibset(0,j_particl)
  if(iand(key_in(k_particl,ispin1),i_test_particl).ne.0)then
   i_ok = 0
   return
  endif
  key_out(k_particl,ispin1) = ibset(key_out(k_particl,ispin1),j_particl)

  k_hole = ishft(j-1,-bit_kind_shift)+1
  j_hole = j-ishft(k_hole-1,bit_kind_shift)-1
  i_test_hole = ibset(0,j_hole)
  if(iand(key_in(k_hole,ispin2),i_test_hole).ne.i_test_hole)then
   i_ok = 0
   return
  endif
  key_out(k_hole,ispin2) = ibclr(key_out(k_hole,ispin2),j_hole)
  k_particl = ishft(l-1,-bit_kind_shift)+1
  j_particl = l-ishft(k_particl-1,bit_kind_shift)-1
  i_test_particl = ibset(0,j_particl)
  if(iand(key_in(k_particl,ispin2),i_test_particl).ne.0)then
   i_ok = 0
   return
  endif
  key_out(k_particl,ispin2) = ibset(key_out(k_particl,ispin2),j_particl)
  i_ok = 1
  end


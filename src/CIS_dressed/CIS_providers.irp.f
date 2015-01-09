
   use bitmasks
 BEGIN_PROVIDER [integer(bit_kind), psi_CIS,(N_int,2,size_psi_CIS)]
 &BEGIN_PROVIDER [integer, psi_CIS_holes,(size_psi_CIS)]
 &BEGIN_PROVIDER [integer, psi_CIS_particl,(size_psi_CIS)]
 &BEGIN_PROVIDER [integer, psi_CIS_spin,(size_psi_CIS)]
 &BEGIN_PROVIDER [integer, psi_CIS_adress,(n_core_cis+1:elec_alpha_num,elec_alpha_num+1:n_act_cis)]
 &BEGIN_PROVIDER [double precision, H_CIS,(size_psi_CIS,size_psi_CIS)]
 BEGIN_DOC
 !key of the CIS-matrix
 END_DOC


 implicit none
 integer :: a !control variable
 integer :: i,j,k,l !variables for going over the occupied (i,j) and virutal (k,l)
 integer :: key !key for CIS-matrix
 integer :: i_hole,j_hole,ispin,l_particle,k_particle
 double precision :: hij


 do a=1,N_int
  psi_CIS(a,1,1)=ref_bitmask(a,1)
  psi_CIS(a,2,1)=ref_bitmask(a,2)
 enddo
 
 psi_CIS_holes(1) = 0
 psi_CIS_particl(1) = 0
 psi_CIS_spin(1) = 0

 !loop on particles: create a particle in k
 do k=elec_alpha_num+1,n_act_cis

  !loop on holes: destroy a particle in i
  do i=n_core_cis+1,elec_alpha_num

   !alpha spin
   ispin=1

   key=2*((k-elec_alpha_num-1)*(elec_alpha_num-n_core_cis) + i-n_core_cis) !index of such an excited determinant in the CIS WF
   psi_CIS_adress(i,k)=key

   do a=1,N_int
    psi_CIS(a,1,key)=ref_bitmask(a,1)
    psi_CIS(a,2,key)=ref_bitmask(a,2)
   enddo

   j_hole=ishft(i-1,-bit_kind_shift)+1
   i_hole=i-ishft(j_hole-1,bit_kind_shift)-1

   psi_CIS(j_hole,ispin,key)=ibclr(psi_CIS(j_hole,ispin,key),i_hole)

   l_particle=ishft(k-1,-bit_kind_shift)+1
   k_particle=k-ishft(l_particle-1,bit_kind_shift)-1

   psi_CIS(l_particle,ispin,key)=ibset(psi_CIS(l_particle,ispin,key),k_particle)
   
   psi_CIS_holes(key) = i
   psi_CIS_particl(key) = k
   psi_CIS_spin(key) = 1

   !beta spin
   ispin=2

   key=key+1

   do a=1,N_int
    psi_CIS(a,1,key)=ref_bitmask(a,1)
    psi_CIS(a,2,key)=ref_bitmask(a,2)
   enddo

   j_hole=ishft(i-1,-bit_kind_shift)+1
   i_hole=i-ishft(j_hole-1,bit_kind_shift)-1

   psi_CIS(j_hole,ispin,key)=ibclr(psi_CIS(j_hole,ispin,key),i_hole)

   l_particle=ishft(k-1,-bit_kind_shift)+1
   k_particle=k-ishft(l_particle-1,bit_kind_shift)-1
   psi_CIS_holes(key) = i
   psi_CIS_particl(key) = k
   psi_CIS_spin(key) = 2

   psi_CIS(l_particle,ispin,key)=ibset(psi_CIS(l_particle,ispin,key),k_particle)
  enddo
 enddo

 !Building the CIS-matrix
 double precision :: diag_H_mat_elem

 do key=1,size_psi_CIS
  H_CIS(key,key)=diag_H_mat_elem(psi_CIS(1,1,key),N_int)

  do a=key+1,size_psi_CIS
   call i_H_j(psi_CIS(1,1,a),psi_CIS(1,1,key),N_int,hij)

   H_CIS(key,a)=hij
   H_CIS(a,key)=hij
  enddo
 enddo
 
 END_PROVIDER


 BEGIN_PROVIDER[double precision, eigenvalues_CIS,(n_state_CIS)]
&BEGIN_PROVIDER[double precision, coefs_CIS, (size_psi_CIS,n_state_CIS)]
&BEGIN_PROVIDER[double precision, s_2_CIS,(n_state_CIS)]
   use bitmasks

 BEGIN_DOC
 !the first states of the CIS matrix
 END_DOC

 implicit none
 integer :: i,j,k
 double precision :: eigvalues(size_psi_CIS),eigvectors(size_psi_CIS,size_psi_CIS)
 double precision :: coefs_tmp(size_psi_CIS)
 double precision :: s2


 !Diagonalisation of CIS-matrix 
 call lapack_diag(eigvalues,eigvectors,H_CIS,size_psi_CIS,size_psi_CIS)
 
 do i = 1,n_state_CIS
  eigenvalues_CIS(i) = eigvalues(i)

  do k=1,size_psi_CIS

  if (dabs(eigvectors(k,i)).ge.10.d-2) then
   write(11,*),'k,i,eigenvectors(k,i)=',k,i,eigvectors(k,i)
   write(11,*),'hole,particl,spin:',psi_CIS_holes(k),psi_CIS_particl(k),psi_CIS_spin(k) 
   write(11,*),''
  endif
   coefs_tmp(k) = eigvectors(k,i)
   coefs_CIS(k,i)=eigvectors(k,i)
  enddo
  call get_s2_u0(psi_CIS,coefs_tmp,size_psi_CIS,size_psi_CIS,s2)
  s_2_CIS(i) = s2
 enddo
 
 END_PROVIDER

 
 BEGIN_PROVIDER [double precision, eigenvalues_CIS_dress_D,(n_state_CIS)]
&BEGIN_PROVIDER [double precision, s_2_CIS_dress_D,(n_state_CIS)]
&BEGIN_PROVIDER [double precision, eigenvectors_CIS_dress_D,(size_psi_CIS,n_state_CIS)]
&BEGIN_PROVIDER [double precision, overlap_D ]
  use bitmasks

 BEGIN_DOC
 !The first states of the CIS matrix dressed by the doubles
 END_DOC
 implicit none
 double precision,allocatable  :: delta_H_matrix_doub(:,:)
 double precision,allocatable  :: eigvalues(:),eigvectors(:,:)
 double precision :: overlap,max_overlap,s2
 integer :: i_overlap,i,j,k
 allocate (delta_H_matrix_doub(size_psi_CIS,size_psi_CIS))
 allocate(eigvalues(size_psi_CIS),eigvectors(size_psi_CIS,size_psi_CIS))
  do i = 1,n_state_CIS
   call dress_by_doubles(eigenvalues_CIS(i),coefs_CIS(1,i),delta_H_matrix_doub,size_psi_CIS) !dressing of the Doubles
   do j = 1,size_psi_CIS
    do k = 1,size_psi_CIS
     delta_H_matrix_doub(j,k) += H_CIS(j,k)
    enddo
   enddo
   call lapack_diag(eigvalues,eigvectors,delta_H_matrix_doub,size_psi_CIS,size_psi_CIS)

   ! state following
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
 ! print*,'overlap = ',max_overlap
   overlap_D=max_overlap
   do k = 1,size_psi_CIS
    eigenvectors_CIS_dress_D(k,i) = eigvectors(k,i_overlap)
    if (dabs(eigvectors(k,i_overlap)).ge.10.d-2) then
     write(12,*),'k,i,eigenvectors(k,i)=',k,i,eigvectors(k,i_overlap)
     write(12,*),'hole,particl,spin:',psi_CIS_holes(k),psi_CIS_particl(k),psi_CIS_spin(k) 
     write(12,*),''
    endif
   enddo
   call get_s2_u0(psi_CIS,eigenvectors_CIS_dress_D(1,i),size_psi_CIS,size_psi_CIS,s2)
   s_2_CIS_dress_D(i) = s2
   eigenvalues_CIS_dress_D(i) = eigvalues(i_overlap)
  enddo

 END_PROVIDER



 BEGIN_PROVIDER [double precision, eigenvalues_CIS_dress_D_dt,(n_state_CIS)]
&BEGIN_PROVIDER [double precision, s_2_CIS_dress_D_dt,(n_state_CIS)]
&BEGIN_PROVIDER [double precision, eigenvectors_CIS_dress_D_dt,(size_psi_CIS,n_state_CIS)
&BEGIN_PROVIDER [double precision, overlap_Ddt]
  use bitmasks

 BEGIN_DOC
 !The first states of the CIS matrix dressed by the doubles and the disconnected triples
 END_DOC
 implicit none
 double precision,allocatable  :: delta_H_matrix_doub(:,:)
 double precision,allocatable  :: eigvalues(:),eigvectors(:,:)
 double precision :: overlap,max_overlap,s2,e_corr
 integer :: i_overlap,i,j,k
 allocate (delta_H_matrix_doub(size_psi_CIS,size_psi_CIS))
 allocate(eigvalues(size_psi_CIS),eigvectors(size_psi_CIS,size_psi_CIS))
 eigenvalues_CIS_dress_D_dt(1) = eigenvalues_cis(1) + dress_T_discon_array_CIS(1)
 eigenvectors_CIS_dress_D_dt(1,1) = 1.d0
 s_2_CIS_dress_D_dt = 0.d0
 print*,'eigenvalues_CIS_dress_D_dt(i)= ',eigenvalues_CIS_dress_D_dt(1) + nuclear_repulsion
 do i = 2, size_psi_CIS
  eigenvectors_CIS_dress_D_dt(i,1) = 0.d0
 enddo
  do i = 2,n_state_CIS
   call dress_by_doubles(eigenvalues_CIS(i),coefs_CIS(1,i),delta_H_matrix_doub,size_psi_CIS) !dressing of the Doubles
!  delta_H_matrix_doub = 0.d0
   
   do j = 1,size_psi_CIS
    do k = 1,size_psi_CIS
     delta_H_matrix_doub(j,k) += H_CIS(j,k)
    enddo
    delta_H_matrix_doub(j,j) += dress_T_discon_array_CIS(j)
   enddo
   do j = 2, size_psi_CIS
    delta_H_matrix_doub(1,j) = 0.d0
   enddo

   double precision :: accu
   accu = 0.d0
   do j = 1, size_psi_CIS
    do k = 1, size_psi_CIS
     accu += delta_H_matrix_doub(j,k) * coefs_CIS(j,i) * coefs_CIS(k,i)
    enddo
   enddo
   call lapack_diag(eigvalues,eigvectors,delta_H_matrix_doub,size_psi_CIS,size_psi_CIS)

   ! state following
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
   print*,i,i_overlap
   print*,'overlap = ',max_overlap
   overlap_Ddt=max_overlap
   do k = 1,size_psi_CIS
    eigenvectors_CIS_dress_D_dt(k,i) = eigvectors(k,i_overlap)
    if (dabs(eigvectors(k,i_overlap)).ge.10.d-2) then
     write(13,*),'k,i,eigenvectors(k,i)=',k,i,eigvectors(k,i_overlap)
     write(13,*),'hole,particl,spin:',psi_CIS_holes(k),psi_CIS_particl(k),psi_CIS_spin(k) 
     write(13,*),''
    endif
   enddo
   call get_s2_u0(psi_CIS,eigenvectors_CIS_dress_D_dt(1,i),size_psi_CIS,size_psi_CIS,s2)
   s_2_CIS_dress_D_dt(i) = s2
   eigenvalues_CIS_dress_D_dt(i) = eigvalues(i_overlap)
   print*,'eigenvalues_CIS_dress_D_dt(i)= ',eigenvalues_CIS_dress_D_dt(i) + nuclear_repulsion
   print*,'Perturbative                 = ',accu+ nuclear_repulsion
   print*,'eigenvalues_CIS              = ',eigenvalues_CIS(i)+ nuclear_repulsion
  enddo

 END_PROVIDER

!BEGIN_PROVIDER [double precision, eigenvalues_CIS_dress_tot,(n_state_CIS)]
!BEGIN_PROVIDER [double precision, s_2_CIS_dress_tot,(n_state_CIS)]
!BEGIN_PROVIDER [double precision, eigenvectors_CIS_dress_tot,(size_psi_CIS,n_state_CIS)]
!BEGIN_PROVIDER [double precision, overlap_tot] 

!BEGIN_DOC
!!The first states of the CIS matrix dressed by the doubles
!END_DOC
!implicit none
!double precision,allocatable  :: delta_H_matrix_doub(:,:)
!double precision,allocatable  :: eigvalues(:),eigvectors(:,:)
!double precision,allocatable  :: delta_H_trip(:,:)
!double precision :: overlap,max_overlap,s2,average_eigvalue
!integer :: i_overlap,i,j,k,m
!allocate (delta_H_matrix_doub(size_psi_CIS,size_psi_CIS),delta_H_trip(size_psi_CIS,size_psi_CIS) )
!allocate(eigvalues(size_psi_CIS),eigvectors(size_psi_CIS,size_psi_CIS))
! do i = 1,n_state_CIS
!  call dress_by_doubles(eigenvalues_CIS(i),coefs_CIS(1,i),delta_H_matrix_doub,size_psi_CIS) !dressing of the Doubles
!  call dress_T_con(eigenvalues_CIS(i),delta_H_trip,size_psi_CIS)
!  do j = 1,size_psi_CIS
!   do k = 1,size_psi_CIS
!    delta_H_matrix_doub(j,k) += H_CIS(j,k)
!    delta_H_matrix_doub(j,k) += delta_H_trip(j,k)
!   enddo
!   delta_H_matrix_doub(j,j) += dress_T_discon_array_CIS(j)
!  enddo
!  call lapack_diag(eigvalues,eigvectors,delta_H_matrix_doub,size_psi_CIS,size_psi_CIS)
!  do m=1,n_state_CIS
!   write(12,*),'m,eigvalues(m)',m,eigvalues(m)
!  enddo
!  ! state following
!  max_overlap = 0.d0
!  do k = 1, size_psi_CIS
!   overlap = 0.d0
!   do j = 1,size_psi_CIS
!    overlap += eigvectors(j,k)*coefs_CIS(j,i)
!   enddo
!   if(dabs(overlap).gt.max_overlap)then
!    max_overlap = dabs(overlap)
!    i_overlap = k
!   endif
!   ! <CIS(i)|state(k)>
!  enddo
!!  print*,'overlap = ',max_overlap
!  overlap_tot=max_overlap
!  do k = 1,size_psi_CIS
!   eigenvectors_CIS_dress_tot(k,i) = eigvectors(k,i_overlap)
!  enddo
!  call get_s2_u0(psi_CIS,eigenvectors_CIS_dress_tot(1,i),size_psi_CIS,size_psi_CIS,s2)
!  s_2_CIS_dress_tot(i) = s2
!  eigenvalues_CIS_dress_tot(i) = eigvalues(i_overlap)
! enddo

!END_PROVIDER








 BEGIN_PROVIDER [double precision, diag_elements, (size_psi_CIS)]
  use bitmasks


 BEGIN_DOC
 !Array of the energy of the CIS determinants ordered in the CIS matrix
 END_DOC

 implicit none
 double precision :: hij
 integer :: i
 
 do i = 1, size_psi_CIS
  call i_H_j(psi_CIS(1,1,i),psi_CIS(1,1,i),N_int,hij)
  diag_elements(i) = hij
 enddo
 
 END_PROVIDER

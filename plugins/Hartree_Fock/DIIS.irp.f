begin_provider [double precision, FPS_SPF_Matrix_AO, (AO_num_align, AO_num)]
  implicit none
  begin_doc 
!   Commutator FPS - SPF
  end_doc
  double precision, allocatable:: scratch(:,:)
  allocate(                       &
    scratch(AO_num_align, AO_num) &
  )

! Compute FP

  call dgemm('N','N',AO_num,AO_num,AO_num,                &
       1.d0,                                              &
       Fock_Matrix_AO,Size(Fock_Matrix_AO,1),             &
       HF_Density_Matrix_AO,Size(HF_Density_Matrix_AO,1), &
       0.d0,                                              & 
       scratch,Size(scratch,1))

! Compute FPS

  call dgemm('N','N',AO_num,AO_num,AO_num, &
       1.d0,                               &
       scratch,Size(scratch,1),            & 
       AO_Overlap,Size(AO_Overlap,1),      &
       0.d0,                               &
       FPS_SPF_Matrix_AO,Size(FPS_SPF_Matrix_AO,1))

! Compute SP

  call dgemm('N','N',AO_num,AO_num,AO_num,                &
       1.d0,                                              &
       AO_Overlap,Size(AO_Overlap,1),                     & 
       HF_Density_Matrix_AO,Size(HF_Density_Matrix_AO,1), &
       0.d0,                                              & 
       scratch,Size(scratch,1))

! Compute FPS - SPF

  call dgemm('N','N',AO_num,AO_num,AO_num,     &
       -1.d0,                                  & 
       scratch,Size(scratch,1),                &
       Fock_Matrix_AO,Size(Fock_Matrix_AO,1),  &
       1.d0,                                   & 
       FPS_SPF_Matrix_AO,Size(FPS_SPF_Matrix_AO,1))

end_provider


 BEGIN_PROVIDER [ double precision, eigenvalues_Fock_matrix_AO, (AO_num) ]
&BEGIN_PROVIDER [ double precision, eigenvectors_Fock_matrix_AO, (AO_num_align,AO_num) ]

   BEGIN_DOC
   ! Eigenvalues and eigenvectors of the Fock matrix over the AO basis
   END_DOC

   implicit none
   
   double precision, allocatable  :: scratch(:,:),work(:),Xt(:,:)
   integer                        :: lwork,info
   integer                        :: i,j
   
   lwork = 3*AO_num - 1
   allocate(                       &
     scratch(AO_num_align,AO_num), &
     work(lwork),                  &
     Xt(AO_num,AO_num)             &
   )
 
! Calculate Xt

  do i=1,AO_num
    do j=1,AO_num
      Xt(i,j) = X_Matrix_AO(j,i)
    enddo
  enddo

! Calculate Fock matrix in orthogonal basis: F' = Xt.F.X

  call dgemm('N','N',AO_num,AO_num,AO_num,     &
       1.d0,                                   &
       Fock_matrix_AO,size(Fock_matrix_AO,1),  &
       X_Matrix_AO,size(X_Matrix_AO,1),        &
       0.d0,                                   &
       eigenvectors_Fock_matrix_AO,size(eigenvectors_Fock_matrix_AO,1))       

  call dgemm('N','N',AO_num,AO_num,AO_num,                              &
       1.d0,                                                            &
       Xt,size(Xt,1),                                                   &
       eigenvectors_Fock_matrix_AO,size(eigenvectors_Fock_matrix_AO,1), &
       0.d0,                                                            &
       scratch,size(scratch,1))
     
! Diagonalize F' to obtain eigenvectors in orthogonal basis C' and eigenvalues
  
   call dsyev('V','U',AO_num,       &
        scratch,size(scratch,1),    &
        eigenvalues_Fock_matrix_AO, &
        work,lwork,info)
 
   if(info /= 0) then
     print *,  irp_here//' failed : ', info
     stop 1
   endif

! Back-transform eigenvectors: C =X.C'

  call dgemm('N','N',AO_num,AO_num,AO_num,     &
       1.d0,                                   &
       X_matrix_AO,size(X_matrix_AO,1),        &
       scratch,size(scratch,1),                &
       0.d0,                                   &
       eigenvectors_Fock_matrix_AO,size(eigenvectors_Fock_matrix_AO,1))       
   
END_PROVIDER

BEGIN_PROVIDER [ double precision, X_matrix_AO, (AO_num_align,AO_num) ]

  BEGIN_DOC
!   Matrix X = S^{-1/2} obtained by SVD
  END_DOC

  implicit none
  
  integer                         :: LDA, LDC
  double precision, allocatable   :: U(:,:),Vt(:,:), D(:)
  integer                         :: info, i, j, k
 
  LDA = size(AO_overlap,1)
  LDC = size(AO_overlap,1)
 
  allocate(         &
    U(LDC,AO_num),  &
    Vt(LDA,AO_num), &
    D(AO_num))

  call svd(            &
       AO_overlap,LDA, &
       U,LDC,          &
       D,              &
       Vt,LDA,         &
       AO_num,AO_num)

  do i=1,AO_num
    if(abs(D(i)) < threshold_overlap_AO_eigenvalues) then
      D(i) = 0.d0
    else
      D(i) = 1.d0/sqrt(D(i))
    endif
    do j=1,AO_num
      X_matrix_AO(j,i) = 0.d0
    enddo
  enddo

  do k=1,AO_num
    if(D(k) /= 0.d0) then
      do j=1,AO_num
        do i=1,MO_tot_num
          X_matrix_AO(i,j) = X_matrix_AO(i,j) + U(i,k)*D(k)*Vt(k,j)
        enddo
      enddo
    endif
  enddo
  

END_PROVIDER

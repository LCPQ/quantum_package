subroutine Roothaan_Hall_SCF

BEGIN_DOC
! Roothaan-Hall algorithm for SCF Hartree-Fock calculation
END_DOC

  implicit none

  double precision               :: energy_SCF,energy_SCF_previous,Delta_energy_SCF,max_error_DIIS
  double precision, allocatable  :: Fock_matrix_DIIS(:,:,:),error_matrix_DIIS(:,:,:)

  integer                        :: iteration_SCF,dim_DIIS,index_dim_DIIS
 
  integer                        :: i,j
 
  allocate(                                       &
    Fock_matrix_DIIS(AO_num,AO_num,max_dim_DIIS), &
    error_matrix_DIIS(AO_num,AO_num,max_dim_DIIS) &
  )

  call write_time(output_hartree_fock)

  write(output_hartree_fock,'(A4, 1X, A16, 1X, A16, 1X, A16)')  &
    '====','================','================','================'
  write(output_hartree_fock,'(A4, 1X, A16, 1X, A16, 1X, A16)')  &
    '  N ', 'Energy  ', 'Energy diff  ', 'DIIS error  '
  write(output_hartree_fock,'(A4, 1X, A16, 1X, A16, 1X, A16)')  &
    '====','================','================','================'

! Initialize energies and density matrices

  energy_SCF_previous = HF_energy
  Delta_energy_SCF = 0.d0
  iteration_SCF = 0
  dim_DIIS = 0
  max_error_DIIS = 1.d0

!
! Start of main SCF loop
!
  do while((max_error_DIIS > threshold_DIIS) .and. (iteration_SCF < n_it_SCF_max))

! Increment cycle number

    iteration_SCF += 1

! Current size of the DIIS space

    dim_DIIS = min(dim_DIIS+1,max_dim_DIIS)
 
! Store Fock and error matrices at each iteration
    
    do j=1,AO_num
      do i=1,AO_num
        index_dim_DIIS = mod(dim_DIIS-1,max_dim_DIIS)+1
        Fock_matrix_DIIS(i,j,index_dim_DIIS) = Fock_matrix_AO(i,j)
        error_matrix_DIIS(i,j,index_dim_DIIS) = FPS_SPF_Matrix_AO(i,j)
      enddo
    enddo
    
! Compute the extrapolated Fock matrix

  call extrapolate_Fock_matrix(         &
    error_matrix_DIIS,Fock_matrix_DIIS, &
    iteration_SCF,dim_DIIS              &
  ) 

    touch Fock_matrix_AO

    MO_coef = eigenvectors_Fock_matrix_AO

! This algorithm still have an issue with linear dependencies
!   do i=1,AO_num
!     write(*,*) i,eigenvalues_Fock_matrix_AO(i)
!   enddo

    touch MO_coef

! Calculate error vectors

    max_error_DIIS = maxval(Abs(FPS_SPF_Matrix_AO))

!   SCF energy

    energy_SCF = HF_energy
    Delta_Energy_SCF = energy_SCF - energy_SCF_previous
    energy_SCF_previous = energy_SCF

! Print results at the end of each iteration

    write(output_hartree_fock,'(I4, 1X, F16.10, 1X, F16.10, 1X, F16.10)')  &
      iteration_SCF, energy_SCF, Delta_energy_SCF, max_error_DIIS

  enddo

!
! End of Main SCF loop
!

  write(output_hartree_fock,'(A4, 1X, A16, 1X, A16, 1X, A16)') &
    '====','================','================','================'
  write(output_hartree_fock,*)
  
  if(.not.no_oa_or_av_opt)then
   call mo_as_eigvectors_of_mo_matrix(Fock_matrix_mo,size(Fock_matrix_mo,1),size(Fock_matrix_mo,2),mo_label,1)
  endif

  call write_double(output_hartree_fock, Energy_SCF, 'Hartree-Fock energy')
  call ezfio_set_hartree_fock_energy(Energy_SCF)

  call write_time(output_hartree_fock)

end

subroutine extrapolate_Fock_matrix(   &
  error_matrix_DIIS,Fock_matrix_DIIS, &
  iteration_SCF,dim_DIIS              &
) 

BEGIN_DOC
! Compute the extrapolated Fock matrix using the DIIS procedure
END_DOC

  implicit none

  double precision,intent(in)   :: Fock_matrix_DIIS(AO_num,AO_num,*),error_matrix_DIIS(AO_num,AO_num,*)
  integer,intent(in)            :: iteration_SCF
  integer,intent(inout)         :: dim_DIIS

  double precision,allocatable  :: B_matrix_DIIS(:,:),X_vector_DIIS(:)

  double precision,allocatable  :: scratch(:,:)
  integer                       :: i,j,k,i_DIIS,j_DIIS

  allocate(                               &
    B_matrix_DIIS(dim_DIIS+1,dim_DIIS+1), &
    X_vector_DIIS(dim_DIIS+1),            &
    scratch(AO_num,AO_num)                &
  )

! Compute the matrices B and X
  do j=1,dim_DIIS
    do i=1,dim_DIIS

      j_DIIS = mod(iteration_SCF-j,max_dim_DIIS)+1
      i_DIIS = mod(iteration_SCF-i,max_dim_DIIS)+1

! Compute product of two errors vectors

      call dgemm('N','N',AO_num,AO_num,AO_num,                      &
           1.d0,                                                    &
           error_matrix_DIIS(1,1,i_DIIS),size(error_matrix_DIIS,1), &
           error_matrix_DIIS(1,1,j_DIIS),size(error_matrix_DIIS,1), &
           0.d0,                                                    &
           scratch,size(scratch,1))                                                  

! Compute Trace

      B_matrix_DIIS(i,j) = 0.d0
      do k=1,AO_num
        B_matrix_DIIS(i,j) += scratch(k,k)
      enddo
    enddo
  enddo

! Pad B matrix and build the X matrix

  do i=1,dim_DIIS
    B_matrix_DIIS(i,dim_DIIS+1) = -1.d0
    B_matrix_DIIS(dim_DIIS+1,i) = -1.d0
    X_vector_DIIS(i) = 0.d0
  enddo
  B_matrix_DIIS(dim_DIIS+1,dim_DIIS+1) = 0.d0
  X_vector_DIIS(dim_DIIS+1) = -1.d0

! Solve the linear system C = B.X

  integer              :: info
  integer,allocatable  :: ipiv(:)

  allocate(          &
    ipiv(dim_DIIS+1) &
  )

  call dsysv('U',dim_DIIS+1,1,           &
    B_matrix_DIIS,size(B_matrix_DIIS,1), &
    ipiv,                                &
    X_vector_DIIS,size(X_vector_DIIS,1), &
    scratch,size(scratch),               &
    info                                 &
  )

 if(info == 0) then
   
! Compute extrapolated Fock matrix

    Fock_matrix_AO(:,:) = 0.d0

    do k=1,dim_DIIS
      do j=1,AO_num
        do i=1,AO_num
          Fock_matrix_AO(i,j) += X_vector_DIIS(k)*Fock_matrix_DIIS(i,j,dim_DIIS-k+1)
        enddo
      enddo
    enddo

  else
    write(*,*) 'Re-initialize DIIS!!'
    dim_DIIS = 0
  endif

! do i=1,AO_num
!   do j=1,AO_num
!     write(*,*) Fock_matrix_AO(i,j)
!   enddo
! enddo

end

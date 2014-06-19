 BEGIN_PROVIDER [ double precision, diagonal_Fock_matrix_mo, (mo_tot_num) ]
&BEGIN_PROVIDER [ double precision, eigenvectors_Fock_matrix_mo, (ao_num_align,mo_tot_num) ]
   implicit none
   BEGIN_DOC
   ! Diagonal Fock matrix in the MO basis
   END_DOC
   
   integer                        :: i,j
   integer                        :: liwork, lwork, n, info
   integer, allocatable           :: iwork(:)
   double precision, allocatable  :: work(:), F(:,:), S(:,:)
   
   allocate(F(ao_num_align,ao_num), S(ao_num_align,ao_num) )
   do j=1,ao_num
     do i=1,ao_num
       S(i,j) = ao_overlap(i,j)
       F(i,j) = Fock_matrix_ao(i,j)
     enddo
   enddo

   n = ao_num
   lwork = 1+6*n + 2*n*n
   liwork = 3 + 5*n
   
   allocate(work(lwork), iwork(liwork) )

   lwork = -1
   liwork = -1
   call dsygvd(1,'v','u',mo_tot_num,F,size(F,1),S,size(S,1),&
     diagonal_Fock_matrix_mo, work, lwork, iwork, liwork, info)

   if (info /= 0) then
     print *,  irp_here//' failed'
     stop 1
   endif
   lwork = int(work(1))
   liwork = iwork(1)
   deallocate(work,iwork)
   allocate(work(lwork), iwork(liwork) )

   call dsygvd(1,'v','u',mo_tot_num,F,size(F,1),S,size(S,1),&
     diagonal_Fock_matrix_mo, work, lwork, iwork, liwork, info)

   if (info /= 0) then
     print *,  irp_here//' failed'
     stop 1
   endif
   do j=1,ao_num
     do i=1,ao_num
       eigenvectors_Fock_matrix_mo(i,j) = F(i,j)
     enddo
   enddo

   deallocate(work, iwork, F, S)
END_PROVIDER
 

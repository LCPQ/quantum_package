 BEGIN_PROVIDER [ double precision, one_body_dm_mo_diff_eigvalues,  (mo_tot_num, 2:N_states) ]
&BEGIN_PROVIDER [ double precision, one_body_dm_mo_diff_eigvectors, (mo_tot_num, mo_tot_num, 2:N_states) ]
 implicit none
 BEGIN_DOC
 ! Eigenvalues and eigenvectors of one_body_dm_mo_diff
 END_DOC
 integer                        :: i,j,istate
 integer                        :: liwork, lwork, n, info
 integer, allocatable           :: iwork(:)
 double precision, allocatable  :: work(:)
 
 
 one_body_dm_mo_diff_eigvectors(1:mo_tot_num, 1:mo_tot_num, 2:N_states) =&
     one_body_dm_mo_diff(1:mo_tot_num, 1:mo_tot_num, 2:N_states)
 
 n = mo_tot_num
 lwork = 1+6*n + 2*n*n
 liwork = 3 + 5*n
 
 allocate(work(lwork), iwork(liwork))
 
 lwork=-1
 liwork=-1
 istate=2
 
 call dsyevd( 'V', 'U', mo_tot_num,                                  &
     one_body_dm_mo_diff_eigvectors(1,1,istate),                     &
     size(one_body_dm_mo_diff_eigvectors,1),                         &
     one_body_dm_mo_diff_eigvalues(1,istate),                        &
     work, lwork, iwork, liwork, info)
 
 
 if (info /= 0) then
   print *,  irp_here//' DSYEVD failed : ', info
   stop 1
 endif
 lwork = int(work(1))
 liwork = iwork(1)
 deallocate(iwork,work)
 
 allocate(work(lwork), iwork(liwork))
 
 do istate=2,N_states
   call dsyevd( 'V', 'U', mo_tot_num,                                &
       one_body_dm_mo_diff_eigvectors(1,1,istate),                   &
       size(one_body_dm_mo_diff_eigvectors,1),                       &
       one_body_dm_mo_diff_eigvalues(1,istate),                      &
       work, lwork, iwork, liwork, info)

   if (info /= 0) then
       print *,  irp_here//' DSYEVD failed : ', info
       stop 1
   endif

 enddo

 deallocate(iwork,work)

END_PROVIDER

BEGIN_PROVIDER [ double precision, transition_natorb, (ao_num,mo_tot_num,2:N_states) ]
 implicit none
 BEGIN_DOC
 ! Natural transition molecular orbitals
 END_DOC

 integer :: istate

 do istate=2,N_states
   call dgemm('N','N',ao_num,mo_tot_num,mo_tot_num, 1.d0,            &
       mo_coef, size(mo_coef,1),                                     &
       one_body_dm_mo_diff_eigvectors(1,1,istate),                   &
       size(one_body_dm_mo_diff_eigvectors,1), 0.d0,                 &
       transition_natorb(1,1,istate), size(transition_natorb,1))
 enddo
END_PROVIDER


BEGIN_PROVIDER [ double precision, phi_s, (2:N_states) ]
 implicit none
 BEGIN_DOC
 ! 
 END_DOC

 integer :: i,istate
 double precision, allocatable :: T(:,:), A(:,:), D(:,:)
 double precision :: trace, norm
 allocate(T(ao_num,ao_num), A(ao_num,ao_num), D(ao_num,ao_num))

  do istate=2,N_states

    call dgemm('N','N',ao_num,ao_num,ao_num,1.d0,                    &
        S_half, size(S_half,1),                                      &
        one_body_dm_ao_attachment(1,1,istate), size(one_body_dm_ao_attachment,1), 0.d0,&
        T, size(T,1))
    call dgemm('N','N',ao_num,ao_num,ao_num,1.d0,                    &
        T, size(T,1),                                                &
        S_half, size(S_half,1), 0.d0,                                &
        A, size(A,1))
!
    call dgemm('N','N',ao_num,ao_num,ao_num,1.d0,                    &
        S_half, size(S_half,1),                                      &
        one_body_dm_ao_detachment(1,1,istate), size(one_body_dm_ao_detachment,1), 0.d0,&
        T, size(T,1))
    call dgemm('N','N',ao_num,ao_num,ao_num,1.d0,                    &
        T, size(T,1),                                                &
        S_half, size(S_half,1), 0.d0,                                &
        D, size(D,1))

    trace = 0.d0
    do i=1,ao_num
      trace = trace + A(i,i)
    enddo
    norm = 0.d0
    do i=1,ao_num
      norm = norm + D(i,i)
    enddo
    norm = 0.5d0*(norm + trace)

    trace = 0.d0
    do i=1,mo_tot_num
      trace = trace + dsqrt(A(i,i)*D(i,i))
    enddo
    phi_s(istate) = trace/norm 
  enddo

END_PROVIDER


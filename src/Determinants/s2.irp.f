subroutine get_s2(key_i,key_j,s2,Nint)
 implicit none
 use bitmasks
 BEGIN_DOC
! Returns <S^2> 
 END_DOC
 integer, intent(in)  :: Nint
 integer(bit_kind), intent(in)  :: key_i(Nint,2)
 integer(bit_kind), intent(in)  :: key_j(Nint,2)
 double precision, intent(out) :: s2
 integer :: exc(0:2,2,2)
 integer :: degree
 double precision :: phase_spsm
 integer :: nup, i

 s2 = 0.d0
 !$FORCEINLINE
 call get_excitation_degree(key_i,key_j,degree,Nint)
 select case (degree)
   case(2)
     call get_double_excitation(key_j,key_i,exc,phase_spsm,Nint)
     if (exc(0,1,1) == 1) then   ! Mono alpha + mono-beta
       if ( (exc(1,1,1) == exc(1,2,2)).and.(exc(1,1,2) == exc(1,2,1)) ) then
         s2 =  -phase_spsm
       endif
     endif
   case(0)
      nup = 0
      do i=1,Nint
        nup += popcnt(iand(xor(key_i(i,1),key_i(i,2)),key_i(i,1)))
      enddo
      s2 = dble(nup)
   end select
end

BEGIN_PROVIDER [ double precision, S_z ]
&BEGIN_PROVIDER [ double precision, S_z2_Sz ]
 implicit none
 BEGIN_DOC
! z component of the Spin
 END_DOC

 S_z = 0.5d0*dble(elec_alpha_num-elec_beta_num)
 S_z2_Sz = S_z*(S_z-1.d0)

END_PROVIDER

BEGIN_PROVIDER [ double precision, expected_s2]
 implicit none
 BEGIN_DOC
! Expected value of S2 : S*(S+1)
 END_DOC
   logical :: has_expected_s2

   call ezfio_has_determinants_expected_s2(has_expected_s2)
   if (has_expected_s2) then
     call ezfio_get_determinants_expected_s2(expected_s2)
   else
     double precision :: S
     S = (elec_alpha_num-elec_beta_num)*0.5d0 
     expected_s2 = S * (S+1.d0)
!     expected_s2 = elec_alpha_num - elec_beta_num + 0.5d0 * ((elec_alpha_num - elec_beta_num)**2*0.5d0 - (elec_alpha_num-elec_beta_num))
   endif

END_PROVIDER 

BEGIN_PROVIDER [ double precision, s2_values, (N_states) ]
 implicit none
 BEGIN_DOC
! array of the averaged values of the S^2 operator on the various states
 END_DOC
 integer :: i
 double precision :: s2
 do i = 1, N_states
  call get_s2_u0(psi_det,psi_coef(1,i),n_det,size(psi_coef,1),s2)
  s2_values(i) = s2
 enddo

END_PROVIDER


subroutine get_s2_u0_old(psi_keys_tmp,psi_coefs_tmp,n,nmax,s2)
 implicit none
 use bitmasks
 integer(bit_kind), intent(in) :: psi_keys_tmp(N_int,2,nmax)
 integer, intent(in) :: n,nmax
 double precision, intent(in) :: psi_coefs_tmp(nmax)
 double precision, intent(out) :: s2
 integer :: i,j,l
 double precision :: s2_tmp
 s2 = 0.d0
 !$OMP PARALLEL DO DEFAULT(NONE) &
 !$OMP PRIVATE(i,j,s2_tmp) SHARED(n,psi_coefs_tmp,psi_keys_tmp,N_int) REDUCTION(+:s2) SCHEDULE(dynamic) 
 do i=1,n
   do j=i+1,n
     call get_s2(psi_keys_tmp(1,1,i),psi_keys_tmp(1,1,j),s2_tmp,N_int)
     s2 += psi_coefs_tmp(i)*psi_coefs_tmp(j)*s2_tmp
   enddo
 enddo
 !$OMP END PARALLEL DO
 s2 = s2+s2
 do i=1,n
   call get_s2(psi_keys_tmp(1,1,i),psi_keys_tmp(1,1,i),s2_tmp,N_int)
   s2 += psi_coefs_tmp(i)*psi_coefs_tmp(i)*s2_tmp
 enddo
 s2 +=  S_z2_Sz
end

subroutine get_s2_u0(psi_keys_tmp,psi_coefs_tmp,n,nmax,s2)
  implicit none
  use bitmasks
  integer(bit_kind), intent(in)  :: psi_keys_tmp(N_int,2,nmax)
  integer, intent(in)            :: n,nmax
  double precision, intent(in)   :: psi_coefs_tmp(nmax)
  double precision, intent(out)  :: s2
  double precision               :: s2_tmp
  integer                        :: i,j,l,jj,ii
  integer, allocatable           :: idx(:)
  
  integer, allocatable           :: shortcut(:), sort_idx(:)
  integer(bit_kind), allocatable :: sorted(:,:), version(:,:)
  integer                        :: sh, sh2, ni, exa, ext, org_i, org_j, endi, pass
  double precision               :: davidson_threshold_bis
  
  allocate (shortcut(0:n+1), sort_idx(n), sorted(N_int,n), version(N_int,n))
  s2 = 0.d0
  davidson_threshold_bis = threshold_davidson
  call sort_dets_ab_v(psi_keys_tmp, sorted, sort_idx, shortcut, version, n, N_int)
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP PRIVATE(i,j,s2_tmp,sh, sh2, ni, exa, ext, org_i, org_j, endi, pass)&
      !$OMP SHARED(n,psi_coefs_tmp,psi_keys_tmp,N_int,threshold_davidson,shortcut,sorted,sort_idx,version)&
      !$OMP REDUCTION(+:s2)
  
  !$OMP DO SCHEDULE(dynamic)
  do sh=1,shortcut(0)
    
    do sh2=1,sh
      exa = 0
      do ni=1,N_int
        exa += popcnt(xor(version(ni,sh), version(ni,sh2)))
      end do
      if(exa > 2) then
        cycle
      end if
      
      do i=shortcut(sh),shortcut(sh+1)-1
        if(sh==sh2) then
          endi = i-1
        else
          endi = shortcut(sh2+1)-1
        end if
        
        do j=shortcut(sh2),endi
          ext = exa
          do ni=1,N_int
            ext += popcnt(xor(sorted(ni,i), sorted(ni,j)))
          end do
          if(ext <= 4) then
            org_i = sort_idx(i)
            org_j = sort_idx(j)
            
            if ( dabs(psi_coefs_tmp(org_j)) + dabs(psi_coefs_tmp(org_i))&
                  > threshold_davidson ) then
              call get_s2(psi_keys_tmp(1,1,org_i),psi_keys_tmp(1,1,org_j),s2_tmp,N_int)
              s2 = s2 + psi_coefs_tmp(org_i)*psi_coefs_tmp(org_j)*s2_tmp
            endif
          end if
        end do
      end do
    end do
  enddo
  !$OMP END DO
  
  !$OMP END PARALLEL
  
  call sort_dets_ba_v(psi_keys_tmp, sorted, sort_idx, shortcut, version, n, N_int)
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP PRIVATE(i,j,s2_tmp,sh, sh2, ni, exa, ext, org_i, org_j, endi, pass)&
      !$OMP SHARED(n,psi_coefs_tmp,psi_keys_tmp,N_int,threshold_davidson,shortcut,sorted,sort_idx,version)&
      !$OMP REDUCTION(+:s2)
  
  !$OMP DO SCHEDULE(dynamic)
  do sh=1,shortcut(0)
    do i=shortcut(sh),shortcut(sh+1)-1
      do j=shortcut(sh),i-1
        ext = 0
        do ni=1,N_int
          ext += popcnt(xor(sorted(ni,i), sorted(ni,j)))
        end do
        if(ext == 4) then
          org_i = sort_idx(i)
          org_j = sort_idx(j)
          
          if ( dabs(psi_coefs_tmp(org_j)) + dabs(psi_coefs_tmp(org_i))&
                > threshold_davidson ) then
            call get_s2(psi_keys_tmp(1,1,org_i),psi_keys_tmp(1,1,org_j),s2_tmp,N_int)
            s2 = s2 + psi_coefs_tmp(org_i)*psi_coefs_tmp(org_j)*s2_tmp
          endif
        end if
      end do
    end do
  enddo
  !$OMP END DO
  
  !$OMP END PARALLEL
  s2 = s2+s2
  do i=1,n
    call get_s2(psi_keys_tmp(1,1,i),psi_keys_tmp(1,1,i),s2_tmp,N_int)
    s2 = s2 + psi_coefs_tmp(i)*psi_coefs_tmp(i)*s2_tmp
  enddo
  s2 = s2 + S_z2_Sz
  deallocate (shortcut, sort_idx, sorted, version)
end

subroutine get_uJ_s2_uI(psi_keys_tmp,psi_coefs_tmp,n,nmax_coefs,nmax_keys,s2,nstates)
 implicit none
 use bitmasks
 integer(bit_kind), intent(in) :: psi_keys_tmp(N_int,2,nmax_keys)
 integer, intent(in) :: n,nmax_coefs,nmax_keys,nstates
 double precision, intent(in) :: psi_coefs_tmp(nmax_coefs,nstates)
 double precision, intent(out) :: s2(nstates,nstates)
 double precision :: s2_tmp,accu
 integer :: i,j,l,jj,ll,kk
 integer, allocatable           :: idx(:)
 double precision, allocatable :: tmp(:,:)
 BEGIN_DOC
 ! returns the matrix elements of S^2 "s2(i,j)" between the "nstates" states 
 ! psi_coefs_tmp(:,i) and psi_coefs_tmp(:,j)
 END_DOC
 s2 = 0.d0
 do ll = 1, nstates
  do jj = 1, nstates
 accu = 0.d0
 !$OMP PARALLEL DEFAULT(NONE)                                         &
 !$OMP PRIVATE (i,j,kk,idx,tmp,s2_tmp) & 
 !$OMP SHARED (ll,jj,psi_keys_tmp,psi_coefs_tmp,N_int,n,nstates)      &
 !$OMP REDUCTION(+:accu)
 allocate(idx(0:n))
   !$OMP DO SCHEDULE(dynamic)
   do i = 1, n
    call get_s2(psi_keys_tmp(1,1,i),psi_keys_tmp(1,1,i),s2_tmp,N_int)
    accu += psi_coefs_tmp(i,ll) * s2_tmp * psi_coefs_tmp(i,jj)
    call filter_connected(psi_keys_tmp,psi_keys_tmp(1,1,i),N_int,i-1,idx)
    do kk=1,idx(0)
     j = idx(kk)
     call get_s2(psi_keys_tmp(1,1,i),psi_keys_tmp(1,1,j),s2_tmp,N_int)
     accu += psi_coefs_tmp(i,ll) * s2_tmp * psi_coefs_tmp(j,jj) + psi_coefs_tmp(i,jj) * s2_tmp * psi_coefs_tmp(j,ll)
    enddo
   enddo
   !$OMP END DO NOWAIT
 deallocate(idx)
 !$OMP BARRIER
 !$OMP END PARALLEL
   s2(ll,jj) += accu
  enddo
 enddo
 do i = 1, nstates
  do j =i+1,nstates
   accu = 0.5d0 * (s2(i,j) + s2(j,i))
   s2(i,j) = accu
   s2(j,i) = accu
  enddo
 enddo
end

subroutine diagonalize_s2_betweenstates(keys_tmp,psi_coefs_inout,n,nmax_keys,nmax_coefs,nstates,s2_eigvalues)
 BEGIN_DOC
! You enter with nstates vectors in psi_coefs_inout that may be coupled by S^2
! The subroutine diagonalize the S^2 operator in the basis of these states. 
! The vectors that you obtain in output are no more coupled by S^2, 
! which does not necessary mean that they are eigenfunction of S^2. 
! n,nmax,nstates = number of determinants, physical dimension of the arrays and number of states
! keys_tmp = array of integer(bit_kind) that represents the determinants 
! psi_coefs(i,j) = coeff of the ith determinant in the jth state
! VECTORS ARE SUPPOSED TO BE ORTHONORMAL IN INPUT
 END_DOC
 implicit none
 use bitmasks
 integer, intent(in) :: n,nmax_keys,nmax_coefs,nstates
 integer(bit_kind), intent(in) :: keys_tmp(N_int,2,nmax_keys)
 double precision, intent(inout) :: psi_coefs_inout(nmax_coefs,nstates)

!integer, intent(in) :: ndets_real,ndets_keys,ndets_coefs,nstates
!integer(bit_kind), intent(in) :: keys_tmp(N_int,2,ndets_keys)
!double precision, intent(inout) :: psi_coefs_inout(ndets_coefs,nstates)
 double precision, intent(out)   :: s2_eigvalues(nstates)


 double precision,allocatable :: s2(:,:),overlap(:,:)
 double precision, allocatable :: eigvalues(:),eigvectors(:,:)
 integer :: i,j,k
 double precision, allocatable :: psi_coefs_tmp(:,:)
 double precision :: accu,coef_contract
 double precision :: u_dot_u,u_dot_v

 print*,''
 print*,'*********************************************************************'
 print*,'Cleaning the various vectors by diagonalization of the S^2 matrix ...'
 print*,''
 print*,'nstates = ',nstates
 allocate(s2(nstates,nstates),overlap(nstates,nstates))
  do i = 1, nstates
    overlap(i,i) = u_dot_u(psi_coefs_inout(1,i),n)
    do j = i+1, nstates
      overlap(i,j) = u_dot_v(psi_coefs_inout(1,j),psi_coefs_inout(1,i),n)
      overlap(j,i) = overlap(i,j)
    enddo
  enddo
 print*,'Overlap matrix in the basis of the states considered'
 do i = 1, nstates
  write(*,'(10(F16.10,X))')overlap(i,:)
 enddo
 call ortho_lowdin(overlap,size(overlap,1),nstates,psi_coefs_inout,size(psi_coefs_inout,1),n)
 print*,'passed ortho'

  do i = 1, nstates
    overlap(i,i) = u_dot_u(psi_coefs_inout(1,i),n)
    do j = i+1, nstates
      overlap(i,j) = u_dot_v(psi_coefs_inout(1,j),psi_coefs_inout(1,i),n)
      overlap(j,i) = overlap(i,j)
    enddo
  enddo
 print*,'Overlap matrix in the basis of the Lowdin orthonormalized states '
 do i = 1, nstates
  write(*,'(10(F16.10,X))')overlap(i,:)
 enddo

 call get_uJ_s2_uI(keys_tmp,psi_coefs_inout,n_det,size(psi_coefs_inout,1),size(keys_tmp,3),s2,nstates)
 print*,'S^2 matrix in the basis of the states considered'
 double precision :: accu_precision_diag,accu_precision_of_diag
 accu_precision_diag = 0.d0
 accu_precision_of_diag = 0.d0
 do i = 1, nstates
  do j = i+1, nstates
   if(  ( dabs(s2(i,i) - s2(j,j)) .le.1.d-10 ) .and. (dabs(s2(i,j) + dabs(s2(i,j)))) .le.1.d-10) then
    s2(i,j) = 0.d0
    s2(j,i) = 0.d0
   endif
  enddo
 enddo
 do i = 1, nstates
  write(*,'(10(F10.6,X))')s2(i,:)
 enddo

 print*,'Diagonalizing the S^2 matrix'

 allocate(eigvalues(nstates),eigvectors(nstates,nstates))
 call lapack_diagd(eigvalues,eigvectors,s2,nstates,nstates)
 print*,'Eigenvalues of s^2'
 do i = 1, nstates
  print*,'s2 = ',eigvalues(i)
  s2_eigvalues(i) = eigvalues(i)
 enddo

 print*,'Building the eigenvectors of the S^2 matrix'
 allocate(psi_coefs_tmp(nmax_coefs,nstates))
 psi_coefs_tmp = 0.d0
 do j = 1, nstates
  do k = 1, nstates
   coef_contract =  eigvectors(k,j)    !  <phi_k|Psi_j>
   do i = 1, n_det
    psi_coefs_tmp(i,j) += psi_coefs_inout(i,k) * coef_contract
   enddo
  enddo
 enddo
 do j = 1, nstates
  accu = 0.d0
   do i = 1, n_det
    accu += psi_coefs_tmp(i,j) * psi_coefs_tmp(i,j)
   enddo
   print*,'Norm of vector = ',accu
   accu = 1.d0/dsqrt(accu)
   do i = 1, n_det
    psi_coefs_inout(i,j) = psi_coefs_tmp(i,j) * accu
   enddo
 enddo
!call get_uJ_s2_uI(keys_tmp,psi_coefs_inout,n_det,size(psi_coefs_inout,1),size(keys_tmp,3),s2,nstates)
!print*,'S^2 matrix in the basis of the NEW states considered'
!do i = 1, nstates
! write(*,'(10(F16.10,X))')s2(i,:)
!enddo

 deallocate(s2,eigvalues,eigvectors,psi_coefs_tmp,overlap)

end


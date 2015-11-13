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
  
  integer                        :: shortcut(0:n+1), sort_idx(n)
  integer(bit_kind)              :: sorted(N_int,n), version(N_int,n)
  integer                        :: sh, sh2, ni, exa, ext, org_i, org_j, endi, pass
  double precision               :: davidson_threshold_bis
  
  !PROVIDE davidson_threshold
  
  s2 = 0.d0
  davidson_threshold_bis = davidson_threshold
  call sort_dets_ab_v(psi_keys_tmp, sorted, sort_idx, shortcut, version, n, N_int)
  
  !$OMP PARALLEL DEFAULT(NONE)                                       &
      !$OMP PRIVATE(i,j,s2_tmp,sh, sh2, ni, exa, ext, org_i, org_j, endi, pass)&
      !$OMP SHARED(n,psi_coefs_tmp,psi_keys_tmp,N_int,davidson_threshold,shortcut,sorted,sort_idx,version)&
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
                  > davidson_threshold ) then
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
      !$OMP SHARED(n,psi_coefs_tmp,psi_keys_tmp,N_int,davidson_threshold,shortcut,sorted,sort_idx,version)&
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
                > davidson_threshold ) then
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
end



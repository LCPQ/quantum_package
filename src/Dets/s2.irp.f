subroutine get_s2(key_i,key_j,phase,Nint)
 implicit none
 use bitmasks
 BEGIN_DOC
! Returns <S^2> 
 END_DOC
 integer, intent(in)  :: Nint
 integer(bit_kind), intent(in)  :: key_i(Nint,2)
 integer(bit_kind), intent(in)  :: key_j(Nint,2)
 double precision, intent(out) :: phase
 integer :: exc(0:2,2,2)
 integer :: degree
 double precision :: phase_spsm
 integer :: nup, i

 phase = 0.d0
 !$FORCEINLINE
 call get_excitation_degree(key_i,key_j,degree,Nint)
 select case (degree)
   case(2)
     call get_double_excitation(key_i,key_j,exc,phase_spsm,Nint)
     if (exc(0,1,1) == 1) then   ! Mono alpha + mono-beta
       if ( (exc(1,1,1) == exc(1,2,2)).and.(exc(1,1,2) == exc(1,2,1)) ) then
         phase =  -phase_spsm
       endif
     endif
   case(0)
      nup = 0
      do i=1,Nint
        nup += popcnt(iand(xor(key_i(i,1),key_i(i,2)),key_i(i,1)))
      enddo
      phase = dble(nup)
   end select
end

BEGIN_PROVIDER [ double precision, S_z ]
&BEGIN_PROVIDER [ double precision, S_z2_Sz ]
 implicit none

 S_z = 0.5d0*dble(elec_alpha_num-elec_beta_num)
 S_z2_Sz = S_z*(S_z-1.d0)

END_PROVIDER

BEGIN_PROVIDER [ double precision, expected_s2]
 implicit none
   PROVIDE ezfio_filename
   logical :: has_expected_s2

   call ezfio_has_determinants_expected_s2(has_expected_s2)
   if (has_expected_s2) then
     call ezfio_get_determinants_expected_s2(expected_s2)
   else
     expected_s2 = elec_alpha_num - elec_beta_num + 0.5d0 * ((elec_alpha_num - elec_beta_num)**2*0.5d0 - (elec_alpha_num-elec_beta_num))
!    call ezfio_set_determinants_expected_s2(expected_s2)
   endif

END_PROVIDER 


subroutine get_s2_u0(psi_keys_tmp,psi_coefs_tmp,n,nmax,s2)
 implicit none
 use bitmasks
 integer(bit_kind), intent(in) :: psi_keys_tmp(N_int,2,nmax)
 integer, intent(in) :: n,nmax
 double precision, intent(in) :: psi_coefs_tmp(nmax)
 double precision, intent(out) :: s2
 integer :: i,j,l
 double precision :: s2_tmp
 s2 = S_z2_Sz
 !$OMP PARALLEL DO DEFAULT(NONE) &
 !$OMP PRIVATE(i,j,s2_tmp) SHARED(n,psi_coefs_tmp,psi_keys_tmp,N_int) &
 !$OMP REDUCTION(+:s2) SCHEDULE(dynamic)
 do i = 1, n
   call get_s2(psi_keys_tmp(1,1,i),psi_keys_tmp(1,1,i),s2_tmp,N_int)
!  print*,'s2_tmp = ',s2_tmp
  do j = 1, n
    call get_s2(psi_keys_tmp(1,1,i),psi_keys_tmp(1,1,j),s2_tmp,N_int)
    if (s2_tmp == 0.d0) cycle
    s2 += psi_coefs_tmp(i)*psi_coefs_tmp(j)*s2_tmp
  enddo
 enddo
 !$OMP END PARALLEL DO
end


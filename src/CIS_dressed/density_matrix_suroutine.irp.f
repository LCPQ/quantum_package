
   subroutine get_dm_from_psi(dets_in,u_in,sze,dim_in,Nint,dm_alpha,dm_beta)
   implicit none
   BEGIN_DOC
   ! Alpha and beta one-body density matrix
   !
   ! dets_in   ::  bitsrings corresponding to the determinants in the wave function
   !
   ! u_in      ::  coefficients of the wave function
   !
   ! sze       ::  number of determinants in the wave function
   !
   ! dim_in    ::  physical dimension of the array u_in and dets_in
   !
   ! Nint      ::  should be equal to N_int
   !               
   ! dm_alpha  ::  alpha one body density matrix
   !
   ! dm_beta   ::  beta  one body density matrix
   END_DOC
   use bitmasks
   integer, intent(in)            :: sze,dim_in,Nint
   integer(bit_kind), intent(in)  :: dets_in(Nint,2,dim_in)
   double precision, intent(in)   :: u_in(dim_in)
   double precision, intent(out)  :: dm_alpha(mo_tot_num,mo_tot_num)
   double precision, intent(out)  :: dm_beta(mo_tot_num,mo_tot_num)
   
   integer                        :: j,k,l
   integer                        :: occ(N_int*bit_kind_size,2)
   double precision               :: ck, cl, ckl
   double precision               :: phase
   integer                        :: h1,h2,p1,p2,s1,s2, degree
   integer                        :: exc(0:2,2,2),n_occ_alpha
   dm_alpha = 0.d0
   dm_beta = 0.d0

   do k=1,sze
     call bitstring_to_list(dets_in(1,1,k), occ(1,1), n_occ_alpha, N_int)
     call bitstring_to_list(dets_in(1,2,k), occ(1,2), n_occ_alpha, N_int)
     ck = u_in(k)
     do l=1,elec_alpha_num
       j = occ(l,1)
       dm_alpha(j,j) += ck*ck
     enddo
     do l=1,elec_beta_num
       j = occ(l,2)
       dm_beta(j,j) += ck*ck
     enddo
     do l=1,k-1
       call get_excitation_degree(dets_in(1,1,k),dets_in(1,1,l),degree,N_int)
       if (degree /= 1) then
         cycle
       endif
       call get_mono_excitation(dets_in(1,1,k),dets_in(1,1,l),exc,phase,N_int)
       call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
       ckl = ck * u_in(l) * phase
       if (s1==1) then
         dm_alpha(h1,p1) += ckl
         dm_alpha(p1,h1) += ckl
       else
         dm_beta(h1,p1) += ckl
         dm_beta(p1,h1) += ckl
       endif
     enddo
   enddo
  end

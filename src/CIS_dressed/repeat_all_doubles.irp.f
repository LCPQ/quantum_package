subroutine repeat_all_doubles(key_in, e_corr)
 implicit none
 integer(bit_kind), intent(in) :: key_in(N_int,2)
 double precision, intent(out) :: e_corr
 integer :: i,j,k,l
 integer :: s1,s2
 integer :: i_ok
 double precision :: hij,get_mo_bielec_integral,diag_H_mat_elem,delta_e
 integer(bit_kind) :: key_out(N_int,2)
 ! same spin (alpha)
 if(mp2_dressing)then
 s1 = 1
 e_corr = 0.d0
 do i = n_core_cis + 1, elec_alpha_num
  do j = i + 1, elec_alpha_num
   do k = elec_alpha_num + 1, n_act_cis
    do l = k+1, n_act_cis
     ! a^+ k(s1) a^+ l(s1) a_j(s1) a_i(s1) |key_in>
      call diexcitation(i,j,k,l,s1,s1,key_in,key_out,i_ok,N_int)
      if(i_ok.ne.0)then
       hij = get_mo_bielec_integral(i,j,k,l,mo_integrals_map) & 
            -get_mo_bielec_integral(i,j,l,k,mo_integrals_map)
       e_corr += hij*hij/(Fock_matrix_diag_mo(i)+Fock_matrix_diag_mo(j) & 
                         -Fock_matrix_diag_mo(l)-Fock_matrix_diag_mo(k))
      endif
    enddo
   enddo
  enddo
 enddo

 s1 = 2
 do i = n_core_cis + 1, elec_alpha_num
  do j = i + 1, elec_alpha_num
   do k = elec_alpha_num + 1, n_act_cis
    do l = k+1, n_act_cis
     ! a^+ k(s1) a^+ l(s1) a_j(s1) a_i(s1) |key_in>
      call diexcitation(i,j,k,l,s1,s1,key_in,key_out,i_ok,N_int)
      if(i_ok.ne.0)then
       hij = get_mo_bielec_integral(i,j,k,l,mo_integrals_map) & 
            -get_mo_bielec_integral(i,j,l,k,mo_integrals_map)
       e_corr += hij*hij/(Fock_matrix_diag_mo(i)+Fock_matrix_diag_mo(j) & 
                         -Fock_matrix_diag_mo(l)-Fock_matrix_diag_mo(k))
      endif
    enddo
   enddo
  enddo
 enddo

 s1 = 1
 s2 = 2
 do i = n_core_cis + 1, elec_alpha_num
  do j = n_core_cis + 1, elec_alpha_num
   do k = elec_alpha_num + 1, n_act_cis
    do l = elec_alpha_num + 1, n_act_cis
     ! a^+ k(s1) a^+ l(s1) a_j(s1) a_i(s1) |key_in>
      call diexcitation(i,j,k,l,s1,s2,key_in,key_out,i_ok,N_int)
      if(i_ok.ne.0)then
       hij = get_mo_bielec_integral(i,j,k,l,mo_integrals_map) 
       e_corr += hij*hij/(Fock_matrix_diag_mo(i)+Fock_matrix_diag_mo(j) & 
                         -Fock_matrix_diag_mo(l)-Fock_matrix_diag_mo(k))
      endif
    enddo
   enddo
  enddo
 enddo

 else
  ! same spin (alpha)
  s1 = 1
  e_corr = 0.d0
  do i = n_core_cis + 1, elec_alpha_num
   do j = i + 1, elec_alpha_num
    do k = elec_alpha_num + 1, n_act_cis
     do l = k+1, n_act_cis
      ! a^+ k(s1) a^+ l(s1) a_j(s1) a_i(s1) |key_in>
       call diexcitation(i,j,k,l,s1,s1,key_in,key_out,i_ok,N_int)
       if(i_ok.ne.0)then
        hij = get_mo_bielec_integral(i,j,k,l,mo_integrals_map) & 
             -get_mo_bielec_integral(i,j,l,k,mo_integrals_map)
        call diexcitation(i,j,k,l,s1,s1,ref_bitmask,key_out,i_ok,N_int)
        delta_e = HF_energy -  diag_H_mat_elem(key_out,N_int)
        e_corr += hij*hij/delta_e
       endif
     enddo
    enddo
   enddo
  enddo

  s1 = 2
  do i = n_core_cis + 1, elec_alpha_num
   do j = i + 1, elec_alpha_num
    do k = elec_alpha_num + 1, n_act_cis
     do l = k+1, n_act_cis
      ! a^+ k(s1) a^+ l(s1) a_j(s1) a_i(s1) |key_in>
       call diexcitation(i,j,k,l,s1,s1,key_in,key_out,i_ok,N_int)
       if(i_ok.ne.0)then
        hij = get_mo_bielec_integral(i,j,k,l,mo_integrals_map) & 
             -get_mo_bielec_integral(i,j,l,k,mo_integrals_map)
        call diexcitation(i,j,k,l,s1,s1,ref_bitmask,key_out,i_ok,N_int)
        delta_e = HF_energy -  diag_H_mat_elem(key_out,N_int)
        e_corr += hij*hij/delta_e
       endif
     enddo
    enddo
   enddo
  enddo

  s1 = 1
  s2 = 2
  do i = n_core_cis + 1, elec_alpha_num
   do j = n_core_cis + 1, elec_alpha_num
    do k = elec_alpha_num + 1, n_act_cis
     do l = elec_alpha_num + 1, n_act_cis
      ! a^+ k(s1) a^+ l(s1) a_j(s1) a_i(s1) |key_in>
       call diexcitation(i,j,k,l,s1,s2,key_in,key_out,i_ok,N_int)
       if(i_ok.ne.0)then
        hij = get_mo_bielec_integral(i,j,k,l,mo_integrals_map) 
        call diexcitation(i,j,k,l,s1,s2,ref_bitmask,key_out,i_ok,N_int)
        delta_e = HF_energy -  diag_H_mat_elem(key_out,N_int)
        e_corr += hij*hij/delta_e
       endif
     enddo
    enddo
   enddo
  enddo
 endif
end

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
  integer(bit_kind) :: i_test_hole,i_test_particl
  character*(512) :: output(2)
  key_out = key_in

  k_hole = ishft(i-1,-bit_kind_shift)+1
  j_hole = i-ishft(k_hole-1,bit_kind_shift)-1
  i_test_hole = ibset(0_bit_kind,j_hole)
  if(iand(key_in(k_hole,ispin1),i_test_hole).ne.i_test_hole)then
   i_ok = 0
   return
  endif
  key_out(k_hole,ispin1) = ibclr(key_out(k_hole,ispin1),j_hole)
  k_particl = ishft(k-1,-bit_kind_shift)+1
  j_particl = k-ishft(k_particl-1,bit_kind_shift)-1
  i_test_particl= ibset(0_bit_kind,j_particl)
  if(iand(key_in(k_particl,ispin1),i_test_particl).ne.0_bit_kind)then
   i_ok = 0
   return
  endif
  key_out(k_particl,ispin1) = ibset(key_out(k_particl,ispin1),j_particl)

  k_hole = ishft(j-1,-bit_kind_shift)+1
  j_hole = j-ishft(k_hole-1,bit_kind_shift)-1
  i_test_hole = ibset(0_bit_kind,j_hole)
  if(iand(key_in(k_hole,ispin2),i_test_hole).ne.i_test_hole)then
   i_ok = 0
   return
  endif
  key_out(k_hole,ispin2) = ibclr(key_out(k_hole,ispin2),j_hole)
  k_particl = ishft(l-1,-bit_kind_shift)+1
  j_particl = l-ishft(k_particl-1,bit_kind_shift)-1
  i_test_particl = ibset(0_bit_kind,j_particl)
  if(iand(key_in(k_particl,ispin2),i_test_particl).ne.0_bit_kind)then
   i_ok = 0
   return
  endif
  key_out(k_particl,ispin2) = ibset(key_out(k_particl,ispin2),j_particl)
  i_ok = 1
  end


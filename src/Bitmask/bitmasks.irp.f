use bitmasks

BEGIN_PROVIDER [ integer, N_int ]
  implicit none
  BEGIN_DOC
  ! Number of 64-bit integers needed to represent determinants as binary strings
  END_DOC
  N_int = (mo_tot_num-1)/bit_kind_size + 1
END_PROVIDER


BEGIN_PROVIDER [ integer(bit_kind), full_ijkl_bitmask, (N_int,4) ]
  implicit none
  BEGIN_DOC
  ! Bitmask to include all possible MOs
  END_DOC
  
  integer                        :: i,j,n
  n = mod(mo_tot_num-1,bit_kind_size)+1
  full_ijkl_bitmask = 0_bit_kind
  do j=1,4
    do i=1,N_int-1
      full_ijkl_bitmask(i,j) = not(0_bit_kind)
    enddo
    do i=1,n
      full_ijkl_bitmask(N_int,j) = ibset(full_ijkl_bitmask(N_int,j),i-1)
    enddo
  enddo
END_PROVIDER


BEGIN_PROVIDER [ integer(bit_kind), HF_bitmask, (N_int,2)]
  implicit none
  BEGIN_DOC
  ! Hartree Fock bit mask
  END_DOC
  integer                        :: i,j,n
  integer                        :: occ(elec_alpha_num)

  HF_bitmask = 0_bit_kind
  do i=1,elec_alpha_num
   occ(i) = i 
  enddo
  call list_to_bitstring( HF_bitmask(1,1), occ, elec_alpha_num, N_int)
  ! elec_alpha_num <= elec_beta_num, so occ is already OK.
  call list_to_bitstring( HF_bitmask(1,2), occ, elec_beta_num, N_int)

END_PROVIDER

BEGIN_PROVIDER [ integer(bit_kind), ref_bitmask, (N_int,2)]
 implicit none
 BEGIN_DOC
! Reference bit mask, used in Slater rules, chosen as Hartree-Fock bitmask
 END_DOC
 ref_bitmask = HF_bitmask
END_PROVIDER



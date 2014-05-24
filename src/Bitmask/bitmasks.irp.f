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

BEGIN_PROVIDER [ integer, N_generators_bitmask ]
 implicit none
 BEGIN_DOC
 ! Number of bitmasks for generators
 END_DOC
 logical                        :: exists
 PROVIDE ezfio_filename
 
 call ezfio_has_bitmasks_N_mask_gen(exists)
 if (exists) then
   call ezfio_get_bitmasks_N_mask_gen(N_generators_bitmask)
   integer                        :: N_int_check
   integer                        :: bit_kind_check
   call ezfio_get_bitmasks_bit_kind(bit_kind_check)
   if (bit_kind_check /= bit_kind) then
     print *,  bit_kind_check, bit_kind
     print *,  'Error: bit_kind is not correct in EZFIO file'
   endif
   call ezfio_get_bitmasks_N_int(N_int_check)
   if (N_int_check /= N_int) then
     print *,  N_int_check, N_int
     print *,  'Error: N_int is not correct in EZFIO file'
   endif
 else
   N_generators_bitmask = 1
   call ezfio_set_bitmasks_N_int(N_int)
   call ezfio_set_bitmasks_bit_kind(bit_kind)
   call ezfio_set_bitmasks_N_mask_gen(N_generators_bitmask)
 endif
 ASSERT (N_generators_bitmask > 0)

END_PROVIDER

BEGIN_PROVIDER [ integer(bit_kind), generators_bitmask, (N_int,2,2,N_generators_bitmask) ]
 implicit none
 BEGIN_DOC
 ! Bitmasks for generator determinants. (N_int, alpha/beta, hole/particle, generator)
 END_DOC
 logical                        :: exists
 PROVIDE ezfio_filename

 call ezfio_has_bitmasks_generators(exists)
 if (exists) then
   call ezfio_get_bitmasks_generators(generators_bitmask)
 else
   generators_bitmask(:,:,1,1) = HF_bitmask
   generators_bitmask(:,1,2,1) = iand(not(HF_bitmask(:,1)),full_ijkl_bitmask(:,1))
   generators_bitmask(:,2,2,1) = iand(not(HF_bitmask(:,2)),full_ijkl_bitmask(:,2))
   call ezfio_set_bitmasks_generators(generators_bitmask)
 endif
 ASSERT (N_generators_bitmask > 0)

END_PROVIDER


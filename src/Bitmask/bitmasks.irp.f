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

 
BEGIN_PROVIDER [ integer(bit_kind), cis_ijkl_bitmask, (N_int,4) ]
  implicit none
  BEGIN_DOC
  ! Bitmask to include all possible single excitations from Hartree-Fock
  END_DOC
  
  integer                        :: i,j,n
  cis_ijkl_bitmask = full_ijkl_bitmask
  cis_ijkl_bitmask(:,1) = HF_bitmask(:,1)
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
 endif
 ASSERT (N_generators_bitmask > 0)

END_PROVIDER

BEGIN_PROVIDER [ integer(bit_kind), generators_bitmask, (N_int,2,6,N_generators_bitmask) ]
 implicit none
 BEGIN_DOC
 ! Bitmasks for generator determinants. (N_int, alpha/beta, hole/particle, generator).
 ! 3rd index is :
 ! * 1 : hole     for single exc
 ! * 2 : particle for single exc
 ! * 3 : hole     for 1st exc of double
 ! * 4 : particle for 1st exc of double
 ! * 5 : hole     for 2dn exc of double
 ! * 6 : particle for 2dn exc of double
 END_DOC
 logical                        :: exists
 PROVIDE ezfio_filename

 call ezfio_has_bitmasks_generators(exists)
 if (exists) then
   call ezfio_get_bitmasks_generators(generators_bitmask)
 else
   integer :: k, ispin
   do k=1,N_generators_bitmask
     do ispin=1,2
       generators_bitmask(:,ispin,s_hole ,k) = full_ijkl_bitmask(:,d_hole1)
       generators_bitmask(:,ispin,s_part ,k) = full_ijkl_bitmask(:,d_part1)
       generators_bitmask(:,ispin,d_hole1,k) = full_ijkl_bitmask(:,d_hole1)
       generators_bitmask(:,ispin,d_part1,k) = full_ijkl_bitmask(:,d_part1)
       generators_bitmask(:,ispin,d_hole2,k) = full_ijkl_bitmask(:,d_hole2)
       generators_bitmask(:,ispin,d_part2,k) = full_ijkl_bitmask(:,d_part2)
     enddo
   enddo
 endif

END_PROVIDER

BEGIN_PROVIDER [ integer, N_reference_bitmask ]
 implicit none
 BEGIN_DOC
 ! Number of bitmasks for reference
 END_DOC
 logical                        :: exists
 PROVIDE ezfio_filename
 
 call ezfio_has_bitmasks_N_mask_ref(exists)
 if (exists) then
   call ezfio_get_bitmasks_N_mask_ref(N_reference_bitmask)
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
   N_reference_bitmask = 1
 endif
 ASSERT (N_reference_bitmask > 0)

END_PROVIDER

BEGIN_PROVIDER [ integer(bit_kind), reference_bitmask, (N_int,2,2,N_reference_bitmask) ]
 implicit none
 BEGIN_DOC
 ! Bitmasks for reference determinants. (N_int, alpha/beta, hole/particle, reference)
 END_DOC
 logical                        :: exists
 PROVIDE ezfio_filename

 call ezfio_has_bitmasks_reference(exists)
 if (exists) then
   call ezfio_get_bitmasks_reference(reference_bitmask)
 else
   reference_bitmask(:,:,s_hole ,1) = HF_bitmask
   reference_bitmask(:,:,s_part ,1) = iand(not(HF_bitmask(:,:)),full_ijkl_bitmask(:,:))
 endif

END_PROVIDER

BEGIN_PROVIDER [ integer, i_bitmask_gen ]
 implicit none
 BEGIN_DOC
 ! Current bitmask for the generators
 END_DOC
 i_bitmask_gen = 1
END_PROVIDER

BEGIN_PROVIDER [ integer, i_bitmask_ref ]
 implicit none
 BEGIN_DOC
 ! Current bitmask for the reference
 END_DOC
 i_bitmask_ref = 1
END_PROVIDER


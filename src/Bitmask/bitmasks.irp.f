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




BEGIN_PROVIDER [ integer(bit_kind), generators_bitmask_restart, (N_int,2,6,N_generators_bitmask) ]
 implicit none
 BEGIN_DOC
 ! Bitmasks for generator determinants.
 ! (N_int, alpha/beta, hole/particle, generator).
 !
 ! 3rd index is :
 !
 ! * 1 : hole     for single exc
 !
 ! * 2 : particle for single exc
 !
 ! * 3 : hole     for 1st exc of double
 !
 ! * 4 : particle for 1st exc of double
 !
 ! * 5 : hole     for 2nd exc of double
 !
 ! * 6 : particle for 2nd exc of double
 !
 END_DOC
 logical                        :: exists
 PROVIDE ezfio_filename

 call ezfio_has_bitmasks_generators(exists)
 if (exists) then
   call ezfio_get_bitmasks_generators(generators_bitmask_restart)
 else
   integer :: k, ispin
   do k=1,N_generators_bitmask
     do ispin=1,2
       generators_bitmask_restart(:,ispin,s_hole ,k) = full_ijkl_bitmask(:,d_hole1)
       generators_bitmask_restart(:,ispin,s_part ,k) = full_ijkl_bitmask(:,d_part1)
       generators_bitmask_restart(:,ispin,d_hole1,k) = full_ijkl_bitmask(:,d_hole1)
       generators_bitmask_restart(:,ispin,d_part1,k) = full_ijkl_bitmask(:,d_part1)
       generators_bitmask_restart(:,ispin,d_hole2,k) = full_ijkl_bitmask(:,d_hole2)
       generators_bitmask_restart(:,ispin,d_part2,k) = full_ijkl_bitmask(:,d_part2)
     enddo
   enddo
 endif

 integer :: i
 do k=1,N_generators_bitmask
   do ispin=1,2
     do i=1,N_int
      generators_bitmask_restart(i,ispin,s_hole ,k) = iand(full_ijkl_bitmask(i,d_hole1),generators_bitmask_restart(i,ispin,s_hole,k) )
      generators_bitmask_restart(i,ispin,s_part ,k) = iand(full_ijkl_bitmask(i,d_part1),generators_bitmask_restart(i,ispin,s_part,k) )
      generators_bitmask_restart(i,ispin,d_hole1,k) = iand(full_ijkl_bitmask(i,d_hole1),generators_bitmask_restart(i,ispin,d_hole1,k) )
      generators_bitmask_restart(i,ispin,d_part1,k) = iand(full_ijkl_bitmask(i,d_part1),generators_bitmask_restart(i,ispin,d_part1,k) )
      generators_bitmask_restart(i,ispin,d_hole2,k) = iand(full_ijkl_bitmask(i,d_hole2),generators_bitmask_restart(i,ispin,d_hole2,k) )
      generators_bitmask_restart(i,ispin,d_part2,k) = iand(full_ijkl_bitmask(i,d_part2),generators_bitmask_restart(i,ispin,d_part2,k) )
     enddo
   enddo
 enddo
END_PROVIDER


BEGIN_PROVIDER [ integer(bit_kind), generators_bitmask, (N_int,2,6,N_generators_bitmask) ]
 implicit none
 BEGIN_DOC
 ! Bitmasks for generator determinants.
 ! (N_int, alpha/beta, hole/particle, generator).
 !
 ! 3rd index is :
 !
 ! * 1 : hole     for single exc
 !
 ! * 2 : particle for single exc
 !
 ! * 3 : hole     for 1st exc of double
 !
 ! * 4 : particle for 1st exc of double
 !
 ! * 5 : hole     for 2nd exc of double
 !
 ! * 6 : particle for 2nd exc of double
 !
 END_DOC
 logical                        :: exists
 PROVIDE ezfio_filename

 call ezfio_has_bitmasks_generators(exists)
 if (exists) then
   call ezfio_get_bitmasks_generators(generators_bitmask)
 else
   integer :: k, ispin, i
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

 do k=1,N_generators_bitmask
   do ispin=1,2
     do i=1,N_int
      generators_bitmask(i,ispin,s_hole ,k) = iand(full_ijkl_bitmask(i,d_hole1),generators_bitmask(i,ispin,s_hole,k) )
      generators_bitmask(i,ispin,s_part ,k) = iand(full_ijkl_bitmask(i,d_part1),generators_bitmask(i,ispin,s_part,k) )
      generators_bitmask(i,ispin,d_hole1,k) = iand(full_ijkl_bitmask(i,d_hole1),generators_bitmask(i,ispin,d_hole1,k) )
      generators_bitmask(i,ispin,d_part1,k) = iand(full_ijkl_bitmask(i,d_part1),generators_bitmask(i,ispin,d_part1,k) )
      generators_bitmask(i,ispin,d_hole2,k) = iand(full_ijkl_bitmask(i,d_hole2),generators_bitmask(i,ispin,d_hole2,k) )
      generators_bitmask(i,ispin,d_part2,k) = iand(full_ijkl_bitmask(i,d_part2),generators_bitmask(i,ispin,d_part2,k) )
     enddo
   enddo
 enddo
END_PROVIDER

BEGIN_PROVIDER [ integer, N_cas_bitmask ]
 implicit none
 BEGIN_DOC
 ! Number of bitmasks for CAS
 END_DOC
 logical                        :: exists
 PROVIDE ezfio_filename
 
 call ezfio_has_bitmasks_N_mask_cas(exists)
 if (exists) then
   call ezfio_get_bitmasks_N_mask_cas(N_cas_bitmask)
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
   N_cas_bitmask = 1
 endif
 ASSERT (N_cas_bitmask > 0)

END_PROVIDER

BEGIN_PROVIDER [ integer(bit_kind), cas_bitmask, (N_int,2,N_cas_bitmask) ]
 implicit none
 BEGIN_DOC
 ! Bitmasks for CAS reference determinants. (N_int, alpha/beta, CAS reference)
 END_DOC
 logical                        :: exists
 integer                        :: i,i_part,i_gen,j,k
 PROVIDE ezfio_filename

 call ezfio_has_bitmasks_cas(exists)
 if (exists) then
   print*,'---------------------'
   print*,'CAS BITMASK RESTART'
   call ezfio_get_bitmasks_cas(cas_bitmask)
   print*,'---------------------'
 else
  if(N_generators_bitmask == 1)then
   do i=1,N_cas_bitmask
     cas_bitmask(:,:,i) = iand(not(HF_bitmask(:,:)),full_ijkl_bitmask(:,:))
   enddo
  else 
   i_part = 2
   i_gen = 1
   do j=1, N_cas_bitmask
    do i=1, N_int
      cas_bitmask(i,1,j) = generators_bitmask_restart(i,1,i_part,i_gen)
      cas_bitmask(i,2,j) = generators_bitmask_restart(i,2,i_part,i_gen)
    enddo
   enddo
  endif
 endif
 do i=1,N_cas_bitmask
   do j = 1, N_cas_bitmask
     do k=1,N_int
       cas_bitmask(k,j,i) = iand(cas_bitmask(k,j,i),full_ijkl_bitmask(k,j))
     enddo
   enddo
 enddo

END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), inact_bitmask, (N_int,2) ]
&BEGIN_PROVIDER [ integer(bit_kind), virt_bitmask, (N_int,2) ]
&BEGIN_PROVIDER [ integer, n_inact_orb ]
&BEGIN_PROVIDER [ integer, n_virt_orb ]
 implicit none
 BEGIN_DOC
 ! inact_bitmask : Bitmask of the inactive orbitals which are supposed to be doubly excited 
 ! in post CAS methods
 ! n_inact_orb   : Number of inactive orbitals
 ! virt_bitmask  : Bitmaks of vritual orbitals which are supposed to be recieve electrons 
 ! in post CAS methods
 ! n_virt_orb    : Number of virtual orbitals
 END_DOC
 logical                        :: exists
 integer                        :: j,i
 integer :: i_hole,i_part,i_gen

 n_inact_orb = 0
 n_virt_orb = 0
 if(N_generators_bitmask == 1)then
  do j = 1, N_int
   inact_bitmask(j,1) = xor(generators_bitmask_restart(j,1,1,1),cas_bitmask(j,1,1))
   inact_bitmask(j,2) = xor(generators_bitmask_restart(j,2,1,1),cas_bitmask(j,2,1))
   virt_bitmask(j,1) = xor(generators_bitmask_restart(j,1,2,1),cas_bitmask(j,1,1))
   virt_bitmask(j,2) = xor(generators_bitmask_restart(j,2,2,1),cas_bitmask(j,2,1))
   n_inact_orb += popcnt(inact_bitmask(j,1))
   n_virt_orb  += popcnt(virt_bitmask(j,1))
  enddo
 else 
   i_hole = 1
   i_gen = 1
   do i = 1, N_int
     inact_bitmask(i,1) = generators_bitmask(i,1,i_hole,i_gen)
     inact_bitmask(i,2) = generators_bitmask(i,2,i_hole,i_gen)
     n_inact_orb += popcnt(inact_bitmask(i,1))
   enddo
   i_part = 2
   i_gen = 3
   do i = 1, N_int
     virt_bitmask(i,1) = generators_bitmask(i,1,i_part,i_gen)
     virt_bitmask(i,2) = generators_bitmask(i,2,i_part,i_gen)
     n_virt_orb  += popcnt(virt_bitmask(i,1))
   enddo
 endif

END_PROVIDER



  BEGIN_PROVIDER [ integer, list_inact, (n_inact_orb)]
 &BEGIN_PROVIDER [ integer, list_virt, (n_virt_orb)]
 BEGIN_DOC
 ! list_inact : List of the inactive orbitals which are supposed to be doubly excited 
 ! in post CAS methods
 ! list_virt  : List of vritual orbitals which are supposed to be recieve electrons 
 ! in post CAS methods
 END_DOC
 implicit none
 integer :: occ_inact(N_int*bit_kind_size)
 integer :: itest,i
 occ_inact = 0
 call bitstring_to_list(inact_bitmask(1,1), occ_inact(1), itest, N_int)
 ASSERT(itest==n_inact_orb)
 do i = 1, n_inact_orb
  list_inact(i) = occ_inact(i)
 enddo

 occ_inact = 0
 call bitstring_to_list(virt_bitmask(1,1), occ_inact(1), itest, N_int)
 ASSERT(itest==n_virt_orb)
 do i = 1, n_virt_orb
  list_virt(i) = occ_inact(i)
 enddo

 END_PROVIDER 

 BEGIN_PROVIDER [ integer(bit_kind), reunion_of_core_inact_bitmask, (N_int,2)]
 implicit none
 BEGIN_DOC
 ! Reunion of the inactive, active and virtual bitmasks
 END_DOC
 integer :: i,j
 do i = 1, N_int
  reunion_of_core_inact_bitmask(i,1) = ior(core_bitmask(i,1),inact_bitmask(i,1))
  reunion_of_core_inact_bitmask(i,2) = ior(core_bitmask(i,2),inact_bitmask(i,2))
 enddo
 END_PROVIDER




 BEGIN_PROVIDER [ integer(bit_kind), reunion_of_bitmask, (N_int,2)]
 implicit none
 BEGIN_DOC
 ! Reunion of the inactive, active and virtual bitmasks
 END_DOC
 integer :: i,j
 do i = 1, N_int
  reunion_of_bitmask(i,1) = ior(ior(cas_bitmask(i,1,1),inact_bitmask(i,1)),virt_bitmask(i,1))
  reunion_of_bitmask(i,2) = ior(ior(cas_bitmask(i,2,1),inact_bitmask(i,2)),virt_bitmask(i,2))
 enddo
 END_PROVIDER


 BEGIN_PROVIDER [ integer(bit_kind), inact_virt_bitmask, (N_int,2)]
 implicit none
 BEGIN_DOC
 ! Reunion of the inactive and virtual bitmasks
 END_DOC
 integer :: i,j
 do i = 1, N_int
  inact_virt_bitmask(i,1) = ior(inact_bitmask(i,1),virt_bitmask(i,1))
  inact_virt_bitmask(i,2) = ior(inact_bitmask(i,2),virt_bitmask(i,2)) 
 enddo
 END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), core_bitmask, (N_int,2)]
 implicit none
 BEGIN_DOC
 ! Bitmask of the core orbitals that are never excited in post CAS method
 END_DOC
 integer :: i,j
 do i = 1, N_int
  core_bitmask(i,1) = iand(ref_bitmask(i,1),reunion_of_bitmask(i,1))
  core_bitmask(i,2) = iand(ref_bitmask(i,2),reunion_of_bitmask(i,2))
 enddo
 END_PROVIDER 

 BEGIN_PROVIDER [integer, list_core, (n_core_orb)]
 BEGIN_DOC
 ! List of the core orbitals that are never excited in post CAS method
 END_DOC
 implicit none
 integer :: occ_core(N_int*bit_kind_size)
 integer :: itest,i
 occ_core = 0
 call bitstring_to_list(core_bitmask(1,1), occ_core(1), itest, N_int)
 ASSERT(itest==n_core_orb)
 do i = 1, n_core_orb
  list_core(i) = occ_core(i)
 enddo
 END_PROVIDER

 BEGIN_PROVIDER [ integer, n_core_orb ]
 implicit none
 BEGIN_DOC
 ! Number of core orbitals that are never excited in post CAS method
 END_DOC
 logical                        :: exists
 integer                        :: j,i
 integer :: i_hole,i_part,i_gen

 n_core_orb = 0
 do j = 1, N_int
  n_core_orb += popcnt(core_bitmask(j,1))
 enddo
 END_PROVIDER 


BEGIN_PROVIDER [ integer, i_bitmask_gen ]
 implicit none
 BEGIN_DOC
 ! Current bitmask for the generators
 END_DOC
 i_bitmask_gen = 1
END_PROVIDER


 BEGIN_PROVIDER [ integer(bit_kind), unpaired_alpha_electrons, (N_int)]
 implicit none
 BEGIN_DOC
 ! Bitmask reprenting the unpaired alpha electrons in the HF_bitmask
 END_DOC
 integer :: i
 unpaired_alpha_electrons = 0_bit_kind
 do i = 1, N_int
  unpaired_alpha_electrons(i) = xor(HF_bitmask(i,1),HF_bitmask(i,2))
 enddo
 END_PROVIDER

BEGIN_PROVIDER [ integer, n_act_orb]
 BEGIN_DOC
 ! number of active orbitals
 END_DOC
 implicit none
 integer :: i,j
 n_act_orb = 0
 do i = 1, N_int
  n_act_orb += popcnt(cas_bitmask(i,1,1))
 enddo
END_PROVIDER

BEGIN_PROVIDER [integer, list_act, (n_act_orb)]
 BEGIN_DOC
 ! list of active orbitals
 END_DOC
 implicit none
 integer :: occ_act(N_int*bit_kind_size)
 integer :: itest,i
 occ_act = 0
 call bitstring_to_list(cas_bitmask(1,1,1), occ_act(1), itest, N_int)
 ASSERT(itest==n_act_orb)
 do i = 1, n_act_orb
  list_act(i) = occ_act(i)
 enddo

END_PROVIDER

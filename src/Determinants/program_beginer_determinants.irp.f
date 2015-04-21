program pouet
 implicit none
 print*,'HF energy = ',ref_bitmask_energy + nuclear_repulsion
 call routine

end
subroutine routine 
 use bitmasks
 implicit none
 integer :: i,j,k,l
 double precision :: hij,get_mo_bielec_integral
 double precision :: hmono,h_bi_ispin,h_bi_other_spin
 integer(bit_kind),allocatable  :: key_tmp(:,:)
 integer, allocatable :: occ(:,:)
 integer       :: n_occ_alpha, n_occ_beta
 ! First checks 
 print*,'N_int            = ',N_int
 print*,'mo_tot_num       = ',mo_tot_num
 print*,'mo_tot_num / 64+1= ',mo_tot_num/64+1
 ! We print the HF determinant
 do i = 1, N_int
  print*,'ref_bitmask(i,1) = ',ref_bitmask(i,1)
  print*,'ref_bitmask(i,2) = ',ref_bitmask(i,2)
 enddo
 print*,''
 print*,'Hartree Fock determinant ...'
 call debug_det(ref_bitmask,N_int)
 allocate(key_tmp(N_int,2))
 ! We initialize key_tmp to the Hartree Fock one
 key_tmp = ref_bitmask
 integer :: i_hole,i_particle,ispin,i_ok,other_spin
 ! We do a mono excitation on the top of the HF determinant
 write(*,*)'Enter the (hole, particle) couple for the mono excitation ...'
 read(5,*)i_hole,i_particle
!!i_hole = 4
!!i_particle = 20
 write(*,*)'Enter the ispin variable ...'
 write(*,*)'ispin = 1 ==> alpha '
 write(*,*)'ispin = 2 ==> beta  '
 read(5,*)ispin
 if(ispin == 1)then
  other_spin = 2
 else if(ispin == 2)then 
  other_spin = 1
 else
  print*,'PB !! '
  print*,'ispin must be 1 or 2 !'
  stop
 endif
!!ispin = 1
 call do_mono_excitation(key_tmp,i_hole,i_particle,ispin,i_ok)
 ! We check if it the excitation was possible with "i_ok"
 if(i_ok == -1)then
  print*,'i_ok = ',i_ok
  print*,'You can not do this excitation because of Pauli principle ...'
  print*,'check your hole particle couple, there must be something wrong ...'
  stop

 endif
 print*,'New det = '
 call debug_det(key_tmp,N_int)
 call i_H_j(key_tmp,ref_bitmask,N_int,hij)
 ! We calculate the H matrix element between the new determinant and HF
 print*,'<D_i|H|D_j>    = ',hij
 print*,''
 print*,''
 print*,'Recalculating it old school style ....'
 print*,''
 print*,''
 ! We recalculate this old school style !!!
 ! Mono electronic part
 hmono = mo_mono_elec_integral(i_hole,i_particle)
 print*,''
 print*,'Mono electronic part '
 print*,''
 print*,'<D_i|h(1)|D_j> = ',hmono
 h_bi_ispin = 0.d0
 h_bi_other_spin = 0.d0
 print*,''
 print*,'Getting all the info for the calculation of the bi electronic part ...'
 print*,''
 allocate (occ(N_int*bit_kind_size,2))
 ! We get the occupation of the alpha electrons in occ(:,1)
 call bitstring_to_list(key_tmp(1,1), occ(1,1), n_occ_alpha, N_int)
 print*,'n_occ_alpha    = ',n_occ_alpha
 print*,'elec_alpha_num = ',elec_alpha_num
 ! We get the occupation of the beta electrons in occ(:,2)
 call bitstring_to_list(key_tmp(1,2), occ(1,2), n_occ_beta, N_int)
 print*,'n_occ_beta    = ',n_occ_beta
 print*,'elec_beta_num = ',elec_beta_num
 ! We print the occupation of the alpha electrons 
 print*,'Alpha electrons !'
 do i = 1, n_occ_alpha
  print*,'i = ',i
  print*,'occ(i,1) = ',occ(i,1)
 enddo
 ! We print the occupation of the beta electrons 
 print*,'Alpha electrons !'
 do i = 1, n_occ_beta
  print*,'i = ',i
  print*,'occ(i,2) = ',occ(i,2)
 enddo
 integer                        :: exc(0:2,2,2),degree,h1,p1,h2,p2,s1,s2
 double precision :: phase

 call get_excitation_degree(key_tmp,ref_bitmask,degree,N_int)
 print*,'degree = ',degree
 call get_mono_excitation(ref_bitmask,key_tmp,exc,phase,N_int)
 call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
 print*,'h1  = ',h1
 print*,'p1  = ',p1
 print*,'s1  = ',s1
 print*,'phase = ',phase
 do i = 1, elec_num_tab(ispin)
  integer :: orb_occupied
  orb_occupied = occ(i,ispin)
  h_bi_ispin += get_mo_bielec_integral(i_hole,orb_occupied,i_particle,orb_occupied,mo_integrals_map) & 
               -get_mo_bielec_integral(i_hole,i_particle,orb_occupied,orb_occupied,mo_integrals_map)
 enddo
 print*,'h_bi_ispin = ',h_bi_ispin

 do i = 1, elec_num_tab(other_spin)
  orb_occupied = occ(i,other_spin)
  h_bi_other_spin += get_mo_bielec_integral(i_hole,orb_occupied,i_particle,orb_occupied,mo_integrals_map) 
 enddo
 print*,'h_bi_other_spin  = ',h_bi_other_spin
 print*,'h_bi_ispin + h_bi_other_spin = ',h_bi_ispin + h_bi_other_spin 

 print*,'Total matrix element =  ',phase*(h_bi_ispin + h_bi_other_spin + hmono)
!i = 1
!j = 1
!k = 1
!l = 1
!hij = get_mo_bielec_integral(i,j,k,l,mo_integrals_map)
!print*,'<ij|kl> = ',hij


end

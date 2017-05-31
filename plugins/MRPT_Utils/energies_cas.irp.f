BEGIN_PROVIDER [ double precision, energy_cas_dyall, (N_states)]
 implicit none
 integer :: i 
 double precision :: energies(N_states)
 do i = 1, N_states
  call u0_H_dyall_u0(energies,psi_active,psi_coef,n_det_ref,psi_det_size,psi_det_size,N_states,i)
  energy_cas_dyall(i) = energies(i)
  print*,  'energy_cas_dyall(i)',  energy_cas_dyall(i)
 enddo
END_PROVIDER 


BEGIN_PROVIDER [ double precision, energy_cas_dyall_no_exchange, (N_states)]
 implicit none
 integer :: i 
 double precision :: energies(N_states)
 do i = 1, N_states
  call u0_H_dyall_u0_no_exchange(energies,psi_active,psi_coef,n_det_ref,psi_det_size,psi_det_size,N_states,i)
  energy_cas_dyall_no_exchange(i) = energies(i)
  print*,  'energy_cas_dyall(i)_no_exchange',  energy_cas_dyall_no_exchange(i)
 enddo
END_PROVIDER 



BEGIN_PROVIDER [ double precision, one_creat, (n_act_orb,2,N_states)]
 implicit none
 integer :: i,j
 integer :: ispin
 integer :: orb, hole_particle,spin_exc
 double precision  :: norm_out(N_states)
 integer(bit_kind), allocatable :: psi_in_out(:,:,:)
 double precision, allocatable :: psi_in_out_coef(:,:)
 allocate (psi_in_out(N_int,2,n_det_ref),psi_in_out_coef(n_det_ref,N_states))
 use bitmasks

 integer :: iorb
 integer :: state_target
 double precision :: energies(n_states)
 do iorb = 1,n_act_orb
  do ispin = 1,2
   orb = list_act(iorb)
   hole_particle = 1
   spin_exc = ispin 
   do i = 1, n_det_ref
    do j = 1, n_states
      psi_in_out_coef(i,j) = psi_coef(i,j)
    enddo
    do j = 1, N_int
     psi_in_out(j,1,i) =  psi_active(j,1,i) 
     psi_in_out(j,2,i) =  psi_active(j,2,i) 
    enddo
   enddo
    do  state_target = 1,N_states
     call apply_exc_to_psi(orb,hole_particle,spin_exc, & 
             norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
     call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det_ref,n_det_ref,n_det_ref,N_states,state_target)
     one_creat(iorb,ispin,state_target) = energy_cas_dyall(state_target)  - energies(state_target)
    enddo
  enddo
 enddo
 deallocate(psi_in_out,psi_in_out_coef)

END_PROVIDER

BEGIN_PROVIDER [ double precision, one_anhil, (n_act_orb,2,N_states)]
 implicit none
 integer :: i,j
 integer :: ispin
 integer :: orb, hole_particle,spin_exc
 double precision  :: norm_out(N_states)
 integer(bit_kind), allocatable :: psi_in_out(:,:,:)
 double precision, allocatable :: psi_in_out_coef(:,:)
 use bitmasks
 allocate (psi_in_out(N_int,2,n_det_ref),psi_in_out_coef(n_det_ref,N_states))

 integer :: iorb
 integer :: state_target
 double precision :: energies(n_states)
 do iorb = 1,n_act_orb
  do ispin = 1,2
   orb = list_act(iorb)
   hole_particle = -1
   spin_exc = ispin 
   do i = 1, n_det_ref
    do j = 1, n_states
      psi_in_out_coef(i,j) = psi_coef(i,j)
    enddo
    do j = 1, N_int
     psi_in_out(j,1,i) =  psi_active(j,1,i) 
     psi_in_out(j,2,i) =  psi_active(j,2,i) 
    enddo
   enddo
   do state_target = 1, N_states
    call apply_exc_to_psi(orb,hole_particle,spin_exc, & 
            norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
    call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det_ref,n_det_ref,n_det_ref,N_states,state_target)
    one_anhil(iorb,ispin,state_target) = energy_cas_dyall(state_target)  -  energies(state_target)
   enddo
  enddo
 enddo
 deallocate(psi_in_out,psi_in_out_coef)

END_PROVIDER

BEGIN_PROVIDER [ double precision, two_creat, (n_act_orb,n_act_orb,2,2,N_states)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin
 integer :: orb_i, hole_particle_i,spin_exc_i
 integer :: orb_j, hole_particle_j,spin_exc_j
 double precision  :: norm_out(N_states)
 integer(bit_kind), allocatable :: psi_in_out(:,:,:)
 double precision, allocatable :: psi_in_out_coef(:,:)
 use bitmasks
 allocate (psi_in_out(N_int,2,n_det_ref),psi_in_out_coef(n_det_ref,N_states))

 integer :: iorb,jorb
 integer :: state_target
 double precision :: energies(n_states)
 do iorb = 1,n_act_orb
  do ispin = 1,2
   orb_i = list_act(iorb)
   hole_particle_i = 1
   spin_exc_i = ispin 
   do jorb = 1, n_act_orb
    do jspin = 1,2
     orb_j = list_act(jorb)
     hole_particle_j = 1
     spin_exc_j = jspin 
     do i = 1, n_det_ref
      do j = 1, n_states
        psi_in_out_coef(i,j) = psi_coef(i,j)
      enddo
      do j = 1, N_int
       psi_in_out(j,1,i) =  psi_active(j,1,i) 
       psi_in_out(j,2,i) =  psi_active(j,2,i) 
      enddo
     enddo
     do state_target = 1 , N_states
      call apply_exc_to_psi(orb_i,hole_particle_i,spin_exc_i, & 
              norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
      call apply_exc_to_psi(orb_j,hole_particle_j,spin_exc_j, & 
              norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
      call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det_ref,n_det_ref,n_det_ref,N_states,state_target)
      two_creat(iorb,jorb,ispin,jspin,state_target) = energy_cas_dyall(state_target)  -   energies(state_target)
     enddo
    enddo
   enddo
  enddo
 enddo
 deallocate(psi_in_out,psi_in_out_coef)

END_PROVIDER

BEGIN_PROVIDER [ double precision, two_anhil, (n_act_orb,n_act_orb,2,2,N_states)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin
 integer :: orb_i, hole_particle_i,spin_exc_i
 integer :: orb_j, hole_particle_j,spin_exc_j
 double precision  :: norm_out(N_states)
 integer(bit_kind), allocatable :: psi_in_out(:,:,:)
 double precision, allocatable :: psi_in_out_coef(:,:)
 use bitmasks
 allocate (psi_in_out(N_int,2,n_det_ref),psi_in_out_coef(n_det_ref,N_states))

 integer :: iorb,jorb
 integer :: state_target
 state_target = 1
 double precision :: energies(n_states)
 do iorb = 1,n_act_orb
  do ispin = 1,2
   orb_i = list_act(iorb)
   hole_particle_i = -1
   spin_exc_i = ispin 
   do jorb = 1, n_act_orb
    do jspin = 1,2
     orb_j = list_act(jorb)
     hole_particle_j = -1
     spin_exc_j = jspin 
     do i = 1, n_det_ref
      do j = 1, n_states
        psi_in_out_coef(i,j) = psi_coef(i,j)
      enddo
      do j = 1, N_int
       psi_in_out(j,1,i) =  psi_active(j,1,i) 
       psi_in_out(j,2,i) =  psi_active(j,2,i) 
      enddo
     enddo
     call apply_exc_to_psi(orb_i,hole_particle_i,spin_exc_i, & 
             norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
     call apply_exc_to_psi(orb_j,hole_particle_j,spin_exc_j, & 
             norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
     call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det_ref,n_det_ref,n_det_ref,N_states,state_target)
     two_anhil(iorb,jorb,ispin,jspin,state_target) = energy_cas_dyall(state_target)  -   energies(state_target)
    enddo
   enddo
  enddo
 enddo
 deallocate(psi_in_out,psi_in_out_coef)

END_PROVIDER

BEGIN_PROVIDER [ double precision, one_anhil_one_creat, (n_act_orb,n_act_orb,2,2,N_States)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin
 integer :: orb_i, hole_particle_i,spin_exc_i
 integer :: orb_j, hole_particle_j,spin_exc_j
 double precision  :: norm_out(N_states)
 integer(bit_kind), allocatable :: psi_in_out(:,:,:)
 double precision, allocatable :: psi_in_out_coef(:,:)
 use bitmasks

 allocate (psi_in_out(N_int,2,n_det_ref),psi_in_out_coef(n_det_ref,N_states))
 integer :: iorb,jorb
 integer :: state_target
 double precision :: energies(n_states)
 do iorb = 1,n_act_orb
  do ispin = 1,2
   orb_i = list_act(iorb)
   hole_particle_i =  1
   spin_exc_i = ispin 
   do jorb = 1, n_act_orb
    do jspin = 1,2
     orb_j = list_act(jorb)
     hole_particle_j = -1
     spin_exc_j = jspin 
      do i = 1, n_det_ref
       do j = 1, n_states
         psi_in_out_coef(i,j) = psi_coef(i,j)
       enddo
       do j = 1, N_int
        psi_in_out(j,1,i) =  psi_active(j,1,i) 
        psi_in_out(j,2,i) =  psi_active(j,2,i) 
       enddo
      enddo
      do state_target = 1, N_states
       call apply_exc_to_psi(orb_j,hole_particle_j,spin_exc_j, & 
               norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
       call apply_exc_to_psi(orb_i,hole_particle_i,spin_exc_i, & 
               norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
       if(orb_i == orb_j .and. ispin .ne. jspin)then  
        call u0_H_dyall_u0_no_exchange(energies,psi_in_out,psi_in_out_coef,n_det_ref,n_det_ref,n_det_ref,N_states,state_target)
        one_anhil_one_creat(iorb,jorb,ispin,jspin,state_target) = energy_cas_dyall_no_exchange(state_target)  -   energies(state_target)
       else
        call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det_ref,n_det_ref,n_det_ref,N_states,state_target)
        one_anhil_one_creat(iorb,jorb,ispin,jspin,state_target) = energy_cas_dyall(state_target)  -   energies(state_target)
       endif
      enddo
    enddo
   enddo
  enddo
 enddo

END_PROVIDER


BEGIN_PROVIDER [ double precision, two_anhil_one_creat, (n_act_orb,n_act_orb,n_act_orb,2,2,2,N_states)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin,kspin
 integer :: orb_i, hole_particle_i,spin_exc_i
 integer :: orb_j, hole_particle_j,spin_exc_j
 integer :: orb_k, hole_particle_k,spin_exc_k
 double precision  :: norm_out(N_states)
 integer(bit_kind), allocatable :: psi_in_out(:,:,:)
 double precision, allocatable :: psi_in_out_coef(:,:)
 use bitmasks
 allocate (psi_in_out(N_int,2,n_det_ref),psi_in_out_coef(n_det_ref,N_states))

 integer :: iorb,jorb
 integer :: korb
 integer :: state_target
 double precision :: energies(n_states)
 do iorb = 1,n_act_orb
  do ispin = 1,2
   orb_i = list_act(iorb)
   hole_particle_i =  1
   spin_exc_i = ispin 
   do jorb = 1, n_act_orb
    do jspin = 1,2
     orb_j = list_act(jorb)
     hole_particle_j = -1
     spin_exc_j = jspin 
     do korb = 1, n_act_orb
      do kspin = 1,2
       orb_k = list_act(korb)
       hole_particle_k = -1
       spin_exc_k = kspin 
       do i = 1, n_det_ref
        do j = 1, n_states
          psi_in_out_coef(i,j) = psi_coef(i,j)
        enddo
        do j = 1, N_int
         psi_in_out(j,1,i) =  psi_active(j,1,i) 
         psi_in_out(j,2,i) =  psi_active(j,2,i) 
        enddo
       enddo

       do state_target = 1, N_states 
        call apply_exc_to_psi(orb_j,hole_particle_j,spin_exc_j, & 
                norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
        call apply_exc_to_psi(orb_k,hole_particle_k,spin_exc_k, & 
                norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
        call apply_exc_to_psi(orb_i,hole_particle_i,spin_exc_i, & 
                norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
        call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det_ref,n_det_ref,n_det_ref,N_states,state_target)
        two_anhil_one_creat(iorb,jorb,korb,ispin,jspin,kspin,state_target) = energy_cas_dyall(state_target)  -  energies(state_target)
       enddo
      enddo
     enddo
    enddo
   enddo
  enddo
 enddo
 deallocate(psi_in_out,psi_in_out_coef)

END_PROVIDER

BEGIN_PROVIDER [ double precision, two_creat_one_anhil, (n_act_orb,n_act_orb,n_act_orb,2,2,2,N_states)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin,kspin
 integer :: orb_i, hole_particle_i,spin_exc_i
 integer :: orb_j, hole_particle_j,spin_exc_j
 integer :: orb_k, hole_particle_k,spin_exc_k
 double precision  :: norm_out(N_states)
 integer(bit_kind), allocatable :: psi_in_out(:,:,:)
 double precision, allocatable :: psi_in_out_coef(:,:)
 use bitmasks
 allocate (psi_in_out(N_int,2,n_det_ref),psi_in_out_coef(n_det_ref,N_states))

 integer :: iorb,jorb
 integer :: korb
 integer :: state_target
 double precision :: energies(n_states)
 do iorb = 1,n_act_orb
  do ispin = 1,2
   orb_i = list_act(iorb)
   hole_particle_i =  1
   spin_exc_i = ispin 
   do jorb = 1, n_act_orb
    do jspin = 1,2
     orb_j = list_act(jorb)
     hole_particle_j =  1
     spin_exc_j = jspin 
     do korb = 1, n_act_orb
      do kspin = 1,2
       orb_k = list_act(korb)
       hole_particle_k = -1
       spin_exc_k = kspin 
       do i = 1, n_det_ref
        do j = 1, n_states
          psi_in_out_coef(i,j) = psi_coef(i,j)
        enddo
        do j = 1, N_int
         psi_in_out(j,1,i) =  psi_active(j,1,i) 
         psi_in_out(j,2,i) =  psi_active(j,2,i) 
        enddo
       enddo
       do state_target = 1, N_states
        call apply_exc_to_psi(orb_k,hole_particle_k,spin_exc_k, & 
                norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
        call apply_exc_to_psi(orb_i,hole_particle_i,spin_exc_i, & 
                norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
        call apply_exc_to_psi(orb_j,hole_particle_j,spin_exc_j, & 
                norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
        call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det_ref,n_det_ref,n_det_ref,N_states,state_target)
        two_creat_one_anhil(iorb,jorb,korb,ispin,jspin,kspin,state_target) = energy_cas_dyall(state_target)  -  energies(state_target)
       enddo
      enddo
     enddo
    enddo
   enddo
  enddo
 enddo
 deallocate(psi_in_out,psi_in_out_coef)

END_PROVIDER

BEGIN_PROVIDER [ double precision, three_creat, (n_act_orb,n_act_orb,n_act_orb,2,2,2,N_states)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin,kspin
 integer :: orb_i, hole_particle_i,spin_exc_i
 integer :: orb_j, hole_particle_j,spin_exc_j
 integer :: orb_k, hole_particle_k,spin_exc_k
 double precision  :: norm_out(N_states)
 integer(bit_kind), allocatable :: psi_in_out(:,:,:)
 double precision, allocatable :: psi_in_out_coef(:,:)
 use bitmasks
 allocate (psi_in_out(N_int,2,n_det_ref),psi_in_out_coef(n_det_ref,N_states))

 integer :: iorb,jorb
 integer :: korb
 integer :: state_target
 double precision :: energies(n_states)
 do iorb = 1,n_act_orb
  do ispin = 1,2
   orb_i = list_act(iorb)
   hole_particle_i =  1
   spin_exc_i = ispin 
   do jorb = 1, n_act_orb
    do jspin = 1,2
     orb_j = list_act(jorb)
     hole_particle_j =  1
     spin_exc_j = jspin 
     do korb = 1, n_act_orb
      do kspin = 1,2
       orb_k = list_act(korb)
       hole_particle_k =  1
       spin_exc_k = kspin 
       do i = 1, n_det_ref
        do j = 1, n_states
          psi_in_out_coef(i,j) = psi_coef(i,j)
        enddo
        do j = 1, N_int
         psi_in_out(j,1,i) =  psi_active(j,1,i) 
         psi_in_out(j,2,i) =  psi_active(j,2,i) 
        enddo
       enddo
       do state_target = 1, N_states
        call apply_exc_to_psi(orb_i,hole_particle_i,spin_exc_i, & 
                norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
        call apply_exc_to_psi(orb_j,hole_particle_j,spin_exc_j, & 
                norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
        call apply_exc_to_psi(orb_k,hole_particle_k,spin_exc_k, & 
                norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
        call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det_ref,n_det_ref,n_det_ref,N_states,state_target)
        three_creat(iorb,jorb,korb,ispin,jspin,kspin,state_target) = energy_cas_dyall(state_target)  -  energies(state_target)
       enddo
      enddo
     enddo
    enddo
   enddo
  enddo
 enddo
 deallocate(psi_in_out,psi_in_out_coef)

END_PROVIDER

BEGIN_PROVIDER [ double precision, three_anhil, (n_act_orb,n_act_orb,n_act_orb,2,2,2,N_states)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin,kspin
 integer :: orb_i, hole_particle_i,spin_exc_i
 integer :: orb_j, hole_particle_j,spin_exc_j
 integer :: orb_k, hole_particle_k,spin_exc_k
 double precision  :: norm_out(N_states)
 integer(bit_kind), allocatable :: psi_in_out(:,:,:)
 double precision, allocatable :: psi_in_out_coef(:,:)
 use bitmasks
 allocate (psi_in_out(N_int,2,n_det_ref),psi_in_out_coef(n_det_ref,N_states))

 integer :: iorb,jorb
 integer :: korb
 integer :: state_target
 double precision :: energies(n_states)
 do iorb = 1,n_act_orb
  do ispin = 1,2
   orb_i = list_act(iorb)
   hole_particle_i = -1
   spin_exc_i = ispin 
   do jorb = 1, n_act_orb
    do jspin = 1,2
     orb_j = list_act(jorb)
     hole_particle_j = -1
     spin_exc_j = jspin 
     do korb = 1, n_act_orb
      do kspin = 1,2
       orb_k = list_act(korb)
       hole_particle_k = -1
       spin_exc_k = kspin 
       do i = 1, n_det_ref
        do j = 1, n_states
          psi_in_out_coef(i,j) = psi_coef(i,j)
        enddo
        do j = 1, N_int
         psi_in_out(j,1,i) =  psi_active(j,1,i) 
         psi_in_out(j,2,i) =  psi_active(j,2,i) 
        enddo
       enddo
       do state_target = 1, N_states
        call apply_exc_to_psi(orb_i,hole_particle_i,spin_exc_i, & 
                norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
        call apply_exc_to_psi(orb_j,hole_particle_j,spin_exc_j, & 
                norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
        call apply_exc_to_psi(orb_k,hole_particle_k,spin_exc_k, & 
                norm_out,psi_in_out,psi_in_out_coef, n_det_ref,n_det_ref,n_det_ref,N_states)
        call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det_ref,n_det_ref,n_det_ref,N_states,state_target)
        three_anhil(iorb,jorb,korb,ispin,jspin,kspin,state_target) = energy_cas_dyall(state_target)  -  energies(state_target)
       enddo
      enddo
     enddo
    enddo
   enddo
  enddo
 enddo
 deallocate(psi_in_out,psi_in_out_coef)

END_PROVIDER



 BEGIN_PROVIDER [ double precision, one_anhil_one_creat_inact_virt, (n_inact_orb,n_virt_orb,N_States)]
&BEGIN_PROVIDER [ double precision, one_anhil_one_creat_inact_virt_norm, (n_inact_orb,n_virt_orb,N_States,2)]
 implicit none
 integer :: i,vorb,j
 integer :: ispin,jspin
 integer :: orb_i, hole_particle_i
 integer :: orb_v
 double precision  :: norm_out(N_states)
 integer(bit_kind), allocatable :: psi_in_out(:,:,:)
 double precision, allocatable :: psi_in_out_coef(:,:)
 use bitmasks
 allocate (psi_in_out(N_int,2,n_det_ref),psi_in_out_coef(n_det_ref,N_states))

 integer :: iorb,jorb,i_ok
 integer :: state_target
 double precision :: energies(n_states)
 double precision :: hij
 double precision :: norm(N_states,2),norm_no_inv(N_states,2),norm_bis(N_states,2)
 double precision :: energies_alpha_beta(N_states,2)


 double precision :: thresh_norm
  
 thresh_norm = 1.d-10



 do vorb = 1,n_virt_orb
  orb_v = list_virt(vorb)
  do iorb = 1, n_inact_orb
   orb_i = list_inact(iorb)
    norm = 0.d0
    norm_bis = 0.d0
    do ispin = 1,2
     do state_target  =1 , N_states
      one_anhil_one_creat_inact_virt_norm(iorb,vorb,state_target,ispin) = 0.d0
     enddo
     do i = 1, n_det_ref
      do j = 1, N_int
       psi_in_out(j,1,i) =  psi_ref(j,1,i) 
       psi_in_out(j,2,i) =  psi_ref(j,2,i) 
      enddo
      call do_mono_excitation(psi_in_out(1,1,i),orb_i,orb_v,ispin,i_ok)
      if(i_ok.ne.1)then
       print*, orb_i,orb_v
       call debug_det(psi_in_out,N_int)
       print*, 'pb, i_ok ne 0 !!!'
      endif
      call i_H_j(psi_in_out(1,1,i),psi_ref(1,1,i),N_int,hij)
      do j = 1, n_states
        double precision ::  coef,contrib
        coef = psi_coef(i,j) !* psi_coef(i,j)
        psi_in_out_coef(i,j) = sign(coef,psi_coef(i,j)) * hij 
        norm(j,ispin) += psi_in_out_coef(i,j) * psi_in_out_coef(i,j) 
      enddo
     enddo
     do j = 1, N_states
      if (dabs(norm(j,ispin)) .le. thresh_norm)then
       norm(j,ispin) = 0.d0
       norm_no_inv(j,ispin) = norm(j,ispin)
       one_anhil_one_creat_inact_virt_norm(iorb,vorb,j,ispin) = 0.d0
      else
       norm_no_inv(j,ispin) = norm(j,ispin)
       one_anhil_one_creat_inact_virt_norm(iorb,vorb,j,ispin) = 1.d0 / norm(j,ispin)
       norm(j,ispin) = 1.d0/dsqrt(norm(j,ispin))
      endif
     enddo
     do i = 1, N_det
       do j = 1, N_states
        psi_in_out_coef(i,j) = psi_in_out_coef(i,j) * norm(j,ispin)
        norm_bis(j,ispin) += psi_in_out_coef(i,j) *  psi_in_out_coef(i,j)
       enddo
       do j = 1, N_int
        psi_in_out(j,1,i) = psi_active(j,1,i)
        psi_in_out(j,2,i) = psi_active(j,2,i)
       enddo
     enddo
     do state_target = 1, N_states
      energies_alpha_beta(state_target, ispin) = - mo_bielec_integral_jj_exchange(orb_i,orb_v)
!     energies_alpha_beta(state_target, ispin) = 0.d0
      if(norm(state_target,ispin) .ne. 0.d0 .and. dabs(norm_no_inv(state_target,ispin)) .gt. thresh_norm)then
       call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det_ref,n_det_ref,n_det_ref,N_states,state_target)
       energies_alpha_beta(state_target, ispin) +=  energies(state_target) 
      endif
     enddo
    enddo ! ispin 
   do state_target = 1, N_states
    if((norm_no_inv(state_target,1) + norm_no_inv(state_target,2)) .ne. 0.d0)then
!    one_anhil_one_creat_inact_virt(iorb,vorb,state_target) = 0.5d0 * & 
!   ( energy_cas_dyall(state_target)  -  energies_alpha_beta(state_target,1) + & 
!     energy_cas_dyall(state_target)  -  energies_alpha_beta(state_target,2) )
!    print*, energies_alpha_beta(state_target,1) , energies_alpha_beta(state_target,2) 
!    print*,  norm_bis(state_target,1) ,  norm_bis(state_target,2)
     one_anhil_one_creat_inact_virt(iorb,vorb,state_target) =  energy_cas_dyall(state_target) - &
      ( energies_alpha_beta(state_target,1) + energies_alpha_beta(state_target,2) ) & 
     /( norm_bis(state_target,1) +  norm_bis(state_target,2) )
    else  
     one_anhil_one_creat_inact_virt(iorb,vorb,state_target) = 0.d0
    endif
   enddo
  enddo
 enddo
 deallocate(psi_in_out,psi_in_out_coef)

END_PROVIDER


BEGIN_PROVIDER [ double precision, one_anhil_inact, (n_inact_orb,n_act_orb,N_States)]
 implicit none
 integer :: i,iorb,j
 integer :: ispin,jspin
 integer :: orb_i, hole_particle_i
 double precision  :: norm_out(N_states)
 integer(bit_kind), allocatable :: psi_in_out(:,:,:)
 double precision, allocatable :: psi_in_out_coef(:,:)
 use bitmasks
 allocate (psi_in_out(N_int,2,n_det_ref),psi_in_out_coef(n_det_ref,N_states))

 integer :: jorb,i_ok,aorb,orb_a
 integer :: state_target
 double precision :: energies(n_states)
 double precision :: hij
 double precision :: norm(N_states,2),norm_no_inv(N_states,2)
 double precision :: energies_alpha_beta(N_states,2)
 double precision :: norm_alpha_beta(N_states,2)

 double precision :: thresh_norm
  
 thresh_norm = 1.d-10

 do aorb = 1,n_act_orb
  orb_a = list_act(aorb)
  do iorb = 1, n_inact_orb
   orb_i = list_inact(iorb)
    do state_target = 1, N_states
     one_anhil_inact(iorb,aorb,state_target) = 0.d0
    enddo
    norm_alpha_beta = 0.d0
    norm = 0.d0
    norm_bis = 0.d0
    do ispin = 1,2
     do i = 1, n_det_ref
      do j = 1, N_int
       psi_in_out(j,1,i) =  psi_ref(j,1,i) 
       psi_in_out(j,2,i) =  psi_ref(j,2,i) 
      enddo
      call do_mono_excitation(psi_in_out(1,1,i),orb_i,orb_a,ispin,i_ok)
      if(i_ok.ne.1)then
       do j = 1, n_states
         psi_in_out_coef(i,j) = 0.d0
       enddo
      else
       call i_H_j(psi_in_out(1,1,i),psi_ref(1,1,i),N_int,hij)
       do j = 1, n_states
         double precision ::  coef,contrib
         coef = psi_coef(i,j) !* psi_coef(i,j)
         psi_in_out_coef(i,j) = sign(coef,psi_coef(i,j)) * hij 
         norm(j,ispin) += psi_in_out_coef(i,j) * psi_in_out_coef(i,j) 
       enddo
      endif
     enddo
     do j = 1, N_states
      if (dabs(norm(j,ispin)) .le. thresh_norm)then
       norm(j,ispin) = 0.d0
       norm_no_inv(j,ispin) = norm(j,ispin)
      else
       norm_no_inv(j,ispin) = norm(j,ispin)
       norm(j,ispin) = 1.d0/dsqrt(norm(j,ispin))
      endif
     enddo
     double precision :: norm_bis(N_states,2)
     do i = 1, N_det
       do j = 1, N_states
        psi_in_out_coef(i,j) = psi_in_out_coef(i,j) * norm(j,ispin)
        norm_bis(j,ispin) +=  psi_in_out_coef(i,j)* psi_in_out_coef(i,j)
       enddo
       do j = 1, N_int
        psi_in_out(j,1,i) = iand(psi_in_out(j,1,i),cas_bitmask(j,1,1))
        psi_in_out(j,2,i) = iand(psi_in_out(j,2,i),cas_bitmask(j,1,1))
       enddo
     enddo
     do state_target = 1, N_states
      energies_alpha_beta(state_target, ispin) = 0.d0
      if(norm(state_target,ispin) .ne. 0.d0 .and. dabs(norm_no_inv(state_target,ispin)) .gt. thresh_norm)then
       call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det_ref,n_det_ref,n_det_ref,N_states,state_target)
       energies_alpha_beta(state_target, ispin) +=  energies(state_target) 
      endif
     enddo
    enddo ! ispin 
   do state_target = 1, N_states
    if((norm_no_inv(state_target,1) + norm_no_inv(state_target,2)) .ne. 0.d0)then
     one_anhil_inact(iorb,aorb,state_target) =  energy_cas_dyall(state_target) - &
      ( energies_alpha_beta(state_target,1) + energies_alpha_beta(state_target,2) ) & 
     /( norm_bis(state_target,1) +  norm_bis(state_target,2) )
    else  
     one_anhil_inact(iorb,aorb,state_target) = 0.d0
    endif
   enddo
  enddo
 enddo
 deallocate(psi_in_out,psi_in_out_coef)
END_PROVIDER


BEGIN_PROVIDER [ double precision, one_creat_virt, (n_act_orb,n_virt_orb,N_States)]
 implicit none
 integer :: i,vorb,j
 integer :: ispin,jspin
 integer :: orb_i, hole_particle_i
 integer :: orb_v
 double precision  :: norm_out(N_states)
 integer(bit_kind), allocatable :: psi_in_out(:,:,:)
 double precision, allocatable :: psi_in_out_coef(:,:)
 use bitmasks
 allocate (psi_in_out(N_int,2,n_det_ref),psi_in_out_coef(n_det_ref,N_states))

 integer :: iorb,jorb,i_ok,aorb,orb_a
 integer :: state_target
 double precision :: energies(n_states)
 double precision :: hij
 double precision :: norm(N_states,2),norm_no_inv(N_states,2)
 double precision :: energies_alpha_beta(N_states,2)
 double precision :: norm_alpha_beta(N_states,2)

 double precision :: thresh_norm
  
 thresh_norm = 1.d-10

 do aorb = 1,n_act_orb
  orb_a = list_act(aorb)
  do vorb = 1, n_virt_orb
   orb_v = list_virt(vorb)
    do state_target = 1, N_states
     one_creat_virt(aorb,vorb,state_target) = 0.d0
    enddo
    norm_alpha_beta = 0.d0
    norm = 0.d0
    norm_bis = 0.d0
    do ispin = 1,2
     do i = 1, n_det_ref
      do j = 1, N_int
       psi_in_out(j,1,i) =  psi_ref(j,1,i) 
       psi_in_out(j,2,i) =  psi_ref(j,2,i) 
      enddo
      call do_mono_excitation(psi_in_out(1,1,i),orb_a,orb_v,ispin,i_ok)
      if(i_ok.ne.1)then
       do j = 1, n_states
         psi_in_out_coef(i,j) = 0.d0
       enddo
      else
       call i_H_j(psi_in_out(1,1,i),psi_ref(1,1,i),N_int,hij)
       do j = 1, n_states
         double precision ::  coef,contrib
         coef = psi_coef(i,j) !* psi_coef(i,j)
         psi_in_out_coef(i,j) = sign(coef,psi_coef(i,j)) * hij 
         norm(j,ispin) += psi_in_out_coef(i,j) * psi_in_out_coef(i,j) 
       enddo
      endif
     enddo
     do j = 1, N_states
      if (dabs(norm(j,ispin)) .le. thresh_norm)then
       norm(j,ispin) = 0.d0
       norm_no_inv(j,ispin) = norm(j,ispin)
      else
       norm_no_inv(j,ispin) = norm(j,ispin)
       norm(j,ispin) = 1.d0/dsqrt(norm(j,ispin))
      endif
     enddo
     double precision :: norm_bis(N_states,2)
     do i = 1, N_det
       do j = 1, N_states
        psi_in_out_coef(i,j) = psi_in_out_coef(i,j) * norm(j,ispin)
        norm_bis(j,ispin) +=  psi_in_out_coef(i,j)* psi_in_out_coef(i,j)
       enddo
       do j = 1, N_int
        psi_in_out(j,1,i) = iand(psi_in_out(j,1,i),cas_bitmask(j,1,1))
        psi_in_out(j,2,i) = iand(psi_in_out(j,2,i),cas_bitmask(j,1,1))
       enddo
     enddo
     do state_target = 1, N_states
      energies_alpha_beta(state_target, ispin) = 0.d0
      if(norm(state_target,ispin) .ne. 0.d0 .and. dabs(norm_no_inv(state_target,ispin)) .gt. thresh_norm)then
       call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det_ref,n_det_ref,n_det_ref,N_states,state_target)
!      print*,  energies(state_target)
       energies_alpha_beta(state_target, ispin) +=  energies(state_target) 
      endif
     enddo
    enddo ! ispin 
   do state_target = 1, N_states
    if((norm_no_inv(state_target,1) + norm_no_inv(state_target,2)) .ne. 0.d0)then
     one_creat_virt(aorb,vorb,state_target) =  energy_cas_dyall(state_target) - &
      ( energies_alpha_beta(state_target,1) + energies_alpha_beta(state_target,2) ) & 
     /( norm_bis(state_target,1) +  norm_bis(state_target,2) )
    else  
     one_creat_virt(aorb,vorb,state_target) = 0.d0
    endif
!   print*, '********'
!   print*, energies_alpha_beta(state_target,1) , energies_alpha_beta(state_target,2)
!   print*, norm_bis(state_target,1) ,  norm_bis(state_target,2)
!   print*, one_creat_virt(aorb,vorb,state_target)
!   print*, one_anhil(aorb,1,state_target)
   enddo
  enddo
 enddo
 deallocate(psi_in_out,psi_in_out_coef)

END_PROVIDER


 BEGIN_PROVIDER [ double precision, one_anhil_one_creat_inact_virt_bis, (n_inact_orb,n_virt_orb,N_det,N_States)]
&BEGIN_PROVIDER [ double precision, corr_e_from_1h1p, (N_States)]
 implicit none
 integer :: i,vorb,j
 integer :: ispin,jspin
 integer :: orb_i, hole_particle_i
 integer :: orb_v
 double precision  :: norm_out(N_states),diag_elem(N_det),interact_psi0(N_det)
 double precision  :: delta_e_inact_virt(N_states)
 integer(bit_kind), allocatable :: psi_in_out(:,:,:)
 double precision, allocatable :: psi_in_out_coef(:,:)
 double precision, allocatable :: H_matrix(:,:),eigenvectors(:,:),eigenvalues(:)
 use bitmasks
 allocate (psi_in_out(N_int,2,n_det_ref),psi_in_out_coef(n_det_ref,N_states),H_matrix(N_det+1,N_det+1))
 allocate (eigenvectors(size(H_matrix,1),N_det+1))
 allocate (eigenvalues(N_det+1))

 integer :: iorb,jorb,i_ok
 integer :: state_target
 double precision :: energies(n_states)
 double precision :: hij
 double precision :: energies_alpha_beta(N_states,2)


 double precision :: accu(N_states),norm
 double precision :: amplitudes_alpha_beta(N_det,2)
 double precision :: delta_e_alpha_beta(N_det,2)
  
 corr_e_from_1h1p = 0.d0
 do vorb = 1,n_virt_orb
  orb_v = list_virt(vorb)
  do iorb = 1, n_inact_orb
   orb_i = list_inact(iorb)
!   print*, '---------------------------------------------------------------------------'
    do j = 1, N_states
     delta_e_inact_virt(j) = fock_core_inactive_total_spin_trace(orb_i,j) & 
                           - fock_virt_total_spin_trace(orb_v,j) 
    enddo
    do ispin = 1,2
     do i = 1, n_det_ref
      do j = 1, N_int
       psi_in_out(j,1,i) =  psi_ref(j,1,i) 
       psi_in_out(j,2,i) =  psi_ref(j,2,i) 
      enddo
      call do_mono_excitation(psi_in_out(1,1,i),orb_i,orb_v,ispin,i_ok)
      if(i_ok.ne.1)then
       print*, orb_i,orb_v
       call debug_det(psi_in_out,N_int)
       print*, 'pb, i_ok ne 0 !!!'
      endif
      interact_psi0(i) = 0.d0
      do j = 1 , N_det
       call i_H_j(psi_in_out(1,1,i),psi_ref(1,1,j),N_int,hij)
       interact_psi0(i) += hij * psi_coef(j,1)
      enddo
      do j = 1, N_int
       psi_in_out(j,1,i) =  psi_active(j,1,i) 
       psi_in_out(j,2,i) =  psi_active(j,2,i) 
      enddo
      call i_H_j_dyall(psi_active(1,1,i),psi_active(1,1,i),N_int,hij)
      diag_elem(i) = hij
     enddo
     do state_target = 1, N_states
      ! Building the Hamiltonian matrix
      H_matrix(1,1) = energy_cas_dyall(state_target)
      do i = 1, N_det
       ! interaction with psi0
       H_matrix(1,i+1)   = interact_psi0(i)!* psi_coef(i,state_target) 
       H_matrix(i+1,1)   = interact_psi0(i)!* psi_coef(i,state_target) 
       ! diagonal elements 
       H_matrix(i+1,i+1) = diag_elem(i) - delta_e_inact_virt(state_target)
!      print*, 'H_matrix(i+1,i+1)',H_matrix(i+1,i+1)
       do j = i+1, N_det
        call i_H_j_dyall(psi_in_out(1,1,i),psi_in_out(1,1,j),N_int,hij)
        H_matrix(i+1,j+1) = hij  !0.d0 !
        H_matrix(j+1,i+1) = hij  !0.d0 !
       enddo
      enddo
      print*, '***'
      do i = 1, N_det+1
       write(*,'(100(F16.10,1X))')H_matrix(i,:)
      enddo
      call lapack_diag(eigenvalues,eigenvectors,H_matrix,size(H_matrix,1),N_det+1)
      corr_e_from_1h1p(state_target) += eigenvalues(1) - energy_cas_dyall(state_target)   
      norm = 0.d0
      do i = 1, N_det
       psi_in_out_coef(i,state_target) = eigenvectors(i+1,1)/eigenvectors(1,1)
!!     if(dabs(psi_coef(i,state_target)*) .gt. 1.d-8)then
       if(dabs(psi_in_out_coef(i,state_target)) .gt. 1.d-8)then
!      if(dabs(interact_psi0(i)) .gt. 1.d-8)then
        delta_e_alpha_beta(i,ispin) = H_matrix(1,i+1) / psi_in_out_coef(i,state_target)   
!       delta_e_alpha_beta(i,ispin) = interact_psi0(i) / psi_in_out_coef(i,state_target)   
        amplitudes_alpha_beta(i,ispin) = psi_in_out_coef(i,state_target) / psi_coef(i,state_target)
       else 
        amplitudes_alpha_beta(i,ispin) = 0.d0
        delta_e_alpha_beta(i,ispin) = delta_e_inact_virt(state_target)
       endif
!!     one_anhil_one_creat_inact_virt_bis(iorb,vorb,i,ispin,state_target) = amplitudes_alpha_beta(i,ispin) 
       norm += psi_in_out_coef(i,state_target) * psi_in_out_coef(i,state_target)
      enddo
      print*, 'Coef '
      write(*,'(100(1X,F16.10))')psi_coef(1:N_det,state_target)
      write(*,'(100(1X,F16.10))')psi_in_out_coef(:,state_target)
      double precision :: coef_tmp(N_det)
      do i = 1, N_det
       coef_tmp(i) = psi_coef(i,1) * interact_psi0(i) / delta_e_alpha_beta(i,ispin)
      enddo
      write(*,'(100(1X,F16.10))')coef_tmp(:)
      print*, 'naked interactions'
      write(*,'(100(1X,F16.10))')interact_psi0(:)
      print*, ''
      
      print*, 'norm ',norm
      norm = 1.d0/(norm)
      accu(state_target) = 0.d0
      do i = 1, N_det
       accu(state_target) += psi_in_out_coef(i,state_target) * psi_in_out_coef(i,state_target) * H_matrix(i+1,i+1)
       do j = i+1, N_det
        accu(state_target) += 2.d0 * psi_in_out_coef(i,state_target) * psi_in_out_coef(j,state_target) *  H_matrix(i+1,j+1) 
       enddo
      enddo
      accu(state_target) = accu(state_target) * norm
      print*, delta_e_inact_virt(state_target)
      print*, eigenvalues(1),accu(state_target),eigenvectors(1,1)
      print*, energy_cas_dyall(state_target) - accu(state_target), one_anhil_one_creat_inact_virt(iorb,vorb,state_target) + delta_e_inact_virt(state_target)

     enddo
    enddo ! ispin 
    do state_target = 1, N_states
      do i = 1, N_det
      one_anhil_one_creat_inact_virt_bis(iorb,vorb,i,state_target) =  0.5d0 * &
      ( delta_e_alpha_beta(i,1) + delta_e_alpha_beta(i,1))
      enddo
    enddo
      print*, '***'
      write(*,'(100(1X,F16.10))')
      write(*,'(100(1X,F16.10))')delta_e_alpha_beta(:,2)
 !    write(*,'(100(1X,F16.10))')one_anhil_one_creat_inact_virt_bis(iorb,vorb,:,1,:)
 !    write(*,'(100(1X,F16.10))')one_anhil_one_creat_inact_virt_bis(iorb,vorb,:,2,:)
    print*, '---------------------------------------------------------------------------'
   enddo
  enddo
 deallocate(psi_in_out,psi_in_out_coef,H_matrix,eigenvectors,eigenvalues)
 print*, 'corr_e_from_1h1p,',corr_e_from_1h1p(:)

END_PROVIDER

subroutine give_singles_and_partial_doubles_1h1p_contrib(matrix_1h1p,e_corr_from_1h1p_singles)
 implicit none
 double precision , intent(inout) :: matrix_1h1p(N_det,N_det,N_states)
 double precision , intent(out)   :: e_corr_from_1h1p_singles(N_states)
 integer :: i,vorb,j
 integer :: ispin,jspin
 integer :: orb_i, hole_particle_i
 integer :: orb_v
 double precision  :: norm_out(N_states),diag_elem(N_det),interact_psi0(N_det)
 double precision  :: delta_e_inact_virt(N_states)
 integer(bit_kind), allocatable :: psi_in_out(:,:,:)
 double precision, allocatable :: psi_in_out_coef(:,:)
 double precision, allocatable :: H_matrix(:,:),eigenvectors(:,:),eigenvalues(:),interact_cas(:,:)
 double precision, allocatable :: delta_e_det(:,:)
 use bitmasks
 allocate (psi_in_out(N_int,2,n_det_ref),psi_in_out_coef(n_det_ref,N_states),H_matrix(N_det+1,N_det+1))
 allocate (eigenvectors(size(H_matrix,1),N_det+1))
 allocate (eigenvalues(N_det+1),interact_cas(N_det,N_det))
 allocate (delta_e_det(N_det,N_det))

 integer :: iorb,jorb,i_ok
 integer :: state_target
 double precision :: energies(n_states)
 double precision :: hij
 double precision :: energies_alpha_beta(N_states,2)
 double precision :: lamda_pt2(N_det)


 double precision :: accu(N_states),norm
 double precision :: amplitudes_alpha_beta(N_det,2)
 double precision :: delta_e_alpha_beta(N_det,2)
 double precision :: coef_array(N_states)
 double precision :: coef_perturb(N_det)
 double precision :: coef_perturb_bis(N_det)
  
 do vorb = 1,n_virt_orb
  orb_v = list_virt(vorb)
  do iorb = 1, n_inact_orb
   orb_i = list_inact(iorb)
    do j = 1, N_states
     delta_e_inact_virt(j) = fock_core_inactive_total_spin_trace(orb_i,j) & 
                           - fock_virt_total_spin_trace(orb_v,j) 
    enddo
    do ispin = 1,2
     do i = 1, n_det_ref
      do j = 1, N_int
       psi_in_out(j,1,i) =  psi_ref(j,1,i) 
       psi_in_out(j,2,i) =  psi_ref(j,2,i) 
      enddo
      call do_mono_excitation(psi_in_out(1,1,i),orb_i,orb_v,ispin,i_ok)
      if(i_ok.ne.1)then
       print*, orb_i,orb_v
       call debug_det(psi_in_out,N_int)
       print*, 'pb, i_ok ne 0 !!!'
      endif
      interact_psi0(i) = 0.d0
      do j = 1 , N_det
       call i_H_j(psi_in_out(1,1,i),psi_ref(1,1,j),N_int,hij)
       call get_delta_e_dyall(psi_ref(1,1,j),psi_in_out(1,1,i),delta_e_det(i,j))
       interact_cas(i,j) = hij
       interact_psi0(i) += hij * psi_coef(j,1)
      enddo
      do j = 1, N_int
       psi_in_out(j,1,i) =  psi_active(j,1,i) 
       psi_in_out(j,2,i) =  psi_active(j,2,i) 
      enddo
      call i_H_j_dyall(psi_active(1,1,i),psi_active(1,1,i),N_int,hij)
      diag_elem(i) = hij
     enddo
     do state_target = 1, N_states
      ! Building the Hamiltonian matrix
      H_matrix(1,1) = energy_cas_dyall(state_target)
      do i = 1, N_det
       ! interaction with psi0
       H_matrix(1,i+1)   = interact_psi0(i)!* psi_coef(i,state_target) 
       H_matrix(i+1,1)   = interact_psi0(i)!* psi_coef(i,state_target) 
       ! diagonal elements 
       H_matrix(i+1,i+1) = diag_elem(i) - delta_e_inact_virt(state_target)
!      print*, 'H_matrix(i+1,i+1)',H_matrix(i+1,i+1)
       do j = i+1, N_det
        call i_H_j_dyall(psi_in_out(1,1,i),psi_in_out(1,1,j),N_int,hij)
        H_matrix(i+1,j+1) = hij  !0.d0 !
        H_matrix(j+1,i+1) = hij  !0.d0 !
       enddo
      enddo
      call lapack_diag(eigenvalues,eigenvectors,H_matrix,size(H_matrix,1),N_det+1)
      e_corr_from_1h1p_singles(state_target) += eigenvalues(1) - energy_cas_dyall(state_target)   
      
      do i = 1, N_det
       psi_in_out_coef(i,state_target) = eigenvectors(i+1,1)/eigenvectors(1,1)
       coef_perturb(i) = 0.d0
       do j = 1, N_det
        coef_perturb(i) += psi_coef(j,state_target) * interact_cas(i,j) *1.d0/delta_e_det(i,j)
       enddo
       coef_perturb_bis(i) = interact_psi0(i) / (eigenvalues(1) - H_matrix(i+1,i+1))
       if(dabs(interact_psi0(i)) .gt. 1.d-12)then
        lamda_pt2(i) = psi_in_out_coef(i,state_target) / interact_psi0(i)
       else 
        lamda_pt2(i) =energy_cas_dyall(state_target) - H_matrix(i+1,i+1)
       endif
      enddo
      if(dabs(eigenvalues(1) - energy_cas_dyall(state_target)).gt.1.d-10)then
       print*, ''
       do i = 1, N_det+1
        write(*,'(100(F16.10))') H_matrix(i,:)
       enddo
       accu = 0.d0
       do i = 1, N_det
        accu(state_target) += psi_in_out_coef(i,state_target) * interact_psi0(i)
       enddo
       print*, ''
       print*, 'e corr diagonal  ',accu(state_target)
       accu = 0.d0
       do i = 1, N_det
        accu(state_target) += coef_perturb(i) * interact_psi0(i)
       enddo
       print*, 'e corr perturb   ',accu(state_target)
       accu = 0.d0
       do i = 1, N_det
        accu(state_target) += coef_perturb_bis(i) * interact_psi0(i)
       enddo
       print*, 'e corr perturb EN',accu(state_target)
       print*, ''
       print*, 'coef diagonalized'
       write(*,'(100(F16.10,1X))')psi_in_out_coef(:,state_target)
       print*, 'coef_perturb'
       write(*,'(100(F16.10,1X))')coef_perturb(:)
       print*, 'coef_perturb EN'
       write(*,'(100(F16.10,1X))')coef_perturb_bis(:)
      endif
      integer :: k
      do k = 1, N_det
       do i = 1, N_det
        matrix_1h1p(i,i,state_target) += interact_cas(k,i) * interact_cas(k,i) * lamda_pt2(k)
        do j = i+1, N_det
         matrix_1h1p(i,j,state_target) += interact_cas(k,i) * interact_cas(k,j) * lamda_pt2(k) 
         matrix_1h1p(j,i,state_target) += interact_cas(k,i) * interact_cas(k,j) * lamda_pt2(k) 
        enddo
       enddo
      enddo
     enddo
    enddo ! ispin 
   enddo
  enddo
 deallocate(psi_in_out,psi_in_out_coef,H_matrix,eigenvectors,eigenvalues,interact_cas)
 deallocate(delta_e_det)
end

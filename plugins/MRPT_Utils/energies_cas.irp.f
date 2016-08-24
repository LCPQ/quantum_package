BEGIN_PROVIDER [ double precision, energy_cas_dyall, (N_states)]
 implicit none
 integer :: i 
 double precision :: energies(N_states_diag)
 do i = 1, N_states
  call u0_H_dyall_u0(energies,psi_active,psi_coef,n_det,psi_det_size,psi_det_size,N_states_diag,i)
  energy_cas_dyall(i) = energies(i)
 enddo
END_PROVIDER 


BEGIN_PROVIDER [ double precision, one_creation, (n_act_orb,2)]
 implicit none
 integer :: i,j
 integer :: ispin
 integer :: orb, hole_particle,spin_exc
 double precision  :: norm_out(N_states_diag)
 integer(bit_kind) :: psi_in_out(N_int,2,n_det)
 double precision :: psi_in_out_coef(n_det,N_states_diag)
 use bitmasks

 integer :: iorb
 do iorb = 1,n_act_orb
  do ispin = 1,2
   orb = list_act(iorb)
   hole_particle = 1
   spin_exc = ispin 
   do i = 1, n_det
    do j = 1, n_states_diag
      psi_in_out_coef(i,j) = psi_coef(i,j)
    enddo
    do j = 1, N_int
     psi_in_out(j,1,i) =  psi_active(j,1,i) 
     psi_in_out(j,1,i) =  psi_active(j,2,i) 
    enddo
   enddo
   integer :: state_target
   state_target = 1
   double precision :: energies(n_states_diag)
   call apply_exc_to_psi(orb,hole_particle,spin_exc, & 
           norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
   call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det,n_det,n_det,N_states_diag,state_target)
   one_creation(iorb,ispin) = energy_cas_dyall(state_target)  - energies(state_target)
  enddo
 enddo

END_PROVIDER

BEGIN_PROVIDER [ double precision, one_anhilation, (n_act_orb,2)]
 implicit none
 integer :: i,j
 integer :: ispin
 integer :: orb, hole_particle,spin_exc
 double precision  :: norm_out(N_states_diag)
 integer(bit_kind) :: psi_in_out(N_int,2,n_det)
 double precision :: psi_in_out_coef(n_det,N_states_diag)
 use bitmasks

 integer :: iorb
 do iorb = 1,n_act_orb
  do ispin = 1,2
   orb = list_act(iorb)
   hole_particle = -1
   spin_exc = ispin 
   do i = 1, n_det
    do j = 1, n_states_diag
      psi_in_out_coef(i,j) = psi_coef(i,j)
    enddo
    do j = 1, N_int
     psi_in_out(j,1,i) =  psi_active(j,1,i) 
     psi_in_out(j,1,i) =  psi_active(j,2,i) 
    enddo
   enddo
   integer :: state_target
   state_target = 1
   double precision :: energies(n_states_diag)
   call apply_exc_to_psi(orb,hole_particle,spin_exc, & 
           norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
   call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det,n_det,n_det,N_states_diag,state_target)
   one_anhilation(iorb,ispin) = energy_cas_dyall(state_target)  -  energies(state_target)
  enddo
 enddo

END_PROVIDER

BEGIN_PROVIDER [ double precision, two_creation, (n_act_orb,n_act_orb,2,2)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin
 integer :: orb_i, hole_particle_i,spin_exc_i
 integer :: orb_j, hole_particle_j,spin_exc_j
 double precision  :: norm_out(N_states_diag)
 integer(bit_kind) :: psi_in_out(N_int,2,n_det)
 double precision :: psi_in_out_coef(n_det,N_states_diag)
 use bitmasks

 integer :: iorb,jorb
 integer :: state_target
 state_target = 1
 double precision :: energies(n_states_diag)
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
     do i = 1, n_det
      do j = 1, n_states_diag
        psi_in_out_coef(i,j) = psi_coef(i,j)
      enddo
      do j = 1, N_int
       psi_in_out(j,1,i) =  psi_active(j,1,i) 
       psi_in_out(j,1,i) =  psi_active(j,2,i) 
      enddo
     enddo
     call apply_exc_to_psi(orb_i,hole_particle_i,spin_exc_i, & 
             norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
     call apply_exc_to_psi(orb_j,hole_particle_j,spin_exc_j, & 
             norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
     call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det,n_det,n_det,N_states_diag,state_target)
     two_creation(iorb,jorb,ispin,jspin) = energy_cas_dyall(state_target)  -   energies(state_target)
    enddo
   enddo
  enddo
 enddo

END_PROVIDER

BEGIN_PROVIDER [ double precision, two_anhilation, (n_act_orb,n_act_orb,2,2)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin
 integer :: orb_i, hole_particle_i,spin_exc_i
 integer :: orb_j, hole_particle_j,spin_exc_j
 double precision  :: norm_out(N_states_diag)
 integer(bit_kind) :: psi_in_out(N_int,2,n_det)
 double precision :: psi_in_out_coef(n_det,N_states_diag)
 use bitmasks

 integer :: iorb,jorb
 integer :: state_target
 state_target = 1
 double precision :: energies(n_states_diag)
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
     do i = 1, n_det
      do j = 1, n_states_diag
        psi_in_out_coef(i,j) = psi_coef(i,j)
      enddo
      do j = 1, N_int
       psi_in_out(j,1,i) =  psi_active(j,1,i) 
       psi_in_out(j,1,i) =  psi_active(j,2,i) 
      enddo
     enddo
     call apply_exc_to_psi(orb_i,hole_particle_i,spin_exc_i, & 
             norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
     call apply_exc_to_psi(orb_j,hole_particle_j,spin_exc_j, & 
             norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
     call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det,n_det,n_det,N_states_diag,state_target)
     two_anhilation(iorb,jorb,ispin,jspin) = energy_cas_dyall(state_target)  -   energies(state_target)
    enddo
   enddo
  enddo
 enddo

END_PROVIDER

BEGIN_PROVIDER [ double precision, one_anhilation_one_creation, (n_act_orb,n_act_orb,2,2)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin
 integer :: orb_i, hole_particle_i,spin_exc_i
 integer :: orb_j, hole_particle_j,spin_exc_j
 double precision  :: norm_out(N_states_diag)
 integer(bit_kind) :: psi_in_out(N_int,2,n_det)
 double precision :: psi_in_out_coef(n_det,N_states_diag)
 use bitmasks

 integer :: iorb,jorb
 integer :: state_target
 state_target = 1
 double precision :: energies(n_states_diag)
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
     do i = 1, n_det
      do j = 1, n_states_diag
        psi_in_out_coef(i,j) = psi_coef(i,j)
      enddo
      do j = 1, N_int
       psi_in_out(j,1,i) =  psi_active(j,1,i) 
       psi_in_out(j,1,i) =  psi_active(j,2,i) 
      enddo
     enddo
     call apply_exc_to_psi(orb_i,hole_particle_i,spin_exc_i, & 
             norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
     call apply_exc_to_psi(orb_j,hole_particle_j,spin_exc_j, & 
             norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
     call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det,n_det,n_det,N_states_diag,state_target)
     one_anhilation_one_creation(iorb,jorb,ispin,jspin) = energy_cas_dyall(state_target)  -   energies(state_target)
    enddo
   enddo
  enddo
 enddo

END_PROVIDER

BEGIN_PROVIDER [ double precision, two_anhilation_one_creation, (n_act_orb,n_act_orb,n_act_orb,2,2,2)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin,kspin
 integer :: orb_i, hole_particle_i,spin_exc_i
 integer :: orb_j, hole_particle_j,spin_exc_j
 integer :: orb_k, hole_particle_k,spin_exc_k
 double precision  :: norm_out(N_states_diag)
 integer(bit_kind) :: psi_in_out(N_int,2,n_det)
 double precision :: psi_in_out_coef(n_det,N_states_diag)
 use bitmasks

 integer :: iorb,jorb
 integer :: korb
 integer :: state_target
 state_target = 1
 double precision :: energies(n_states_diag)
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
       do i = 1, n_det
        do j = 1, n_states_diag
          psi_in_out_coef(i,j) = psi_coef(i,j)
        enddo
        do j = 1, N_int
         psi_in_out(j,1,i) =  psi_active(j,1,i) 
         psi_in_out(j,1,i) =  psi_active(j,2,i) 
        enddo
       enddo
       call apply_exc_to_psi(orb_i,hole_particle_i,spin_exc_i, & 
               norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
       call apply_exc_to_psi(orb_j,hole_particle_j,spin_exc_j, & 
               norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
       call apply_exc_to_psi(orb_k,hole_particle_k,spin_exc_k, & 
               norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
       call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det,n_det,n_det,N_states_diag,state_target)
       two_anhilation_one_creation(iorb,jorb,korb,ispin,jspin,kspin) = energy_cas_dyall(state_target)  -  energies(state_target)
      enddo
     enddo
    enddo
   enddo
  enddo
 enddo

END_PROVIDER

BEGIN_PROVIDER [ double precision, two_creation_one_anhilation, (n_act_orb,n_act_orb,n_act_orb,2,2,2)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin,kspin
 integer :: orb_i, hole_particle_i,spin_exc_i
 integer :: orb_j, hole_particle_j,spin_exc_j
 integer :: orb_k, hole_particle_k,spin_exc_k
 double precision  :: norm_out(N_states_diag)
 integer(bit_kind) :: psi_in_out(N_int,2,n_det)
 double precision :: psi_in_out_coef(n_det,N_states_diag)
 use bitmasks

 integer :: iorb,jorb
 integer :: korb
 integer :: state_target
 state_target = 1
 double precision :: energies(n_states_diag)
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
       do i = 1, n_det
        do j = 1, n_states_diag
          psi_in_out_coef(i,j) = psi_coef(i,j)
        enddo
        do j = 1, N_int
         psi_in_out(j,1,i) =  psi_active(j,1,i) 
         psi_in_out(j,1,i) =  psi_active(j,2,i) 
        enddo
       enddo
       call apply_exc_to_psi(orb_i,hole_particle_i,spin_exc_i, & 
               norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
       call apply_exc_to_psi(orb_j,hole_particle_j,spin_exc_j, & 
               norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
       call apply_exc_to_psi(orb_k,hole_particle_k,spin_exc_k, & 
               norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
       call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det,n_det,n_det,N_states_diag,state_target)
       two_creation_one_anhilation(iorb,jorb,korb,ispin,jspin,kspin) = energy_cas_dyall(state_target)  -  energies(state_target)
      enddo
     enddo
    enddo
   enddo
  enddo
 enddo

END_PROVIDER

BEGIN_PROVIDER [ double precision, three_creation, (n_act_orb,n_act_orb,n_act_orb,2,2,2)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin,kspin
 integer :: orb_i, hole_particle_i,spin_exc_i
 integer :: orb_j, hole_particle_j,spin_exc_j
 integer :: orb_k, hole_particle_k,spin_exc_k
 double precision  :: norm_out(N_states_diag)
 integer(bit_kind) :: psi_in_out(N_int,2,n_det)
 double precision :: psi_in_out_coef(n_det,N_states_diag)
 use bitmasks

 integer :: iorb,jorb
 integer :: korb
 integer :: state_target
 state_target = 1
 double precision :: energies(n_states_diag)
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
       do i = 1, n_det
        do j = 1, n_states_diag
          psi_in_out_coef(i,j) = psi_coef(i,j)
        enddo
        do j = 1, N_int
         psi_in_out(j,1,i) =  psi_active(j,1,i) 
         psi_in_out(j,1,i) =  psi_active(j,2,i) 
        enddo
       enddo
       call apply_exc_to_psi(orb_i,hole_particle_i,spin_exc_i, & 
               norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
       call apply_exc_to_psi(orb_j,hole_particle_j,spin_exc_j, & 
               norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
       call apply_exc_to_psi(orb_k,hole_particle_k,spin_exc_k, & 
               norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
       call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det,n_det,n_det,N_states_diag,state_target)
       three_creation(iorb,jorb,korb,ispin,jspin,kspin) = energy_cas_dyall(state_target)  -  energies(state_target)
      enddo
     enddo
    enddo
   enddo
  enddo
 enddo

END_PROVIDER

BEGIN_PROVIDER [ double precision, three_anhilation, (n_act_orb,n_act_orb,n_act_orb,2,2,2)]
 implicit none
 integer :: i,j
 integer :: ispin,jspin,kspin
 integer :: orb_i, hole_particle_i,spin_exc_i
 integer :: orb_j, hole_particle_j,spin_exc_j
 integer :: orb_k, hole_particle_k,spin_exc_k
 double precision  :: norm_out(N_states_diag)
 integer(bit_kind) :: psi_in_out(N_int,2,n_det)
 double precision :: psi_in_out_coef(n_det,N_states_diag)
 use bitmasks

 integer :: iorb,jorb
 integer :: korb
 integer :: state_target
 state_target = 1
 double precision :: energies(n_states_diag)
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
       do i = 1, n_det
        do j = 1, n_states_diag
          psi_in_out_coef(i,j) = psi_coef(i,j)
        enddo
        do j = 1, N_int
         psi_in_out(j,1,i) =  psi_active(j,1,i) 
         psi_in_out(j,1,i) =  psi_active(j,2,i) 
        enddo
       enddo
       call apply_exc_to_psi(orb_i,hole_particle_i,spin_exc_i, & 
               norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
       call apply_exc_to_psi(orb_j,hole_particle_j,spin_exc_j, & 
               norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
       call apply_exc_to_psi(orb_k,hole_particle_k,spin_exc_k, & 
               norm_out,psi_in_out,psi_in_out_coef, n_det,n_det,n_det,N_states_diag)
       call u0_H_dyall_u0(energies,psi_in_out,psi_in_out_coef,n_det,n_det,n_det,N_states_diag,state_target)
       three_anhilation(iorb,jorb,korb,ispin,jspin,kspin) = energy_cas_dyall(state_target)  -  energies(state_target)
      enddo
     enddo
    enddo
   enddo
  enddo
 enddo

END_PROVIDER

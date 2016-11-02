program test_two_bod
 implicit none
 read_wf = .True.
 touch read_wf
 call routine
end
subroutine routine
 implicit none
 integer :: i,j,k,l
 integer :: h1,p1,h2,p2,s1,s2
 double precision :: accu,get_two_body_dm_ab_map_element,get_mo_bielec_integral
 accu = 0.d0

 ! Diag part of the core two body dm 
 do i = 1, n_core_orb
  h1 = list_core(i)
  do j = 1, n_core_orb
   h2 = list_core(j)
   accu += two_body_dm_ab_diag_core(j,i) * mo_bielec_integral_jj(h1,h2)
  enddo
 enddo

 ! Diag part of the active two body dm
 do i = 1, n_act_orb
  h1 = list_act(i)
  do j = 1, n_act_orb
   h2 = list_act(j)
   accu += two_body_dm_ab_diag_act(j,i) * mo_bielec_integral_jj(h1,h2)
  enddo
 enddo

 ! Diag part of the active <-> core two body dm
 do i = 1, n_act_orb
  h1 = list_act(i)
  do j = 1, n_core_orb
   h2 = list_core(j)
   accu += two_body_dm_diag_core_act(j,i) * mo_bielec_integral_jj(h1,h2)
  enddo
 enddo
 print*,'BI ELECTRONIC   =  ',accu

 double precision :: accu_extra_diag
 accu_extra_diag = 0.d0
 ! purely active part of the two body dm 
 do l = 1, n_act_orb  ! p2 
  p2 = list_act(l)
  do k = 1, n_act_orb  ! h2 
   h2 = list_act(k)
   do j = 1, n_act_orb  ! p1 
    p1 = list_act(j)
    do i = 1,n_act_orb   ! h1 
     h1 = list_act(i)
     accu_extra_diag += two_body_dm_ab_big_array_act(i,j,k,l) * get_mo_bielec_integral(h1,h2,p1,p2,mo_integrals_map)
    enddo
   enddo
  enddo
 enddo
 
 ! core <-> active part of the two body dm  
 do l = 1, n_act_orb  ! p1 
  p1 = list_act(l)
  do k = 1, n_act_orb  ! h1 
   h1 = list_act(k)
    do i = 1,n_core_orb ! h2 
     h2 = list_core(i)
     accu_extra_diag += two_body_dm_ab_big_array_core_act(i,k,l) * get_mo_bielec_integral(h1,h2,p1,h2,mo_integrals_map)
    enddo
  enddo
 enddo

 print*,'extra_diag = ',accu_extra_diag
 double precision :: average_mono
 call get_average(mo_mono_elec_integral,one_body_dm_mo,average_mono)
 print*,'BI ELECTRONIC   =  ',accu+accu_extra_diag
 print*,'MONO ELECTRONIC =  ',average_mono
 print*,'Total elec      =  ',accu+average_mono + accu_extra_diag
 print*,'Total           =  ',accu+average_mono+nuclear_repulsion +accu_extra_diag
 double precision :: e_0,hij
 call u_0_H_u_0(e_0,psi_coef,n_det,psi_det,N_int)
 print*,'<Psi| H |Psi>   =  ',e_0 + nuclear_repulsion
 integer :: degree
 integer                        :: exc(0:2,2,2)
 double precision               :: phase
 integer :: n_elements
 n_elements = 0
 accu = 0.d0
 do i = 1, N_det
  do j = i+1, N_det
   call get_excitation_degree(psi_det(1,1,i),psi_det(1,1,j),degree,N_int)
   if(degree.gt.2)cycle
!  if(degree.ne.1)cycle
   call get_excitation(psi_det(1,1,i),psi_det(1,1,j),exc,degree,phase,N_int)
   call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
   if(s1.eq.s2)cycle
   n_elements += 1
   call i_H_j(psi_det(1,1,i),psi_det(1,1,j),N_int,hij)
   accu += 2.d0 * hij * psi_coef(i,1) * psi_coef(j,1)
  enddo
 enddo
 print*,'n_elements  = ',n_elements
 print*,'<Psi| extra diag   ',accu
 print*,'dm                 ',accu_extra_diag
 

end

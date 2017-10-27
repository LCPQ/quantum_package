program test_wf
 implicit none
 read_wf = .True.
 touch read_wf
 call routine
end



subroutine routine
 use bitmasks
 implicit none
 integer :: iref
 print*, 'which reference slater determinant dou you want ?'
 read(5,*)iref
 integer :: i
 double precision :: delta_e,h0i
 double precision :: accu_e, hiiref,accu_coef,hij
 double precision, allocatable :: contrib(:)
 integer, allocatable :: iorder(:)
 integer, allocatable           :: idx(:)
 allocate(idx(0:N_det))
 print*, '***********************'
 print*, '***********************'
 print*, '***********************'
 print*, 'You chose that SD :'
 call debug_det(psi_det(1,1,iref),N_int)
 call get_excitation_degree(ref_bitmask,psi_det(1,1,iref),degree,N_int)
 call i_H_j(ref_bitmask,psi_det(1,1,iref),N_int,h0i)
 call i_H_j(psi_ref(1,1,iref),psi_det(1,1,iref),N_int,hii)
 call get_excitation(ref_bitmask,psi_det(1,1,iref),exc,degree,phase,N_int)
 call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)

 print*,'degree =',degree
 print*,'phase  =',phase
 if(degree == 1)then
  print*,'s1',s1
  print*,'h1,p1 = ',h1,p1
 else if (degree ==2)then
  print*,'s1',s1
  print*,'h1,p1 = ',h1,p1
  print*,'s2',s2
  print*,'h2,p2 = ',h2,p2
 endif
 if(degree.ne.0)then
  delta_e = hii - ref_bitmask_energy
  if(h0i.ne.0.d0)then
   if (delta_e > 0.d0) then
     coef_2_2 = 0.5d0 * (delta_e - dsqrt(delta_e * delta_e + 4.d0 * h0i * h0i ))/ h0i 
   else
     coef_2_2 = 0.5d0 * (delta_e + dsqrt(delta_e * delta_e + 4.d0 * h0i * h0i )) /h0i 
   endif
  endif
 endif
 print*,'h0i              =',h0i
 print*,'delta E          =',delta_e
 print*,'coef 2x2         =',coef_2_2!*psi_coef(1,1)
 print*,'amplitude        =',psi_coef(iref,1)/psi_coef(1,1)
 print*, '***********************'
 print*, '***********************'
 print*, '***********************'
 
 call filter_connected_i_H_psi0(psi_det,psi_det(1,1,iref),N_int,N_det,idx)


 call i_H_j(psi_det(1,1,iref),psi_det(1,1,iref),N_int,hiiref)
 print*, 'passed the connection browsing'
 allocate(contrib(idx(0)-1),iorder(idx(0)-1))

 accu_e = 0.d0
 accu_coef = 0.d0
 print*, iref
 print*, idx(0)
 do i = 1, idx(0)
  contrib(i) = 0.d0
  iorder(i) = idx(i)
  if(idx(i)==iref)then
   cycle
  else
   call get_excitation_degree(psi_det(1,1,iref),psi_det(1,1,idx(i)),degree,N_int)
   if(degree.gt.2)cycle
   call i_H_j(psi_det(1,1,iref),psi_det(1,1,idx(i)),N_int,hij)
   accu_coef += psi_coef(idx(i),1) * hij / (var_energy_mr(1) - hiiref)
   contrib(i) = -dabs(psi_coef(idx(i),1) * hij / (var_energy_mr(1) - hiiref))
   accu_e += psi_coef(idx(i),1) * hij
  endif
 enddo
 print*, 'passed the contributions '

 integer :: degree
 integer :: exc(0:2,2,2)
 double precision :: phase
 integer :: h1,h2,p1,p2,s1,s2
 accu_e += psi_coef(iref,1) * hiiref
 accu_e = accu_e / psi_coef(iref,1)
 print*, psi_coef(iref,1),accu_coef
 print*, var_energy_mr(1),accu_e
 call dsort(contrib,iorder,idx(0)-1)
 print*, 'passed sorting the contributions'
 accu_coef = 0.d0
 double precision :: accu_second_order,coef_2_2, hii
 accu_second_order = 0.d0
 
 print*, ''
 do i = 1, idx(0)
  if (iorder(i)==iref)cycle
   call get_excitation_degree(psi_det(1,1,iref),psi_det(1,1,iorder(i)),degree,N_int)
   if(degree.gt.2)cycle
   print*, ''
   print*, '==============================================='
   print*, ' i ',i,iorder(i)
   call debug_det(psi_det(1,1,iorder(i)),N_int)
   call i_H_j(psi_det(1,1,iorder(i)),psi_det(1,1,iorder(i)),N_int,hii)
   call i_H_j(psi_det(1,1,iref),psi_det(1,1,iorder(i)),N_int,hij)
   call get_excitation(psi_det(1,1,iref),psi_det(1,1,iorder(i)),exc,degree,phase,N_int)
   call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
   print*,'Info with respect to the chosen ref determinant'
   print*,'degree =',degree
   if(degree == 1)then
    print*,'s1',s1
    print*,'h1,p1 = ',h1,p1
   else
    print*,'s1',s1
    print*,'h1,p1 = ',h1,p1
    print*,'s2',s2
    print*,'h2,p2 = ',h2,p2
   endif
   print*,'coef             =',psi_coef(iorder(i),1)
   print*,'phase            =',phase
   print*,'hij              =',hij
   print*,'contrib          =',psi_coef(iorder(i),1) * hij / (var_energy_mr(1) - hiiref)
   print*,'relative contrib =',psi_coef(iorder(i),1) * hij / (var_energy_mr(1) - hiiref)/psi_coef(iref,1)
   accu_coef += psi_coef(iorder(i),1) * hij / (var_energy_mr(1) - hiiref)

   print*,'Info with respect to the HF-like determinant '
   call get_excitation_degree(ref_bitmask,psi_det(1,1,iorder(i)),degree,N_int)
   call i_H_j(ref_bitmask,psi_det(1,1,iorder(i)),N_int,h0i)
   call get_excitation(ref_bitmask,psi_det(1,1,iorder(i)),exc,degree,phase,N_int)
   call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
   print*,'degree =',degree
   if(degree == 1)then
    print*,'s1',s1
    print*,'h1,p1 = ',h1,p1
   else if (degree ==2)then
    print*,'s1',s1
    print*,'h1,p1 = ',h1,p1
    print*,'s2',s2
    print*,'h2,p2 = ',h2,p2
   endif
    if(degree.ne.0)then
     delta_e = hii - ref_bitmask_energy
     if(h0i.ne.0.d0)then
      if (delta_e > 0.d0) then
        coef_2_2 = 0.5d0 * (delta_e - dsqrt(delta_e * delta_e + 4.d0 * h0i * h0i ))/ h0i 
      else
        coef_2_2 = 0.5d0 * (delta_e + dsqrt(delta_e * delta_e + 4.d0 * h0i * h0i )) /h0i 
      endif
     endif
   print*,'h0i              =',h0i
   print*,'coef 2x2         =',coef_2_2*psi_coef(1,1)
   print*,'delta E          =',delta_e
   print*,'sec order conrib =',coef_2_2 * hij/(var_energy_mr(1) - hiiref) * psi_coef(1,1)
     accu_second_order += coef_2_2 * hij/(var_energy_mr(1) - hiiref) * psi_coef(1,1)
    endif
 enddo
 print*, 'Total comparison ....'
 print*, psi_coef(iref,1),accu_coef,accu_second_order


end


program printwf
 implicit none 
 read_wf = .True.
 touch read_wf
 print*,'ref_bitmask_energy = ',ref_bitmask_energy
 call routine

end

subroutine routine
 implicit none 
 integer :: i
 integer :: degree
 double precision :: hij,hii,coef_1,h00
 integer          :: exc(0:2,2,2)
 double precision :: phase
 integer :: h1,p1,h2,p2,s1,s2
 double precision :: get_mo_bielec_integral
 double precision :: norm_mono_a,norm_mono_b
 double precision :: norm_mono_a_2,norm_mono_b_2
 double precision :: norm_mono_a_pert_2,norm_mono_b_pert_2
 double precision :: norm_mono_a_pert,norm_mono_b_pert
 norm_mono_a = 0.d0
 norm_mono_b = 0.d0
 norm_mono_a_2 = 0.d0
 norm_mono_b_2 = 0.d0
 norm_mono_a_pert = 0.d0
 norm_mono_b_pert = 0.d0
 norm_mono_a_pert_2 = 0.d0
 norm_mono_b_pert_2 = 0.d0
 do i = 1, min(500,N_det)
  print*,''
  print*,'i = ',i
  call debug_det(psi_det(1,1,i),N_int)
  call get_excitation_degree(psi_det(1,1,i),psi_det(1,1,1),degree,N_int)
  print*,'degree = ',degree
  if(degree == 0)then
   print*,'Reference determinant '
   call i_H_j(psi_det(1,1,i),psi_det(1,1,i),N_int,h00)
  else 
   call i_H_j(psi_det(1,1,i),psi_det(1,1,i),N_int,hii)
   call i_H_j(psi_det(1,1,1),psi_det(1,1,i),N_int,hij)
   coef_1 = hij/(h00-hii)
   call get_excitation(psi_det(1,1,1),psi_det(1,1,i),exc,degree,phase,N_int)
   call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
   print*,'phase = ',phase
   if(degree == 1)then
    print*,'s1',s1
    print*,'h1,p1 = ',h1,p1
    if(s1 == 1)then
     norm_mono_a += dabs(psi_coef(i,1)/psi_coef(1,1))
     norm_mono_a_2 += dabs(psi_coef(i,1)/psi_coef(1,1))**2
     norm_mono_a_pert += dabs(coef_1)
     norm_mono_a_pert_2 += dabs(coef_1)**2
    else
     norm_mono_b += dabs(psi_coef(i,1)/psi_coef(1,1))
     norm_mono_b_2 += dabs(psi_coef(i,1)/psi_coef(1,1))**2
     norm_mono_b_pert += dabs(coef_1)
     norm_mono_b_pert_2 += dabs(coef_1)**2
    endif
!  print*,'< h | Ka| p > = ',get_mo_bielec_integral(h1,list_act(1),list_act(1),p1,mo_integrals_map)
    double precision :: hmono,hdouble
    call  i_H_j_verbose(psi_det(1,1,1),psi_det(1,1,i),N_int,hij,hmono,hdouble)
    print*,'hmono         = ',hmono
    print*,'hdouble       = ',hdouble
    print*,'hmono+hdouble = ',hmono+hdouble
    print*,'hij           = ',hij
   else
    print*,'s1',s1
    print*,'h1,p1 = ',h1,p1
    print*,'s2',s2
    print*,'h2,p2 = ',h2,p2
!  print*,'< h | Ka| p > = ',get_mo_bielec_integral(h1,h2,p1,p2,mo_integrals_map)
   endif
   
   print*,'<Ref| H |D_I> = ',hij
   print*,'Delta E       = ',h00-hii
   print*,'coef pert (1) = ',coef_1
  endif
   print*,'amplitude     = ',psi_coef(i,1)/psi_coef(1,1)

 enddo


 print*,''
 print*,''
 print*,''
 print*,'L1 norm of mono alpha = ',norm_mono_a
 print*,'L1 norm of mono beta  = ',norm_mono_b
 print*, '---'
 print*,'L2 norm of mono alpha = ',norm_mono_a_2
 print*,'L2 norm of mono beta  = ',norm_mono_b_2
 print*, '-- perturbative mono'
 print*,''
 print*,'L1 norm of pert alpha = ',norm_mono_a_pert
 print*,'L1 norm of pert beta  = ',norm_mono_b_pert
 print*,'L2 norm of pert alpha = ',norm_mono_a_pert_2
 print*,'L2 norm of pert beta  = ',norm_mono_b_pert_2

end

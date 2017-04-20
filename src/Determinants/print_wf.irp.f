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
 double precision :: hij
 integer          :: exc(0:2,2,2)
 double precision :: phase
 integer :: h1,p1,h2,p2,s1,s2
 double precision :: get_mo_bielec_integral
 double precision :: norm_mono_a,norm_mono_b
 norm_mono_a = 0.d0
 norm_mono_b = 0.d0
 do i = 1, min(500,N_det)
  print*,''
  print*,'i = ',i
  call debug_det(psi_det(1,1,i),N_int)
  call get_excitation_degree(psi_det(1,1,i),psi_det(1,1,1),degree,N_int)
  print*,'degree = ',degree
  if(degree == 0)then
   print*,'Reference determinant '
  else 
   call i_H_j(psi_det(1,1,i),psi_det(1,1,i),N_int,hij)
   call get_excitation(psi_det(1,1,1),psi_det(1,1,i),exc,degree,phase,N_int)
   call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
   print*,'phase = ',phase
!  if(degree == 1)then
!   print*,'s1',s1
!   print*,'h1,p1 = ',h1,p1
!   if(s1 == 1)then
!    norm_mono_a += dabs(psi_coef(i,1)/psi_coef(1,1))
!   else
!    norm_mono_b += dabs(psi_coef(i,1)/psi_coef(1,1))
!   endif
!  print*,'< h | Ka| p > = ',get_mo_bielec_integral(h1,list_act(1),list_act(1),p1,mo_integrals_map)
!  double precision :: hmono,hdouble
!  call  i_H_j_verbose(psi_det(1,1,1),psi_det(1,1,i),N_int,hij,hmono,hdouble)
!  print*,'hmono         = ',hmono
!  print*,'hdouble       = ',hdouble
!  print*,'hmono+hdouble = ',hmono+hdouble
!  print*,'hij           = ',hij
!  else
!   print*,'s1',s1
!   print*,'h1,p1 = ',h1,p1
!   print*,'s2',s2
!   print*,'h2,p2 = ',h2,p2
!  print*,'< h | Ka| p > = ',get_mo_bielec_integral(h1,h2,p1,p2,mo_integrals_map)
!  endif
   
   print*,'<Ref| H |D_I> = ',hij
  endif
  print*,'amplitude = ',psi_coef(i,1)/psi_coef(1,1)

 enddo


 print*,''
 print*,''
 print*,''
 print*,'mono alpha = ',norm_mono_a
 print*,'mono beta  = ',norm_mono_b

end

use bitmasks

BEGIN_PROVIDER [integer, mo_inp_num]
 implicit none
 BEGIN_DOC
! This is the number of orbitals involved in the entanglement calculation. 
! It is taken equal to the number of active orbitals n_act_orb. 
 END_DOC
 mo_inp_num = n_act_orb

END_PROVIDER

 BEGIN_PROVIDER [integer, mo_inp_list, (N_int*bit_kind_size)]
&BEGIN_PROVIDER [integer, mo_inp_list_rev, (mo_tot_num)]
&BEGIN_PROVIDER [integer(bit_kind), mo_inp_bit_list, (N_int)]
 implicit none
 BEGIN_DOC
! mo_inp_list is the list of the orbitals involved in the entanglement calculation. 
! It is taken equal to the list of active orbitals list_act.
! mo_inp_list_rev is a list such that mo_inp_list_rev(mo_inp_list(i))=i.
 END_DOC
 integer :: i
 
 do i = 1, mo_inp_num
  mo_inp_list(i)=list_act(i)
 enddo

 do i = 1, mo_inp_num
  mo_inp_list_rev(mo_inp_list(i))=i
 enddo
 call list_to_bitstring( mo_inp_bit_list, mo_inp_list, mo_inp_num, N_int)
END_PROVIDER


 BEGIN_PROVIDER [double precision, entropy_one_orb, (mo_inp_num)]
&BEGIN_PROVIDER [double precision, entropy_two_orb, (mo_inp_num,mo_inp_num)]
 implicit none
 BEGIN_DOC
! entropy_one_orb is the one-orbital von Neumann entropy S(1)_i
! entropy_two_orb is the two-orbital von Neumann entropy S(2)_ij.
 END_DOC

 double precision, allocatable :: ro1(:,:),ro2(:,:,:)
 integer :: i,j,k,l,ii,jj,iii,istate,kl,info
 integer, allocatable :: occ(:,:)
 integer       :: n_occ_alpha, n_occ_beta
 logical, allocatable :: zocc(:,:)
 logical :: zalpha, zbeta, zalpha2, zbeta2
 integer :: exc(0:2,2,2),degree,h1,p1,h2,p2,spin1,spin2
 double precision :: phase
 integer(bit_kind) :: key_tmp(N_int), key_tmp2(N_int)
 integer :: ip
 double precision, parameter :: eps=10.d0**(-14)
 double precision :: w(16), work(3*16) 


 allocate(ro1(4,mo_inp_num),ro2(16,16,(mo_inp_num*(mo_inp_num-1)/2)))
 
 entropy_one_orb = 0.d0
 entropy_two_orb = 0.d0
 ro1 = 0.d0
 ro2 = 0.d0
 
 allocate (occ(N_int*bit_kind_size,2))
 allocate (zocc(mo_tot_num,2))

 istate = 1 !Only GS, to be generalized...
 do ii=1,N_det
  ! We get the occupation of the alpha electrons in occ(:,1)
  call bitstring_to_list(psi_det(1,1,ii), occ(1,1), n_occ_alpha, N_int)
  ! We get the occupation of the beta electrons in occ(:,2)
  call bitstring_to_list(psi_det(1,2,ii), occ(1,2), n_occ_beta, N_int)
  zocc = .false.
  do i=1,n_occ_alpha
    zocc(occ(i,1),1)=.true.
  enddo
  do i=1,n_occ_beta
    zocc(occ(i,2),2)=.true.
  enddo
  
  do k=1,mo_inp_num
   zalpha = zocc(mo_inp_list(k),1)
   zbeta = zocc(mo_inp_list(k),2)
! mono start
   if (zbeta.and.zalpha) then
     ro1(4,k) = ro1(4,k) + psi_coef(ii,istate)**2  ! double occupied
   elseif (zalpha) then
     ro1(2,k) = ro1(2,k) + psi_coef(ii,istate)**2  ! single alpha
   elseif (zbeta) then
     ro1(3,k) = ro1(3,k) + psi_coef(ii,istate)**2  ! single beta
   else
     ro1(1,k) = ro1(1,k) + psi_coef(ii,istate)**2  ! empty
   endif
! mono stop
! double start
   if (k.eq.mo_inp_num) cycle
   do l=k+1,mo_inp_num
    kl=(l-1)*(l-2)/2+k
    zalpha2 = zocc(mo_inp_list(l),1)
    zbeta2 = zocc(mo_inp_list(l),2)

    if (zbeta.and.zalpha.and.zbeta2.and.zalpha2) then
      ro2(16,16,kl) = ro2(16,16,kl) + psi_coef(ii,istate)**2  ! both double occupied
    else if (zbeta.and.zalpha.and.zbeta2) then
      ro2(15,15,kl) = ro2(15,15,kl) + psi_coef(ii,istate)**2  ! one double, one beta
    else if (zbeta.and.zalpha.and.zalpha2) then
      ro2(13,13,kl) = ro2(13,13,kl) + psi_coef(ii,istate)**2  ! one double, one alpha
    else if (zbeta.and.zbeta2.and.zalpha2) then
      ro2(14,14,kl) = ro2(14,14,kl) + psi_coef(ii,istate)**2  ! one beta, one double
    else if (zalpha.and.zbeta2.and.zalpha2) then
      ro2(12,12,kl) = ro2(12,12,kl) + psi_coef(ii,istate)**2  ! one alpha, one double
    else if (zalpha.and.zbeta) then
      ro2(11,11,kl) = ro2(11,11,kl) + psi_coef(ii,istate)**2  ! one double, one empty
    else if (zbeta2.and.zalpha2) then
      ro2(8,8,kl) = ro2(8,8,kl) + psi_coef(ii,istate)**2      ! one empty, one double
    else if (zbeta.and.zalpha2) then
      ro2(10,10,kl) = ro2(10,10,kl) + psi_coef(ii,istate)**2  ! one beta, one alpha
    else if (zalpha.and.zbeta2) then
      ro2(9,9,kl) = ro2(9,9,kl) + psi_coef(ii,istate)**2      ! one alpha, one beta
    else if (zbeta.and.zbeta2) then
      ro2(7,7,kl) = ro2(7,7,kl) + psi_coef(ii,istate)**2      ! one beta, one beta
    else if (zalpha.and.zalpha2) then
      ro2(6,6,kl) = ro2(6,6,kl) + psi_coef(ii,istate)**2      ! one alpha, one alpha
    else if (zbeta) then
      ro2(5,5,kl) = ro2(5,5,kl) + psi_coef(ii,istate)**2      ! one beta, one empty
    else if (zbeta2) then
      ro2(4,4,kl) = ro2(4,4,kl) + psi_coef(ii,istate)**2      ! one empty, one beta
    else if (zalpha) then
      ro2(3,3,kl) = ro2(3,3,kl) + psi_coef(ii,istate)**2      ! one alpha, one empty
    else if (zalpha2) then
      ro2(2,2,kl) = ro2(2,2,kl) + psi_coef(ii,istate)**2      ! one empty, one alpha
    else 
      ro2(1,1,kl) = ro2(1,1,kl) + psi_coef(ii,istate)**2      ! both empty
    end if
   enddo
  enddo
! stop double

  if (ii.eq.N_det) cycle
!Off Diagonal Elements  
  do jj=ii+1,N_det

   call get_excitation_degree(psi_det(1,1,ii),psi_det(1,1,jj),degree,N_int)
   if (degree.gt.2) cycle
   ip=0
   do iii =1,N_int
     key_tmp(iii) =  ior(xor(psi_det(iii,1,ii),psi_det(iii,1,jj)),xor(psi_det(iii,2,ii),psi_det(iii,2,jj)))
     ip += popcnt(key_tmp(iii))
   enddo
   if (ip.ne.2) cycle !They involve more than 2 orbitals.
   ip=0 
   do iii=1,N_int
    ip += popcnt(iand(key_tmp(iii),mo_inp_bit_list(iii)))
   enddo
   if (ip.ne.2) cycle !They do not involve orbitals of the list.

   if (degree.eq.2) then
    call get_double_excitation(psi_det(1,1,ii),psi_det(1,1,jj),exc,phase,N_int)
    call decode_exc(exc,degree,h1,p1,h2,p2,spin1,spin2) 
    k=mo_inp_list_rev(h1)
    l=mo_inp_list_rev(p1)
    if (k.gt.l) then
      kl=(k-1)*(k-2)/2+l
    else
      kl=(l-1)*(l-2)/2+k
    endif

    if ((.not.zocc(mo_inp_list(l),1)).and.(.not.zocc(mo_inp_list(l),2))&
    .and.(zocc(mo_inp_list(k),1)).and.(zocc(mo_inp_list(k),2))) then
     ro2(8,11,kl) += phase*psi_coef(ii,istate)*psi_coef(jj,istate) 
     ro2(11,8,kl) = ro2(8,11,kl)
    endif

    if ((zocc(mo_inp_list(l),1)).and.(.not.zocc(mo_inp_list(l),2))&
    .and.(.not.zocc(mo_inp_list(k),1)).and.(zocc(mo_inp_list(k),2))) then
     ro2(9,10,kl) -= phase*psi_coef(ii,istate)*psi_coef(jj,istate) !negative
     ro2(10,9,kl) = ro2(9,10,kl)
    endif
    if ((zocc(mo_inp_list(k),1)).and.(.not.zocc(mo_inp_list(k),2))&
    .and.(.not.zocc(mo_inp_list(l),1)).and.(zocc(mo_inp_list(l),2))) then
     ro2(9,10,kl) -= phase*psi_coef(ii,istate)*psi_coef(jj,istate) !negative
     ro2(10,9,kl) = ro2(9,10,kl)
    endif
   endif

   if (degree.eq.1) then
    call get_mono_excitation(psi_det(1,1,ii),psi_det(1,1,jj),exc,phase,N_int)
    call decode_exc(exc,degree,h1,p1,h2,p2,spin1,spin2) 
    k=mo_inp_list_rev(h1)
    l=mo_inp_list_rev(p1)
    if (k.gt.l) then
      kl=(k-1)*(k-2)/2+l
    else
      kl=(l-1)*(l-2)/2+k
    endif

    if ((.not.(zocc(mo_inp_list(l),2))).and.&
    (.not.(zocc(mo_inp_list(l),1))).and.(zocc(mo_inp_list(k),1))&
    .and.(.not.(zocc(mo_inp_list(k),2)))) then
      ro2(2,3,kl) += phase*psi_coef(ii,istate)*psi_coef(jj,istate)
      ro2(3,2,kl)=ro2(2,3,kl)
    endif

    if ((.not.(zocc(mo_inp_list(l),2))).and.&
    (.not.(zocc(mo_inp_list(l),1))).and.(zocc(mo_inp_list(k),2))&
    .and.(.not.(zocc(mo_inp_list(k),1)))) then
      ro2(4,5,kl) += phase*psi_coef(ii,istate)*psi_coef(jj,istate)
      ro2(5,4,kl)=ro2(4,5,kl)
    endif

         
    if ((.not.(zocc(mo_inp_list(l),1))).and.&    !k doubly occupied, l empty
        (.not.(zocc(mo_inp_list(l),2))).and.&
        (zocc(mo_inp_list(k),1)).and.&
        (zocc(mo_inp_list(k),2))) then
     if (k.gt.l) then
      if (spin1.eq.1) then !spin alpha
       ro2(8,9,kl) += phase*psi_coef(ii,istate)*psi_coef(jj,istate)
       ro2(9,8,kl)=ro2(8,9,kl)
      else !spin beta
       ro2(8,10,kl) -= phase*psi_coef(ii,istate)*psi_coef(jj,istate) !negative
       ro2(10,8,kl)=ro2(8,10,kl)
      endif
     else ! k.lt.l
      if (spin1.eq.1) then !spin alpha
       ro2(10,11,kl) -= phase*psi_coef(ii,istate)*psi_coef(jj,istate) !negative
       ro2(11,10,kl)=ro2(10,11,kl)
      else !spin beta
       ro2(9,11,kl) += phase*psi_coef(ii,istate)*psi_coef(jj,istate)
       ro2(11,9,kl)=ro2(9,11,kl)
      endif
     endif
    endif

    if ((.not.(zocc(mo_inp_list(l),1))).and.&    !k alpha, l beta
        (.not.(zocc(mo_inp_list(k),2))).and.&
        (zocc(mo_inp_list(k),1)).and.&
        (zocc(mo_inp_list(l),2))) then
     if (k.gt.l) then
      if (spin1.eq.1) then !spin alpha
       ro2(10,11,kl) -= phase*psi_coef(ii,istate)*psi_coef(jj,istate) !negative
       ro2(11,10,kl)=ro2(10,11,kl)
      else !spin beta
       print*, "problem in k alpha l beta k.gt.l spin beta"
      endif
     else ! k.lt.l
      if (spin1.eq.1) then !spin alpha
       ro2(8,9,kl) += phase*psi_coef(ii,istate)*psi_coef(jj,istate)
       ro2(9,8,kl)=ro2(8,9,kl)
      else !spin beta
       print*, "problem in k alpha l beta k.lt.l spin beta"
      endif
     endif
    endif
 
    if ((.not.(zocc(mo_inp_list(k),1))).and.&    !k beta, l alpha
        (.not.(zocc(mo_inp_list(l),2))).and.&
        (zocc(mo_inp_list(l),1)).and.&
        (zocc(mo_inp_list(k),2))) then
     if (k.gt.l) then
      if (spin1.eq.2) then !spin beta
       ro2(9,11,kl) += phase*psi_coef(ii,istate)*psi_coef(jj,istate)
       ro2(11,9,kl)=ro2(9,11,kl)
      else !spin alpha
       print*, "problem in k beta l alpha k.gt.l spin alpha"
      endif
     else ! k.lt.l
      if (spin1.eq.2) then !spin beta
       ro2(8,10,kl) -= phase*psi_coef(ii,istate)*psi_coef(jj,istate) !negative
       ro2(10,8,kl)=ro2(8,10,kl)
      else !spin alpha
       print*, "problem in k beta l alpha k.lt.l spin alpha"
      endif
     endif
    endif

    if (zocc(mo_inp_list(l),1).and.(.not.zocc(mo_inp_list(l),2))&
    .and.(zocc(mo_inp_list(k),1)).and.(zocc(mo_inp_list(k),2))) then
     ro2(12,13,kl) -= phase*psi_coef(ii,istate)*psi_coef(jj,istate)
     ro2(13,12,kl) = ro2(12,13,kl)
    endif

    if (zocc(mo_inp_list(l),2).and.(.not.zocc(mo_inp_list(l),1))&
    .and.(zocc(mo_inp_list(k),1)).and.(zocc(mo_inp_list(k),2))) then
     ro2(14,15,kl) -= phase*psi_coef(ii,istate)*psi_coef(jj,istate) 
     ro2(15,14,kl) = ro2(14,15,kl)
    endif
   endif
  enddo  
 enddo



 entropy_one_orb=0.d0
 do k=1,mo_inp_num
  do i=1,4
   if (ro1(i,k).ge.eps) then
    entropy_one_orb(k) = entropy_one_orb(k)-ro1(i,k)*log(ro1(i,k)) 
   endif
  enddo
 enddo

 entropy_two_orb=0.d0
 do k=1,mo_inp_num
  do l=1,k
   if (k.eq.l) cycle
   kl=(k-1)*(k-2)/2+l
   call dsyev('N','U',16,ro2(1,1,kl),16,w,work,3*16,info)
   if (info.ne.0) then
    write(*,*) "Errore in dsyev"
   endif
   do j=1,16
    if (w(j).ge.eps) then
      entropy_two_orb(k,l) = entropy_two_orb(k,l)-w(j)*log(w(j)) 
      entropy_two_orb(l,k) = entropy_two_orb(k,l)
    elseif ((w(j)).lt.(-eps)) then
      write(6,*) "Negative Eigenvalue. You have a big problem..."
      write(6,*) w(j)
      stop
    endif
   enddo
  enddo
 enddo


 deallocate (occ,zocc,ro1,ro2)

END_PROVIDER

BEGIN_PROVIDER [double precision, mutinf, (mo_inp_num,mo_inp_num)]
implicit none
BEGIN_DOC
!mutinf is the mutual information (entanglement), calculated as I_ij=0.5*[S(1)_i+S(1)_j-S(2)_ij]
!see the refence: 10.1016/j.chemphys.2005.10.018
END_DOC
 integer :: i,j
! mutal information:
 mutinf = 0.d0
 do i=1,mo_inp_num
   do j=1,mo_inp_num
     if (j.eq.i) cycle
     mutinf(i,j)=-0.5d0*(entropy_two_orb(i,j)-entropy_one_orb(i)-entropy_one_orb(j)) 
   enddo
 enddo 
END_PROVIDER

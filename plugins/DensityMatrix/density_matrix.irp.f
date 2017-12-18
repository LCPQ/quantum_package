 use bitmasks
 BEGIN_PROVIDER [ integer, iunit_two_body_dm_aa ]
&BEGIN_PROVIDER [ integer, iunit_two_body_dm_ab ]
&BEGIN_PROVIDER [ integer, iunit_two_body_dm_bb ]
   implicit none
   use bitmasks
   BEGIN_DOC
   ! Temporary files for 2-body dm calculation
   END_DOC
   integer                        :: getUnitAndOpen
   
   iunit_two_body_dm_aa = getUnitAndOpen(trim(ezfio_filename)//'/work/two_body_aa.tmp','w')
   iunit_two_body_dm_ab = getUnitAndOpen(trim(ezfio_filename)//'/work/two_body_ab.tmp','w')
   iunit_two_body_dm_bb = getUnitAndOpen(trim(ezfio_filename)//'/work/two_body_bb.tmp','w')
   ! Compute two body DM in file
   integer                        :: k,l,degree, idx,i
   integer                        :: exc(0:2,2,2),n_occ_alpha
   double precision               :: phase, coef
   integer                        :: h1,h2,p1,p2,s1,s2
   double precision               :: ck, cl
   character*(128), parameter     :: f = '(i8,4(x,i5),x,d16.8)'
   do k=1,det_num
     ck = (det_coef_provider(k)+det_coef_provider(k))
     do l=1,k-1
       cl = det_coef_provider(l)
       call get_excitation_degree(det_provider(1,1,k),det_provider(1,1,l),degree,N_int)
       if (degree == 2) then
         call get_double_excitation(det_provider(1,1,k),det_provider(1,1,l),exc,phase,N_int)
         call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
         call bielec_integrals_index(h1,h2,p1,p2,idx)
         ckl = phase*ck*cl
         select case (s1+s2)
           case(2) ! alpha alpha
             write(iunit_two_body_dm_aa,f) idx, h1,h2,p1,p2, ckl
             call bielec_integrals_index(h1,h2,p2,p1,idx)
             write(iunit_two_body_dm_aa,f) idx, h1,h2,p2,p1, -ckl
           case(3) ! alpha beta
             write(iunit_two_body_dm_ab,f) idx, h1,h2,p1,p2, ckl
           case(4) ! beta beta
             write(iunit_two_body_dm_bb,f) idx, h1,h2,p1,p2, ckl
             call bielec_integrals_index(h1,h2,p2,p1,idx)
             write(iunit_two_body_dm_bb,f) idx, h1,h2,p2,p1, -ckl
         end select
       else if (degree == 1) then
         call get_mono_excitation(det_provider(1,1,k),det_provider(1,1,l),exc,phase,N_int)
         call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
         double precision               :: ckl
         ckl = phase*ck*cl
         call bitstring_to_list(det_provider(1,1,k), occ(1,1), n_occ_alpha, N_int)
         call bitstring_to_list(det_provider(1,2,k), occ(1,2), n_occ_alpha, N_int)
         select case (s1)
           case (1)  ! Alpha single excitation
             integer                        :: occ(N_int*bit_kind_size,2)
             do i  = 1, elec_alpha_num
               p2=occ(i,1)
               h2=p2
               call bielec_integrals_index(h1,h2,p1,p2,idx)
               write(iunit_two_body_dm_aa,f) idx, h1,h2,p1,p2, ckl
               call bielec_integrals_index(h1,h2,p2,p1,idx)
               write(iunit_two_body_dm_aa,f) idx, h1,h2,p2,p1, -ckl
             enddo
             do i = 1, elec_beta_num
               p2=occ(i,2)
               h2=p2
               call bielec_integrals_index(h1,h2,p1,p2,idx)
               write(iunit_two_body_dm_ab,f) idx, h1,h2,p1,p2, ckl
             enddo
           case (2)  ! Beta single excitation
             do i  = 1, elec_alpha_num
               p2=occ(i,1)
               h2=p2
               call bielec_integrals_index(h1,h2,p1,p2,idx)
               write(iunit_two_body_dm_ab,f) idx, h1,h2,p1,p2, ckl
             enddo
             do i = 1, elec_beta_num
               p2=occ(i,2)
               h2=p2
               call bielec_integrals_index(h1,h2,p1,p2,idx)
               write(iunit_two_body_dm_bb,f) idx, h1,h2,p1,p2, ckl
               call bielec_integrals_index(h1,h2,p2,p1,idx)
               write(iunit_two_body_dm_bb,f) idx, h1,h2,p2,p1, -ckl
             enddo
         end select
       endif
     enddo
   enddo
   ! Sort file
   ! Merge coefs
   
   close(iunit_two_body_dm_aa)
   close(iunit_two_body_dm_ab)
   close(iunit_two_body_dm_bb)
   character*(128)                :: filename
   filename = trim(ezfio_filename)//'/work/two_body_aa.tmp'
   call system('sort -n '//trim(filename)//' > '//trim(filename)//'2 ; cp '//trim(filename)//'2 '//trim(filename))
   filename = trim(ezfio_filename)//'/work/two_body_ab.tmp'
   call system('sort -n '//trim(filename)//' > '//trim(filename)//'2 ; cp '//trim(filename)//'2 '//trim(filename))
   filename = trim(ezfio_filename)//'/work/two_body_bb.tmp'
   call system('sort -n '//trim(filename)//' > '//trim(filename)//'2 ; cp '//trim(filename)//'2 '//trim(filename))
   iunit_two_body_dm_aa = getUnitAndOpen(trim(ezfio_filename)//'/work/two_body_aa.tmp','r')
   iunit_two_body_dm_ab = getUnitAndOpen(trim(ezfio_filename)//'/work/two_body_ab.tmp','r')
   iunit_two_body_dm_bb = getUnitAndOpen(trim(ezfio_filename)//'/work/two_body_bb.tmp','r')
END_PROVIDER
 
 
 BEGIN_TEMPLATE
 
BEGIN_PROVIDER [ integer, size_two_body_dm_$AA ]
   implicit none
   use bitmasks
   BEGIN_DOC
   ! Size of the two body $ALPHA density matrix
   END_DOC
   integer *8                     :: key, key_old
   rewind(iunit_two_body_dm_$AA)
   size_two_body_dm_$AA = 0
   key     = 0_8
   key_old = key
   do while (.True.)
     read(iunit_two_body_dm_$AA,*,END=99) key
     if (key /= key_old) then
       size_two_body_dm_$AA += 1
       key_old = key
     endif
   end do
   99 continue
   
END_PROVIDER
 
 BEGIN_PROVIDER [ integer, two_body_dm_index_$AA, (4,size_two_body_dm_$AA) ]
&BEGIN_PROVIDER [ double precision, two_body_dm_value_$AA, (size_two_body_dm_$AA) ]
   implicit none
   use bitmasks
   BEGIN_DOC
   ! Two body $ALPHA density matrix
   END_DOC
   rewind(iunit_two_body_dm_$AA)
   integer *8                     :: key, key_old
   integer                        :: ii, i,j,k,l
   double precision               :: c
   key     = 0_8
   key_old = key
   ii = 0
   do while (.True.)
     read(iunit_two_body_dm_$AA,*,END=99) key, i,j,k,l, c
     if (key /= key_old) then
       ii += 1
       two_body_dm_index_$AA(1,ii) = i
       two_body_dm_index_$AA(2,ii) = j
       two_body_dm_index_$AA(3,ii) = k
       two_body_dm_index_$AA(4,ii) = l
       two_body_dm_value_$AA(ii) = 0.d0
       key_old = key
     endif
     two_body_dm_value_$AA(ii) += c
   enddo
   99 continue
   close(iunit_two_body_dm_$AA, status='DELETE')
END_PROVIDER
 
 SUBST [ AA, ALPHA ]
 
 aa ; alpha-alpha ;;
 ab ; alpha-beta  ;;
 bb ; beta-beta   ;;
 
 END_TEMPLATE
 
 
 BEGIN_PROVIDER [ double precision, two_body_dm_diag_aa, (mo_tot_num,mo_tot_num)]
&BEGIN_PROVIDER [ double precision, two_body_dm_diag_bb, (mo_tot_num,mo_tot_num)]
&BEGIN_PROVIDER [ double precision, two_body_dm_diag_ab, (mo_tot_num,mo_tot_num)]
   implicit none
   use bitmasks
   BEGIN_DOC
   ! diagonal part of the two body density matrix
   END_DOC
   integer                        :: i,j,k,e1,e2
   integer                        :: occ(N_int*bit_kind_size,2)
   double precision               :: ck
   integer                        :: n_occ_alpha
   two_body_dm_diag_aa=0.d0
   two_body_dm_diag_ab=0.d0
   two_body_dm_diag_bb=0.d0
   do k = 1, det_num
     call bitstring_to_list(det_provider(1,1,k), occ(1,1), n_occ_alpha, N_int)
     call bitstring_to_list(det_provider(1,2,k), occ(1,2), n_occ_alpha, N_int)
     ck = det_coef_provider(k) * det_coef_provider(k)
     do i = 1,elec_alpha_num
       e1=occ(i,1)
       do j = 1,elec_alpha_num
         e2=occ(j,1)
         ! alpha-alpha
         two_body_dm_diag_aa(e1,e2) = two_body_dm_diag_aa(e1,e2) + ck
       enddo
       do j = 1,elec_beta_num
         e2=occ(j,2)
         ! alpha-beta
         two_body_dm_diag_ab(e1,e2) = two_body_dm_diag_ab(e1,e2) + ck
       enddo
     enddo
     do i = 1,elec_beta_num
       e1=occ(i,2)
       do j = 1,elec_beta_num
         e2=occ(j,2)
         ! beta-beta
         two_body_dm_diag_bb(e1,e2) = two_body_dm_diag_bb(e1,e2) + ck
       enddo
     enddo
   enddo
END_PROVIDER

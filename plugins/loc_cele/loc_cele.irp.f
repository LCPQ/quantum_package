      program loc_rasorb 

      implicit none

      BEGIN_DOC
      !     This program performs a localization of the active orbitals
      !     of a CASSCF wavefunction, reading the orbitals from a RASORB
      !     file of molcas.
      !     id1=max is the number of MO in a given symmetry.
      END_DOC

      integer id1,i_atom,shift,shift_h

      parameter (id1=300)



      character*1 jobz,uplo

      character*64 file1,file2

      character*72 string(id1,8),cdum

      double precision :: cmo(id1,id1,1),cmoref(id1,id1,1),newcmo(id1,id1,1)

       double precision ::s(id1,id1,1),dum,ddum(id1,id1),ovl(id1,id1)

      double precision :: w(id1),work(3*id1),t(id1,id1),wi(id1,id1)

      integer n,i,j,k,l,nmo(8),isym,nsym,idum,nrot(8),irot(id1,8)

      integer ipiv(id1),info,lwork

      logical *1 z54
      print*,'passed the first copy'

      z54=.false.

 

      !Read the name of the RasOrb file

 
      print*,'Entering in the loc program'

!     read(5,*) z54
      print*,'before = '
      accu_norm = 0.d0
      do i =1,mo_tot_num
       accu_norm += dabs(mo_overlap(i,i))
      enddo
      print*,'accu_norm = ',accu_norm

      nsym = 1

      nmo(1) = mo_tot_num

      print*,'nmo(1) = ',nmo(1)

      cmo = 0.d0
      do isym=1,nsym

       do i=1,nmo(isym)

        do j = 1, ao_num

         cmo(j,i,isym) = mo_coef(j,i)

        enddo

       enddo

      enddo
      print*,'passed the first copy'



      do isym=1,nsym

       do j=1,mo_tot_num

        do i=1,ao_num

         newcmo(i,j,isym)=cmo(i,j,isym)

        enddo

       enddo

      enddo
      print*,'passed the copy'



      nrot(1) = 64  ! number of orbitals to be localized


      integer :: index_rot(1000,1)


       cmoref = 0.d0
       irot = 0

! H2 molecule for the mixed localization
      do i=1,64
       irot(i,1) = i+2
      enddo

      do i=1,17
       cmoref(i+1,i,1)=1.d0
      enddo
       cmoref(19,19-1,1)=1.d0
       cmoref(20,19-1,1)=-1.d0
       cmoref(19,20-1,1)=-1.d0
       cmoref(20,20-1,1)=-1.d0
       cmoref(21,20-1,1)=2.d0
       cmoref(22,21-1,1)=1.d0
       cmoref(23,22-1,1)=1.d0
       cmoref(24,23-1,1)=1.d0


       cmoref(25,24-1,1)=1.d0
       cmoref(26,24-1,1)=-1.d0
       cmoref(25,25-1,1)=-1.d0
       cmoref(26,25-1,1)=-1.d0
       cmoref(27,25-1,1)=2.d0
       cmoref(28,26-1,1)=1.d0
       cmoref(29,27-1,1)=1.d0
       cmoref(30,28-1,1)=1.d0
      
       cmoref(31,29-1,1)=1.d0
       cmoref(32,29-1,1)=-1.d0
       cmoref(31,30-1,1)=-1.d0
       cmoref(32,30-1,1)=-1.d0
       cmoref(33,30-1,1)=2.d0
       cmoref(34,31-1,1)=1.d0
       cmoref(35,32-1,1)=1.d0
       cmoref(36,33-1,1)=1.d0

       do i=33,49
        cmoref(i+5,i,1)= 1.d0
       enddo

       cmoref(55,52-2,1)=1.d0
       cmoref(56,52-2,1)=-1.d0
       cmoref(55,53-2,1)=-1.d0
       cmoref(56,53-2,1)=-1.d0
       cmoref(57,53-2,1)=2.d0
       cmoref(58,54-2,1)=1.d0
       cmoref(59,55-2,1)=1.d0
       cmoref(60,56-2,1)=1.d0

       cmoref(61,57-2,1)=1.d0
       cmoref(62,57-2,1)=-1.d0
       cmoref(61,58-2,1)=-1.d0
       cmoref(62,58-2,1)=-1.d0
       cmoref(63,58-2,1)=2.d0
       cmoref(64,59-2,1)=1.d0
       cmoref(65,60-2,1)=1.d0
       cmoref(66,61-2,1)=1.d0

       cmoref(67,62-2,1)=1.d0
       cmoref(68,62-2,1)=-1.d0
       cmoref(67,63-2,1)=-1.d0
       cmoref(68,63-2,1)=-1.d0
       cmoref(69,63-2,1)=2.d0
       cmoref(70,64-2,1)=1.d0
       cmoref(71,65-2,1)=1.d0
       cmoref(72,66-2,1)=1.d0
! H2 molecule
!      do i=1,66
!       irot(i,1) = i
!      enddo
!
!      do i=1,18
!       cmoref(i,i,1)=1.d0
!      enddo
!       cmoref(19,19,1)=1.d0
!       cmoref(20,19,1)=-1.d0
!       cmoref(19,20,1)=-1.d0
!       cmoref(20,20,1)=-1.d0
!       cmoref(21,20,1)=2.d0
!       cmoref(22,21,1)=1.d0
!       cmoref(23,22,1)=1.d0
!       cmoref(24,23,1)=1.d0
!
!
!       cmoref(25,24,1)=1.d0
!       cmoref(26,24,1)=-1.d0
!       cmoref(25,25,1)=-1.d0
!       cmoref(26,25,1)=-1.d0
!       cmoref(27,25,1)=2.d0
!       cmoref(28,26,1)=1.d0
!       cmoref(29,27,1)=1.d0
!       cmoref(30,28,1)=1.d0
!      
!       cmoref(31,29,1)=1.d0
!       cmoref(32,29,1)=-1.d0
!       cmoref(31,30,1)=-1.d0
!       cmoref(32,30,1)=-1.d0
!       cmoref(33,30,1)=2.d0
!       cmoref(34,31,1)=1.d0
!       cmoref(35,32,1)=1.d0
!       cmoref(36,33,1)=1.d0
!
!       do i=34,51
!        cmoref(i+3,i,1)= 1.d0
!       enddo
!
!       cmoref(55,52,1)=1.d0
!       cmoref(56,52,1)=-1.d0
!       cmoref(55,53,1)=-1.d0
!       cmoref(56,53,1)=-1.d0
!       cmoref(57,53,1)=2.d0
!       cmoref(58,54,1)=1.d0
!       cmoref(59,55,1)=1.d0
!       cmoref(60,56,1)=1.d0
!
!       cmoref(61,57,1)=1.d0
!       cmoref(62,57,1)=-1.d0
!       cmoref(61,58,1)=-1.d0
!       cmoref(62,58,1)=-1.d0
!       cmoref(63,58,1)=2.d0
!       cmoref(64,59,1)=1.d0
!       cmoref(65,60,1)=1.d0
!       cmoref(66,61,1)=1.d0
!
!       cmoref(67,62,1)=1.d0
!       cmoref(68,62,1)=-1.d0
!       cmoref(67,63,1)=-1.d0
!       cmoref(68,63,1)=-1.d0
!       cmoref(69,63,1)=2.d0
!       cmoref(70,64,1)=1.d0
!       cmoref(71,65,1)=1.d0
!       cmoref(72,66,1)=1.d0
! H atom
!      do i=1,33
!       irot(i,1) = i
!      enddo
!
!      do i=1,18
!       cmoref(i,i,1)=1.d0
!      enddo
!       cmoref(19,19,1)=1.d0
!       cmoref(20,19,1)=-1.d0
!       cmoref(19,20,1)=-1.d0
!       cmoref(20,20,1)=-1.d0
!       cmoref(21,20,1)=2.d0
!       cmoref(22,21,1)=1.d0
!       cmoref(23,22,1)=1.d0
!       cmoref(24,23,1)=1.d0


!       cmoref(25,24,1)=1.d0
!       cmoref(26,24,1)=-1.d0
!       cmoref(25,25,1)=-1.d0
!       cmoref(26,25,1)=-1.d0
!       cmoref(27,25,1)=2.d0
!       cmoref(28,26,1)=1.d0
!       cmoref(29,27,1)=1.d0
!       cmoref(30,28,1)=1.d0
!      
!       cmoref(31,29,1)=1.d0
!       cmoref(32,29,1)=-1.d0
!       cmoref(31,30,1)=-1.d0
!       cmoref(32,30,1)=-1.d0
!       cmoref(33,30,1)=2.d0
!       cmoref(34,31,1)=1.d0
!       cmoref(35,32,1)=1.d0
!       cmoref(36,33,1)=1.d0
 
       ! Definition of the index of the MO to be rotated
!      irot(2,1) = 21  ! the first mo to be rotated is the 21 th MO 
!      irot(3,1) = 22  ! etc....
!      irot(4,1) = 23  ! 
!      irot(5,1) = 24  ! 
!      irot(6,1) = 25  ! 

!N2
!       irot(1,1) = 5
!       irot(2,1) = 6
!       irot(3,1) = 7
!       irot(4,1) = 8
!       irot(5,1) = 9
!       irot(6,1) = 10
!
!       cmoref(5,1,1) = 1.d0   ! 
!       cmoref(6,2,1) = 1.d0   ! 
!       cmoref(7,3,1) = 1.d0   ! 
!       cmoref(40,4,1) = 1.d0   ! 
!       cmoref(41,5,1) = 1.d0   ! 
!       cmoref(42,6,1) =  1.d0   ! 
!END N2

!HEXATRIENE
!       irot(1,1) = 20
!       irot(2,1) = 21
!       irot(3,1) = 22
!       irot(4,1) = 23
!       irot(5,1) = 24
!       irot(6,1) = 25
!
!       cmoref(7,1,1)   = 1.d0   ! 
!       cmoref(26,1,1)  = 1.d0   ! 
!       cmoref(45,2,1)  = 1.d0   ! 
!       cmoref(64,2,1)  = 1.d0   ! 
!       cmoref(83,3,1)  = 1.d0   ! 
!       cmoref(102,3,1) = 1.d0   ! 
!       cmoref(7,4,1)   = 1.d0   ! 
!       cmoref(26,4,1)  = -1.d0   ! 
!       cmoref(45,5,1)  = 1.d0   ! 
!       cmoref(64,5,1)  = -1.d0   ! 
!       cmoref(83,6,1)  = 1.d0   ! 
!       cmoref(102,6,1) = -1.d0   ! 
!END HEXATRIENE

!!!!H2                                 H2 CAS
!        irot(1,1) = 1
!        irot(2,1) = 2
!        
!        cmoref(1,1,1) = 1.d0
!        cmoref(37,2,1) = 1.d0  
!END H2
!!!!  LOCALIZATION ON THE BASIS FUNCTIONS
!        do i = 1, nrot(1)
!          irot(i,1) = i 
!          cmoref(i,i,1) = 1.d0
!        enddo

!END BASISLOC

!       do i = 1, nrot(1)
!        irot(i,1) = 4+i 
!       enddo
       do i = 1, nrot(1)
        print*,'irot(i,1) = ',irot(i,1)
       enddo
!       pause
 
       ! you define the guess vectors that you want 
       ! the new MO to be close to
       ! cmore(i,j,1) = < AO_i | guess_vector_MO(j) >
       ! i goes from 1 to ao_num 
       ! j goes from 1 to nrot(1) 

       ! Here you must go to the GAMESS output file 
       ! where the AOs are listed and explicited 
       ! From the basis of this knowledge you can build your 
       ! own guess vectors for the MOs
       ! The new MOs are provided in output 
       ! in the same order than the guess MOs 
!      do i = 1, nrot(1)
!       j = 5+(i-1)*15
!       cmoref(j,i,1)    = 0.2d0
!       cmoref(j+3,i,1)  = 0.12d0
!       print*,'j = ',j
!      enddo
!      pause




      print*,'passed the definition of the referent vectors '
      do i = 1, ao_num
       do j =1, ao_num
        s(i,j,1) =  ao_overlap(i,j)
       enddo
      enddo
      !Now big loop over symmetry

 

      do isym=1,nsym

      if (nrot(isym).eq.0) cycle



      write (6,*) 

      write (6,*) 

      write (6,*) 

      write (6,*) 'WORKING ON SYMMETRY',isym

      write (6,*) 



 

      !Compute the overlap matrix <ref|vec>

 



!     do i=1,nmo(isym)
      do j=1,nrot(isym)
       do i=1,ao_num
        ddum(i,j)=0.d0
        do k=1,ao_num
         ddum(i,j)=ddum(i,j)+s(i,k,isym)*cmo(k,irot(j,isym),isym)
        enddo
       enddo
      enddo



      do i=1,nrot(isym)

      do j=1,nrot(isym)

      ovl(i,j)=0.d0

      do k=1,ao_num
!     do k=1,mo_tot_num

      ovl(i,j)=ovl(i,j)+cmoref(k,i,isym)*ddum(k,j)

      enddo

      enddo

      enddo



      call maxovl(nrot(isym),nrot(isym),ovl,t,wi)



      do i=1,nrot(isym)
        do j=1,ao_num
!         write (6,*) 'isym,',isym,nrot(isym),nmo(isym)
         newcmo(j,irot(i,isym),isym)=0.d0
          do k=1,nrot(isym)
           newcmo(j,irot(i,isym),isym)=newcmo(j,irot(i,isym),isym) + cmo(j,irot(k,isym),isym)*t(k,i)
          enddo
         enddo
      enddo
!     if(dabs(newcmo(3,19,1) - mo_coef(3,19)) .gt.1.d-10 )then
!      print*,'Something wrong bitch !!'
!      print*,'newcmo(3,19,1) = ',newcmo(3,19,1)
!      print*,'mo_coef(3,19)  = ',mo_coef(3,19)
!      stop
!     endif



      enddo !big loop over symmetry

      10 format (4E19.12)


!  Now we copyt the newcmo into the mo_coef
      
      mo_coef = 0.d0
      do isym=1,nsym
       do i=1,nmo(isym)
        do j = 1, ao_num
         mo_coef(j,i) = newcmo(j,i,isym)
        enddo
       enddo
      enddo
!      pause


! we say that it hase been touched, and valid and that everything that 
! depends on mo_coef must not be reprovided
      double precision :: accu_norm
      touch mo_coef
      print*,'after  = '
      accu_norm = 0.d0
      do i =1,mo_tot_num
       accu_norm += dabs(mo_overlap(i,i))
      enddo
      print*,'accu_norm = ',accu_norm
! We call the routine that saves mo_coef in the ezfio format 
      call save_mos



   

      stop

      end

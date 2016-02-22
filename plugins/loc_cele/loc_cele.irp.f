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



      nrot(1) = 6   ! number of orbitals to be localized


      integer :: index_rot(1000,1)


       cmoref = 0.d0
 
       ! Definition of the index of the MO to be rotated
!      irot(2,1) = 21  ! the first mo to be rotated is the 21 th MO 
!      irot(3,1) = 22  ! etc....
!      irot(4,1) = 23  ! 
!      irot(5,1) = 24  ! 
!      irot(6,1) = 25  ! 
!      do i = 1,12
!        irot(i,1) = i+6
!      enddo
       irot(1,1) = 5
       irot(2,1) = 6
       irot(3,1) = 7
       irot(4,1) = 8
       irot(5,1) = 9
       irot(6,1) = 10
       do i = 1, nrot(1)
        print*,'irot(i,1) = ',irot(i,1)
       enddo
       pause
       cmoref(4,1,1) =  1.d0 ! 2S function
       cmoref(5,2,1) =  1.d0 ! 2S function
       cmoref(6,3,1) =  1.d0 ! 2S function
       cmoref(19,4,1) =  1.d0 ! 2S function
       cmoref(20,5,1) =  1.d0 ! 2S function
       cmoref(21,6,1) =  1.d0 ! 2S function
 
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
       
       ! C-C bonds 
       ! 1-2
!      i_atom = 1
!      shift = (i_atom -1) * 15
!      cmoref(1+shift,1,1) = -0.012d0 ! 2S function
!      cmoref(2+shift,1,1) =  0.18d0  ! 
!      cmoref(3+shift,1,1) =  0.1d0   ! 

!      cmoref(5+shift,1,1) = -0.1d0   ! 2pX function
!      cmoref(6+shift,1,1) = -0.1d0   ! 2pZ function

!      i_atom = 2
!      shift = (i_atom -1) * 15
!      cmoref(1+shift,1,1) = -0.012d0 ! 2S function
!      cmoref(2+shift,1,1) =  0.18d0  ! 
!      cmoref(3+shift,1,1) =  0.1d0   ! 

!      cmoref(5+shift,1,1) =  0.1d0   ! 2pX function
!      cmoref(6+shift,1,1) =  0.1d0   ! 2pZ function


!      ! 1-3
!      i_atom = 1
!      shift = (i_atom -1) * 15
!      cmoref(1+shift,2,1) = -0.012d0 ! 2S function
!      cmoref(2+shift,2,1) =  0.18d0  ! 
!      cmoref(3+shift,2,1) =  0.1d0   ! 

!      cmoref(5+shift,2,1) =  0.1d0   ! 2pX function
!      cmoref(6+shift,2,1) = -0.1d0   ! 2pZ function

!      i_atom = 3
!      shift = (i_atom -1) * 15
!      cmoref(1+shift,2,1) = -0.012d0 ! 2S function
!      cmoref(2+shift,2,1) =  0.18d0  ! 
!      cmoref(3+shift,2,1) =  0.1d0   ! 

!      cmoref(5+shift,2,1) = -0.1d0   ! 2pX function
!      cmoref(6+shift,2,1) =  0.1d0   ! 2pZ function

!      ! 4-6
!      i_atom = 4
!      shift = (i_atom -1) * 15
!      cmoref(1+shift,3,1) = -0.012d0 ! 2S function
!      cmoref(2+shift,3,1) =  0.18d0  ! 
!      cmoref(3+shift,3,1) =  0.1d0   ! 

!      cmoref(5+shift,3,1) =  0.1d0   ! 2pX function
!      cmoref(6+shift,3,1) = -0.1d0   ! 2pZ function

!      i_atom = 6
!      shift = (i_atom -1) * 15
!      cmoref(1+shift,3,1) = -0.012d0 ! 2S function
!      cmoref(2+shift,3,1) =  0.18d0  ! 
!      cmoref(3+shift,3,1) =  0.1d0   ! 

!      cmoref(5+shift,3,1) = -0.1d0   ! 2pX function
!      cmoref(6+shift,3,1) =  0.1d0   ! 2pZ function


!      ! 6-5
!      i_atom = 6
!      shift = (i_atom -1) * 15
!      cmoref(1+shift,4,1) = -0.012d0 ! 2S function
!      cmoref(2+shift,4,1) =  0.18d0  ! 
!      cmoref(3+shift,4,1) =  0.1d0   ! 

!      cmoref(5+shift,4,1) =  0.1d0   ! 2pX function
!      cmoref(6+shift,4,1) =  0.1d0   ! 2pZ function

!      i_atom = 5
!      shift = (i_atom -1) * 15
!      cmoref(1+shift,4,1) = -0.012d0 ! 2S function
!      cmoref(2+shift,4,1) =  0.18d0  ! 
!      cmoref(3+shift,4,1) =  0.1d0   ! 

!      cmoref(5+shift,4,1) = -0.1d0   ! 2pX function
!      cmoref(6+shift,4,1) = -0.1d0   ! 2pZ function


!      ! 2-4
!      i_atom = 2
!      shift = (i_atom -1) * 15
!      cmoref(1+shift,5,1) = -0.012d0 ! 2S function
!      cmoref(2+shift,5,1) =  0.18d0  ! 
!      cmoref(3+shift,5,1) =  0.1d0   ! 

!      cmoref(6+shift,5,1) =  0.1d0   ! 2pZ function

!      i_atom = 4
!      shift = (i_atom -1) * 15
!      cmoref(1+shift,5,1) = -0.012d0 ! 2S function
!      cmoref(2+shift,5,1) =  0.18d0  ! 
!      cmoref(3+shift,5,1) =  0.1d0   ! 

!      cmoref(6+shift,5,1) = -0.1d0   ! 2pZ function


!      ! 3-5
!      i_atom = 3
!      shift = (i_atom -1) * 15
!      cmoref(1+shift,6,1) = -0.012d0 ! 2S function
!      cmoref(2+shift,6,1) =  0.18d0  ! 
!      cmoref(3+shift,6,1) =  0.1d0   ! 

!      cmoref(6+shift,6,1) =  0.1d0   ! 2pZ function

!      i_atom = 5
!      shift = (i_atom -1) * 15
!      cmoref(1+shift,6,1) = -0.012d0 ! 2S function
!      cmoref(2+shift,6,1) =  0.18d0  ! 
!      cmoref(3+shift,6,1) =  0.1d0   ! 

!      cmoref(6+shift,6,1) = -0.1d0   ! 2pZ function

!      ! C-H bonds 
!      ! 2-7
!      i_atom = 2
!      shift = (i_atom -1) * 15
!      cmoref(1+shift,7,1) = -0.012d0 ! 2S function
!      cmoref(2+shift,7,1) =  0.18d0  ! 
!      cmoref(3+shift,7,1) =  0.1d0   ! 

!      cmoref(5+shift,7,1) = -0.1d0   ! 2pX function
!      cmoref(6+shift,7,1) =  0.1d0   ! 2pZ function
!      
!      i_atom = 7
!      shift_h = (6-1) * 15 + (i_atom - 6)*5
!      cmoref(1+shift_h,7,1) = 0.12d0 ! 1S function

!      ! 4-10
!      i_atom = 4
!      shift = (i_atom -1) * 15
!      cmoref(1+shift,8,1) = -0.012d0 ! 2S function
!      cmoref(2+shift,8,1) =  0.18d0  ! 
!      cmoref(3+shift,8,1) =  0.1d0   ! 

!      cmoref(5+shift,8,1) = -0.1d0   ! 2pX function
!      cmoref(6+shift,8,1) = -0.1d0   ! 2pZ function
!      
!      i_atom = 10
!      shift_h = (6-1) * 15 + (i_atom - 6)*5
!      cmoref(1+shift_h,8,1) = 0.12d0 ! 1S function

!      ! 5-11
!      i_atom = 5
!      shift = (i_atom -1) * 15
!      cmoref(1+shift,9,1) = -0.012d0 ! 2S function
!      cmoref(2+shift,9,1) =  0.18d0  ! 
!      cmoref(3+shift,9,1) =  0.1d0   ! 

!      cmoref(5+shift,9,1) =  0.1d0   ! 2pX function
!      cmoref(6+shift,9,1) = -0.1d0   ! 2pZ function
!      
!      i_atom = 11
!      shift_h = (6-1) * 15 + (i_atom - 6)*5
!      cmoref(1+shift_h,9,1) = 0.12d0 ! 1S function

!      ! 3-8 
!      i_atom = 3
!      shift = (i_atom -1) * 15
!      cmoref(1+shift,10,1) = -0.012d0 ! 2S function
!      cmoref(2+shift,10,1) =  0.18d0  ! 
!      cmoref(3+shift,10,1) =  0.1d0   ! 
!                       
!      cmoref(5+shift,10,1) =  0.1d0   ! 2pX function
!      cmoref(6+shift,10,1) =  0.1d0   ! 2pZ function
!      
!      i_atom = 8 
!      shift_h = (6-1) * 15 + (i_atom - 6)*5
!      cmoref(1+shift_h,10,1) = 0.12d0 ! 1S function

!      ! 1-9
!      i_atom = 1
!      shift = (i_atom -1) * 15
!      cmoref(1+shift,11,1) = -0.012d0 ! 2S function
!      cmoref(2+shift,11,1) =  0.18d0  ! 
!      cmoref(3+shift,11,1) =  0.1d0   ! 
!                       
!      cmoref(6+shift,11,1) =  0.1d0   ! 2pZ function

!      i_atom = 9 
!      shift_h = (6-1) * 15 + (i_atom - 6)*5
!      cmoref(1+shift_h,11,1) = 0.12d0 ! 1S function

!      
!      ! 6-12
!      i_atom = 6
!      shift = (i_atom -1) * 15
!      cmoref(1+shift,12,1) = -0.012d0 ! 2S function
!      cmoref(2+shift,12,1) =  0.18d0  ! 
!      cmoref(3+shift,12,1) =  0.1d0   ! 
!                       
!      cmoref(6+shift,12,1) = -0.1d0   ! 2pZ function

!      i_atom = 12 
!      shift_h = (6-1) * 15 + (i_atom - 6)*5
!      cmoref(1+shift_h,12,1) = 0.12d0 ! 1S function
!      cmoref(12,1,1) = 1.d0   ! 

!      cmoref(21,2,1) = 1.d0   ! 
!      cmoref(30,2,1) = 1.d0   ! 

!      cmoref(39,3,1) = 1.d0   ! 
!      cmoref(48,3,1) = 1.d0   ! 

!      cmoref(3,4,1)  = 1.d0   ! 
!      cmoref(12,4,1) =-1.d0   ! 

!      cmoref(21,5,1) = 1.d0   ! 
!      cmoref(30,5,1) =-1.d0   ! 

!      cmoref(39,6,1) = 1.d0   ! 
!      cmoref(48,6,1) =-1.d0   ! 




      print*,'passed the definition of the referent vectors '
      !Building the S (overlap) matrix in the AO basis. 

 

      do i = 1, ao_num
       do j = 1, ao_num
        s(i,j,1) = ao_overlap(i,j)
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
      do i=1,ao_num

      do j=1,nrot(isym)

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
         write (6,*) 'isym,',isym,nrot(isym),nmo(isym)
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

      10 format (4E20.12)


!  Now we copyt the newcmo into the mo_coef
      
      mo_coef = 0.d0
      do isym=1,nsym
       do i=1,nmo(isym)
        do j = 1, ao_num
         mo_coef(j,i) = newcmo(j,i,isym)
        enddo
       enddo
      enddo
!     if(dabs(newcmo(3,19,1) - mo_coef(3,19)) .gt.1.d-10 )then
      print*,'mo_coef(3,19)',mo_coef(3,19)
      pause


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

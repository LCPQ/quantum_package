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
        
        double precision               :: cmo(id1,id1,1),cmoref(id1,id1,1),newcmo(id1,id1,1)
        
        double precision               :: s(id1,id1,1),dum,ddum(id1,id1),ovl(id1,id1)
        
        double precision               :: w(id1),work(3*id1),t(id1,id1),wi(id1,id1)
        
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
        
        
        
        nrot(1) = 2   ! number of orbitals to be localized
        
        
        integer                        :: index_rot(1000,1)
        
        
        cmoref = 0.d0
        irot = 0
        
        irot(1,1) = 48
        irot(2,1) = 49
        cmoref(21,1,1)   = -0.7d0 !
        cmoref(27,1,1)   = -0.4d0 !
        cmoref(22,2,1)   =  0.7d0 !
        cmoref(28,2,1)   =  0.4d0 !
        
        ! ESATRIENE with 3 bonding and anti bonding orbitals
        ! First bonding orbital for esa
        !      cmoref(7,1,1)   = 1.d0   !
        !      cmoref(26,1,1)  = 1.d0   !
        ! Second bonding orbital for esa
        !      cmoref(45,2,1)  = 1.d0   !
        !      cmoref(64,2,1)  = 1.d0   !
        ! Third bonding orbital for esa
        !      cmoref(83,3,1)  = 1.d0   !
        !      cmoref(102,3,1) = 1.d0   !
        
        ! First anti bonding orbital for esa
        !      cmoref(7,4,1)   = 1.d0   !
        !      cmoref(26,4,1)  = -1.d0   !
        ! Second anti bonding orbital for esa
        !      cmoref(45,5,1)  = 1.d0   !
        !      cmoref(64,5,1)  = -1.d0   !
        ! Third anti bonding orbital for esa
        !      cmoref(83,6,1)  = 1.d0   !
        !      cmoref(102,6,1) = -1.d0   !
        
        ! ESATRIENE with 2 bonding and anti bonding orbitals
        ! AND 2 radical orbitals
        ! First radical orbital
        !      cmoref(7,1,1)   = 1.d0   !
        ! First bonding orbital
        !      cmoref(26,2,1)  = 1.d0   !
        !      cmoref(45,2,1)  = 1.d0   !
        ! Second bonding orbital
        !      cmoref(64,3,1)  = 1.d0   !
        !      cmoref(83,3,1)  = 1.d0   !
        ! Second radical orbital for esa
        !      cmoref(102,4,1) = 1.d0   !
        
        ! First anti bonding orbital for esa
        !      cmoref(26,5,1)  = 1.d0   !
        !      cmoref(45,5,1)  =-1.d0   !
        ! Second anti bonding orbital for esa
        !      cmoref(64,6,1)  = 1.d0   !
        !      cmoref(83,6,1)  =-1.d0   !
        
       !! ESATRIENE with 1 central bonding and anti bonding orbitals
       !! AND 4 radical orbitals
       !! First radical orbital
       !cmoref(7,1,1)   = 1.d0   !
       !! Second radical orbital
       !cmoref(26,2,1)  = 1.d0   !
       !! First bonding orbital
       !cmoref(45,3,1)  = 1.d0   !
       !cmoref(64,3,1)  = 1.d0   !
       !! Third radical orbital for esa
       !cmoref(83,4,1)  = 1.d0   !
       !! Fourth radical orbital for esa
       !cmoref(102,5,1) = 1.d0   !
       !! First anti bonding orbital
       !cmoref(45,6,1)  = 1.d0   !
       !cmoref(64,6,1)  =-1.d0   !
        
        
        do i = 1, nrot(1)
          print*,'irot(i,1) = ',irot(i,1)
        enddo
        
        
        
        
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
        double precision               :: accu_norm
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

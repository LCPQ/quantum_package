
  subroutine cal_quad(n_quad, quad, weight)
! --------------------------------------------------------------------------------
! 
! Arguments  : subroutine cal_quad
! Description: evaluates quadrature points an weights
! 
! Authors    : B. Lévy, P. Pernot
! Date       : 15 Nov 2000
! --------------------------------------------------------------------------------
  implicit none
  integer, intent(in) :: n_quad
  double precision, intent(out) :: weight(n_quad)
  double precision, intent(out) :: quad(n_quad,3)
  
! local:
    double precision, parameter :: zero=0.d0, one= 1.d0

    double precision, parameter :: p=0.707106781186547462d0
    double precision, parameter :: q=0.577350269189625842d0
    double precision, parameter :: r=0.301511344577763629d0
    double precision, parameter :: s=0.904534033733290888d0

    double precision, parameter :: fourpi= 12.5663706143591725d0

    double precision, parameter :: a6=0.166666666666666657d0
    double precision, parameter :: a18=0.333333333333333329d-01
    double precision, parameter :: b18=0.666666666666666657d-01
    double precision, parameter :: a26=0.476190476190476164d-01
    double precision, parameter :: b26=0.380952380952380987d-01
    double precision, parameter :: c26=0.321428571428571397d-01
    double precision, parameter :: a50=0.126984126984126984d-01
    double precision, parameter :: b50=0.225749559082892431d-01
    double precision, parameter :: c50=0.210937500000000014d-01
    double precision, parameter :: d50=0.201733355379188697d-01

    double precision            :: apt(3,6),bpt(3,12),cpt(3,8),dpt(3,24)
    double precision            :: awght,bwght,cwght,dwght
    double precision            :: s1, s2, s3
    integer             :: idim, ipt, i1, i2, i3, is1, is2, is3
    integer             :: iquad

! begin:
! l_here ='cal_quad'
! call enter (l_here,3)

! verifications:
!   message = 'in '//trim(l_here)//', number of dimensions='//&
!             trim(encode(dimensions_nb))//', must be 3'
!   call ensure(message, dimensions_nb .eq. 3 )

!   message = 'in '//trim(l_here)//', invalid number of quadrature points ='&
!             //trim(encode(n_quad))
!   call ensure(message,(n_quad-2)*(n_quad-6)*(n_quad-18)*(n_quad-26)*(n_quad-50) .eq. 0)

! initialize weights
    awght = zero
    bwght = zero
    cwght = zero
    dwght = zero

! type A points : (+/-1,0,0)
    awght=a6*fourpi
    ipt= 1
    apt=0.
    do idim = 1, 3
      apt(idim,ipt)=one
      ipt=ipt+1
      apt(idim,ipt)=-one
      ipt=ipt+1
    enddo     

! type B points : (+/-p,+/-p,0) with p= 1/sqrt(2)
    if(n_quad.gt.6) then

      awght=a18*fourpi
      bwght=b18*fourpi

      s1=p
      s2=p
      ipt= 1
      bpt=0.
      do idim = 1, 3
        i1=idim+1
        if(i1.gt.3) i1=i1-3
        i2=idim+2
        if(i2.gt.3) i2=i2-3
        do is1= 1,2
          do is2= 1,2
            bpt(i1,ipt)=s1
            bpt(i2,ipt)=s2
            s2=-s2
            ipt=ipt+1
          enddo      
          s1=-s1
        enddo       
      enddo    
    endif

! type C points : (+/-q,+/-q,+/-q) with q= 1/sqrt(3)
   if(n_quad.gt.18) then

      awght=a26*fourpi
      bwght=b26*fourpi
      cwght=c26*fourpi

      s1=q
      s2=q
      s3=q
      ipt= 1
      cpt=0.
      do is1= 1,2
        do is2= 1,2
          do is3= 1,2
            cpt(1,ipt)=s1
            cpt(2,ipt)=s2
            cpt(3,ipt)=s3
            s3=-s3
            ipt=ipt+1
          enddo       
          s2=-s2
        enddo       
        s1=-s1
      enddo    
    endif

! type D points : (+/-r,+/-r,+/-s)
    if(n_quad.gt.26) then

      awght=a50*fourpi
      bwght=b50*fourpi
      cwght=c50*fourpi
      dwght=d50*fourpi

      ipt= 1
      dpt=0.
      do i1= 1, 3
        s1=s
        s2=r
        s3=r
        i2=i1+1
        if(i2.gt.3) i2=i2-3
        i3=i1+2
        if(i3.gt.3) i3=i3-3
        do is1= 1,2
          do is2= 1,2
            do is3= 1,2
              dpt(i1,ipt)=s1
              dpt(i2,ipt)=s2
              dpt(i3,ipt)=s3
              s3=-s3
              ipt=ipt+1
            enddo         
            s2=-s2
          enddo        
          s1=-s1
        enddo         
      enddo     
    endif

! fill the points and weights tables
    iquad= 1
    do ipt= 1, 6
      do idim = 1, 3
        quad(iquad,idim)=apt(idim,ipt)
      enddo
      weight(iquad)=awght
      iquad=iquad+1
    enddo     

    if(n_quad.gt.6) then
      do ipt= 1,12
        do idim = 1, 3
          quad(iquad,idim)=bpt(idim,ipt)
        enddo
        weight(iquad)=bwght
        iquad=iquad+1
      enddo            
    endif

    if(n_quad.gt.18) then
      do ipt= 1,8
        do idim = 1, 3
          quad(iquad,idim)=cpt(idim,ipt)
        enddo
        weight(iquad)=cwght
        iquad=iquad+1
      enddo            
    endif

    if(n_quad.gt.26) then
      do ipt= 1,24
        do idim = 1, 3
          quad(iquad,idim)=dpt(idim,ipt)
        enddo
        weight(iquad)=dwght
        iquad=iquad+1
      enddo            
    endif

!   if (debug) then
!     write(6,*)
!     write(6,'(1X,a)') trim(l_here)//'-d : '//&
!       '------------------------------------------------------'
!     write(6,'(1X,a)') trim(l_here)//'-d : '//'  I    Weight      Quad_points'
!     write(6,'(1X,a)') trim(l_here)//'-d : '//&
!       '-----  ----------  -----------------------------------'
!     do iquad= 1, n_quad
!        write(6,'(1X,A,i5,4e12.3)') trim(l_here)//'-d : ',&
!          iquad,weight(iquad),quad(iquad,1:3)
!     enddo
!     write(6,'(1X,a)') trim(l_here)//'-d : '//&
!       '------------------------------------------------------'
!     write(6,*)
!   endif

! call exit (l_here,3)

  end subroutine cal_quad
